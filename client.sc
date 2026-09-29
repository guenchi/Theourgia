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

;;; (theourgia client) -- what a caller needs to reach a store, and
;;; nothing about what a store holds.
;;;
;;; NEVER: THIS LIBRARY IMPORTS NEITHER THE CORE NOR THE ACTOR SYSTEM. A
;;; client that had to load them would pay for the server it is trying to
;;; talk to; `f0-ondemand.sc` measured that cost at 200ms from source.
;;; The only things here are a path rule, a digest and the filesystem.
(library (theourgia client)
  (export socket-path run-root store-key
          call! answer-limit no-daemon-errno?
          ensure-daemon! serve-log-path start-budget-ms socket-dir-refusal
          request-frame envelope-version answer-field readable-shape?
          exit-code? symbol-char? wire-safe-spelling? verb-spelling-error
          next-attempt-token select-report eval-admit! eval-slots-count)
  (import (rnrs base) (rnrs control) (rnrs bytevectors) (rnrs unicode)
          ;; NOTE: `write` AND `call-with-string-output-port` ARE HERE FOR ONE
          ;; REASON: `wire-safe-spelling?` asks the writer whether a symbol
          ;; prints as its own spelling, rather than carrying a second
          ;; model of when Chez escapes one.
          (only (chezscheme) getenv guard raise sleep make-time
                current-time time-second time-nanosecond
                write call-with-string-output-port
                read open-string-input-port eof-object? with-exception-handler
                parameterize char-whitespace? get-process-id
                process close-port get-string-all)
          (only (theourgia ffi)
                real-path env-or wall-clock-ms file-size mkdir-p! sun-path-max
                path-case-sensitive? theourgia-stage
                unix-socket-connect fd-read fd-close write-all!
                spawn-detached! trace-event! fs-error? fs-error-errno
                unreadable-entry? unreadable-entry-path unreadable-entry-reason
                entry-type read-entry
                ;; F100b point 6: the pid wait, the hold seam, the record.
                waitpid-status hold-point! theourgia-fault
                with-mutation-record mutation-record
                ;; The admission of evaluations: the slot files, their locks,
                ;; the mark that keeps a lock from a child, the pool's size.
                mkdir-p-unrecorded! file-ensure-unrecorded! lock-try-acquire! lock-release!
                lock-fd fd-close-on-exec! online-processors)
          (only (theourgia answers) classify-failure)
          (only (theourgia render) render-wire)
          (only (theourgia digest) sha256 bytevector->hex))

  ;; ---- where a daemon's socket goes ----------------------------------------
  ;;
  ;; KEY: ONE FUNCTION, AND BOTH SIDES IMPORT IT. The daemon creates the
  ;; socket and every client looks for it; if the rule is written twice
  ;; they are two rules, and the day they differ a client starts a second
  ;; daemon for a store that already has one.
  ;;
  ;; NOTE: NOT BESIDE THE STORE, and this is the reason rather than a
  ;; preference: `sun_path` holds 104 bytes on macOS and FreeBSD. A store
  ;; may sit anywhere and be arbitrarily deep, so `<store>/socket` is a
  ;; path that simply fails to bind for stores that are otherwise fine --
  ;; the daemon then reports `listener-down` and looks broken.
  ;;
  ;; NOTE: AND THE STORE KEEPS NO POINTER FILE, so copying a store does not
  ;; carry a daemon with it.
  ;; NOTE: AN EMPTY VALUE IS NOT A ROOT, and `env-or` is where that rule
  ;; lives -- it was three separate length tests in three files until an
  ;; empty `THEOURGIA_RUN` put this socket directory at the filesystem
  ;; root and the daemon panicked trying to make it.
  (define (run-root)
    (or (env-or "THEOURGIA_RUN")
        (string-append (or (env-or "HOME") "/tmp") "/.theourgia/run")))

  ;; KEY: THE KEY IS THE PATH'S ONE REAL NAME. Several spellings reach one
  ;; directory -- a trailing slash, a `.`, a symlink, a relative path --
  ;; and a key derived from the spelling gives one store several
  ;; identities, which means several daemons and several locks.
  ;;
  ;; NEVER: MEASURED ON WHAT THIS REPLACES. The old key concatenated the
  ;; spelling in front of the device and inode, so `/tmp/x`, `/tmp/x/`, a
  ;; symlink to it and `/tmp/./x` produced FOUR different keys while
  ;; `stat` reported one device and one inode for all four. Its comment
  ;; said two names for one store reach one socket; they reached four.
  ;;
  ;; NOTE: A STORE THAT DOES NOT EXIST YET STILL NEEDS A KEY -- `init` is a
  ;; verb, and `serve` on a fresh directory is a thing people do. So the
  ;; LONGEST EXISTING PREFIX is resolved and the components below it are
  ;; appended.
  ;;
  ;; NEVER: RESOLVING ONLY THE PARENT IS NOT ENOUGH, AND THE WAY IT FAILS IS
  ;; QUIET. This first stopped after one level, and the comment claimed
  ;; the un-resolvable case was "stable for as long as the store does not
  ;; exist" -- true, and beside the point: the key a store gets before it
  ;; is created has to be the key it gets after. It was not. Measured on
  ;; macOS, where /tmp is a symlink to /private/tmp:
  ;;
  ;;   /tmp/probe/a, parent absent: f3b4bbe6ac2bdd72
  ;;   /tmp/probe/a, after mkdir:   a09d8755d6773226
  ;;
  ;; A caller that computed the socket path before `init` and a caller
  ;; that computed it after looked for two different sockets, and the one
  ;; that found nothing ran the store locally instead of saying so.
  ;; Climbing costs one `realpath` per absent level and returns exactly
  ;; what the old code did whenever the old code resolved anything.
  (define (split-last path)
    (let loop ((i (- (string-length path) 1)))
      (cond
        ((< i 0) (values "." path))
        ((char=? (string-ref path i) #\/)
         (values (if (zero? i) "/" (substring path 0 i))
                 (substring path (+ i 1) (string-length path))))
        (else (loop (- i 1))))))

  (define (join-under base parts)
    (if (null? parts)
        base
        (join-under (string-append (if (string=? base "/") "" base)
                                   "/"
                                   (car parts))
                    (cdr parts))))

  ;; NEVER: `.` AND `..` BELOW THE RESOLVED PREFIX ARE NOT FOLDED LEXICALLY.
  ;; Appended verbatim, `<dir>/x/a`, `<dir>/x/./a` and `<dir>/x/y/../a`
  ;; hashed to three different keys while the store did not exist, and
  ;; agreed the moment `realpath` could fold them -- a key that depends on
  ;; the spelling until the store appears.
  ;;
  ;; NEVER: AND THE FIRST FIX FOR IT WAS JUSTIFIED BY A CLAIM THAT IS FALSE.
  ;; It folded `.` and `..` as text, on the reasoning that "the prefix
  ;; came from realpath so it holds no symlinks, and the components below
  ;; it do not exist and therefore cannot be any". The second half does
  ;; not follow: folding `x/y/..` away can put back a component that DOES
  ;; exist and may be a symlink, which lexical folding then never
  ;; resolves. It also let `..` walk above the filesystem root.
  ;;
  ;; KEY: SO EACH COMPONENT IS APPLIED TO A PATH THAT IS REAL AS FAR AS IT
  ;; GOES, and resolved again as it is applied:
  ;;
  ;;   `.`   is dropped -- it names the same directory, whatever it is.
  ;;   `..`  is applied to the accumulated path and the result RESOLVED,
  ;;         so it means what the filesystem says it means, and cannot
  ;;         climb past the root.
  ;;   other is appended, and resolved if it now names something -- which
  ;;         is what picks up a symlink that came back into the path.
  ;;
  ;; What cannot be resolved stays as written: that is the part that does
  ;; not exist yet, and it is the same text whichever spelling was used.
  (define (split-on-slash text)
    (let walk ((i 0) (word '()) (out '()))
      (cond
        ((= i (string-length text))
         (reverse (if (null? word) out (cons (list->string (reverse word)) out))))
        ((char=? (string-ref text i) #\/)
         (walk (+ i 1) '() (if (null? word) out (cons (list->string (reverse word)) out))))
        (else (walk (+ i 1) (cons (string-ref text i) word) out)))))

  (define (parent-of path)
    (let-values (((dir base) (split-last path)))
      (if (string=? base "") path dir)))

  (define (step-onto here part)
    (cond
      ((string=? part ".") here)
      ((string=? part "..")
       (let ((up (parent-of here)))
         (or (real-path up) up)))
      (else
       (let ((joined (join-under here (list part))))
         (or (real-path joined) joined)))))

  (define (walk-down base parts)
    (let step ((here base) (rest parts))
      (if (null? rest)
          here
          (step (step-onto here (car rest)) (cdr rest)))))

  (define (resolved-name store)
    (or (real-path store)
        (let climb ((dir store) (tail '()))
          (let-values (((parent base) (split-last dir)))
            (cond
              ;; A trailing slash: nothing named here, keep climbing.
              ((string=? base "")
               (if (string=? parent dir) store (climb parent tail)))
              ((real-path parent)
               => (lambda (resolved)
                    (walk-down resolved (split-on-slash
                                          (join-under "" (cons base tail))))))
              ;; Reached the top without resolving anything at all.
              ((string=? parent dir) store)
              (else (climb parent (cons base tail))))))))

  ;; NEVER: THE KEY MUST NOT CHANGE WHEN THE STORE APPEARS, and on a
  ;; case-insensitive filesystem it did. `resolved-name` resolves as far
  ;; as the path exists and keeps the caller's spelling for the rest, so
  ;; while the store is absent `.../Foo` and `.../foo` produce two names
  ;; -- and once it exists `realpath` answers the filesystem's spelling
  ;; for both, so one of those two keys changes under a client that is
  ;; still running. Measured on macOS, which is one of the two platforms
  ;; this ships to.
  ;;
  ;; NEVER: AND IT IS ASKED OF THE FILESYSTEM, NOT ASSUMED FROM THE PLATFORM.
  ;; A case-sensitive volume mounted on a case-insensitive machine is
  ;; ordinary, and folding there would give one key to two stores that
  ;; really are different -- the worse direction of the two. `unknown`
  ;; therefore folds nothing.
  ;;
  ;; NOTE: THE QUESTION IS PUT TO THE NEAREST EXISTING DIRECTORY, because a
  ;; path that is not there yet has no filesystem to answer for it, and
  ;; the absent store is the whole case this exists for.
  ;; THROUGH THE DOOR (F100a): absent climbs, as before; a stat that fails
  ;; for any other reason raises instead of climbing past a directory it
  ;; could not see into.
  (define (nearest-existing path)
    (if (not (eq? (entry-type path) 'absent))
        path
        (let-values (((parent base) (split-last path)))
          (if (string=? parent path) path (nearest-existing parent)))))

  (define (key-name store)
    (let ((name (resolved-name store)))
      (if (eq? #f (path-case-sensitive? (nearest-existing name)))
          (string-downcase name)
          name)))

  (define (store-key store)
    (substring (bytevector->hex (sha256 (string->utf8 (key-name store)))) 0 16))

  (define (socket-path store)
    (string-append (run-root) "/" (store-key store) "/socket"))

  ;; ---- the admission of evaluations ---------------------------------------
  ;;
  ;; KEY: A BOUND ON HOW MANY EVALUATIONS RUN AT ONCE, AT THE ENTRANCE EVERY
  ;; core.sc EVALUATION PASSES. Without it N shells give N evaluations, each
  ;; within its own quotas. The bound is K slots, K files
  ;; <run root>/eval-slots/<i>; an evaluation runs while it holds an
  ;; exclusive flock on one of them, and the lock is released by the system
  ;; when the process that took it ends -- so a holder that dies frees its
  ;; slot, and a reused pid cannot hold one.
  ;;
  ;; NEVER: A CHILD DOES NOT INHERIT THE SLOT. The descriptor is marked
  ;; close-on-exec as soon as the lock is taken, before the worker or the
  ;; runner is spawned (proc-spawn! marks nothing): a runner outliving its
  ;; supervisor would otherwise hold the slot. A mark that fails releases
  ;; the lock and raises, as the descriptor's failure.
  ;;
  ;; NEVER: THE SLOT FILES ARE ADMINISTRATION, NOT THE REQUEST'S WRITES. The
  ;; directory and the files are made, when absent, with the unrecorded
  ;; variants, so a first use of a run root does not appear in an unrelated
  ;; refusal's written clause. They are never removed or replaced while held.
  ;;
  ;; NOTE: WHAT THIS BOUNDS: cooperating clients of one run root, the command
  ;; line and MCP shells alike. It is not a machine-wide bound on process
  ;; trees: an evaluation library called directly, or a worker started by
  ;; hand, does not pass here; two run roots are two pools; and the slot is
  ;; released when core.sc ends even if a descendant it orphaned is still
  ;; running. K is operator configuration, and two processes with different
  ;; K on one root see different ranges -- a mismatch is not detected.

  ;; K: THEOURGIA_EVAL_SLOTS in this process's environment, else the online
  ;; processors (ffi.sc's one reading, with its test seam). -> a positive
  ;; integer, or #f when the value is not one -- never unlimited by accident.
  (define (eval-slots-count)
    (let ((v (env-or "THEOURGIA_EVAL_SLOTS")))
      (if v
          (let ((n (string->number v 10)))
            (and n (exact? n) (integer? n) (> n 0) n))
          (online-processors))))

  ;; -> (values <lock> #f) once a slot is held, or (values #f <refusal>):
  ;; (error bad-request (reason eval-slots)) for a K that is not a positive
  ;; integer, (error eval-busy (slots K) (waited-ms n)) when no slot came
  ;; free within timeout-ms. The wait is not part of the evaluation's own
  ;; deadline, which starts at ready. A filesystem condition making the
  ;; slots raises, for the request's table.
  ;;
  ;; NEVER: NO ATTEMPT AFTER THE BUDGET. The slots are tried at once, then
  ;; every 100 ms, each sleep cut to what is left of timeout-ms, and the
  ;; budget is read before EVERY attempt, each slot's included: no attempt
  ;; begins once a reading has reached timeout-ms (one begun just before can
  ;; finish just after), and a refusal waits timeout-ms, not the next whole
  ;; step. The one exception is the very
  ;; first attempt, on the first slot, which is made whatever the clock
  ;; reads: a millisecond boundary crossed at the start must not refuse a
  ;; request with a 1 ms budget on an empty pool. The wait is measured on
  ;; the monotonic clock, as the MCP shell's watchdog is, so a step of the
  ;; wall clock neither stretches it nor cuts it short.
  (define (monotonic-ms)
    (let ((t (current-time 'time-monotonic)))
      (+ (* 1000 (time-second t)) (div (time-nanosecond t) 1000000))))

  (define (eval-admit! timeout-ms)
    (let ((k (eval-slots-count)))
      (if (not k)
          (values #f '(error bad-request (reason eval-slots)))
          (let ((dir (string-append (run-root) "/eval-slots")))
            (define (slot i) (string-append dir "/" (number->string i)))
            (mkdir-p-unrecorded! dir)
            (let make ((i 0))
              (when (< i k)
                (file-ensure-unrecorded! (slot i))
                (make (+ i 1))))
            (hold-point! 'eval-admission)
            (let ((start (monotonic-ms)))
              (define (elapsed) (- (monotonic-ms) start))
              (define (busy waited)
                (values #f (list 'error 'eval-busy (list 'slots k) (list 'waited-ms waited))))
              (let try ((first? #t))
                (let ((held (let scan ((i 0))
                              (and (< i k)
                                   (or (and first? (= i 0)) (< (elapsed) timeout-ms))
                                   (or (lock-try-acquire! (slot i) 'exclusive)
                                       (scan (+ i 1)))))))
                  (if held
                      (begin
                        (guard (e (#t (guard (x (#t #f)) (lock-release! held)) (raise e)))
                          (parameterize ((theourgia-stage 'admission))
                            (fd-close-on-exec! (lock-fd held))))
                        (values held #f))
                      (let ((waited (elapsed)))
                        (if (>= waited timeout-ms)
                            (busy waited)
                            (begin
                              (sleep (make-time 'time-duration
                                                (* 1000000 (min 100 (- timeout-ms waited))) 0))
                              (let ((waited (elapsed)))
                                (if (>= waited timeout-ms)
                                    (busy waited)
                                    (try #f))))))))))))))

  ;; ---- one call: connect, send a frame, read one answer, close -------------
  ;;
  ;; NEVER: THIS IS ALL THE CLIENT KNOWS ABOUT THE PROTOCOL. It does not read
  ;; the answer, does not know which verbs exist and does not know what
  ;; any of them mean; it hands back the bytes between the frame it sent
  ;; and the newline that ended the reply.
  ;;
  ;; NOTE: ONE CONNECTION PER CALL, AND THAT IS NOT A SIMPLIFICATION TO BE
  ;; OPTIMISED AWAY LATER. The daemon closes an idle connection after its
  ;; frame timeout, so a client that held one open to reuse would race
  ;; that close on every call after the first and lose whichever request
  ;; it sent into the gap.

  ;; The same ceiling the listener applies to a frame. An answer that
  ;; passes it is not truncated and used -- it is refused, because a
  ;; truncated s-expression read back as a smaller one is a wrong answer
  ;; rather than a missing one.
  (define (answer-limit) (* 32 1024 1024))

  ;; KEY: POSITIVE libc ERRNO VALUES, MEASURED 2026-09-18 on macOS 25.3.0
  ;; and FreeBSD 15.0-RELEASE, where all three agree:
  ;;
  ;;   ENOENT 2   ECONNREFUSED 61   ENOTSOCK 38
  ;;
  ;; NEVER: NOT THE LIST IN `rpc.sc`. That one holds NEGATIVE libuv status
  ;; codes (-2, -61, -111...), which is a different numbering from a
  ;; different library; -111 is there because Linux's ECONNREFUSED is
  ;; 111, and mixing the two would make a plain `read` failure look like
  ;; "no daemon" on one platform and nothing on another.
  ;;
  ;; NOTE: LINUX DIFFERS AND IS NOT MEASURED (ECONNREFUSED 111, ENOTSOCK
  ;; 88). It does not need a branch yet only because `sockaddr-un`
  ;; refuses to build an address there at all; when that platform is
  ;; measured, this list is the second place to change, and it says so
  ;; here rather than waiting to be found.
  (define ENOENT 2)
  (define ECONNREFUSED 61)
  (define ENOTSOCK 38)

  (define (no-daemon-errno? code)
    (or (equal? code ENOENT)
        (equal? code ECONNREFUSED)
        (equal? code ENOTSOCK)))

  ;; The three shapes of outcome, and the caller can tell them apart:
  ;;   (answer <bytevector>)      a whole reply
  ;;   (no-daemon <errno>)        nothing is listening; starting one is
  ;;                              a reasonable next move
  ;;   (transport-error <what>)   something else went wrong, and
  ;;                              NEVER: retrying locally is NOT a reasonable
  ;;                              next move: the request may have been
  ;;                              taken and acted on.
;; NEVER: A PATH THAT CANNOT FIT IS AN ANSWER, NOT AN EXCEPTION. `sockaddr-un`
  ;; refuses a path longer than `sun_path`, correctly and loudly -- but it
  ;; refuses by raising, and nothing above caught it, so a run root long
  ;; enough to push the socket past 104 bytes ended the client with
  ;;
  ;;   Exception in sockaddr-un: socket path does not fit in sun_path
  ;;
  ;; on stderr and no answer at all. This program's whole contract is that
  ;; stdout carries the answer and the exit code is the verdict; a raw
  ;; condition is neither.
  ;;
  ;; NOTE: CHECKED BEFORE CONNECTING, not caught afterwards. The length is
  ;; knowable without touching the system, and reading it here means no
  ;; guess about which raised condition was which.
  (define (socket-path-fits? path)
    (< (bytevector-length (string->utf8 path)) (sun-path-max)))

  (define (path-too-long path)
    (list 'error 'socket-path-too-long
          (list 'path path)
          (list 'length (bytevector-length (string->utf8 path)))
          (list 'max (sun-path-max))))

  (define (call! path frame timeout-ms)
    (if (not (socket-path-fits? path))
        ;; NEVER: `not-sent`, NOT `transport-error`. Nothing went out, so the
        ;; outcome is not unknown -- it is known, and it is that this
        ;; request did not happen. Reported as a transport error the
        ;; caller was told "execution may be unknown", which is the same
        ;; untrue sentence the MCP shell used to produce for a server that
        ;; would not start.
        (list 'not-sent (path-too-long path))
        (call-on-socket path frame timeout-ms)))

  ;; NEVER: ONLY A FAILURE TO CONNECT MAY BE CALLED `no-daemon`. The guard
  ;; used to enclose the exchange as well, so an error raised AFTER the
  ;; frame had gone out could be classified as "nothing was listening" --
  ;; and the caller answers that by starting a server and sending again.
  ;; A request whose outcome is unknown would have been repeated.
  ;;
  ;; NOTE: Whether an errno in that list can actually occur after a
  ;; successful connect is beside the point: the classification must be
  ;; scoped to the step it describes, not to whatever the guard happens to
  ;; enclose.
  (define (call-on-socket path frame timeout-ms)
    ;; NEVER: EVERYTHING THAT FAILS HERE FAILED BEFORE ANYTHING WAS SENT, and
    ;; that is decided by WHERE the failure happened, not by which errno
    ;; it carried. Classified by errno, a connect failure outside the
    ;; three-name list was reported as a transport error -- which the
    ;; callers render as "execution may be unknown" -- for a request that
    ;; provably never left. EACCES on the socket's directory is the
    ;; ordinary way to reach that.
    ;;
    ;; NOTE: THE ERRNO STILL DECIDES ONE THING, and only one: whether it is
    ;; worth starting a server. "Was anything sent" and "should I start
    ;; one" are two questions, and they were being answered by one test.
    (let ((fd (guard (e ((fs-error? e)
                         (let ((code (fs-error-errno e)))
                           (if (no-daemon-errno? code)
                               (list 'no-daemon code)
                               (list 'not-sent
                                     (list 'error 'connect-failed
                                           (list 'path path)
                                           (list 'errno code)))))))
                (unix-socket-connect path timeout-ms))))
      (if (pair? fd)
          fd
          ;; NOTE: THE STEP HAS A NAME so that a failure in it can be armed.
          ;; `close-fail@client` makes the close below refuse, which is
          ;; the only way to ask what this does when it does -- and what
          ;; it used to do was throw away a complete answer.
          (parameterize ((theourgia-stage 'client))
          ;; NEVER: FROM HERE ON, EVERY FAILURE IS A TRANSPORT ERROR. The bytes
          ;; may have gone out, so the one thing this must never say is
          ;; that nobody was reached.
          (guard (e ((fs-error? e)
                     (close-noting-failure fd)
                     (list 'transport-error (fs-error-errno e))))
            (let ((outcome (exchange-on fd frame timeout-ms path)))
              ;; NEVER: THE ANSWER IS THE RESULT, AND CLOSING CANNOT UNMAKE IT.
              ;; This close used to sit inside the guard with its failure
              ;; treated like any other: a close that raised threw away a
              ;; complete answer, reported `transport-error` -- "it may or
              ;; may not have happened" -- for a request that demonstrably
              ;; had, and then closed a second time from the guard clause,
              ;; where a second failure escaped the guard altogether.
              ;; Measured: close errors 4 then 9 raised 9 and the answer
              ;; was gone.
              (close-noting-failure fd)
              outcome))))))

  ;; A DESCRIPTOR WE ARE DONE WITH. The failure is worth knowing about and
  ;; is worth nothing to the caller, who already has the outcome.
  (define (close-noting-failure fd)
    (guard (e ((fs-error? e)
               (trace-event! 'close-failed (fs-error-errno e) #f)
               #f))
      (fd-close fd)))

  ;; NEVER: A REQUEST THAT DEMONSTRABLY DID NOT LEAVE IS `not-sent`, AND THE
  ;; PROOF IS A COUNT. Every failure from here down used to be classified
  ;; by the guard above as `transport-error` -- "it may have been carried
  ;; out" -- including a write that failed before a single byte went out.
  ;; `write-all!` reports its progress to a callback; a failure with the
  ;; count still at zero is the same situation as a failed connect, and is
  ;; answered the same way. Once any byte has gone, the outcome is
  ;; genuinely unknown and stays so.
  (define (exchange-on fd frame timeout-ms path)
    (let* ((sent 0)
           (unsent (guard (e ((and (fs-error? e) (= sent 0))
                              (list 'not-sent
                                    (list 'error 'write-failed
                                          (list 'path path)
                                          (list 'errno (fs-error-errno e))))))
                     (write-all! fd frame path (lambda (n) (set! sent n)))
                     #f)))
      (if unsent
          unsent
          (exchange-read fd timeout-ms))))

  (define (exchange-read fd timeout-ms)
    (let ((deadline (+ (wall-clock-ms) timeout-ms)))
      (let loop ((chunks '()) (total 0))
        (cond
          ((> total (answer-limit))
           (list 'transport-error 'answer-too-large))
          ((>= (wall-clock-ms) deadline)
           (list 'transport-error 'timeout))
          (else
           (let ((chunk (fd-read fd 65536)))
             (cond
               ;; NEVER: EOF BEFORE THE TERMINATOR IS NOT AN EMPTY ANSWER. The
               ;; daemon may have taken the request and died after acting
               ;; on it, so what this reports is that the outcome is
               ;; UNKNOWN -- never that nothing happened.
               ((zero? (bytevector-length chunk))
                (list 'transport-error 'lost-answer))
               (else
                (let ((chunks (cons chunk chunks))
                      (total (+ total (bytevector-length chunk))))
                  ;; NOTE: THE TERMINATOR IS THE LAST BYTE, not "a newline
                  ;; somewhere", which is the weaker rule `datum-line?`
                  ;; applies on the other two paths. Both are safe for
                  ;; the same reason -- the renderer writes a newline
                  ;; inside a string as the two characters \n, so the
                  ;; only raw byte 10 in an answer is the one that ends
                  ;; it -- and this one does not depend on that being
                  ;; true.
                  ;; NEVER: THE CEILING IS CHECKED BEFORE THE ANSWER IS
                  ;; RETURNED, not only at the top of the loop. Tested
                  ;; only there, a final chunk carrying the terminator
                  ;; came back whatever its size: the limit held for
                  ;; answers that arrived in pieces and not for the one
                  ;; that arrived in one.
                  (if (> total (answer-limit))
                      (list 'transport-error 'answer-too-large)
                  (if (= 10 (bytevector-u8-ref chunk (- (bytevector-length chunk) 1)))
                      (list 'answer (join-bytes (reverse chunks) total))
                      (loop chunks total)))))))))))) 

  (define (join-bytes chunks total)
    (let ((out (make-bytevector total)))
      (let place ((chunks chunks) (at 0))
        (if (null? chunks)
            out
            (let ((n (bytevector-length (car chunks))))
              (bytevector-copy! (car chunks) 0 out at n)
              (place (cdr chunks) (+ at n)))))))

  ;; ---- starting the daemon this store has not got yet ---------------------
  ;;
  ;; KEY: READINESS IS A SUCCESSFUL CONNECTION, not a file appearing. The
  ;; socket exists from the moment it is bound, which is before the
  ;; daemon is listening on it, and a client that raced to the file would
  ;; connect into nothing and call the daemon broken.
  (define (start-budget-ms) 10000)

  ;; THE NAME OF ONE START (F100b item 3): 16 lowercase hex characters, the
  ;; client's pid then a counter, each 8 digits. The daemon echoes it as the
  ;; last clause of every startup report, so the report this start's daemon
  ;; wrote is told from any other in the shared log. A new one per start:
  ;; two starts from one process (the MCP shell) must not share one (H7, E9).
  (define attempt-counter 0)
  (define (hex8 n)
    (let ((h (number->string (mod n 4294967296) 16)))
      (string-append (make-string (- 8 (string-length h)) #\0) (string-downcase h))))
  (define (next-attempt-token)
    (set! attempt-counter (+ attempt-counter 1))
    (string-append (hex8 (get-process-id)) (hex8 attempt-counter)))

  ;; The daemon's own output, beside its socket. NEVER: THE CLIENT DECIDES
  ;; THIS AND PASSES IT: the daemon does not work the path out for
  ;; itself. Two sides deriving one path separately is the shape that
  ;; already cost a batch here -- a socket path computed before a store
  ;; existed and again after did not agree -- and this path has the same
  ;; ingredients.
  (define (serve-log-path store)
    (string-append (run-root) "/" (store-key store) "/serve.log"))

  ;; NOTE: ONLY WHAT THIS START WROTE. The log is appended to across every
  ;; start against this store, so the error from a previous failure is
  ;; sitting in it: reporting the last error in the whole file would
  ;; answer today's question with last week's answer, and it would look
  ;; entirely plausible. The length is taken BEFORE the spawn and nothing
  ;; before it is ever read.
  ;; THROUGH THE DOOR (F100a): absent is 0 as before; otherwise the size is
  ;; taken by OPENING the log (file-size), as the base did, so a log that is
  ;; there and cannot be read raises unreadable-entry and start-one! refuses
  ;; the start naming it (K8). A stat would succeed on a 000 log and let the
  ;; start go ahead: measured, F100a suite-1's K8 row.
  (define (log-length path)
    (if (eq? (entry-type path) 'absent) 0 (file-size path)))

  (define (ensure-daemon! argv store socket)
    ;; NEVER: ASK BEFORE STARTING ONE. Spawning unconditionally works -- the
    ;; second process loses the daemon's lock and exits, so there is
    ;; still exactly one -- but it costs a process start on every call
    ;; and, worse, it appends that loser's refusal to the log. That line
    ;; is an `(error ...)` form sitting in the file the NEXT failed start
    ;; will be read from; the offset rule is what keeps it from being
    ;; relayed, and a rule that has to keep saving you is one to stop
    ;; leaning on.
    (cond
      ;; NOTE: THE SAME REFUSAL BEFORE STARTING ANYTHING. Spawning a server
      ;; onto a path it cannot bind would leave it to fail in the log and
      ;; report itself as "would not start", which is true and hides the
      ;; one fact that would fix it.
      ((not (socket-path-fits? socket)) (path-too-long socket))
      ((connects? socket) 'ready)
      ;; AND A SOCKET WHOSE DIRECTORY IS NOT THERE, for the same reason: the
      ;; daemon refuses it (theourgiad.sc) and the start would be read back
      ;; out of the log only after the whole start budget. The default
      ;; socket is exempt, because its directory is the run directory this
      ;; client makes in start-one!, before the daemon is spawned.
      ((and (not (string=? socket (socket-path store)))
            (socket-dir-refusal socket))
       => (lambda (refusal) refusal))
      (else (start-one! argv store socket))))

  ;; -> #f when the directory a socket goes in is a directory, otherwise
  ;; the refusal, as a datum. ONE TEXT FOR BOTH PLACES THAT REFUSE: the
  ;; daemon's `serve` (theourgiad.sc, for a --socket given by hand) and the
  ;; start above. A socket is never given a directory by being served: the
  ;; directory is the caller's to make (F15, as F82 for export-md).
  ;;
  ;;   absent, or a file where the directory must be
  ;;       -> (error socket-dir-missing (dir <dir>))
  ;;   a directory on the way that cannot be searched
  ;;       -> (error unreadable (path <p>) (reason <r>)), R1's naming; it
  ;;          is not "missing", and saying so would send the caller looking
  ;;          for a directory that is there.
  (define (socket-dir-refusal socket)
    (let ((dir (dirname-of socket)))
      (guard (e ((unreadable-entry? e)
                 (list 'error 'unreadable
                       (list 'path (unreadable-entry-path e))
                       (list 'reason (unreadable-entry-reason e)))))
        (if (eq? (entry-type dir) 'directory)
            #f
            (list 'error 'socket-dir-missing (list 'dir dir))))))

  ;; ---- starting one, and saying why it did not start (F100b point 6) ------
  ;;
  ;; A START IS NAMED, WAITED FOR BY ITS PID, AND ANSWERED FROM ITS OWN REPORT.
  ;; - The token (next-attempt-token) is passed as `--attempt`; the daemon
  ;;   echoes it as the last clause of every startup report (item 3).
  ;; - The client's own filesystem work -- the run directory -- is in a
  ;;   record scope of its own. Its entries go ONLY in a `(client-written
  ;;   ...)` clause, present when non-empty; the client never merges them
  ;;   into the report's `written` (item 2, M2b Q-1).
  ;; - The daemon's pid is retained and polled with waitpid-status in the
  ;;   socket wait (item 4). When it has exited before the socket appeared,
  ;;   the log is read -- ONLY THEN, or when the budget is spent -- and the
  ;;   report carrying this start's token is selected (select-report).
  ;; - The answer, item 6: `(error serve-start-failed (kind K) <the report's
  ;;   clauses after its kind, verbatim, attempt included> (exit n | signal
  ;;   n) [(client-written ...)])`; with no report carrying the token,
  ;;   `(kind exited) (exit n | signal n) (attempt T)`; a daemon still alive
  ;;   at the budget with no socket, `(kind timeout) (pid p) (log <path>)`,
  ;;   and the daemon is LEFT ALONE.
  ;; - A failure of the client's own step (the run directory, the log's
  ;;   length, the spawn) is the table's answer over an EMPTY record, in the
  ;;   same `(kind K) <clauses>` shape, with client-written (M2 Q6).
  (define (start-one! argv store socket)
    (let* ((token (next-attempt-token))
           (argv (append argv (list "--attempt" token)))
           (log-path (serve-log-path store))
           (outcome
            (with-mutation-record
              (lambda ()
                (let ((answer
                       (guard (e ((classify-failure e '())
                                  => (lambda (a) (own-start-failure a))))
                         (start-watched! argv socket log-path token))))
                  (cons answer (mutation-record)))))))
      (with-client-written (car outcome) (cdr outcome))))

  ;; The table's answer for the client's own step, as serve-start-failed.
  (define (own-start-failure a)
    (append (list 'error 'serve-start-failed (list 'kind (cadr a))) (cddr a)))

  ;; client-written, when non-empty, as the last clause of a refusal.
  (define (with-client-written answer entries)
    (if (and (pair? answer) (eq? (car answer) 'error) (pair? entries))
        (append answer (list (list 'client-written entries)))
        answer))

  (define (start-watched! argv socket log-path token)
    ;; THE LOG'S LENGTH IS TAKEN BEFORE THE SPAWN, and nothing before it is
    ;; read (see log-length). NOT under stage `client`: D8 puts the reads
    ;; AFTER the exit there (exited-answer), and a client-stage fault aimed
    ;; at the connection's close (CS-6's close-fail@client) must not fire in
    ;; this length probe.
    (let ((before (log-length log-path)))
      ;; NEVER: MAKING THE LOG'S DIRECTORY IS PART OF STARTING ONE; its
      ;; failure is the client's own (above).
      (mkdir-p! (dirname-of log-path))
      (when (eq? (theourgia-fault) 'client-extra-child) (extra-child!))
      ;; KEY: ONE EVENT PER PROCESS ACTUALLY STARTED, so "did it start
      ;; one?" is a count and not a matter of looking soon enough.
      (let ((pid (spawn-detached! argv)))
        (trace-event! 'spawn pid #f)
        (let ((deadline (+ (wall-clock-ms) (start-budget-ms))))
          (let wait ()
            (cond
              ((connects? socket) 'ready)
              ((waitpid-status pid)
               => (lambda (status) (exited-answer status log-path before token)))
              ((>= (wall-clock-ms) deadline)
               ;; THE DAEMON IS LEFT ALONE: it may be about to bind (E8).
               (list 'error 'serve-start-failed '(kind timeout) (list 'pid pid) (list 'log log-path)))
              (else
               (sleep (make-time 'time-duration 50000000 0))
               (wait))))))))

  ;; THE LOG IS READ ONLY HERE, AFTER THE EXIT WAS OBSERVED (item 4), under
  ;; stage `client` (D8), behind the client-scan hold (item 7). A reader
  ;; failure is the table's unreadable naming the log, with the status.
  (define (exited-answer status log-path before token)
    (hold-point! 'client-scan)
    (let ((bytes (guard (e ((unreadable-entry? e) e))
                   (parameterize ((theourgia-stage 'client))
                     (let ((b (read-entry log-path))) (if (bytevector? b) b (make-bytevector 0)))))))
      (if (unreadable-entry? bytes)
          (append (own-start-failure (classify-failure bytes '())) (list status))
          (let ((report (select-report bytes before token)))
            (if report
                (append (list 'error 'serve-start-failed (list 'kind (cadr report)))
                        (cddr report)
                        (list status))
                (list 'error 'serve-start-failed '(kind exited) status (list 'attempt token)))))))

  ;; INJECTION ONLY (`client-extra-child@client`, G6): a child of this
  ;; process that has exited and not been reaped, observed as a zombie
  ;; before the daemon is spawned, so a wait that collects ANY child would
  ;; take its status for the daemon's (PID2).
  (define (extra-child!)
    (let ((pid (spawn-detached! (list "/bin/sh" "-c" "exit 0"))))
      (let poll ((k 0))
        (unless (or (>= k 100) (zombie? pid))
          (sleep (make-time 'time-duration 20000000 0))
          (poll (+ k 1))))))
  (define (zombie? pid)
    (let* ((p (process (string-append "ps -o stat= -p " (number->string pid))))
           (t (get-string-all (car p))))
      (close-port (car p)) (close-port (cadr p))
      (and (string? t) (> (string-length t) 0)
           (let loop ((i 0))
             (and (< i (string-length t))
                  (or (char=? (string-ref t i) #\Z) (loop (+ i 1))))))))

  ;; ---- the report this start's daemon wrote (item 3) ----------------------
  ;;
  ;; A PURE FUNCTION OF BYTES, AN OFFSET AND A TOKEN, exported for H3.
  ;; - The offset is in BYTES (the log's length before the spawn); the
  ;;   scan starts there if it is 0 or just after a newline, else at the
  ;;   first newline at or after it -- a line begun before the offset is
  ;;   not this start's.
  ;; - Only WHOLE lines are read: an unterminated tail is not a report yet.
  ;; - A line that does not parse is skipped, not fatal.
  ;; - The report is the LAST line whose datum carries `(attempt T)`; none
  ;;   gives #f.
  ;; - Each line is peer text and is asked readable-shape? first.
  (define (select-report bytes offset token)
    (let* ((n (bytevector-length bytes))
           (start (cond
                    ((or (<= offset 0) (> offset n)) (if (> offset n) n 0))
                    ((= 10 (bytevector-u8-ref bytes (- offset 1))) offset)
                    (else (let find ((i offset))
                            (cond ((>= i n) n)
                                  ((= 10 (bytevector-u8-ref bytes i)) (+ i 1))
                                  (else (find (+ i 1)))))))))
      (let loop ((i start) (from start) (found #f))
        (cond
          ((>= i n) found)
          ((= 10 (bytevector-u8-ref bytes i))
           (loop (+ i 1) (+ i 1) (let ((d (line-datum bytes from i)))
                                   (if (carries-token? d token) d found))))
          (else (loop (+ i 1) from found))))))

  ;; A LINE IS A REPORT ONLY IF IT IS ONE DATUM (M2b1 review r1, F1): text
  ;; after the datum, even an unmatched parenthesis, makes the whole line
  ;; not a report, so a second read must find the end.
  ;; NO GUARD AROUND THE DECODE (F111; F100b M3a r1's reading): Chez's
  ;; utf8->string never raises on invalid bytes, it gives U+FFFD for a lone
  ;; continuation byte, an overlong form and a truncated sequence. Outside a
  ;; string, readable-shape? refuses a line holding one; inside a string it is
  ;; an ordinary character and the line reads as it did before (M3b review
  ;; r1, F3). A guard with no case behind it is not kept.
  (define (line-datum bytes from to)
    (let ((text (let ((b (make-bytevector (- to from))))
                  (bytevector-copy! bytes from b 0 (- to from))
                  (utf8->string b))))
      (and (readable-shape? text)
           (guard (e (#t #f))
             (let* ((port (open-string-input-port text))
                    (d (read port)))
               (and (not (eof-object? d))
                    (eof-object? (read port))
                    d))))))

  ;; THE SHAPE THE DAEMON WRITES, AND ONLY IT (M2b1 review r1, F2):
  ;; `(error <kind> <clause> ...)`, each clause a list headed by a symbol,
  ;; the LAST clause exactly `(attempt T)`. A line carrying the token in any
  ;; other shape is not this start's report: its kind would be relayed as
  ;; whatever stood in the kind's place.
  (define (carries-token? d token)
    (and (list? d) (>= (length d) 3)
         (eq? (car d) 'error) (symbol? (cadr d))
         (let clauses ((cs (cddr d)))
           (cond
             ((null? cs) #t)
             ((and (list? (car cs)) (pair? (car cs)) (symbol? (caar cs))) (clauses (cdr cs)))
             (else #f)))
         (equal? (list-ref d (- (length d) 1)) (list 'attempt token))))

  (define (connects? socket)
    (guard (e ((fs-error? e) #f))
      (let ((fd (unix-socket-connect socket 1000)))
        (fd-close fd)
        #t)))

  (define (dirname-of path)
    (let-values (((dir base) (split-last path))) dir))

  ;; ---- the envelope a request travels in ----------------------------------
  ;;
  ;; NEVER: ONE PLACE, AND THIS IS IT. The client packs it and the daemon
  ;; unpacks it, which are two processes and two libraries; what must not
  ;; be two is the SHAPE. It lives here rather than in `rpc.sc` because
  ;; the client cannot import `rpc.sc` -- loading the dispatcher is the
  ;; cost this whole split exists to avoid -- and `rpc.sc` re-exports this
  ;; name so that nothing which used to get it from there had to change.
  ;;
  ;; NOTE: `(theourgia render)` IMPORTS NOTHING BUT `(chezscheme)`, which is
  ;; why reaching it from here does not undo the point of the split. That
  ;; is a fact about that library today, not a promise it makes; a day it
  ;; grows an import, this import is one of the places to look.
  (define (all-strings? xs)
    (or (null? xs)
        (and (string? (car xs)) (all-strings? (cdr xs)))))

  (define (field key fields default)
    (let look ((fs fields))
      (cond
        ((null? fs) default)
        ((and (pair? (car fs)) (eq? (caar fs) key)) (cdar fs))
        (else (look (cdr fs))))))

  (define (envelope-version) 1)

  ;; NEVER: THE WIRE FORM IS POSITIONAL; THIS PROCEDURE IS NOT, and the
  ;; difference is deliberate. Five of the fields are strings standing
  ;; next to each other -- store, actor, writer, cwd, stdin -- and a
  ;; caller that swapped two of them would produce a frame that parses,
  ;; dispatches, and does the work as somebody else: the actor used as
  ;; the writer writes into another agent's drafts, and nothing anywhere
  ;; reports it. So the caller names its fields and only this procedure
  ;; knows their order.
  ;;
  ;; NOTE: `writer` IS ALWAYS PRESENT AND IS #f WHEN UNBOUND. Not omitted:
  ;; an envelope whose length varies is one the reader has to guess
  ;; about, and "unbound" is a thing to say rather than a thing to leave
  ;; out. The core refuses a draft verb that arrives with #f there.
  ;; NEVER: WHAT THE READER MAY BE HANDED, ASKED BEFORE IT IS HANDED ANYTHING.
  ;; `read` implements the whole of Scheme's lexical syntax, and two parts
  ;; of it are not safe to run on bytes somebody else wrote:
  ;;
  ;;   - a numeric literal builds its value before anything can judge it.
  ;;     `#e1e100000` is eleven characters asking for a 332193-bit integer;
  ;;     measured, 1e5 -> 3.3 ms, 1e6 -> 69 ms, 5e6 -> 756 ms, and the
  ;;     exponent costs the sender one character each time.
  ;;   - a datum label makes a CYCLE. `#0=(answer (x 1) . #0#)` reads
  ;;     perfectly well and then every walk over it runs forever: measured
  ;;     on the field reader below, a timeout at 8 s, no answer, no refusal.
  ;;
  ;; NEVER: SO BOTH DIRECTIONS ARE GUARDED BY THIS ONE RULE. The daemon asks it
  ;; of a request and the three programs that read a reply ask it of a
  ;; reply: a request and a reply are read by the SAME reader, so what it
  ;; must not be handed is one fact and not two.
  ;;
  ;; NEVER: A WHITELIST, AND THE REASON IS TWO ROUNDS OF THE SAME DEFECT. This
  ;; was a blacklist: it knew where a string ends and named the constructs
  ;; that can hide a quote. Twice that list was short by one character.
  ;;
  ;;   round one: a line comment. `; a quote "` turned the scan into
  ;;     "inside a string" and a datum label after it was never looked at.
  ;;   round two: a backslash. Chez allows it to escape a character INSIDE
  ;;     AN IDENTIFIER, so `\"` is an identifier character to the reader
  ;;     and a string opener to this scanner -- measured:
  ;;     `(answer \" . #0=((x 1) . #0#))` passed, and read as a cycle.
  ;;     `(\" #e1e100000)` passed too, which is the allocating literal this
  ;;     guard also exists to refuse.
  ;;
  ;; KEY: THE SECOND TIME IS THE ARGUMENT. A blacklist is a claim to have
  ;; enumerated a language's syntax, and the review that found the
  ;; backslash found it by reading Chez's lexical reference while this
  ;; file had generalised from the cases someone thought of. A third
  ;; character was only ever a matter of time.
  ;;
  ;; NEVER: SO WHAT IS ALLOWED IS LISTED INSTEAD, and the set is deliberately
  ;; SMALLER THAN THE LANGUAGE: an envelope is machine-written and needs
  ;; parentheses, whitespace, strings, `#f`/`#t`, and the characters
  ;; symbols and numbers are spelled with. Anything else is refused
  ;; without being understood. Refusing something Chez would have read is
  ;; safe; reading something this does not model is not.
  ;;
  ;; NOTE: INSIDE A STRING NOTHING IS JUDGED. That is a caller's own text --
  ;; a title, a commit message, a block's body -- and an escape there
  ;; still consumes two characters.
  (define (symbol-char? c)
    (or (and (char<=? #\a c) (char<=? c #\z))
        (and (char<=? #\A c) (char<=? c #\Z))
        (and (char<=? #\0 c) (char<=? c #\9))
        ;; NOTE: SPELLED OUT RATHER THAN `memv`, because this library imports
        ;; a deliberately narrow set of primitives -- see the note on the
        ;; import list -- and a guard is the wrong place to widen it.
        (char=? c #\-) (char=? c #\.) (char=? c #\_) (char=? c #\?)
        (char=? c #\!) (char=? c #\*) (char=? c #\/) (char=? c #\<)
        (char=? c #\>) (char=? c #\=) (char=? c #\+) (char=? c #\:)
        (char=? c #\%) (char=? c #\&) (char=? c #\~) (char=? c #\^)
        (char=? c #\@) (char=? c #\$)))

  ;; NEVER: ONE ALPHABET, TWO USERS. This says whether a name can be written
  ;; as a symbol and read back as the same symbol. `readable-shape?` uses
  ;; the same characters to decide what it will hand to the reader, and
  ;; the dispatcher uses this to decide whether a verb can be printed at
  ;; all -- so the two cannot drift into disagreeing about what a symbol
  ;; is spelled with.
  ;;
  ;; NOTE: EMPTY IS NOT A SPELLING. `(string->symbol "")` is a symbol the
  ;; writer emits as `||`, which this guard refuses on the way back.
  (define (wire-safe-spelling? text)
    (and (string? text)
         (> (string-length text) 0)
         ;; NEVER: THE WRITER IS ASKED, NOT MODELLED. A character test cannot
         ;; answer this: Chez escapes a symbol whose spelling could be
         ;; READ AS SOMETHING ELSE, which is not a property of the
         ;; characters at all. Measured -- `a.b` and `x-` survive, while
         ;; `.`, `-x`, `1`, `1abc`, `+inf.0` and `@` come back as
         ;; `\x2E;`, `\x2D;x`, `\x31;` and so on. Every one of those
         ;; passed the alphabet test alone, and the frame carrying it was
         ;; then refused at the far end for the backslash, so the two
         ;; routes gave different answers for the same verb.
         ;;
         ;; KEY: THIS IS THE BLACKLIST LESSON ONE LEVEL UP. Twice a hand
         ;; model of the language was short by one case; the fix is to
         ;; stop modelling and put the question to the writer itself.
         (string=? text (symbol-written (string->symbol text)))
         ;; NOTE: AND THE ALPHABET STILL APPLIES, because the two conditions
         ;; guard different ends. The writer says the spelling comes out
         ;; unchanged; the alphabet says the characters it comes out as
         ;; are ones `readable-shape?` will hand back to the reader. A
         ;; symbol can round-trip through `write` and still be spelled
         ;; with something the guard refuses.
         (let ok ((i 0))
           (or (>= i (string-length text))
               (and (symbol-char? (string-ref text i)) (ok (+ i 1)))))))

  (define (symbol-written sym)
    (call-with-string-output-port (lambda (port) (write sym port))))

  ;; NEVER: ONE PLACE WORDS THIS REFUSAL, AND THREE CALLERS ASK IT. The
  ;; dispatcher asks it in the layer both routes share; the command line
  ;; and the thin client ask it BEFORE deciding whether to forward,
  ;; because a frame carrying a verb they cannot print would be refused by
  ;; the reader's guard at the far end as `not-a-datum` -- a different
  ;; answer from the one a local call gives for the same input. Measured
  ;; exactly that way while writing this: forwarded said `(error
  ;; bad-request (reason not-a-datum))`, local said the refusal below.
  ;;
  ;; NOTE: IT LIVES HERE AND NOT IN `(theourgia rpc)` because the thin client
  ;; imports neither the core nor the actor system -- that is the whole
  ;; point of it -- and a refusal worded in the core would either be
  ;; copied or would drag the core into a program built to avoid it.
  ;;
  ;; NOTE: IT TAKES THE TEXT, NOT A SYMBOL: every caller has the spelling
  ;; before it has a symbol, and a symbol made from unprintable text is
  ;; the thing being refused.
  (define (verb-spelling-error spelling)
    (and (not (wire-safe-spelling? spelling))
         (list 'error 'bad-request 'unknown-verb (list 'spelling spelling))))

  (define (shape-delimiter? c)
    (or (char-whitespace? c) (char=? c #\() (char=? c #\)) (char=? c #\")))

  (define (readable-shape? text)
    (let ((n (string-length text)))
      (let scan ((i 0) (in-string #f) (seen #f))
        (cond
          ((>= i n) seen)
          (else
           (let ((c (string-ref text i)))
             (cond
               (in-string
                (cond
                  ((char=? c #\\) (scan (+ i 2) #t seen))
                  ((char=? c #\") (scan (+ i 1) #f seen))
                  (else (scan (+ i 1) #t seen))))
               ((char=? c #\") (scan (+ i 1) #t #t))
               ;; NEVER: `#` ONLY AS `#f` OR `#t`, AND ONLY WHEN WHAT FOLLOWS
               ;; ENDS IT. Every other dispatch form either allocates
               ;; before anything can judge it (`#e1e100000`), makes a
               ;; cycle (`#0=`), or hides a quote (`#\"`, `#|`, `#;`).
               ((char=? c #\#)
                (and (< (+ i 1) n)
                     (let ((d (string-ref text (+ i 1))))
                       (or (char=? d #\f) (char=? d #\t)))
                     (or (>= (+ i 2) n) (shape-delimiter? (string-ref text (+ i 2))))
                     (scan (+ i 2) #f #t)))
               ((char-whitespace? c) (scan (+ i 1) #f seen))
               ((or (char=? c #\() (char=? c #\))) (scan (+ i 1) #f #t))
               ((symbol-char? c) (scan (+ i 1) #f #t))
               (else #f))))))))

  ;; NEVER: WHAT THIS PROCESS CAN ACTUALLY LEAVE WITH, and nothing wider.
  ;; The field was checked with `integer?`, which in Scheme is TRUE OF
  ;; `37.0` -- a flonum with an integral value -- and `exit` given that
  ;; leaves with 1. It also admitted numbers past what a status can hold:
  ;; measured, `(exit 4294967337)` leaves with 41, NEVER: silently, because
  ;; the value is truncated somewhere below us. A peer's number that this
  ;; process cannot relay is not a small problem with the answer; it is an
  ;; envelope this version does not understand, and the caller is told
  ;; that rather than handed a status nobody chose.
  ;;
  ;; NOTE: 0..255 IS THE RANGE A PROCESS LEAVES WITH. Our own daemon writes
  ;; only 0 or 1 here, so nothing legitimate is refused by this.
  ;;
  ;; KEY: AND IT IS ONE PROCEDURE, BECAUSE THE LAST ONE WAS NOT. The comment
  ;; above `answer-field` records three copies of a lookup being fixed one
  ;; at a time; this predicate had two, and they were wrong in the same
  ;; way at the same time.
  (define (exit-code? n)
    (and (integer? n) (exact? n) (>= n 0) (<= n 255)))

  ;; NEVER: ONE FIELD READER FOR THE THREE PROGRAMS THAT READ THIS ENVELOPE.
  ;; What comes back was written by a peer, so its shape is not a given:
  ;; `assq` demands a proper list of pairs and raises on anything else, and
  ;; `(answer . broken)` -- a datum that reads perfectly well -- raised
  ;; straight past the refusal that exists for exactly this. Each reader
  ;; had its own copy of the lookup and they were fixed one at a time,
  ;; which is how two of three came to be right. It lives here, beside the
  ;; packer, because a reader and a writer of one shape belong together.
  ;;
  ;; Answers the field's VALUE, or #f when there is no such field, when
  ;; the datum is not an answer, or when what arrived has no shape at all.
  ;; NEVER: THE FIRST FIELD OF THAT NAME DECIDES, and a value that fails the
  ;; check is a refusal rather than a reason to keep looking. Written as
  ;; "scan until something satisfies `ok?`", an envelope carrying the same
  ;; field twice -- `(answer (exit "x") (exit 0))` -- would have been
  ;; refused by the old lookup and accepted by this one, which is a peer
  ;; choosing which of its own values we read.
  (define (answer-field envelope name ok?)
    (and (pair? envelope)
         (eq? 'answer (car envelope))
         (let look ((xs (cdr envelope)))
           (cond
             ((not (pair? xs)) #f)
             ((and (pair? (car xs)) (eq? name (caar xs)))
              (and (pair? (cdar xs))
                   (ok? (cadr (car xs)))
                   (cadr (car xs))))
             (else (look (cdr xs)))))))

  (define (request-frame store verb args fields)
    (let ((actor (field 'actor fields "rpc"))
          (writer (field 'writer fields #f))
          (mode (field 'mode fields 'wire))
          (cwd (field 'cwd fields #f))
          (stdin (field 'stdin fields #f)))
      (unless (and (string? store) (string? actor) (symbol? verb) (all-strings? args))
        (assertion-violation 'request-frame
          "store and actor are strings, verb a symbol, args strings"
          (list store actor verb args)))
      (unless (or (not writer) (string? writer))
        (assertion-violation 'request-frame "writer is a string or #f" writer))
      (unless (memq-1 mode '(wire human))
        (assertion-violation 'request-frame "mode is wire or human" mode))
      (unless (or (not cwd) (string? cwd))
        (assertion-violation 'request-frame "cwd is a string or #f" cwd))
      (unless (or (not stdin) (string? stdin))
        (assertion-violation 'request-frame "stdin is a string or #f" stdin))
      ;; NEVER: THE STORE TRAVELS BY ITS RESOLVED NAME, and every frame is built
      ;; here so that no caller can forget to. The socket key has always
      ;; been the resolved path -- which is how `--store .` reaches the
      ;; daemon serving `/abs/store` at all -- while the NAME inside the
      ;; envelope was whatever the caller typed, and the daemon compared
      ;; that against its own. So the request arrived at the right daemon
      ;; and was refused by it as being for another store: measured,
      ;; `(error transport-store-mismatch (serving "/tmp/b22A/store")
      ;; (asked "."))`, from inside the store's own directory. Two
      ;; spellings of one store are one store, and one rule decides what
      ;; its name is.
      (string->utf8
        (render-wire
          (append (list 'request (envelope-version) (resolved-name store)
                        actor writer mode cwd stdin verb)
                  args)))))

  (define (memq-1 x xs)
    (and (pair? xs) (or (eq? x (car xs)) (memq-1 x (cdr xs)))))
)
