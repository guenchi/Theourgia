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

;; The evaluation worker: one child process, three pipes, and nothing of
;; the store's write side in scope.
;;
;; NEVER: THE ORDER AT THE TOP OF THIS FILE IS THE WHOLE SAFETY ARGUMENT, and
;; it is not an ordering anybody may tidy:
;;
;;   1. `setsid!` -- a session of our own, so the supervisor can signal
;;      the whole group and reach anything this evaluation spawns.
;;   2. `RLIMIT_CPU` -- installed BEFORE a byte of untrusted source is
;;      read, so no source can run under an inherited unlimited one.
;;   3. `(ready <pgid>)` -- only now, because the supervisor reads it as
;;      "the group exists and the limits are on". Before it arrives the
;;      supervisor may only kill this pid; after it, the whole group.
;;   4. `go` -- the supervisor's acknowledgement. NEVER: Nothing is read and
;;      no grandchild may be started before it: between `ready` being
;;      sent and `go` coming back, the supervisor is deciding whether
;;      this worker exists at all.
;;
;; NEVER: THREE DESCRIPTORS, AND THE USER GETS NEITHER OF THE OTHER TWO.
;; stdout is the protocol and nothing else; the user's output and errors
;; go to stderr wrapped in `(out …)` / `(err …)` frames written by this
;; file, outside the sandbox. Evaluated code is handed the wrappers, so
;; it cannot reach the protocol and cannot forge an answer.
;;
;; KEY: THE PROTOCOL IS "READY, THEN ZERO OR MORE (incomplete ...) NOTES, THEN
;; ONE ANSWER". A note is written by this worker when a load of the store
;; hears of a writer it could not read, and it is the whole clause heard so
;; far; the answer is written last and carries none. The supervisor keeps
;; the last note and appends it to whatever answer it returns, a limit or a
;; lost worker included (K14; review r4, A1-1). A refusal of the handshake
;; comes before any load and so carries none.
;;
;; KEY: AN ANSWER'S SHAPE IS NEVER THE SOURCE'S (F92, F93). An eval answer is
;; `(ok ...)` or an `(error <name> ...)` of this worker's or the
;; supervisor's. The worker's own refusals (bad-source, eval-context,
;; eval-denied, eval-value) are raised as a private record, `worker-refusal`,
;; which no source can build, and a list headed `error` that a library raises
;; during the worker's own steps -- reading the source and the cut, loading
;; the store -- is made one (`worker-step`), so a malformed source is refused
;; as before; anything else those steps raise, except an unreadable entry or
;; a condition, answers the fixed message, as before. A condition the source raises keeps the fixed-message answer,
;; `(error eval-exception (kind raised) (message "..."))`. Any other value
;; the source raises while it is evaluated is carried as data inside
;; `(error eval-exception (kind raised) ...)`: as `(value <v>)` when
;; it can be written and read back, `(error ...)`-shaped lists included, and
;; as `(reason unwritable-value) (type <t>)` otherwise, t one of procedure,
;; port, cycle, too-large, or unsupported (a value `write` cannot give back,
;; such as a record or a hashtable), so a raise never puts a line on the
;; protocol that the supervisor cannot read.

(import (chezscheme) (theourgia datum-code) (theourgia store) (theourgia reduce)
        (theourgia eval-context) (theourgia code-project)
        (only (theourgia log) load-listener-add! merge-unreadable incomplete-clause
              unreadable-entry? unreadable-entry-path unreadable-entry-reason)
        (only (theourgia ffi) setsid! setrlimit! RLIMIT_CPU))

(define args (cdr (command-line)))
(define store (car args))
(define cut-text (cadr args))
(define under (caddr args))
;; NOTE: THE CPU CEILING ARRIVES IN argv, and it has to: it must be installed
;; before `ready`, `ready` comes before `go`, and `go` is the first thing
;; the supervisor sends. There is no earlier channel than the argument
;; vector.
(define cpu-seconds (string->number (cadddr args)))
(define output-limit (string->number (car (cddddr args))))

;; ---- the protocol stream ----------------------------------------------------
;;
;; NEVER: HELD IN A NAME THE SANDBOX NEVER SEES.
(define protocol (standard-output-port))

(define (say-datum! value)
  (put-bytevector protocol
                  (string->utf8
                    (call-with-string-output-port
                      (lambda (port)
                        (parameterize ((print-graph #f) (print-length #f)
                                       (print-level #f) (print-unicode #f)
                                       (print-gensym #f))
                          (write value port)
                          (newline port))))))
  (flush-output-port protocol))

;; ---- the user's two streams, framed -----------------------------------------
;;
;; NOTE: FLUSHED AFTER EVERY WRITE, INSIDE THE WRAPPER. Chez 10.1's
;; `make-custom-textual-output-port` will not take a buffer-mode and
;; `custom-port-buffer-size` may not be zero, so "unbuffered" has to be
;; arranged by flushing here -- where the user cannot decline it.
;; Measured on the buffered version: 65,537 displays handed over 65,536
;; bytes, and a quota counted downstream was a quota counted on the wrong
;; number.
(define diagnostics (standard-error-port))

(define (frame! tag text)
  (put-bytevector diagnostics
                  (string->utf8
                    (call-with-string-output-port
                      (lambda (port)
                        (parameterize ((print-graph #f) (print-length #f)
                                       (print-level #f) (print-unicode #f)
                                       (print-gensym #f))
                          (write (list tag text) port)
                          (newline port))))))
  (flush-output-port diagnostics))

(define (framed-port tag)
  (make-custom-textual-output-port
    (symbol->string tag)
    (lambda (string start count)
      (when (> count 0) (frame! tag (substring string start (+ start count))))
      count)
    #f #f
    (lambda () #t)))

(define user-out (framed-port 'out))
(define user-err (framed-port 'err))

;; ---- the order that is the safety argument ----------------------------------

(define pgid (setsid!))
;; NOTE: THE CEILING IS ADDED TO THE CPU ALREADY SPENT. RLIMIT_CPU counts
;; the process's whole life, and the imports above have used some of it
;; before this line; the supervisor's wall budget for the evaluation starts
;; at ready, so the CPU budget starts here too, or a start-up that was slow
;; enough would leave a legal evaluation less CPU than wall time. The
;; request is that much larger than the argument, so on a host whose
;; inherited hard limit sits between the two it is refused where the bare
;; argument would have fit; a refusal leaves the inherited limits in force,
;; is said in the diag line below, and does not stop ready, as before.
(define cpu-ceiling (+ cpu-seconds (div (+ (cpu-time) 999) 1000)))
(define cpu-status (setrlimit! RLIMIT_CPU cpu-ceiling cpu-ceiling))
;; NOTE: SAID OUT LOUD, so a row can read the ceiling this worker reports
;; having requested, and that the request came before any source was sent:
;; it precedes `ready`, and nothing is sent before ready. test/eval-ready.sc
;; ER-3 starts this worker directly and reads this line; it reads the
;; report, not the kernel. (An `EV-CPU-before-input` row this comment once
;; named never existed in the tree.)
(frame! 'diag (string-append "rlimit-cpu " (number->string cpu-ceiling)
                             (if (= cpu-status 0) "" " (refused)")))
(say-datum! (list 'ready pgid))

;; NEVER: NOTHING BELOW THIS LINE RUNS UNTIL THE SUPERVISOR SAYS SO.
(define go (read (current-input-port)))
(unless (eq? go 'go)
  (say-datum! '(error eval-worker-unavailable (reason handshake)))
  (exit 1))

;; NOTE: TWO MORE DATUMS, IN THIS ORDER: the view, then the source. `read`
;; is the framing -- there is no second framing scheme to keep in step,
;; and a draft body that is arbitrary binary survives it (measured:
;; bytevectors, embedded NUL, CRLF and non-BMP characters all round-trip).
(define view (read (current-input-port)))
(define source-datum (read (current-input-port)))

(define working-writer (and (pair? view) (cadr view)))
(define working-cut (and (pair? view) (caddr view)))
(define drafts (if (pair? view) (cadddr view) '()))
(define source (if (and (pair? source-datum) (eq? 'source (car source-datum)))
                   (cadr source-datum)
                   ""))

;; ---- the sandbox ------------------------------------------------------------

(define allowed-libraries
  '((rnrs base) (rnrs control) (rnrs lists) (rnrs sorting)
    (rnrs exceptions) (rnrs conditions) (rnrs mutable-pairs)
    (rnrs mutable-strings) (rnrs unicode) (rnrs bytevectors)))

;; ---- the worker's own refusals (F92, F93) -------------------------------------
;;
;; NEVER: THE WORKER'S OWN REFUSALS ARE A PRIVATE TYPE, NOT A LIST (R1). The
;; answer guard used to pass any raised pair headed `error` through as the
;; answer, because these refusals were raised as such lists -- so a source
;; that raised one forged the answer's shape: an improper pair on the wire,
;; or a procedure the supervisor could not read back and reported as a dead
;; worker. A refusal is now this record, which no source can make, and it
;; carries the answer it stands for.
(define-record-type worker-refusal (fields answer))
(define (refuse! answer) (raise (make-worker-refusal answer)))

;; NEVER: WHAT THE WORKER'S OWN STEPS RAISE IS NEVER THE SOURCE'S VALUE. Only
;; what the source raises while it is evaluated is data; a malformed source
;; raises nothing, it is refused. The three branches answer as before F92:
;;   - a list headed `error` is the refusal of a library the worker called
;;     -- `(error bad-source (reason unbalanced) ...)` from the reader -- and
;;     is answered as itself;
;;   - the worker's own refusal (`refuse!` is called inside these steps), an
;;     unreadable entry or a condition goes on to `answer`'s own clauses;
;;   - anything else -- the store's load raises `log-error`, a record, and
;;     one path a bare symbol -- answers the fixed message. Passed on, it
;;     would reach `raised-answer` and be reported as a value the source
;;     raised.
(define (worker-step thunk)
  (guard (e ((and (pair? e) (eq? (car e) 'error)) (refuse! e))
            ((or (worker-refusal? e) (unreadable-entry? e) (condition? e)) (raise e))
            (#t (refuse! '(error eval-exception (kind raised) (message "Evaluation raised an exception")))))
    (thunk)))

;; -> #f when the value can be written back as data, or the kind of problem:
;; procedure, port, cycle, size, unsupported. One rule for what the worker
;; returns and for what the source raises.
(define (value-problem value)
  (let ((active (make-eq-hashtable)) (count 0))
    (call/cc
      (lambda (k)
        (define (refuse kind) (k kind))
        (define (visit x depth)
          (set! count (+ count 1))
          (when (or (> count output-limit) (> depth 256)) (refuse 'size))
          (cond
            ((procedure? x) (refuse 'procedure))
            ((port? x) (refuse 'port))
            ((or (pair? x) (vector? x))
             (when (hashtable-ref active x #f) (refuse 'cycle))
             (hashtable-set! active x #t)
             (if (pair? x) (begin (visit (car x) (+ depth 1)) (visit (cdr x) (+ depth 1)))
                 (vector-for-each (lambda (v) (visit v (+ depth 1))) x))
             (hashtable-delete! active x))
            ((string? x) (when (> (string-length x) output-limit) (refuse 'size)))
            ((bytevector? x) (when (> (bytevector-length x) output-limit) (refuse 'size)))
            ((or (null? x) (boolean? x) (number? x) (char? x) (symbol? x)) (values))
            (else (refuse 'unsupported))))
        (visit value 0)
        #f))))

(define (limited-value? value index)
  (let ((problem (value-problem value)))
    (when problem
      (refuse! (list 'error 'eval-value (list 'kind problem) (list 'index index))))))

;; WHAT THE SOURCE RAISED, AS DATA (R2): whatever it looks like -- an
;; `(error ...)` list included -- it is carried inside eval-exception and
;; never becomes the answer's shape. A value that cannot be written back is
;; named by its kind instead of reaching the protocol, where the supervisor
;; could not read it and would report a worker that died.
(define (raised-answer v)
  (let ((problem (value-problem v)))
    (if problem
        (list 'error 'eval-exception (list 'kind 'raised) (list 'reason 'unwritable-value)
              (list 'type (if (eq? problem 'size) 'too-large problem)))
        (list 'error 'eval-exception (list 'kind 'raised) (list 'value v)))))

;; NEVER: THE OVERLAY IS THE SAME RULE `working-read` USES: a block with a live
;; draft reads as that draft, everything else as what is committed. The
;; drafts came with the request, snapshotted before this process existed,
;; so a commit landing during the run cannot change what is evaluated.
(define (overlay-for id)
  (let loop ((ds drafts))
    (cond ((null? ds) #f)
          ((equal? id (car (car ds))) (cadddr (car ds)))
          (else (loop (cdr ds))))))

;; WHAT THE LOADS THIS WORKER OPENS COULD NOT READ is said as a note the
;; moment a load hears it, and the supervisor appends the last note to the
;; answer it returns, whatever that answer is (K10, K14): a value computed
;; without a writer's records and answered plainly would be a quiet absence.
;; The worker listens the way rpc dispatch does, under the store string it
;; hands to every load. A note is said only when what was heard grows, so
;; the last one is the whole of it; an evaluation stopped by a limit after
;; the load has already said it.
(define heard '())
(load-listener-add! store
  (lambda (found)
    (let ((merged (merge-unreadable heard found)))
      (unless (equal? merged heard)
        (set! heard merged)
        (let ((clause (incomplete-clause heard)))
          (when clause (say-datum! clause)))))))

(define (answer)
  ;; NEVER: AN ENTRY THAT CANNOT BE READ IS NAMED, BEFORE THE CATCH-ALL (F79),
  ;; as every route of K1 names it: a writers/ directory this process
  ;; cannot list fails the load before any incomplete note can be heard,
  ;; and the catch-all would call it an exception the evaluation raised.
  (guard (e ((worker-refusal? e) (worker-refusal-answer e))
            ((unreadable-entry? e)
             (list 'error 'unreadable
                   (list 'path (unreadable-entry-path e))
                   (list 'reason (unreadable-entry-reason e))))
            ;; A CONDITION -- (car '()), an assertion -- keeps today's answer.
            ((condition? e) '(error eval-exception (kind raised) (message "Evaluation raised an exception")))
            (#t (raised-answer e)))
    (let-values
        (((state body env)
          ;; THE WORKER'S OWN STEPS: reading the source and the cut, loading
          ;; the store, the library the source is evaluated under. What they
          ;; raise is the worker's refusal, never the source's value (F92).
          (worker-step
            (lambda ()
              (let* ((forms (datum-source-read (string->utf8 source)))
                     (cut (and (not (string=? cut-text ""))
                               (let ((v (datum-source-read (string->utf8 cut-text))))
                                 (unless (= (length v) 1) (refuse! '(error bad-source (reason expected-one-cut)))) (caar v))))
                     (state (if cut (open-and-reduce store cut) (open-and-reduce store))))
                (unless (= (length forms) 1) (refuse! '(error bad-source (reason expected-one-form))))
                (unless (reduction? state) (refuse! '(error eval-context (reason unavailable-cut))))
                (eval-context! state drafts)
                (let* ((body (caar forms))
                       (body
                         (if (string=? under "") body
                             (begin
                               (unless (and (eq? 'library (code-field state under 'kind)) (eq? 'datum (code-field state under 'mode)))
                                 (refuse! '(error eval-context (reason library-required))))
                               (unless (for-all (lambda (spec) (or (equal? spec '(rnrs)) (member spec allowed-libraries)))
                                                (code-field state under 'imports))
                                 (refuse! '(error eval-denied (operation library-import))))
                               (cons 'let (cons '() (append (map (lambda (id)
                                                                   (or (overlay-for id) (code-field state id 'body)))
                                                                 (code-children state under))
                                                            (list body)))))))
                       (env (apply environment
                              (append allowed-libraries
                                      '((only (theourgia eval-context) store-cut blocks block display write newline open-string-input-port current-error-port flush-output-port))))))
                    (values state body env)))))))
      (let ((vs (call-with-values
                  (lambda ()
                    (parameterize ((current-output-port user-out)
                                   (current-error-port user-err))
                      (eval body env)))
                  list)))
        (do ((xs vs (cdr xs)) (i 0 (+ i 1))) ((null? xs)) (limited-value? (car xs) i))
        ;; NEVER: THE CUT POSITION CARRIES THE CUT THIS EVALUATION ACTUALLY
        ;; USED, NEVER: never #f. It is the coordinate that makes the run
        ;; reproducible -- ask again at this cut, with these drafts, and
        ;; the same thing is read -- and it is what `pinned-by-default`
        ;; and `--latest` are judged by. The request's own `--cut` is an
        ;; input; this is what the store resolved it to.
        (list 'ok (list 'values vs)
              (list 'working-view working-writer (reduce-applied-cut state)
                    (map (lambda (d) (cons (car d) (cadr d))) drafts)))))))

(say-datum! (answer))
(flush-output-port user-out)
(flush-output-port user-err)
