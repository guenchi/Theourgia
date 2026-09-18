#!chezscheme
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

;;; (theourgia ffi) -- the syscalls the log layer needs and Chez does
;;; not offer: advisory locking, truncation, flushing, and a write that
;;; is allowed to be short.
;;;
;;; WHY THESE FIVE AND NOT A FILESYSTEM LIBRARY. Every one of them
;;; exists because the log format's guarantees are stated in terms of
;;; them: a record is committed when write and fsync have both returned
;;; for its bytes, one writer at a time is enforced by flock on a file
;;; that is never deleted or replaced, and the only repair the format
;;; permits is ftruncate of a torn tail. A Chez port cannot express any
;;; of that -- it will not hand out its descriptor, it flushes when it
;;; likes, and it has no notion of a partial write. So the descriptor is
;;; owned here, from open to close.
;;;
;;; CONSTANTS ARE MEASURED, NOT REMEMBERED. Compiled against the system
;;; headers on each platform rather than taken from a table:
;;;
;;;   macOS 15.3 (arm64)  O_RDONLY 0  O_WRONLY 1  O_RDWR 2  O_APPEND 8
;;;                       LOCK_SH 1  LOCK_EX 2  LOCK_NB 4  LOCK_UN 8
;;;                       SEEK_SET 0  SEEK_CUR 1  SEEK_END 2
;;;                       EINTR 4  EIO 5  EEXIST 17
;;;                       F_FULLFSYNC 51  sizeof(off_t) 8
;;;   FreeBSD 15.0        identical for all of the above except
;;;                       F_FULLFSYNC, which that platform does not
;;;                       define at all and which is never issued there
;;;
;;; A third platform means compiling the same program there. The values
;;; agreeing across two Unixes is exactly what makes assuming a third
;;; tempting, and exactly what would make a wrong one hard to notice.
;;;
;;; open(2) IS VARIADIC AND ITS MODE IS NOT PASSED FROM HERE. The third
;;; argument lives in the `...`, so a fixed-arity foreign declaration
;;; puts it where a named argument goes while the callee reads it with
;;; va_arg: the mode that arrives is whatever was on the stack, measured
;;; elsewhere in this tree as 0o010 for a request of 0644. The
;;; declaration below therefore names only the two arguments that are
;;; named in C, and O_CREAT is never in the flags -- mode is not read
;;; without it. Creating a file is done by Chez instead, in
;;; file-ensure!, and the descriptor is opened afterwards. That is one
;;; extra path resolution and it is the deliberate price of not passing
;;; a mode through a calling convention that will not carry it.
;;;
;;; TWO OBLIGATIONS AROUND CLEANUP, AND THEY ARE NOT ONE. This is
;;; written down because it has already been got wrong once and will be
;;; reached for again:
;;;
;;;   a) cleanup running while an exception is already on its way out
;;;      must not raise, or it replaces a real failure with its own and
;;;      the caller is told the wrong thing;
;;;   b) a failure on the NORMAL path must not be swallowed, or an
;;;      operation reports success it did not achieve.
;;;
;;; Putting a close in an unwind thunk satisfies (a) and violates (b):
;;; measured here, a descriptor closing with EIO after a successful
;;; write and flush vanished, the rename went ahead, and the whole
;;; replacement returned success. The shape that satisfies both is to
;;; act on the way THROUGH and clear a flag, leaving the unwind to act
;;; only when the flag says the way through did not happen.

;;; ERRORS USE igropyr's durable-error VECTOR, TAG AND ARITY. A caller
;;; of this library also calls durable-write-file!, and one guard should
;;; cover both, so (igropyr durable)'s exported durable-error? -- which
;;; checks for exactly three fields -- must accept what is raised here.
;;; A fourth field for errno would make that predicate answer #f on our
;;; errors, silently, which is the defect its own comment warns about.
;;; So errno rides in the third field, which becomes a pair:
;;;
;;;   #(durable-error <op> (<path-or-fd> . <errno>))
;;;
;;; <op> stays a plain symbol so `eq?` dispatch on the failing step
;;; works the same as it does for igropyr's own errors; <errno> is #f
;;; when the failure was not a syscall refusal. fs-error-target and
;;; fs-error-errno read the two halves.
;;;
;;; THE TRACE IS PRODUCT CODE, not a test harness. With
;;; THEOURGIA_TRACE=1 set, each lock, truncate, write and flush reports
;;; one line on stderr through (theourgia trace), which owns the switch
;;; and the format. <subject> is whatever the caller named the
;;; descriptor and defaults to the descriptor number, which is why every
;;; entry point takes an optional label.
;;;
;;; THE SET OF OPERATIONS THAT EMIT A LINE IS PART OF THE INTERFACE,
;;; because tests assert the sequence and a line added later moves every
;;; assertion that counts them. It is: flock, lock-wait, unlock,
;;; ftruncate, write, fsync, and barrier. open, close and link emit
;;; nothing. fsync emits ONE line even on macOS, where it is two
;;; syscalls -- a platform-conditional second event would make the same
;;; run read differently on the two platforms this ships to.
;;;
;;; SUBJECTS ARE PATHS, NOT DESCRIPTOR NUMBERS, because a reader of the
;;; trace has to tell one flush from another -- the log's from the
;;; registry's -- and a descriptor number is reused within a run and
;;; means nothing across one. fd-open remembers the path it opened and
;;; fd-close forgets it, so every entry point defaults its subject to
;;; that; the optional label overrides it. A lock's subject carries the
;;; mode as well, (<path> . exclusive) or (<path> . shared), because
;;; which lock was taken is not recoverable from the outcome.
;;;
;;; FAULT INJECTION LIVES AT THREE CALL SITES AND NOWHERE ELSE -- the
;;; write, the flush and the open -- armed by THEOURGIA_FAULT=<name> or
;;; <name>:<path-substring>:
;;;
;;;   short-write               the first write offers min(7, n) bytes
;;;   eintr-once                the first write is skipped, reporting EINTR
;;;   write-eio-after-partial   min(7, n) bytes, then EIO
;;;   fsync-fail                fsync reports EIO
;;;   no-log-fsync              fsync is SKIPPED and reports success
;;;   open-fail                 the open reports the errno it is given
;;;
;;; THE OPEN IS THERE BECAUSE TWO OF ITS FAILURES ARE REACHABLE FROM A
;;; FIXTURE AND THE REST ARE NOT. `rm` gives ENOENT and `chmod 000` gives
;;; EACCES, so a caller that propagated those two and quietly skipped
;;; every other reason would pass every row a fixture could write --
;;; while the failure a busy process actually meets, running out of
;;; descriptors, went untested because nothing could produce it on
;;; demand. `open-fail@<stage>:file=<sub>:errno=EMFILE` produces it.
;;;
;;; THE ERRNO QUALIFIER BELONGS TO THIS FAULT ALONE and is REQUIRED.
;;; EMFILE, EACCES and ENOENT are accepted by name, and so is any
;;; positive integer -- because the property this fault exists to test is
;;; that a caller treats EVERY reason alike, and a qualifier restricted
;;; to a fixed list would leave "every reason except the ones we thought
;;; of" untested by construction. Anything else is refused at startup.
;;;
;;; IT IS REQUIRED RATHER THAN DEFAULTED. Every possible default is a
;;; class a fixture can already produce with rm or chmod, so a case that
;;; MEANT to name a class and misspelled the qualifier would silently run
;;; as a weaker case that looks identical in the log. Requiring it also
;;; settles what a path ending in `:errno=...` means: the tail after the
;;; LAST `:errno=` is the qualifier, so such a path is written by
;;; appending a real one.
;;;
;;; AND IT IS READ FOR open-fail AND NOTHING ELSE. Stripping the tail
;;; from every fault's argument would change where an existing case
;;; points: `fsync-fail@commit:file=a:errno=b` is a path substring with a
;;; colon in it, and a parser that helpfully removed the tail would aim
;;; that case at `a` instead. A broadened target reads, in a test log,
;;; exactly like the case that was written.
;;;
;;; THE NON-EMPTY CHECK IS ON THE RESULTING SUBSTRING, not on what was
;;; typed. `file=` alone and `:errno=X` alone both leave a non-empty
;;; argument and an EMPTY substring -- and an empty substring matches
;;; every path, which is the one thing a path-scoped fault must never do.
;;;
;;; open-fail IS ONE-SHOT WITHIN ITS STAGE, like short-write: the point
;;; of it is that one open fails, and a persistent one would also take
;;; out whatever the fixture opens to read the result afterwards.
;;;
;;; A FLUSH FAULT'S PATH QUALIFIER MAY SAY WHICH KIND OF FLUSH.
;;; `dir=<substring>` matches only a directory flush, `file=<substring>`
;;; only a file's, and a bare substring means file. Without that the two
;;; cannot be told apart: an atomic replacement flushes a temporary
;;; inside the target's directory and then the directory itself, so a
;;; substring naming the directory ALSO matches the temporary's path and
;;; the fault lands on the earlier flush, while a substring naming the
;;; file misses the directory entirely. The directory window that L18(b)
;;; exists to break was unreachable.
;;;
;;; THE SPEC IS <fault>@<stage>[:<path substring>] AND THE STAGE IS NOT
;;; OPTIONAL. stage is one of deliver-barrier, commit, registry,
;;; publish, snapshot, repair, and the caller says which one it is in by
;;; parameterizing theourgia-stage.
;;;
;;; TWO CLASSES, AND THE DIFFERENCE IS DELIBERATE. short-write,
;;; eintr-once and open-fail are ONE-SHOT within their stage: they model a transient
;;; event, and a caller that retries must be able to succeed.
;;; fsync-fail, no-log-fsync, and write-eio-after-partial once it has
;;; delivered its partial are PERSISTENT within their stage: they model
;;; a device or a filesystem that has stopped working, and one that
;;; healed on the retry would let a test pass for the wrong reason.
;;; Both classes end with the stage.
;;;
;;; WITHOUT IT A FAULT LANDS ON WHATEVER RUNS FIRST, which is not a
;;; hypothetical: once records are flushed before delivery, a
;;; path-scoped fsync-fail aimed at the log file hits the PRE-DELIVERY
;;; flush and never reaches the commit flush the case meant to break;
;;; and once the registry is written with write-all!, the one-shot
;;; short-write is consumed by the registry's temporary file instead of
;;; the log record. Both reads as the injection having worked. The
;;; one-shot is therefore counted per stage as well: a fault fires on
;;; the first qualifying operation IN ITS STAGE and not before.
;;;
;;; THE SUBSTRING IS MATCHED AGAINST THE WHOLE PATH, DIRECTORIES AND
;;; ALL, which is a trap worth naming because it has already sprung
;;; once: a fixture whose DIRECTORY was named after the fault
;;; ("...fsync-fail_000001.sexp/reg/instances.sexp") matched the
;;; registry's path too, and the run showed both flushes failing --
;;; which reads exactly like the injection leaking. Keep the substring
;;; out of the enclosing directory names, or the control is not a
;;; control.
;;;
;;; A BARRIER IS THE OTHER HALF OF THE SAME SWITCH. THEOURGIA_BARRIER=
;;; <name>:<fifo> parks the process at the named point until a
;;; controller writes to the fifo, which is how a test puts a second
;;; process between two steps of the first. It reports (trace barrier
;;; <name> #f) BEFORE parking, so the controller learns the point was
;;; reached rather than guessing from a sleep. Both halves are behind
;;; THEOURGIA_INJECT because two switches in two repositories is how a
;;; test runner ends up with one of them off.
;;;
;;; EVERY ONE OF THESE CHANGES WHAT THE SYSCALL DOES rather than lying
;;; about what it did. A short write really writes seven bytes; a skipped
;;; write really writes none; a skipped fsync really does not flush; a
;;; failed open really leaves no descriptor open, and raises the same
;;; condition carrying the same errno field that a real failure raises,
;;; so a caller has nothing by which to tell the two apart. The
;;; rule is the one (igropyr inject) states: a return value may only be
;;; replaced when skipping the call leaves the world as if it had failed,
;;; because faking "the write failed" after performing the write would
;;; put bytes on disk that the caller has been told are not there.
;;;
;;; A SKIPPED FLUSH EMITS NO TRACE LINE, deliberately. no-log-fsync
;;; exists so that a test asserting the write/flush/unlock order goes
;;; red; printing the line anyway would forge the evidence the test
;;; reads, which is the one thing an injection may not do.
;;;
;;; TWO SWITCHES, AND THE OUTER ONE ACTS AT EXPANSION TIME. Injection
;;; code exists only in a build expanded with THEOURGIA_INJECT=on; with
;;; the variable unset or "off" there is no fault branch in the object at
;;; all -- not a disabled one, not a flag test. THEOURGIA_FAULT then
;;; chooses WHICH fault at run time, and nothing reads it in a build that
;;; has no injection code to select from.
;;;
;;; THE OUTER SWITCH IS THE ONE THAT MATTERS. A run-time-only gate would
;;; let a released process be told to skip its flushes by its
;;; environment, which for a durability library is not a debugging
;;; convenience but a way to turn off the guarantee the library exists to
;;; make. This is the same mechanism and the same reasoning as (igropyr
;;; inject), which is where it is stated at length.
;;;
;;; An armed EXPANSION says so on stderr, and so does an armed process.
;;; Neither is a tripwire: the expansion banner describes the process
;;; doing the expanding, so a tree still holding an object compiled while
;;; armed stays silent about it. What stops an armed artifact is not
;;; compiling one -- development runs from source.

(library (theourgia ffi)
  (export fd-open fd-close
          with-exclusive-lock with-shared-lock
          ftruncate! fsync! fsync-dir! write-all!
          fd-seek! fd-size file-size file-ensure! fd-path link!
          setsid! session-id signal-pid! process-alive-signal0?
          setrlimit! getrlimit RLIMIT_CPU process-rss-bytes
          barrier!
          lock-acquire! lock-try-acquire! current-lock-acquire
          lock-release! current-lock-release lock-fd lock-held?
          path-device-inode path-version
          fs-error? fs-error-op fs-error-target fs-error-errno
          theourgia-fault theourgia-fault-armed? theourgia-stage known-stages
          report-fault?
          trace-enabled? trace-enable! trace-event!
          directory-entries file-is-directory? file-is-regular? rename-over!
          unlink! file-create-exclusive! mkdir-p!
          source-reader-open source-reader-next source-reader-at source-reader-observer!
          source-datum-print exec-argv!
          process-id wall-clock-ms machine-home)
  (import (chezscheme)
          (only (igropyr platform)
                platform-os ensure-supported-platform! load-first-shared-object!)
          (only (igropyr util) string-contains?)
          (theourgia trace))

  ;; Host adapters for the datum projection. Admission lives in source-lex;
  ;; all source-reader callers must admit the entire source before opening.
  ;; Chez annotation positions are characters, as documented at
  ;; https://cisco.github.io/ChezScheme/csug9.5/syntax.html .
  (define reader-observer (vector #f))
  (define (source-reader-observer! proc)
    (unless (or (not proc) (procedure? proc))
      (assertion-violation 'source-reader-observer! "Expected procedure or false"))
    (vector-set! reader-observer 0 proc))
  (define source-mode-prefix "#!chezscheme\n")
  (define (source-reader-open text)
    (vector (open-input-string (string-append source-mode-prefix text))
            (source-file-descriptor "datum-input" 0) 0 text))
  (define (source-reader-next reader)
    (let ((observer (vector-ref reader-observer 0)))
      (when observer (observer (vector-ref reader 3) (max 0 (- (vector-ref reader 2) (string-length source-mode-prefix))))))
    (trace-event! 'source-reader-enter (vector-ref reader 2) #f)
    (let-values (((a end) (get-datum/annotations (vector-ref reader 0) (vector-ref reader 1) (vector-ref reader 2))))
      (vector-set! reader 2 end)
      (if (eof-object? a) a (source-annotation a))))
  (define (source-annotation a)
    (let ((src (annotation-source a)) (expr (annotation-expression a))
          (datum (annotation-stripped a)))
      (list datum
            (- (source-object-bfp src) (string-length source-mode-prefix))
            (- (source-object-efp src) (string-length source-mode-prefix))
            (if (and (pair? datum) (eq? (car datum) 'library) (list? expr))
                (map source-annotation (filter annotation? expr)) '()))))
  (define (source-reader-at text offset)
    (let ((reader (source-reader-open (substring text offset (string-length text)))))
      (source-reader-next reader)))
  (define (source-datum-print datum)
    (unless (string=? (scheme-version) "Chez Scheme Version 10.1.0")
      (raise '(error unsupported-printer-version)))
    (parameterize ((pretty-line-length 72) (pretty-one-line-limit 72)
                   (pretty-initial-indent 0) (pretty-standard-indent 2)
                   (pretty-maximum-lines #f) (print-length #f) (print-level #f)
                   (print-radix 10) (print-graph #f) (print-gensym #f)
                   (print-unicode #f))
      (call-with-string-output-port (lambda (port) (pretty-print datum port)))))

  ;; Replace the launcher process without a shell or argument interpolation.
  (define (exec-argv! args)
    (let* ((width (foreign-sizeof 'void*)) (argv (foreign-alloc (* width (+ 1 (length args)))))
           (strings
             (map (lambda (arg)
                    (let* ((b (string->utf8 arg)) (n (bytevector-length b)) (p (foreign-alloc (+ n 1))))
                      (do ((i 0 (+ i 1))) ((= i n)) (foreign-set! 'unsigned-8 p i (bytevector-u8-ref b i)))
                      (foreign-set! 'unsigned-8 p n 0) p)) args)))
      (do ((ps strings (cdr ps)) (i 0 (+ i 1))) ((null? ps))
        (foreign-set! 'void* argv (* i width) (car ps)))
      (foreign-set! 'void* argv (* (length args) width) 0)
      ((foreign-procedure "execvp" (string void*) int) (car args) argv)
      (for-each foreign-free strings) (foreign-free argv)
      (raise '(error launcher-unavailable))))

  ;; PLATFORM DETECTION AND SHARED-OBJECT LOADING COME FROM IGROPYR, and
  ;; this is one of the three places allowed to say so. Seventy-four
  ;; lines stood here as a copy of `(igropyr platform)`, watched against
  ;; drift by a digest table and a comparison fixture; the copy is gone
  ;; and so are they.
  ;;
  ;; ONLY THREE NAMES ARE TAKEN, not the forty-six that library exports.
  ;; `machine-name`, `platform-arch` and the three string helpers went
  ;; with the copy because nothing here calls them -- what the import
  ;; list says is what this core actually uses, which is the whole point
  ;; of routing the dependency through a file of its own.
  ;;
  ;; THE REST OF THIS FILE IS THE CORE'S OWN. A hundred and forty-three
  ;; definitions below are about this tree's syscalls, fault injection
  ;; and durability, not about igropyr; `test/facades.sexp` lists the
  ;; files that MAY import igropyr, and does not claim they hold nothing
  ;; else.


  (define platform-checked (begin (ensure-supported-platform!) #t))

  ;; THE PLATFORM LAYER READS THE ENVIRONMENT AND INJECTS THE SWITCH.
  ;; (theourgia trace) takes neither getenv nor a parameter so that it
  ;; stays portable to a host with no environment; this is the one place
  ;; that knows there is one.
  (define trace-switch-installed
    ;; ⛔ THIS OVERWRITES ANY `trace-enable!` MADE BEFORE THIS LIBRARY
    ;; LOADS. A program that calls `(trace-enable! #t)` and then reaches
    ;; a path that first loads this file gets its setting replaced by
    ;; whatever the environment says -- and the failure is silent:
    ;; `trace-event!` is `(when (trace-enabled?) ...)`, so every call
    ;; afterwards returns perfectly normally and writes nothing. Measured
    ;; while chasing a daemon row: the flag read #t in the launching
    ;; script and #f inside the scheduler. ⇒ ACROSS PROCESSES, TURN
    ;; TRACING ON WITH `THEOURGIA_TRACE=1`, never with a call.
    (begin (trace-enable! (equal? (getenv "THEOURGIA_TRACE") "1")) #t))

  ;; ---- the filesystem operations that are not R6RS ------------------------
  ;;
  ;; Directory listing, the two file-kind predicates, renaming over an
  ;; existing name, and the process id are all host facilities with no
  ;; R6RS spelling. They live here, with the syscalls, so that everything
  ;; above this file is portable R6RS -- the same reason open, flock and
  ;; fsync are here. Chez provides all five; another host substitutes
  ;; this file and nothing else.
  (define (directory-entries path) (directory-list path))
  (define (file-is-directory? path) (file-directory? path))
  (define (file-is-regular? path) (file-regular? path))

  ;; Renaming over an existing name is the install step of every atomic
  ;; replacement, and it is NOT R6RS: the standard has delete-file but no
  ;; rename at all. Measured on both targets: it replaces the target
  ;; atomically and the source is gone afterwards.
  ;; DIRECTORY-ENTRY OPERATIONS ARE TRACED because the crash model
  ;; (design section 13', open question 6) reconstructs the
  ;; only-what-was-persisted state from the trace alone: an entry created
  ;; by one of these survives only if its directory was fsynced
  ;; afterwards. A rename that is not in the trace is a rename the
  ;; reconstruction would silently keep.
  (define (rename-over! from to)
    (unless (and (string? from) (string? to))
      (assertion-violation 'rename-over! "paths must be strings" from to))
    (rename-file from to)
    (trace-event! 'rename (cons from to) #f))

  ;; CREATE-IF-ABSENT'S SIBLING: create-or-fail. Both are here rather
  ;; than at the call site so that every directory entry this library
  ;; makes appear is traced in one place -- the crash model reconstructs
  ;; surviving entries from the trace, and an untraced create is an entry
  ;; the reconstruction cannot know existed. Returns #f when the name is
  ;; already taken, which is a collision the caller retries, and raises
  ;; for everything else.
  (define (file-create-exclusive! path)
    (unless (string? path)
      (assertion-violation 'file-create-exclusive! "path must be a string" path))
    (let ((port (guard (e ((i/o-file-already-exists-error? e) #f)
                          ((fs-error? e) (raise e))
                          (#t (raise e)))
                  (open-file-output-port path))))
      (and port
           (begin (close-port port)
                  (trace-event! 'create path #f)
                  path))))

  ;; CREATE A DIRECTORY, PARENTS INCLUDED. R6RS has no mkdir; this is
  ;; the platform layer's job like every other name-space change, and it
  ;; is traced for the same reason the others are -- the crash model
  ;; rebuilds surviving entries from the trace.
  (define (mkdir-p! path)
    (unless (and (string? path) (> (string-length path) 0))
      (assertion-violation 'mkdir-p! "path must be a non-empty string" path))
    (let loop ((i 1))
      (cond
        ((> i (string-length path))
         (unless (file-is-directory? path)
           (mkdir-one! path))
         path)
        ((or (= i (string-length path)) (char=? (string-ref path i) #\/))
         (let ((prefix (substring path 0 i)))
           (unless (or (string=? prefix "") (file-is-directory? prefix))
             (mkdir-one! prefix)))
         (loop (+ i 1)))
        (else (loop (+ i 1))))))

  ;; A SWALLOWED FAILURE IS ACCEPTED ONLY IF THE DIRECTORY IS THERE
  ;; AFTERWARDS. The race this tolerates is another process creating the
  ;; same directory first; every other failure -- a permission error, a
  ;; regular file already occupying the name -- was being reported as
  ;; success, so mkdir-p! on "/dev/null" answered "/dev/null".
  (define (mkdir-one! path)
    (guard (e (#t (unless (file-is-directory? path)
                    (raise (fs-err 'mkdir path #f)))))
      (mkdir path)
      (trace-event! 'create path #f)))

  (define (unlink! path)
    (unless (string? path)
      (assertion-violation 'unlink! "path must be a string" path))
    (guard (e ((fs-error? e) (raise e))
              (#t (raise (fs-err 'unlink path #f))))
      (delete-file path)
      (trace-event! 'unlink path #f)
      path))

  (define (process-id) (get-process-id))

  ;; THE MACHINE HOME IS A SEAM AND A HAZARD, and it is read here because
  ;; reading the environment is this layer's job. THEOURGIA_HOME lets a
  ;; fixture keep its own registry -- without it every test on a machine
  ;; would share one water mark. It is also a switch that lets a process
  ;; ignore a rollback, which is exactly what the registry exists to
  ;; catch, so a non-default value announces itself on startup rather
  ;; than being silently in force.
  (define (machine-home)
    (let ((v (getenv "THEOURGIA_HOME")))
      (if (and (string? v) (> (string-length v) 0))
          v
          (let ((h (getenv "HOME")))
            (string-append (if (string? h) h "/tmp") "/.theourgia")))))

  (define machine-home-announced
    (let ((v (getenv "THEOURGIA_HOME")))
      (when (and (string? v) (> (string-length v) 0))
        (let ((p (current-error-port)))
          (put-string p "(theourgia machine-home ")
          (put-string p v)
          (put-string p ")\n")
          (flush-output-port p)))
      #t))

  ;; MILLISECONDS SINCE THE EPOCH, and it has to come from here because
  ;; it is the one clock the record format names. Chez's `real-time` is
  ;; milliseconds since THIS PROCESS started -- it reads 26 a moment
  ;; after startup -- so a log written with it carries timestamps that
  ;; are not comparable across restarts and are not the epoch
  ;; milliseconds the format specifies. The segment age trigger reads
  ;; the same clock, and with process time it computes a negative age
  ;; and never rotates.
  (define (wall-clock-ms)
    (let ((t (current-time)))
      (+ (* 1000 (time-second t)) (quotient (time-nanosecond t) 1000000))))

  (define libc
    (load-first-shared-object!
      'theourgia-ffi
      '("libc.dylib" "libc.so.7" "libc.so.6" "libc.so")))

  ;; Two named arguments only: see the note on open(2) above.
  (define c-open  (foreign-procedure "open"  (string int) int))
  (define c-close (foreign-procedure "close" (int) int))
  (define c-fsync (foreign-procedure "fsync" (int) int))
  (define c-fcntl (foreign-procedure "fcntl" (int int int) int))
  (define c-flock (foreign-procedure "flock" (int int) int))
  (define c-ftruncate (foreign-procedure "ftruncate" (int integer-64) int))
  (define c-lseek (foreign-procedure "lseek" (int integer-64 int) integer-64))
  (define c-write (foreign-procedure "write" (int u8* size_t) ssize_t))
  (define c-link  (foreign-procedure "link"  (string string) int))
  (define c-stat  (foreign-procedure "stat"  (string u8*) int))

  ;; ---- the process calls the daemon and the eval guardian need ------------
  ;;
  ;; `setsid` is called by a CHILD, first thing, so that the guardian can
  ;; later signal the whole group: `kill(-pgid, ...)` reaches the child
  ;; and anything it started, and igropyr's own `proc-kill!` reaches only
  ;; the direct child. A grandchild holding the output pipe open is the
  ;; case that makes the difference visible.
  (define c-setsid (foreign-procedure "setsid" () int))
  (define c-getsid (foreign-procedure "getsid" (int) int))
  (define c-kill (foreign-procedure "kill" (int int) int))
  (define c-setrlimit (foreign-procedure "setrlimit" (int u8*) int))
  (define c-getrlimit (foreign-procedure "getrlimit" (int u8*) int))

  ;; errno is a per-thread location reached through a function, and the
  ;; function has a different name on the BSDs than on glibc. Whichever
  ;; one this libc exports is the one used; neither present is a
  ;; platform this library has not been ported to, and saying so at load
  ;; time is better than reporting every failure with a blank cause.
  (define c-errno-location
    (cond
      ((foreign-entry? "__error") (foreign-procedure "__error" () void*))
      ((foreign-entry? "__errno_location")
       (foreign-procedure "__errno_location" () void*))
      (else
       (assertion-violation 'theourgia-ffi
                            "no errno location symbol in libc" libc))))

  (define (errno) (foreign-ref 'int (c-errno-location) 0))

  ;; ---- processes ----------------------------------------------------------

  (define ESRCH 3)
  (define EPERM 1)

  ;; A NEW SESSION, SO THE GUARDIAN CAN SIGNAL THE WHOLE GROUP LATER.
  ;; Answers the new session id, or raises with the errno: a child that
  ;; believed it had its own group when it had not would be killed one
  ;; process at a time, and the one holding the pipe open is usually not
  ;; the one the guardian knows about.
  (define (setsid!)
    (let ((r (c-setsid)))
      (if (= r -1)
          (assertion-violation 'setsid! "setsid failed" (errno))
          r)))

  ;; ⭐ SO THAT THE EFFECT CAN BE READ SOMEWHERE OTHER THAN THE RETURN
  ;; VALUE. `setsid!` answers the new session id, which is also the
  ;; caller's own pid -- so a row that compares the two is satisfied by
  ;; an implementation that returns the pid and starts no session at all.
  ;; `session-id` asks the kernel, and it can be asked about a process
  ;; that did NOT call setsid, which is what makes a before-and-after
  ;; comparison possible. Zero means the caller.
  (define (session-id pid)
    (let ((r (c-getsid pid)))
      (if (= r -1) #f r)))

  ;; `kill` AS IT COMES: 0, or the errno. The callers want to tell the
  ;; failures apart -- ESRCH is "no such process" and EPERM is "there is
  ;; one and it is not yours", which is still ALIVE -- so this does not
  ;; collapse them into a boolean.
  (define (signal-pid! pid signum)
    (let ((r (c-kill pid signum)))
      (if (= r 0) 0 (errno))))

  ;; ⚠️ EPERM MEANS ALIVE. `kill(pid, 0)` asks the kernel whether a
  ;; process exists that this one may signal; a process owned by somebody
  ;; else answers EPERM, and reading that as "gone" is how a sampler
  ;; decides a running child has died. ESRCH, and only ESRCH, is absence.
  (define (process-alive-signal0? pid)
    (let ((r (signal-pid! pid 0)))
      (cond ((= r 0) #t)
            ((= r EPERM) #t)
            (else #f))))

  ;; ---- resource limits ----------------------------------------------------
  ;;
  ;; `struct rlimit` is two 64-bit values, current then maximum, on every
  ;; platform this tree builds on. It is passed as bytes rather than
  ;; described to Chez because a record layout is what the kernel reads,
  ;; and this way the two fields cannot be swapped by a declaration that
  ;; looks right.
  (define RLIMIT_CPU 0)

  (define (rlimit-bytes cur max)
    (let ((bv (make-bytevector 16 0)))
      (bytevector-u64-native-set! bv 0 cur)
      (bytevector-u64-native-set! bv 8 max)
      bv))

  (define (setrlimit! resource cur max)
    (let ((r (c-setrlimit resource (rlimit-bytes cur max))))
      (if (= r 0) 0 (errno))))

  (define (getrlimit resource)
    (let ((bv (make-bytevector 16 0)))
      (if (= 0 (c-getrlimit resource bv))
          (cons (bytevector-u64-native-ref bv 0) (bytevector-u64-native-ref bv 8))
          (cons #f (errno)))))

  ;; ---- resident size, per platform ----------------------------------------
  ;;
  ;; ⚠️ BYTES, ON EVERY PLATFORM. macOS reports bytes, FreeBSD and Linux
  ;; report PAGES, and a reading that forgot to multiply is still a
  ;; positive number that grows with the child's allocations -- it would
  ;; pass any cell that only asks for "positive and rising", and would
  ;; then let a child four thousand times over its limit run free.
  ;;
  ;; ⚠️ #f WHEN IT CANNOT BE READ, never 0. "I could not look" and "it is
  ;; using nothing" are opposite facts, and a sampler that treats the
  ;; first as the second reports a healthy child for a process that has
  ;; gone.
  (define PROC_PIDTASKINFO 4)

  (define c-proc-pidinfo
    (and (foreign-entry? "proc_pidinfo")
         (foreign-procedure "proc_pidinfo" (int int integer-64 u8* int) int)))

  (define (rss-darwin pid)
    (and c-proc-pidinfo
         (let ((bv (make-bytevector 256 0)))
           (let ((n (c-proc-pidinfo pid PROC_PIDTASKINFO 0 bv 256)))
             (and (> n 0)
                  ;; pti_resident_size is the second 64-bit field of
                  ;; proc_taskinfo, after pti_virtual_size.
                  (bytevector-u64-native-ref bv 8))))))

  (define (rss-linux pid)
    (let ((path (string-append "/proc/" (number->string pid) "/statm")))
      (and (file-exists? path)
           (guard (e (#t #f))
             (let* ((text (call-with-input-file path get-line))
                    (parts (let loop ((cs (string->list text)) (cur '()) (out '()))
                             (cond ((null? cs)
                                    (reverse (if (null? cur) out (cons (list->string (reverse cur)) out))))
                                   ((char=? (car cs) #\space)
                                    (loop (cdr cs) '() (if (null? cur) out (cons (list->string (reverse cur)) out))))
                                   (else (loop (cdr cs) (cons (car cs) cur) out))))))
               (and (pair? (cdr parts))
                    ;; ⚠️ THE PAGE SIZE IS ASKED FOR, not written down.
                    ;; statm counts pages, and 4096 is only the common
                    ;; case: on a machine with 16 KiB pages a hardcoded
                    ;; 4096 under-reports every reading by four, quietly
                    ;; and everywhere.
                    (let ((pages (string->number (cadr parts))))
                      (and pages (* pages (c-getpagesize))))))))))

  ;; FreeBSD keeps it in `kinfo_proc`, reached through sysctl, and the
  ;; offset of `ki_rssize` is not derivable from a header at runtime.
  ;;
  ;; ⭐ SO IT WAS MEASURED, ON THE MACHINE IT HAS TO WORK ON. A probe
  ;; started two children -- one touching a large allocation, one not --
  ;; read `ps -o rss=` for both at the same moment, and scanned the whole
  ;; struct for a slot that reproduced BOTH readings. FreeBSD 15.0-RELEASE,
  ;; page 4096, one candidate and no other:
  ;;
  ;;     offset 264, read as PAGES: big 57465 -> 224 MiB (ps: 224 MiB)
  ;;                                small  5767 ->  23 MiB (ps:  23 MiB)
  ;;
  ;; ⚠️ READ AS 64 BITS, AND THE FIRST ANSWER HERE WAS WRONG. This said
  ;; 32 bits, reasoning that the 64-bit read matched only because the
  ;; next field happened to be zero. The header settles it the other way:
  ;; `/usr/include/x86/_types.h` defines `__segsz_t` as `__int64_t` under
  ;; `__LP64__`, so on amd64 `ki_rssize` IS 64 bits and the upper half
  ;; belongs to the value. The probe could not tell the two readings
  ;; apart because every number it saw was small -- agreement between two
  ;; readings is not evidence when both would agree on small values.
  ;; A 32-bit read would truncate above 2^32 pages.
  ;;
  ;; ⚠️ AND THE STRUCT SIZE IS CHECKED, because an offset is a claim about
  ;; a layout. sysctl reports 1088 bytes there; a kernel that returns a
  ;; different size is one this offset was never measured against, and
  ;; answering #f is the honest response to that.
  (define CTL_KERN 1)
  (define KERN_PROC 14)
  (define KERN_PROC_PID 1)
  (define KI_RSSIZE-OFFSET 264)
  (define KINFO_PROC-SIZE 1088)

  (define c-sysctl
    (and (foreign-entry? "sysctl")
         (foreign-procedure "sysctl" (u8* unsigned-int u8* u8* void* size_t) int)))
  (define c-getpagesize
    (and (foreign-entry? "getpagesize")
         (foreign-procedure "getpagesize" () int)))

  (define (rss-freebsd pid)
    (and c-sysctl c-getpagesize
         (let ((mib (make-bytevector 16 0))
               (buf (make-bytevector 4096 0))
               (len (make-bytevector 8 0)))
           (bytevector-u32-native-set! mib 0 CTL_KERN)
           (bytevector-u32-native-set! mib 4 KERN_PROC)
           (bytevector-u32-native-set! mib 8 KERN_PROC_PID)
           (bytevector-u32-native-set! mib 12 pid)
           (bytevector-u64-native-set! len 0 4096)
           (and (= 0 (c-sysctl mib 4 buf len 0 0))
                (= (bytevector-u64-native-ref len 0) KINFO_PROC-SIZE)
                (* (bytevector-u64-native-ref buf KI_RSSIZE-OFFSET) (c-getpagesize))))))

  ;; ⛔ AND ZERO IS NOT A READING. Every branch below can answer 0 for a
  ;; process that is on its way out -- Linux prints `0 0 0 0 0 0 0` in
  ;; statm once the task has no memory descriptor -- and 0 is true in
  ;; Scheme, so it would be handed back as a measurement rather than as
  ;; the absence of one. A caller sampling a worker would see its memory
  ;; drop to zero and keep sampling.
  (define (process-rss-bytes pid)
    (let ((n (cond
               ((string=? (machine-kind) "darwin") (rss-darwin pid))
               ((string=? (machine-kind) "linux") (rss-linux pid))
               ((string=? (machine-kind) "freebsd") (rss-freebsd pid))
               (else #f))))
      (and n (> n 0) n)))

  (define (machine-kind)
    (let ((m (symbol->string (machine-type))))
      (cond
        ((substring-index "osx" m) "darwin")
        ((substring-index "macos" m) "darwin")
        ((substring-index "le" m) (if (substring-index "fb" m) "freebsd" "linux"))
        ((substring-index "fb" m) "freebsd")
        (else "unknown"))))

  (define (substring-index needle hay)
    (let* ((n (string-length needle)) (h (string-length hay)))
      (let loop ((i 0))
        (cond ((> (+ i n) h) #f)
              ((string=? needle (substring hay i (+ i n))) i)
              (else (loop (+ i 1)))))))

  (define O_RDONLY 0)
  (define O_WRONLY 1)
  (define O_RDWR   2)
  (define O_APPEND 8)
  (define LOCK_SH 1)
  (define LOCK_EX 2)
  (define LOCK_NB 4)
  (define LOCK_UN 8)
  (define SEEK_SET 0)
  (define SEEK_CUR 1)
  (define SEEK_END 2)
  (define EINTR 4)
  (define EIO 5)
  (define EEXIST 17)
  ;; THE THREE WAYS AN OPEN FAILS THAT A BARRIER HAS TO SURVIVE,
  ;; named because an injected fault says which one it is: the file is
  ;; gone, this process may not read it, or this process has no
  ;; descriptor left. The first two are reachable from a fixture with
  ;; rm and chmod; the third is not reachable at all without this seam,
  ;; which is why the seam exists.
  (define ENOENT 2)
  (define EACCES 13)
  (define EMFILE 24)
  (define F_FULLFSYNC 51)
  (define macos? (eq? platform-os 'macos))

  ;; ---- errors -----------------------------------------------------------

  (define (fs-err op subject code) (vector 'durable-error op (cons subject code)))

  (define (fs-error? r)
    (and (vector? r)
         (= 3 (vector-length r))
         (eq? (vector-ref r 0) 'durable-error)))

  (define (fs-error-op r) (vector-ref r 1))

  ;; The third field is a pair here and a bare path in igropyr's own
  ;; errors, so both readers accept both shapes rather than assuming
  ;; which library raised.
  (define (fs-error-target r)
    (let ((x (vector-ref r 2)))
      (if (pair? x) (car x) x)))

  (define (fs-error-errno r)
    (let ((x (vector-ref r 2)))
      (and (pair? x) (cdr x))))

  (define (fail! op subject) (raise (fs-err op subject (errno))))

  ;; ---- what descriptor is that ------------------------------------------

  ;; A descriptor number is meaningless in a trace and is reused within a
  ;; run, so the path it was opened with is kept beside it. Only fd-open
  ;; records and only the closing paths forget; a descriptor that came
  ;; from somewhere else simply has no entry and falls back to its
  ;; number, which is a worse subject but not a wrong one.
  ;;
  ;; THE CONDITION THIS DEPENDS ON, stated rather than assumed: a
  ;; descriptor obtained from fd-open is closed by fd-close and by
  ;; nothing else. Close one another way and its entry survives; the
  ;; kernel then hands the number to some other file, and the next
  ;; unlabelled fsync! on it resolves the OLD path -- which puts the
  ;; wrong name in the trace and, worse, lets a path-scoped fault skip a
  ;; flush it was never aimed at. Nothing in this library can detect
  ;; that, so it is a rule for callers and not a check.
  (define fd-paths (make-eqv-hashtable))

  (define (fd-path fd) (hashtable-ref fd-paths fd #f))

  (define (forget-fd! fd) (hashtable-delete! fd-paths fd))

  (define (subject-of fd opts)
    (cond ((pair? opts) (car opts))
          ((fd-path fd))
          (else fd)))

  ;; ---- fault injection ---------------------------------------------------

  ;; Read by the process doing the EXPANDING, exactly as (igropyr
  ;; inject) and (igropyr checked) read theirs. Development runs from
  ;; source, so expansion and run are the same process and the variable
  ;; behaves the way a run-time one would; a compiled object simply has
  ;; no fault code to select.
  (meta define inject-mode
    (let ((v (getenv "THEOURGIA_INJECT")))
      (cond
        ((or (not v) (string=? v "off")) 'off)
        ((string=? v "on") 'on)
        (else (assertion-violation 'theourgia-ffi
                "THEOURGIA_INJECT must be \"on\", \"off\", or unset" v)))))

  (meta define inject-visit-announced
    (begin (when (eq? inject-mode 'on)
             (display "theourgia: EXPANDING WITH FAULT INJECTION ON\n"
                      (current-error-port)))
           #t))

;; THE STAGES A DURABILITY POINT MAY BELONG TO, in one place and above
  ;; the two builds, because the checked build and the plain one must
  ;; mean the same thing by a stage name.
  ;; `report` IS NOT A DURABILITY POINT and is in this list anyway. Every
  ;; fault is aimed at a stage, and the one thing a case needs to be able
  ;; to break here is the step that BUILDS THE ANSWER after the record is
  ;; already durable -- the only place where "committed" and "described"
  ;; come apart. Leaving it out would mean that step could not be armed,
  ;; and a step no case can arm reads, in every log, exactly like a step
  ;; that passed.
  (define known-stages
    ;; `conn` is not a step in making something durable, and it is here
    ;; on purpose: the daemon's connection processes need a way to be
    ;; made to die, and a fault that cannot be named is a failure mode no
    ;; row can arm. It is validated here so a misspelling is refused at
    ;; startup like every other one.
    '(deliver-barrier commit registry publish snapshot repair report working index conn))

  ;; A STAGE IS PART OF MAKING SOMETHING DURABLE, not a decoration a
  ;; caller may leave off. A staged fault never matches a call that
  ;; declares no stage, so an unstaged flush is a step of the system that
  ;; no case can arm -- and a step nothing can arm reads, in every log,
  ;; exactly like a step that passed. Two of them sat in the publish path
  ;; until someone wrote a row that tried to arm them. A required
  ;; argument is the difference between a rule people remember and a rule
  ;; the shape of the call enforces.
  (define (check-stage! who stage)
    (unless (and (symbol? stage) (memq stage known-stages))
      (assertion-violation who "stage must be one of the known stages"
                           (list stage known-stages))))

  (meta-cond
    ((eq? inject-mode 'on)

     (define fault-spec (getenv "THEOURGIA_FAULT"))

     ;; NOFLOCK: THE PRODUCT'S LOCK IS REMOVED AND THE BARRIER IS KEPT.
     ;; It exists so that the rows asserting mutual exclusion can be
     ;; shown to fail when there is no mutual exclusion -- a lock is
     ;; invisible to every assertion about the answers that come back,
     ;; so without a way to take it away those rows are green for a
     ;; build that never locks at all.
     ;; IT SKIPS THE SYSCALL AND THE TWO EVENTS THAT DESCRIBE IT. The
     ;; log layer still announces `enter-critical` on its own, which is
     ;; what makes that event the discriminator: under NOFLOCK a second
     ;; process reaches enter-critical while the first is still inside.
     (define no-flock?
       (let ((v (getenv "THEOURGIA_NOFLOCK")))
         (and v (string=? v "1"))))

     ;; The stage a caller is currently in. #f means "no stage
     ;; declared", and a staged fault never matches that -- an
     ;; unlabelled call site cannot be the one a case is aiming at.
     (define theourgia-stage (make-parameter #f))

     ;; RETURNS TWO STRINGS, and the callers make the symbols they need.
     ;; An earlier version symbolized the head here, which was right for
     ;; its first caller and silently wrong for its second: the stage
     ;; came back as a symbol and was handed to string->symbol.
     (define (split-at-colon s)
       (let ((n (string-length s)))
         (let loop ((i 0))
           (cond
             ((= i n) (values s #f))
             ((char=? (string-ref s i) #\:)
              (values (substring s 0 i) (substring s (+ i 1) n)))
             (else (loop (+ i 1)))))))

     ;; <fault>@<stage>[:<substring>]
     (define (split-at-sign s)
       (let ((n (string-length s)))
         (let loop ((i 0))
           (cond
             ((= i n) (values s #f))
             ((char=? (string-ref s i) #\@)
              (values (substring s 0 i) (substring s (+ i 1) n)))
             (else (loop (+ i 1)))))))

     ;; file=/dir= prefix on the path qualifier; bare means file.
     (define (split-kind a)
       (cond
         ((not (string? a)) (values 'file a))
         ;; `>=`, NOT `>`. With `>` a bare "file=" is not recognised as a
         ;; prefix at all and becomes the literal substring "file=" -- a
         ;; path qualifier that silently aims at nothing in particular
         ;; instead of being refused for naming nothing.
         ((and (>= (string-length a) 5) (string=? (substring a 0 5) "file="))
          (values 'file (substring a 5 (string-length a))))
         ((and (>= (string-length a) 4) (string=? (substring a 0 4) "dir="))
          (values 'dir (substring a 4 (string-length a))))
         (else (values 'file a))))

     (define-values (fault-name fault-stage fault-arg)
       (if fault-spec
           (let-values (((head rest) (split-at-sign fault-spec)))
             (if rest
                 (let-values (((stage arg) (split-at-colon rest)))
                   (values (string->symbol head) stage arg))
                 (values (string->symbol head) #f #f)))
           (values #f #f #f)))

     ;; A NAME NOTHING IMPLEMENTS IS REFUSED, NOT ARMED. Every selector
     ;; below compares `fault-name` against a literal, so a misspelling
     ;; matches none of them -- and the run then announces itself as
     ;; armed, injects nothing, and passes. That is the worst shape a
     ;; test failure can take: the case reports success, and the evidence
     ;; it reports for that success is that a fault was armed.
     ;; open-fail DOES NOT REACH EVERY READ. It is armed inside
     ;; `fd-open`, and the parts of the store that read a whole small
     ;; file -- the metadata, the instance, a retirement marker -- open a
     ;; Chez port directly instead. A case that wants one of THOSE reads
     ;; to fail cannot get there with a fault at all; it parks the
     ;; process at a `barrier!` and moves the file aside. Written down
     ;; because the obvious next move is to reach for `open-fail`, and it
     ;; would arm, announce itself, and inject nothing.
     (define known-faults
       '(short-write eintr-once write-eio-after-partial write-eio-first
         fsync-fail no-log-fsync stat-fail open-fail report-fail
         conn-raise store-raise writer-raise writer-raise-late writer-hold))

     (define fault-name-checked
       (when (and fault-name (not (memq fault-name known-faults)))
         (assertion-violation 'theourgia-ffi
           "THEOURGIA_FAULT names no fault this build implements"
           (list fault-spec known-faults))))

     (define (theourgia-fault) fault-name)
     (define (theourgia-fault-armed?) (and fault-name #t))

     ;; Refused at startup rather than ignored: a spec without a stage
     ;; would fire somewhere, and "somewhere" reads in a test log
     ;; exactly like the intended place.
     (define fault-stage-checked
       (when fault-name
         (unless (and (string? fault-stage)
                      (memq (string->symbol fault-stage) known-stages))
           (assertion-violation 'theourgia-ffi
             "THEOURGIA_FAULT must be <fault>@<stage>[:<substring>]"
             fault-spec known-stages))))

     (define fault-stage-symbol
       (and (string? fault-stage) (string->symbol fault-stage)))

     ;; AN ERRNO QUALIFIER, FOR THE ONE FAULT THAT NEEDS TO SAY WHICH
     ;; FAILURE. `open-fail` exists to ask whether a caller treats every
     ;; reason an open can fail the same way, so the reason is part of
     ;; the spec: `open-fail@<stage>:file=<sub>:errno=EMFILE`.
     ;;
     ;; IT IS READ FOR `open-fail` AND FOR NOTHING ELSE. Stripping it
     ;; from every fault's argument would silently change what an
     ;; existing fault points at: `fsync-fail@commit:file=a:errno=b` is a
     ;; path substring with a colon in it, and a parser that helpfully
     ;; removed the tail would aim that case at `a` instead -- a
     ;; broadened target reads, in the test log, exactly like the case
     ;; that was written.
     ;;
     ;; IT IS REQUIRED, AND THE LAST ONE WINS. An optional qualifier
     ;; needs a default, and every possible default is a failure class a
     ;; fixture could already produce -- so a case that MEANT to name a
     ;; class and misspelled the qualifier would run as a weaker case
     ;; that looks identical in the log. Requiring it also settles the
     ;; ambiguity a literal path ending in `:errno=...` would otherwise
     ;; create: the tail after the LAST `:errno=` is the qualifier, so a
     ;; path that genuinely ends that way is written by appending a real
     ;; one.
     ;;
     ;; It is taken off before the path qualifier is read, because
     ;; `file=` runs to the end of the string.
     (define (split-errno a)
       (let ((n (and (string? a) (string-length a))))
         (let loop ((k (and n (- n 7))))
           (cond
             ((or (not n) (< k 0)) (values a #f))
             ((string=? (substring a k (+ k 7)) ":errno=")
              (values (substring a 0 k) (substring a (+ k 7) n)))
             (else (loop (- k 1)))))))

     (define-values (fault-arg-head fault-errno-text)
       (if (eq? fault-name 'open-fail)
           (split-errno fault-arg)
           (values fault-arg #f)))

     ;; A NAME OR A NUMBER. The three names are the classes a case
     ;; usually wants; the number is there because the property under
     ;; test is that the caller treats EVERY reason alike, and a fault
     ;; that could only produce a fixed list would leave "every reason
     ;; except the ones we thought of" untested by construction.
     (define fault-errno
       (and fault-errno-text
            (cond ((string=? fault-errno-text "EMFILE") EMFILE)
                  ((string=? fault-errno-text "EACCES") EACCES)
                  ((string=? fault-errno-text "ENOENT") ENOENT)
                  ;; DECIMAL DIGITS, NOT "whatever `string->number`
                  ;; accepts". That reader takes `24/1` and `#x18` too,
                  ;; and a spec whose grammar is the whole of Scheme's
                  ;; numeric syntax is a grammar nobody can check a case
                  ;; against.
                  ((and (> (string-length fault-errno-text) 0)
                        (let loop ((i 0))
                          (cond ((= i (string-length fault-errno-text)) #t)
                                ((and (char<=? #\0 (string-ref fault-errno-text i))
                                      (char<=? (string-ref fault-errno-text i) #\9))
                                 (loop (+ i 1)))
                                (else #f))))
                   (let ((n (string->number fault-errno-text)))
                     (if (> n 0) n 'unknown)))
                  (else 'unknown))))

     ;; REFUSED AT STARTUP, like the stage. A missing or unreadable
     ;; qualifier would otherwise select some failure the case did not
     ;; ask for, and a run that injects the wrong class reads exactly
     ;; like a run that injected the right one.
     (define fault-errno-checked
       (when (eq? fault-name 'open-fail)
         (when (or (not fault-errno) (eq? fault-errno 'unknown))
           (assertion-violation 'theourgia-ffi
             "open-fail needs :errno=EMFILE|EACCES|ENOENT or a positive integer"
             fault-spec))))

     (define-values (fault-kind fault-substring) (split-kind fault-arg-head))

     (define (in-fault-stage?)
       (eq? (theourgia-stage) fault-stage-symbol))

     ;; A PATH-SCOPED FAULT WITH NO PATH IS REFUSED AT STARTUP. Both of
     ;; these exist to hit one file and not the other -- the log's flush
     ;; and the registry's -- so a missing substring would make the
     ;; injection either inert or global, and both of those read in a
     ;; test log exactly like the fault having worked. An empty substring
     ;; is the same mistake: it matches every path.
     ;; A path substring stays REQUIRED for the two flush faults: the
     ;; stage says which phase, the substring says which file, and L19
     ;; needs both to tell the log's flush from the registry's inside
     ;; one phase.
     ;; THE CHECK IS ON WHAT THE MATCHER WILL USE, not on what was
     ;; typed. `:errno=...` alone, or a bare `file=`, both leave a
     ;; NON-empty argument and an EMPTY substring -- and an empty
     ;; substring matches every path, which is the one outcome a
     ;; path-scoped fault must never have. Checking the argument passed
     ;; those two through.
     ;; `dir=` DISTINGUISHES A DIRECTORY FLUSH FROM A FILE'S, and an open
     ;; is neither -- `open-fault` matches on the path and never consults
     ;; the kind. Accepting the qualifier and then ignoring it is worse
     ;; than refusing it: the case reads as aimed at a directory and
     ;; fires on the first ordinary file whose path contains the string.
     (define fault-kind-checked
       (when (and (eq? fault-name 'open-fail) (eq? fault-kind 'dir))
         (assertion-violation 'theourgia-ffi
           "open-fail takes file=<substring>; it has no directory to distinguish"
           fault-spec)))

     (define fault-argument-checked
       (when (memq fault-name '(fsync-fail no-log-fsync open-fail))
         (unless (and (string? fault-substring) (> (string-length fault-substring) 0))
           (assertion-violation 'theourgia-ffi
                                "this fault needs a non-empty path substring"
                                fault-spec))))

     ;; ANNOUNCED, NOT ASSUMED ABSENT: a process running with a fault
     ;; selected says so before it does anything.
     (define fault-announced
       (when (and fault-name)
         (guard (e (#t (void)))
           (let ((p (current-error-port)))
             (fprintf p "(theourgia fault-injection-armed ~a)\n" fault-spec)
             (flush-output-port p)))))

     ;; One box for all of them, because exactly one fault is ever
     ;; selected: fresh -> done for the two that fire once, fresh ->
     ;; partial -> EIO for the one that fires twice.
     (define fault-state (box 'fresh))

       ;; `string-contains?` COMES FROM THE FACADE'S IMPORT, and this is
       ;; the only place in this library that uses it.
       ;;
       ;; THIS BRANCH IS EXPANDED ONLY WHEN INJECTION IS ON, which has
       ;; now cost two defects of opposite sign in the same six lines.
       ;; First the platform layer copied into this library brought a
       ;; second definition, the two collided here and nowhere else, and
       ;; fourteen fault-injection fixtures failed to load the library
       ;; while every ordinary run passed. Then the copy was replaced by
       ;; a forward onto igropyr, the one definition went with it, and
       ;; this reference became unbound -- again invisible to every
       ;; ordinary run, and reported by the suite as a fifteen-minute
       ;; HANG, because `cli1` starts two children that die instantly
       ;; and then writes to a fifo nothing will ever read.
       ;;
       ;; ⛔ SO NOTHING IN HERE IS COVERED BY A RUN WITH INJECTION OFF.
       ;; A change to this library is measured with THEOURGIA_INJECT=on
       ;; as well, or it is not measured.

     ;; The path a fault is aimed at, taken from the subject when it is
     ;; one and from the registry otherwise.
     (define (fault-path-match? fd subject)
       (let ((p (cond ((string? subject) subject)
                      ((and (pair? subject) (string? (car subject))) (car subject))
                      (else (fd-path fd)))))
         (and (string? p) (string? fault-substring)
              (string-contains? p fault-substring))))

     (define barrier-spec (getenv "THEOURGIA_BARRIER"))

     (define-values (barrier-name barrier-fifo)
       (if barrier-spec
           (let-values (((n f) (split-at-colon barrier-spec)))
             (values (string->symbol n) f))
           (values #f #f)))

     (define barrier-argument-checked
       (when barrier-name
         (unless (and (string? barrier-fifo) (> (string-length barrier-fifo) 0))
           (assertion-violation 'theourgia-ffi
                                "THEOURGIA_BARRIER needs <name>:<fifo path>"
                                barrier-spec))))

     ;; REPORTED BEFORE PARKING, or the controller has nothing to wait
     ;; for and falls back to sleeping -- and a sleep is not a
     ;; synchronisation, it is a race that usually wins.
     ;;
     ;; Opening a fifo for reading blocks until a writer opens it, so
     ;; the open IS the rendezvous and the byte is the release. Chez's
     ;; port is used rather than a read(2) binding because blocking on
     ;; open is exactly the behaviour wanted and nothing here needs a
     ;; partial read.
     (define (barrier! name)
       (when (and barrier-name (eq? name barrier-name))
         (trace-event! 'barrier name #f)
         (guard (e (#t (void)))
           (let ((p (open-file-input-port barrier-fifo)))
             (get-u8 p)
             (close-port p)))))

     ;; A ONE-SHOT STAT FAILURE, path-scoped like the write faults.
     ;; Without it, "a probe that cannot answer is not an answer of no"
     ;; is a rule no input can exercise: the guarded and unguarded
     ;; versions of a size probe behave identically whenever stat works,
     ;; so the guard survives every mutation unpunished.
     ;; THE REPORTING STEP, WHICH RUNS AFTER THE RECORD IS DURABLE. It is
     ;; armed like the others so that a case can ask what the answer says
     ;; when the report cannot be built -- the one situation in which
     ;; "committed" and "described" come apart, and the one this store
     ;; used to answer by calling a durable write an error.
     ;; THIS ONE NAMES ITS STAGE STATICALLY. Every other fault fires
     ;; inside a durability call, which parameterizes `theourgia-stage`
     ;; as it runs; building an answer is not a durability call and is
     ;; inside no stage at all. So the spec still carries `@report` --
     ;; refused at startup if absent or unknown, like every other -- and
     ;; it is compared against the name rather than against a dynamic
     ;; value that is never set here.
     (define (report-fault?)
       (and fault-name
            (eq? fault-name 'report-fail)
            (eq? fault-stage-symbol 'report)
            (eq? (unbox fault-state) 'fresh)
            (begin (set-box! fault-state 'done) #t)))

     (define (stat-fault? path)
       (and fault-name
            (eq? fault-name 'stat-fail)
            (in-fault-stage?)
            (or (not fault-arg) (fault-path-match? #f path))
            (eq? (unbox fault-state) 'fresh)
            (begin (set-box! fault-state 'done) #t)))

     ;; -> an errno to fail the open with, or #f
     ;;
     ;; ONE SHOT, like the others: the barrier opens the segment once,
     ;; and a fault that fired on every open would also take out the
     ;; fixture's own reading of the file afterwards.
     (define (open-fault path)
       (and fault-name
            (eq? fault-name 'open-fail)
            (in-fault-stage?)
            (fault-path-match? #f path)
            (eq? (unbox fault-state) 'fresh)
            (begin (set-box! fault-state 'done)
                   fault-errno)))

     ;; -> skip | fail | #f
     (define (fsync-fault fd subject kind)
       (cond
         ((not fault-name) #f)
         ((not (in-fault-stage?)) #f)
         ((not (eq? kind fault-kind)) #f)
         ((and (eq? fault-name 'no-log-fsync) (fault-path-match? fd subject)) 'skip)
         ((and (eq? fault-name 'fsync-fail) (fault-path-match? fd subject)) 'fail)
         (else #f)))

     ;; THE STATE MOVES ONLY IF THE WRITE ACTUALLY HAPPENED. Marking the
     ;; fault spent before the call would let a real EINTR from the
     ;; kernel consume it, and the run would then look like a short write
     ;; that never occurred -- and for the EIO fault, "after partial"
     ;; would be a lie: the retry would be refused with nothing on disk.
     ;; THE PATH QUALIFIER APPLIES TO WRITE FAULTS TOO. An earlier
     ;; version checked the stage and then ignored fault-arg entirely,
     ;; so short-write@commit:000001.sexp fired on whatever the commit
     ;; stage wrote first -- the registry's temporary file as readily as
     ;; the log record. A qualifier that is accepted and then ignored is
     ;; worse than one that is refused: the run looks aimed.
     (define (write-once fd bv count subject)
       (if (and (in-fault-stage?)
                (or (not fault-arg) (fault-path-match? fd subject)))
           (staged-write-once fd bv count)
           (real-write fd bv count)))

     (define (staged-write-once fd bv count)
       (case fault-name
         ((eintr-once)
          (if (eq? (unbox fault-state) 'fresh)
              (begin (set-box! fault-state 'done) (values -1 EINTR))
              (real-write fd bv count)))
         ((short-write)
          (if (eq? (unbox fault-state) 'fresh)
              (let-values (((n code) (real-write fd bv (min short-count count))))
                (when (> n 0) (set-box! fault-state 'done))
                (values n code))
              (real-write fd bv count)))
         ((write-eio-after-partial)
          (if (eq? (unbox fault-state) 'fresh)
              (let-values (((n code) (real-write fd bv (min short-count count))))
                (when (> n 0) (set-box! fault-state 'partial))
                (values n code))
              (values -1 EIO)))
         ;; FAILS BEFORE THE FIRST BYTE. write-eio-after-partial cannot
         ;; produce this: it fails only after its partial has landed, so
         ;; the outcome that means "reserved and nothing written" had no
         ;; way to be reached, and a caller could not be shown that a
         ;; retry is safe there.
         ((write-eio-first)
          (if (eq? (unbox fault-state) 'fresh)
              (begin (set-box! fault-state 'done) (values -1 EIO))
              (real-write fd bv count)))
         (else (real-write fd bv count)))))

    (else
     (define theourgia-stage (make-parameter #f))
     ;; NOTHING ABOVE EXISTS IN THIS BUILD. These two are the whole of
     ;; what the rest of the library calls, and they say "no fault" in a
     ;; form the compiler can fold away.
     (define (theourgia-fault) #f)
     (define (theourgia-fault-armed?) #f)
     (define (stat-fault? path) #f)
     (define (report-fault?) #f)
     (define (open-fault path) #f)
     (define (fsync-fault fd subject kind) #f)
     (define (barrier! name) (void))
     (define no-flock? #f)
     (define (write-once fd bv count subject) (real-write fd bv count))))

  ;; ---- opening ----------------------------------------------------------

  ;; Creating the file is Chez's job, for the reason given at the top:
  ;; O_CREAT would need a mode and the mode cannot be passed. no-fail
  ;; makes an existing file acceptable and no-truncate leaves its
  ;; contents alone, so this is create-if-absent and nothing else. It is
  ;; separate and exported because a caller sometimes wants only this --
  ;; the lock file has to exist before anyone opens it read-only.
  (define (file-ensure! path)
    (unless (string? path)
      (assertion-violation 'file-ensure! "path must be a string" path))
    (guard (e ((fs-error? e) (raise e))
              (#t (raise (fs-err 'create path #f))))
      (unless (file-exists? path)
        (close-port
          (open-file-output-port path (file-options no-fail no-truncate)))
        (trace-event! 'create path #f))
      path))

  ;; COMBINED WITH bitwise-ior AND NOT WITH ADDITION, because a repeated
  ;; flag is a caller's slip and addition turns it into a DIFFERENT flag:
  ;; O_APPEND twice is 16, which is O_SHLOCK on both platforms here, so
  ;; '(write append append) would have opened a shared-locked descriptor
  ;; positioned at byte zero and overwritten the log. Enough repeats
  ;; reach O_CREAT (512) and O_TRUNC (1024), and O_CREAT is the one flag
  ;; this declaration of open(2) cannot survive -- it would make the
  ;; kernel read a mode that was never passed. Or is idempotent, so none
  ;; of that is reachable.
  ;;
  ;; Flags are symbols rather than a number so that a caller cannot pass
  ;; a numeric constant that means something else on the other platform.
  ;; O_CREAT, O_EXCL and O_TRUNC are deliberately absent: creation goes
  ;; through file-ensure!, and truncation through ftruncate! under the
  ;; lock, which is the only truncation this format permits at all.
  (define (flags->int who flags)
    (let loop ((xs flags) (access #f) (extra 0))
      (cond
        ((null? xs) (bitwise-ior (or access O_RDONLY) extra))
        (else
         (let ((f (car xs)))
           (case f
             ((read)       (loop (cdr xs) (or access O_RDONLY) extra))
             ((write)      (loop (cdr xs) O_WRONLY extra))
             ((read-write) (loop (cdr xs) O_RDWR extra))
             ((append)     (loop (cdr xs) access (bitwise-ior extra O_APPEND)))
             ((create)     (loop (cdr xs) access extra))
             (else (assertion-violation who "unknown open flag" f flags))))))))

  (define (fd-open path . opts)
    (unless (string? path)
      (assertion-violation 'fd-open "path must be a string" path))
    (let ((flags (if (pair? opts) (car opts) '(read))))
      (unless (list? flags)
        (assertion-violation 'fd-open "flags must be a list of symbols" flags))
      (when (memq 'create flags) (file-ensure! path))
      ;; THE INJECTED FAILURE IS RAISED THE WAY A REAL ONE IS, with the
      ;; same condition and the same errno field, so a caller cannot
      ;; treat "the fixture asked for this" as a separate case. In a
      ;; build without injection `open-fault` is the constant #f and this
      ;; folds away.
      ;;
      ;; AND IT STANDS WHERE THE FAILURE IT MODELS WOULD HAPPEN, which is
      ;; AFTER the create and not before it. Creating is a separate step
      ;; here, so a real `c-open` that fails on a create-mode open leaves
      ;; the file behind exactly as this does; injecting before the create
      ;; would leave the world in a state the failure being modelled never
      ;; produces. The rule is the one stated at the top of this file: a
      ;; return value may only be replaced when skipping the call leaves
      ;; the world as if it had failed.
      (let ((injected (open-fault path)))
        (when injected (raise (fs-err 'open path injected))))
      (let ((fd (c-open path (flags->int 'fd-open flags))))
        (when (< fd 0) (fail! 'open path))
        (hashtable-set! fd-paths fd path)
        fd)))

  ;; The entry is dropped BEFORE the close, so that a close which fails
  ;; cannot leave a descriptor number mapped to a path it no longer
  ;; refers to -- the number is reusable either way, and a stale mapping
  ;; would put the wrong path in the next trace.
  (define (fd-close fd)
    (let ((subject (subject-of fd '())))
      (forget-fd! fd)
      (let ((rc (c-close fd)))
        (when (< rc 0) (fail! 'close subject))
        (void))))

  ;; Closing on the way out of a dynamic-wind, where raising would
  ;; replace whatever was already on its way out.
  (define (close-quietly fd)
    (forget-fd! fd)
    (c-close fd))

  ;; ---- flushing ---------------------------------------------------------

  ;; fsync then F_FULLFSYNC on macOS, both required to succeed, matching
  ;; (igropyr durable). fsync there tells the drive nothing about its own
  ;; write cache; F_FULLFSYNC asks it to empty one. Nothing in this
  ;; process can observe the difference, so what the tests around this
  ;; assert is the call sequence, not durability itself.
  ;;
  ;; ONE TRACE LINE PER CALL, emitted after both steps have returned.
  ;; The platform-conditional second step is not a second event: a test
  ;; asserting an order of flushes would otherwise read differently on
  ;; the two platforms this runs on.
  ;; ONE FLUSH SEQUENCE, USED TWICE. fsync then F_FULLFSYNC on macOS,
  ;; both required to succeed, matching (igropyr durable). fsync there
  ;; tells the drive nothing about its own write cache; F_FULLFSYNC asks
  ;; it to empty one. Nothing in this process can observe the
  ;; difference, so what the tests around this assert is the call
  ;; sequence, not durability itself.
  ;;
  ;; THE OP SYMBOLS ARE PARAMETERS BECAUSE THE DIRECTORY FLUSH NEEDS ITS
  ;; OWN, and the two callers must not be two copies of the sequence. An
  ;; earlier version wrote it out twice, which cost the directory flush
  ;; its fault hook -- it could not be injected at all, so L18's and
  ;; L19's directory barriers had nothing to test against -- and left a
  ;; second place for the macOS branch to drift out of step silently.
  ;; (igropyr durable) explains why the symbols differ: a failure before
  ;; a rename leaves the old contents intact, one after it does not, and
  ;; collapsing them takes away the distinction the caller most needs.
  ;;
  ;; ONE TRACE LINE PER CALL, emitted after both steps have returned.
  ;; The platform-conditional second step is not a second event: a test
  ;; asserting an order of flushes would otherwise read differently on
  ;; the two platforms this runs on.
  (define (flush! fd subject fsync-op fullfsync-op kind)
    (case (fsync-fault fd subject kind)
      ;; Skipped AND unreported: see the note at the top on why this one
      ;; must not print a line. In a build without injection fsync-fault
      ;; is the constant #f and this dispatch folds away.
      ((skip) (void))
      ((fail) (raise (fs-err fsync-op subject EIO)))
      (else
       (let ((rc (c-fsync fd)))
         (when (< rc 0) (fail! fsync-op subject)))
       (when macos?
         (let ((rc (c-fcntl fd F_FULLFSYNC 0)))
           (when (< rc 0) (fail! fullfsync-op subject))))
       (trace-event! 'fsync subject #f)
       (void))))

  (define (fsync! fd subject stage)
    (check-stage! 'fsync! stage)
    (parameterize ((theourgia-stage stage))
      (flush! fd (subject-of fd (list subject)) 'fsync 'fullfsync 'file)))

  ;; A directory is opened read-only because opening one for writing
  ;; fails with EISDIR; fsync takes a descriptor for the file, not a
  ;; writable channel to it, so a read-only one flushes it fine. The
  ;; descriptor is closed even when the flush raises.
  (define (fsync-dir! path stage)
    (check-stage! 'fsync-dir! stage)
    (parameterize ((theourgia-stage stage))
    (let ((subject path))
      (let ((fd (c-open path O_RDONLY)))
        (when (< fd 0) (fail! 'dir-open subject))
        (let ((done (box #f)))
          (dynamic-wind
            void
            (lambda () (flush! fd subject 'dir-fsync 'dir-fullfsync 'dir))
            (lambda ()
              (unless (unbox done)
                (set-box! done #t)
                (close-quietly fd))))))
      (void))))

  ;; ---- size, position, truncation ---------------------------------------

  (define (whence->int who w)
    (case w
      ((set start) SEEK_SET)
      ((current cur) SEEK_CUR)
      ((end) SEEK_END)
      (else (assertion-violation who "unknown whence" w))))

  (define (fd-seek! fd offset whence . opts)
    (let ((subject (subject-of fd opts)))
      (let ((pos (c-lseek fd offset (whence->int 'fd-seek! whence))))
        (when (< pos 0) (fail! 'lseek subject))
        pos)))

  (define (fd-size fd . opts)
    (let ((subject (subject-of fd opts)))
      (let ((here (c-lseek fd 0 SEEK_CUR)))
        (when (< here 0) (fail! 'lseek subject))
        (let ((end (c-lseek fd 0 SEEK_END)))
          (when (< end 0) (fail! 'lseek subject))
          (let ((back (c-lseek fd here SEEK_SET)))
            (when (< back 0) (fail! 'lseek subject))
            end)))))

  ;; By seeking rather than by stat: a stat structure's layout differs
  ;; between these platforms and would have to be measured field by
  ;; field, while lseek returns one integer that means the same thing
  ;; everywhere.
  (define (file-size path)
    (when (stat-fault? path) (fail-with! 'stat path EIO))
    (let ((fd (fd-open path '(read))))
      (let ((done (box #f)))
        (dynamic-wind
          void
          (lambda () (fd-size fd path))
          (lambda ()
            (unless (unbox done)
              (set-box! done #t)
              (close-quietly fd)))))))

  (define (fail-with! op target code)
    (raise (fs-err op target code)))

  (define (ftruncate! fd length . opts)
    (unless (and (integer? length) (exact? length) (>= length 0))
      (assertion-violation 'ftruncate! "length must be a non-negative exact integer" length))
    (let ((subject (subject-of fd opts)))
      (let ((rc (c-ftruncate fd length)))
        (when (< rc 0) (fail! 'ftruncate subject))
        (trace-event! 'ftruncate subject length)
        (void))))

  ;; ---- writing ----------------------------------------------------------

  ;; A SHORT WRITE IS NOT AN ERROR AND EINTR IS NOT A FAILURE. write(2)
  ;; is permitted to accept fewer bytes than it was offered, and to
  ;; return -1/EINTR having accepted none; a caller that treats either
  ;; as failure loses records that were written, or writes them twice.
  ;; This returns only when the accepted counts sum to exactly the
  ;; length offered.
  ;;
  ;; ZERO PROGRESS IS AN ERROR. write returning 0 with bytes still to go
  ;; cannot be retried into success, and looping on it would turn a
  ;; broken descriptor into a hang -- the failure a test suite reports
  ;; worst.
  ;;
  ;; THE TAIL IS COPIED WHEN A WRITE IS SHORT. Chez's u8* passes the
  ;; address of a bytevector's data and there is no way to offset it, so
  ;; continuing from byte k means handing over a bytevector that starts
  ;; at k. Short writes to a local file are rare and a record is a line,
  ;; so the copy is paid on a path that mostly does not run; the
  ;; alternative -- a C buffer held across the loop -- would have to be
  ;; freed on every escape from it, including the raising ones.
  ;; THE ONLY PLACE write(2) IS CALLED, which is what makes the three
  ;; write faults one decision rather than three scattered ones. Returns
  ;; the count accepted and #f, or -1 and the errno to report. write-once
  ;; wraps it and is chosen at expansion time: in a build without
  ;; injection it IS this, with no branch in between.
  (define (real-write fd bv count)
    (let ((r (c-write fd bv count)))
      (if (< r 0) (values -1 (errno)) (values r #f))))

  (define short-count 7)

  ;; HOW FAR IT GOT IS REPORTED, NOT INFERRED. A caller that has to
  ;; classify "nothing written" against "partly written" was measuring
  ;; the file before and after -- and a size probe that itself fails
  ;; (a descriptor table that is full, say) then reads as "no progress",
  ;; which is precisely the answer that makes a retry duplicate the
  ;; record. The optional progress procedure is called with the running
  ;; total before each failure can escape, so the count comes from the
  ;; loop that did the writing.
  (define (write-all! fd bv . opts)
    (unless (bytevector? bv)
      (assertion-violation 'write-all! "not a bytevector" bv))
    (let* ((subject (subject-of fd opts))
           (progress (if (and (pair? opts) (pair? (cdr opts)) (procedure? (cadr opts)))
                         (cadr opts)
                         (lambda (n) (if #f #f))))
           (total (bytevector-length bv)))
      (let loop ((pos 0) (chunk bv))
        (if (>= pos total)
            total
            (let-values (((n code) (write-once fd chunk (- total pos) subject)))
              (cond
                ((and (< n 0) (= code EINTR))
                 (loop pos chunk))
                ((< n 0)
                 (raise (fs-err 'write subject code)))
                ((= n 0)
                 (raise (fs-err 'write subject #f)))
                (else
                 (trace-event! 'write subject n)
                 (progress (+ pos n))
                 (let ((pos (+ pos n)))
                   (if (>= pos total)
                       (loop pos chunk)
                       (let ((tail (make-bytevector (- total pos))))
                         (bytevector-copy! bv pos tail 0 (- total pos))
                         (loop pos tail)))))))))))

  ;; ---- identity ----------------------------------------------------------

  ;; A STORE'S IDENTITY IS BOUND TO ITS DIRECTORY'S (device, inode), so
  ;; that a copy made to another path or another machine is refused the
  ;; identity it copied. That needs two fields of struct stat, and this
  ;; is the one place this library reads a C structure -- the thing its
  ;; header says it avoided, taken deliberately and narrowly because
  ;; nothing else can answer the question.
  ;;
  ;; MEASURED ON BOTH TARGETS, and they are NOT the same, which is
  ;; exactly why this is not read from a table:
  ;;
  ;;   macOS 15.3 arm64   sizeof(struct stat) 144
  ;;                      st_dev at 0, FOUR bytes    st_ino at 8, eight
  ;;   FreeBSD 15.0       sizeof(struct stat) 224
  ;;                      st_dev at 0, EIGHT bytes   st_ino at 8, eight
  ;;
  ;; Reading eight bytes of st_dev on macOS would take st_mode and
  ;; st_nlink along with it and produce an enormous number that differs
  ;; run to run -- an identity check that fails for a store nobody
  ;; touched. st_ino agrees on both, and that agreement is measured and
  ;; not assumed.
  ;;
  ;; A THIRD PLATFORM MEANS COMPILING THE PROGRAM THERE. The buffer is
  ;; oversized so that a wrong guess cannot smash the heap, but an
  ;; oversized buffer does not make wrong offsets right: the values
  ;; would simply be some other fields, silently.
  (define stat-buffer-size 512)
  (define st-dev-offset 0)
  (define st-ino-offset 8)

  (define (path-device-inode path)
    (unless (string? path)
      (assertion-violation 'path-device-inode "path must be a string" path))
    (let ((buf (make-bytevector stat-buffer-size 0)))
      (let ((rc (c-stat path buf)))
        (when (< rc 0) (fail! 'stat path))
        (values (if macos?
                    (bytevector-u32-native-ref buf st-dev-offset)
                    (bytevector-u64-native-ref buf st-dev-offset))
                (bytevector-u64-native-ref buf st-ino-offset)))))

  ;; Version hints invalidate derived indexes; they never prove a record.
  ;; ctime catches replacements and same-length edits even if mtime is restored.
  (define (path-version path)
    (let-values (((device inode) (path-device-inode path)))
      (let ((modified (file-modification-time path))
            (changed (file-change-time path)))
        (list device inode (time-second modified) (time-nanosecond modified)
              (time-second changed) (time-nanosecond changed)
              (and (file-is-regular? path) (file-size path))))))

  ;; ---- linking ----------------------------------------------------------

  ;; THE WHOLE PUBLISH PROTOCOL RESTS ON THIS CALL FAILING. link(2)
  ;; refuses when the target exists, and that refusal -- not a preceding
  ;; existence test -- is what makes installing a segment safe against a
  ;; second process doing the same thing at the same moment. A check
  ;; followed by a link is two operations with a gap; this is one.
  ;; Measured on both platforms: EEXIST, 17.
  ;;
  ;; SO THE REFUSAL IS A RESULT, NOT AN ERROR. 'exists is an ordinary
  ;; answer the caller acts on (re-read the target and compare); every
  ;; other failure raises, because none of them has a next step this
  ;; library could name.
  (define (link! from to)
    (unless (and (string? from) (string? to))
      (assertion-violation 'link! "paths must be strings" from to))
    (let ((rc (c-link from to)))
      (cond
        ((>= rc 0) (trace-event! 'link to #f) 'linked)
        ((= (errno) EEXIST) 'exists)
        (else (fail! 'link to)))))

  ;; ---- locking ----------------------------------------------------------

  ;; flock IS PER OPEN FILE DESCRIPTION, NOT PER PROCESS. Two of these
  ;; nested on the same path in one process deadlock against each other,
  ;; because the inner one opens a second descriptor and waits for the
  ;; outer one to let go. That is a property of the syscall and not
  ;; something this wrapper can soften; the lock is held for a bounded
  ;; region and never re-entered.
  ;;
  ;; THE NON-BLOCKING ATTEMPT COMES FIRST so that waiting is observable.
  ;; Whether a writer had to queue behind another is exactly what a
  ;; concurrency test needs to see, and it cannot be recovered from the
  ;; outcome: the same final state is reached whether the wait happened
  ;; or not.
  ;; NEITHER HELPER CREATES THE LOCK FILE, and that is what makes "a
  ;; reader changes no files" a property of the code rather than a rule
  ;; someone has to keep. An earlier version called file-ensure! here,
  ;; so taking a SHARED lock on a store whose lock file was missing
  ;; CREATED it -- measured, a zero-byte file appearing under a
  ;; read-only command, which is the invariant read-only callers exist
  ;; to preserve and the file-set assertion in the reader case would
  ;; have caught only if it thought to look for a file nobody wrote.
  ;;
  ;; The lock file is created once, when the store is initialised, and
  ;; is then never deleted or replaced. file-ensure! is exported for
  ;; that one caller. A missing lock file here means the store was never
  ;; initialised or has been damaged, and refusing says so at the point
  ;; where it is still true.
  ;; A LOCK THAT OUTLIVES A CALL, because the log layer's session takes
  ;; the store lock in one operation and releases it in another: a
  ;; scoped helper cannot express that, and a session returned from
  ;; inside one would be holding nothing.
  ;;
  ;; THE COST IS THAT RELEASING IS NOW SOMEBODY'S JOB. The scoped
  ;; helpers below still exist and are still what to use for anything
  ;; that fits in one call; this pair is for the one caller that cannot,
  ;; and that caller owns the release on every path out, including the
  ;; raising ones. Nothing here can do it for them -- that is the whole
  ;; difference between the two shapes, and it is why this one is not
  ;; the default.
  (define-record-type lock-handle
    (fields path mode-name (mutable fd) (mutable held)))

  (define (lock-fd l) (lock-handle-fd l))
  (define (lock-held? l) (and (lock-handle-held l) #t))

  (define (mode->int who m)
    (case m
      ((exclusive) LOCK_EX)
      ((shared) LOCK_SH)
      (else (assertion-violation who "mode must be exclusive or shared" m))))

  ;; THE NON-BLOCKING ATTEMPT COMES FIRST so that waiting is observable.
  ;; Whether a writer had to queue behind another is exactly what a
  ;; concurrency test needs to see, and it cannot be recovered from the
  ;; outcome: the same final state is reached whether the wait happened
  ;; or not.
  (define (lock-acquire! path mode)
    (unless (string? path)
      (assertion-violation 'lock-acquire! "path must be a string" path))
    (let* ((m (mode->int 'lock-acquire! mode))
           (subject (cons path mode)))
      (let ((fd (c-open path O_RDONLY)))
        (when (< fd 0) (fail! 'open path))
        (hashtable-set! fd-paths fd path)
        (unless no-flock?
          (guard (e (#t (close-quietly fd) (raise e)))
            (let ((rc (c-flock fd (+ m LOCK_NB))))
              (when (< rc 0)
                (trace-event! 'lock-wait subject #f)
                (let retry ()
                  (let ((rc (c-flock fd m)))
                    (when (< rc 0)
                      (if (= (errno) EINTR)
                          (retry)
                          (fail! 'flock path))))))))
          (trace-event! 'flock subject #f))
        (make-lock-handle path mode fd #t))))

  ;; ⛔ THE ATTEMPT THAT NEVER WAITS. `lock-acquire!` blocks when the lock
  ;; is held, and blocking is exactly what an actor may not do: the flock
  ;; runs on the scheduler's own thread, so a process parked in it stops
  ;; every other process in the VM -- including the one that would have
  ;; released the lock. A daemon that finds the lock held has to SAY so
  ;; and leave, and a store process retrying has to come back and try
  ;; again later; both need an answer, not a wait.
  ;;
  ;; Answers a lock handle, or #f when somebody else holds it.
  (define (lock-try-acquire! path mode)
    (unless (string? path)
      (assertion-violation 'lock-try-acquire! "path must be a string" path))
    (let ((m (mode->int 'lock-try-acquire! mode)))
      (let ((fd (c-open path O_RDONLY)))
        (when (< fd 0) (fail! 'open path))
        (hashtable-set! fd-paths fd path)
        (cond
          (no-flock? (make-lock-handle path mode fd #t))
          (else
           (let ((rc (guard (e (#t (close-quietly fd) (raise e)))
                       (c-flock fd (+ m LOCK_NB)))))
             (cond
               ((< rc 0) (close-quietly fd) #f)
               (else (trace-event! 'flock (cons path mode) #f)
                     (make-lock-handle path mode fd #t)))))))))

  ;; Idempotent, because the caller owning the release will sometimes
  ;; release on two paths out of the same region and must not have to
  ;; track which one ran.
  (define (lock-release! l)
    (unless (lock-handle? l)
      (assertion-violation 'lock-release! "not a lock" l))
    (when (lock-handle-held l)
      (lock-handle-held-set! l #f)
      (let* ((fd (lock-handle-fd l))
             (subject (cons (lock-handle-path l) (lock-handle-mode-name l)))
             (released (>= (c-flock fd LOCK_UN) 0)))
        (close-quietly fd)
        (when released (trace-event! 'unlock subject #f))))
    (void))

  ;; NEITHER HELPER CREATES THE LOCK FILE, and that is what makes "a
  ;; reader changes no files" a property of the code rather than a rule
  ;; someone has to keep. An earlier version called file-ensure! here,
  ;; so taking a SHARED lock on a store whose lock file was missing
  ;; CREATED it -- measured, a zero-byte file appearing under a
  ;; read-only command, which is the invariant read-only callers exist
  ;; to preserve.
  ;;
  ;; The lock file is created once, when the store is initialised, and
  ;; is then never deleted or replaced. file-ensure! is exported for
  ;; that one caller. A missing lock file here means the store was never
  ;; initialised or has been damaged, and refusing says so at the point
  ;; where it is still true.
  ;;
  ;; flock IS PER OPEN FILE DESCRIPTION, NOT PER PROCESS. Two of these
  ;; nested on the same path in one process deadlock against each other,
  ;; because the inner one opens a second descriptor and waits for the
  ;; outer one to let go. That is a property of the syscall and not
  ;; something this wrapper can soften.
  ;;
  ;; BUILT ON THE PAIR ABOVE so the acquisition sequence has one
  ;; implementation. What this adds is the release, on every path out.
  ;; ⛔ HOW A LOCK IS TAKEN IS THE PROGRAM'S DECISION, NOT THIS FILE'S.
  ;; The default is the blocking acquire, so every existing caller --
  ;; the CLI above all -- behaves exactly as before. A daemon sets this
  ;; once, for its whole lifetime, to an attempt that never waits: an
  ;; actor parked in `flock` runs on the scheduler's own thread and
  ;; stops every other process in the VM, including the one that would
  ;; have released the lock.
  ;;
  ;; ⚠️ AND IT IS SET, NOT PARAMETERIZED AROUND A PROCESS. Measured:
  ;; Chez parameters are per OS THREAD, and green threads share one --
  ;; so a `parameterize` in one actor is visible to every actor that
  ;; runs during its extent, and gone again afterwards. Neither half is
  ;; what a per-process override would need. What keeps the CLI on the
  ;; default is that nothing in the CLI ever sets this.
  (define current-lock-acquire (make-parameter lock-acquire!))

  ;; ⛔ THE PAIR IS OVERRIDDEN TOGETHER OR NOT AT ALL. A strategy in
  ;; which somebody other than the caller opens the descriptor must
  ;; also be the one to close it: a release that ran here would shut a
  ;; descriptor its owner still has in a table, and the number would be
  ;; handed to the next `open` in this same VM while that table still
  ;; named it. Symmetry is the point, so the default is the matching
  ;; direct release and every caller below goes through both.
  (define current-lock-release (make-parameter lock-release!))

  (define (call-with-lock who path mode-name proc)
    (unless (procedure? proc)
      (assertion-violation who "not a procedure" proc))
    (let ((l ((current-lock-acquire) path mode-name)))
      (dynamic-wind
        void
        (lambda () (proc (lock-fd l)))
        (lambda () ((current-lock-release) l)))))

  (define (with-exclusive-lock path proc)
    (call-with-lock 'with-exclusive-lock path 'exclusive proc))

  (define (with-shared-lock path proc)
    (call-with-lock 'with-shared-lock path 'shared proc)))
