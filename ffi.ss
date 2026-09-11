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
;;; FAULT INJECTION LIVES AT TWO CALL SITES AND NOWHERE ELSE, armed by
;;; THEOURGIA_FAULT=<name> or <name>:<path-substring>:
;;;
;;;   short-write               the first write offers min(7, n) bytes
;;;   eintr-once                the first write is skipped, reporting EINTR
;;;   write-eio-after-partial   min(7, n) bytes, then EIO
;;;   fsync-fail                fsync reports EIO
;;;   no-log-fsync              fsync is SKIPPED and reports success
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
;;; TWO CLASSES, AND THE DIFFERENCE IS DELIBERATE. short-write and
;;; eintr-once are ONE-SHOT within their stage: they model a transient
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
;;; write really writes none; a skipped fsync really does not flush. The
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
          barrier!
          lock-acquire! lock-release! lock-fd lock-held?
          path-device-inode
          fs-error? fs-error-op fs-error-target fs-error-errno
          theourgia-fault theourgia-fault-armed? theourgia-stage
          trace-enabled? trace-enable! trace-event!
          directory-entries file-is-directory? file-is-regular? rename-over!
          unlink! file-create-exclusive! mkdir-p!
          process-id wall-clock-ms machine-home)
  (import (chezscheme)
          (theourgia trace)
          (only (igropyr platform)
                platform-os ensure-supported-platform!
                load-first-shared-object!))

  (define platform-checked (begin (ensure-supported-platform!) #t))

  ;; THE PLATFORM LAYER READS THE ENVIRONMENT AND INJECTS THE SWITCH.
  ;; (theourgia trace) takes neither getenv nor a parameter so that it
  ;; stays portable to a host with no environment; this is the one place
  ;; that knows there is one.
  (define trace-switch-installed
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

     (define known-stages
       '(deliver-barrier commit registry publish snapshot repair))

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
         ((and (> (string-length a) 5) (string=? (substring a 0 5) "file="))
          (values 'file (substring a 5 (string-length a))))
         ((and (> (string-length a) 4) (string=? (substring a 0 4) "dir="))
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

     (define-values (fault-kind fault-substring) (split-kind fault-arg))

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
     (define fault-argument-checked
       (when (memq fault-name '(fsync-fail no-log-fsync))
         (unless (and (string? fault-arg) (> (string-length fault-arg) 0))
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

     (define (string-contains? s sub)
       (let ((n (string-length s)) (m (string-length sub)))
         (let loop ((i 0))
           (cond
             ((> (+ i m) n) #f)
             ((string=? (substring s i (+ i m)) sub) #t)
             (else (loop (+ i 1)))))))

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
     (define (stat-fault? path)
       (and fault-name
            (eq? fault-name 'stat-fail)
            (in-fault-stage?)
            (or (not fault-arg) (fault-path-match? #f path))
            (eq? (unbox fault-state) 'fresh)
            (begin (set-box! fault-state 'done) #t)))

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

  (define (fsync! fd . opts)
    (flush! fd (subject-of fd opts) 'fsync 'fullfsync 'file))

  ;; A directory is opened read-only because opening one for writing
  ;; fails with EISDIR; fsync takes a descriptor for the file, not a
  ;; writable channel to it, so a read-only one flushes it fine. The
  ;; descriptor is closed even when the flush raises.
  (define (fsync-dir! path . opts)
    (let ((subject (if (pair? opts) (car opts) path)))
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
      (void)))

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
  (define (call-with-lock who path mode-name proc)
    (unless (procedure? proc)
      (assertion-violation who "not a procedure" proc))
    (let ((l (lock-acquire! path mode-name)))
      (dynamic-wind
        void
        (lambda () (proc (lock-fd l)))
        (lambda () (lock-release! l)))))

  (define (with-exclusive-lock path proc)
    (call-with-lock 'with-exclusive-lock path 'exclusive proc))

  (define (with-shared-lock path proc)
    (call-with-lock 'with-shared-lock path 'shared proc)))
