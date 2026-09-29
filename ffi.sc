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
;;; CONSTANTS ARE MEASURED, NOT REMEMBERED, AND NONE IS WRITTEN HERE.
;;; Every flag, errno, request number, struct size, offset and width this
;;; file passes to or reads from the kernel is the platform's row in
;;; (theourgia platform-numbers): the output of test/probe/layout.c,
;;; compiled against the system headers on that platform.
;;;
;;; This file once held them as literals, measured on macOS and FreeBSD,
;;; with this warning: the values agreeing across two Unixes is exactly
;;; what makes assuming a third tempting, and exactly what would make a
;;; wrong one hard to notice. The third, Linux, disagreed on seven of
;;; them -- O_APPEND among them, so a segment opened for append was
;;; written from its start -- and none of it announced itself until a
;;; Linux machine ran the core. A fifth platform means compiling the
;;; probe there and adding its row.
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
;;; only when the flag says the way through did not happen. F100a applies
;;; it to every close of a descriptor that was never written (read-closing:
;;; read-entry, read-entry-range, list-entries, file-size, fsync-dir!) and
;;; to the lock's release (call-with-lock): checked on the way through,
;;; quiet in the unwind.

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
;;; TWO CONDITIONS, BY PRIMITIVE CLASS (F100 D1). A NON-MUTATING primitive
;;; that fails raises `unreadable-entry` (path reason errno); a MUTATING one
;;; raises the durable-error vector above (op subject errno). Absence is
;;; the unreadable-entry whose errno is ENOENT or ENOTDIR; only the four R1
;;; queries answer `absent` for it instead. THE DOOR COSTS +12 ms PER START
;;; FROM SOURCE (measured, (theourgia ffi) loaded alone, F100a; see
;;; test/f0-ondemand.sc's F0-2 header); a .so build does not expand this file
;;; at start (not measured). The errno is kept at every site, in two forms: unreadable-entry carries its NAME where this file
;;; knows one (EACCES, ENOENT, ...; the number otherwise), durable-error its
;;; NUMBER, as the exported constants are. Unifying the two is its own item. Every filesystem operation of the product goes through this
;;; file (test/unreadable-census.sc, section `door`). One line per export:
;;;
;;;   entry-type          stat            unreadable-entry | absent
;;;   entry-name-type     lstat           unreadable-entry | absent; a
;;;                                       symbolic link is `link`, never
;;;                                       followed
;;;   read-entry          open, read      unreadable-entry | absent
;;;   list-entries        opendir, read   unreadable-entry | absent
;;;   size-entry          stat            unreadable-entry | absent
;;;   entry-bytes         open, read      unreadable-entry, ENOENT included
;;;   read-entry-range    open, lseek,    unreadable-entry, ENOENT included;
;;;                       read            an offset at or past the end is an
;;;                                       empty bytevector, a range that
;;;                                       crosses the end a shorter one
;;;   directory-entries   opendir, read   unreadable-entry, ENOENT included
;;;   file-is-directory?  stat            unreadable-entry; #f when absent
;;;   file-is-regular?    stat            unreadable-entry; #f when absent
;;;   path-device-inode   stat            unreadable-entry, ENOENT included
;;;   path-version        stat, times     unreadable-entry, ENOENT included
;;;   file-size           open, lseek     unreadable-entry, ENOENT included
;;;   fd-open             open            unreadable-entry, ENOENT included
;;;     with 'create      create, open    durable-error from the create;
;;;                                       unreadable-entry from the open
;;;   fd-seek! fd-size    lseek           unreadable-entry naming the path
;;;   fd-close            close           durable-error for a descriptor
;;;                                       written or truncated, else
;;;                                       unreadable-entry
;;;   lock-acquire!       open, flock     unreadable-entry; waits only on
;;;                                       contention
;;;   lock-try-acquire!   open, flock     unreadable-entry; #f on contention
;;;   lock-release!       flock, close    unreadable-entry, both attempted
;;;   with-exclusive-lock with-shared-lock
;;;                       as acquire and release; the release is checked
;;;                       on a normal exit, quiet on an escape
;;;   write-all!          write           durable-error
;;;   write-one!          write, once     durable-error; a short write is one
;;;                                       (op write, errno #f), never retried
;;;   ftruncate!          ftruncate       durable-error
;;;   fsync! fsync-dir!   fsync           durable-error; fsync-dir!'s open
;;;                                       of the directory is
;;;                                       unreadable-entry (op dir-open)
;;;   file-ensure!        stat, create,   unreadable-entry from the stat
;;;                       close           and from the close of the port
;;;                                       it never wrote; durable-error
;;;                                       from the create
;;;   file-create-exclusive!  create,     durable-error from the create;
;;;                       close           unreadable-entry from the close
;;;                                       of the port it never wrote; #f
;;;                                       when the name exists
;;;   overwrite-entry!    create, open,   as its steps: durable-error from
;;;                       truncate, write a mutating one, unreadable-entry
;;;                                       from the stat, the create's
;;;                                       unwritten close or the open. Its
;;;                                       record:
;;;                                       (create p) (truncate p) (write
;;;                                       p) for a fresh path, (truncate
;;;                                       p) (write p) for an existing
;;;                                       file; no (write p) for empty
;;;                                       bytes
;;;   mkdir-p!            stat, mkdir     unreadable-entry from a stat;
;;;                                       durable-error from a mkdir; a
;;;                                       directory another process made
;;;                                       first is success
;;;   mkdir-exclusive!    mkdir           durable-error; 'exists on EEXIST,
;;;                                       whatever occupies the name
;;;   rename-over!        rename          durable-error
;;;   link!               link            durable-error; 'exists on EEXIST
;;;   unlink!             unlink          durable-error
;;;   file-is-socket? real-path           #f for any failure (their
;;;                                       handlers are F100c's)
;;;   path-case-sensitive?  pathconf; `unknown` for any failure, not
;;;                       classed (it asks about the volume, not an entry)
;;;   EIO ENOENT EACCES ENOTDIR EBADF ECHILD  the errno numbers, as
;;;                                       durable-error carries them
;;;   waitpid-status      waitpid         #f while the child runs, (exit n)
;;;                                       or (signal n) once it has ended;
;;;                                       durable-error (op waitpid) for a
;;;                                       pid that is not a child (ECHILD)
;;;   hold-point!         (injection only) blocks at a named stage until a
;;;                                       release file exists (THEOURGIA_HOLD)
;;;   errno-text          strerror        the system's message for an errno
;;;                                       number; no filesystem access (the
;;;                                       reason (theourgia answers) gives a
;;;                                       durable-error, F100b)
;;;
;;; A read's descriptor, never written, is closed on the normal path with
;;; its failure raised as unreadable-entry (close-fail reaches it where the
;;; reader opened a descriptor, not a directory stream).
;;;
;;; THE MUTATION RECORD (with-mutation-record, mutation-record,
;;; mutation-set-self!) is described where it is defined.
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
;;;   stat-fail                 a stat reports EIO, or the errno it is given
;;;   read-fail-after           a read, once it has produced at least one
;;;                             byte, reports the errno it is given
;;;   readdir-fail-after        a listing, once it has produced at least
;;;                             one entry, reports the errno it is given
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
;;; AND IT IS READ FOR THE FAULTS THAT FAIL WITH AN ERRNO -- open-fail,
;;; stat-fail, read-fail-after and readdir-fail-after -- AND NOTHING ELSE.
;;; It is required for all of them but stat-fail, which predates it and
;;; reports EIO when it is absent. Stripping the tail from every fault's
;;; argument would change where an existing case points: `fsync-fail@commit:file=a:errno=b` is a path substring with a
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
          ftruncate! fsync! fsync-dir! write-all! write-one!
          fd-seek! fd-size file-size file-ensure! fd-path link!
          setsid! session-id signal-pid! process-alive-signal0?
          setrlimit! getrlimit RLIMIT_CPU process-rss-bytes isolate-evaluation!
          barrier!
          lock-acquire! lock-try-acquire! current-lock-acquire
          lock-release! current-lock-release lock-fd lock-held?
          path-device-inode path-version real-path
          fs-error? fs-error-op fs-error-target fs-error-errno errno-text
          theourgia-fault theourgia-fault-armed? theourgia-stage known-stages
          report-fault?
          trace-enabled? trace-enable! trace-event!
          directory-entries file-is-directory? file-is-regular? file-is-socket? rename-over!
          entry-type entry-name-type read-entry list-entries
          unreadable-entry? unreadable-entry-path unreadable-entry-reason
          unreadable-entry-errno make-unreadable-entry
          unlink! file-create-exclusive! mkdir-p! mkdir-exclusive!
          size-entry overwrite-entry! entry-bytes read-entry-range
          with-mutation-record mutation-record mutation-set-self!
          EIO ENOENT EACCES ENOTDIR EBADF ECHILD waitpid-status
          hold-point! hold-sleeper-set!
          source-reader-open source-reader-next source-reader-at source-reader-observer!
          source-datum-print exec-argv! exec-argv-env! path-executable? rmdir!
          unix-socket-connect fd-read socket-timeout! sun-path-max sockaddr-un
          redirect-stdio! spawn-detached! spawn-captured! reap-children! path-case-sensitive?
          fd-close-on-exec! fd-close-on-exec? online-processors mkdir-p-unrecorded!
          file-ensure-unrecorded!
          process-id wall-clock-ms machine-home env-or)
  (import (chezscheme)
          (only (igropyr platform)
                platform-os ensure-supported-platform! load-first-shared-object!)
          (only (igropyr util) string-contains?)
          (only (theourgia platform-numbers)
                platform-number platform-field platform-field-present?
                platform-struct-size platform-struct-value platform-type-size)
          (theourgia trace))

  ;; ---- a platform's struct fields, at the row's offsets and sizes ----------
  ;;
  ;; Every read or write of a field inside a struct the kernel fills or
  ;; reads goes through these, so the offset and the width both come from
  ;; the platform's row (theourgia platform-numbers). SIGNEDNESS IS THE
  ;; READER'S, from the C type: the rows carry none, and st_size (off_t)
  ;; is the one field read signed.
  (define (row-uint-ref bv sname fname)
    (bytevector-uint-ref bv (platform-field sname fname 'offset) (native-endianness)
                         (platform-field sname fname 'size)))
  (define (row-sint-ref bv sname fname)
    (bytevector-sint-ref bv (platform-field sname fname 'offset) (native-endianness)
                         (platform-field sname fname 'size)))
  (define (row-uint-set! bv sname fname value)
    (bytevector-uint-set! bv (platform-field sname fname 'offset) value (native-endianness)
                          (platform-field sname fname 'size)))

  ;; A pid the kernel writes back (posix_spawn's first argument), at the
  ;; width of the row's pid_t, read unsigned as it always was.
  (define (make-pid-buffer) (make-bytevector (platform-type-size 'pid_t) 0))
  (define (pid-buffer-ref bv)
    (bytevector-uint-ref bv 0 (native-endianness) (platform-type-size 'pid_t)))

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

  ;; THE SAME REPLACEMENT WITH AN ENVIRONMENT OF THE CALLER'S MAKING: execve on
  ;; a path already resolved, with `env` ("NAME=value" strings) as the whole
  ;; environment the new program sees -- nothing of this process's own is
  ;; inherited. If exec returns, the same raise as exec-argv!.
  (define (exec-argv-env! path args env)
    (let* ((width (foreign-sizeof 'void*))
           (c-strings
             (lambda (xs)
               (map (lambda (s)
                      (let* ((b (string->utf8 s)) (n (bytevector-length b)) (p (foreign-alloc (+ n 1))))
                        (do ((i 0 (+ i 1))) ((= i n)) (foreign-set! 'unsigned-8 p i (bytevector-u8-ref b i)))
                        (foreign-set! 'unsigned-8 p n 0) p)) xs)))
           (vector-of
             (lambda (ps)
               (let ((v (foreign-alloc (* width (+ 1 (length ps))))))
                 (do ((xs ps (cdr xs)) (i 0 (+ i 1))) ((null? xs))
                   (foreign-set! 'void* v (* i width) (car xs)))
                 (foreign-set! 'void* v (* (length ps) width) 0)
                 v)))
           (arg-strings (c-strings args)) (env-strings (c-strings env))
           (argv (vector-of arg-strings)) (envp (vector-of env-strings)))
      ((foreign-procedure "execve" (string void* void*) int) path argv envp)
      (for-each foreign-free arg-strings) (for-each foreign-free env-strings)
      (foreign-free argv) (foreign-free envp)
      (raise '(error launcher-unavailable))))

  ;; A PROCESS ABOUT TO RUN SOMETHING IT DOES NOT TRUST: a session of its own,
  ;; so the supervisor can signal the whole group, and a CPU ceiling. Shared
  ;; by the Scheme worker and the runner's launcher, which both call it
  ;; before they say ready.
  ;; NOTE: THE CEILING IS ADDED TO THE CPU ALREADY SPENT. RLIMIT_CPU counts
  ;; the process's whole life, and its start-up has used some of it before
  ;; this call; the supervisor's wall budget starts at ready, so the CPU
  ;; budget starts here too, or a slow start-up would leave a legal
  ;; evaluation less CPU than wall time. A refused request (a host whose
  ;; inherited hard limit sits below it) leaves the inherited limits in
  ;; force; the caller says so and carries on.
  ;; -> (values pgid ceiling status), status 0 when the limit was set.
  (define (isolate-evaluation! cpu-seconds)
    (let* ((pgid (setsid!))
           (ceiling (+ cpu-seconds (div (+ (cpu-time) 999) 1000)))
           (status (setrlimit! RLIMIT_CPU ceiling ceiling)))
      (values pgid ceiling status)))

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
    ;; NEVER: THIS OVERWRITES ANY `trace-enable!` MADE BEFORE THIS LIBRARY
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
  ;; A LISTING THAT IS NOT AN R1 QUERY: absence raises unreadable-entry
  ;; with its errno, ENOENT or ENOTDIR, like every other failure (F100
  ;; D1). list-entries answers `absent` instead, for a caller that has a
  ;; next step for it.
  (define (directory-entries path)
    (let ((r (list-entries/errno path)))
      (if (vector? r) (unreadable! path (vector-ref r 1)) r)))
  ;; THE TYPE QUESTIONS ARE ASKED OF entry-type, so a path that cannot be
  ;; read raises unreadable-entry instead of answering #f -- #f is the
  ;; answer for a path that is not there, or is something else.
  (define (file-is-directory? path) (eq? (entry-type path) 'directory))
  (define (file-is-regular? path) (eq? (entry-type path) 'regular))

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
  ;; A MUTATING PRIMITIVE, SO durable-error WITH THE SYSCALL'S ERRNO (F100
  ;; D1). Noted after success, and on EIO, the one failure under which
  ;; POSIX allows the rename to have partly happened.
  (define (rename-over! from to)
    (unless (and (string? from) (string? to))
      (assertion-violation 'rename-over! "paths must be strings" from to))
    (let ((rc (c-rename from to)))
      (cond
        ((>= rc 0)
         (note! (list 'rename from to))
         (trace-event! 'rename (cons from to) #f))
        (else
         (let ((code (errno)))
           (when (eqv? code EIO) (note! (list 'rename from to)))
           (raise (fs-err 'rename to code)))))))

  ;; CREATE-IF-ABSENT'S SIBLING: create-or-fail. Both are here rather
  ;; than at the call site so that every directory entry this library
  ;; makes appear is traced in one place -- the crash model reconstructs
  ;; surviving entries from the trace, and an untraced create is an entry
  ;; the reconstruction cannot know existed. Returns #f when the name is
  ;; already taken, which is a collision the caller retries, and raises
  ;; for everything else.
  ;; The create is Chez's (open(2) cannot be handed a mode from here), so
  ;; its errno is read from Chez's condition class (condition-errno). The
  ;; create is noted before the close, which can still fail.
  (define (file-create-exclusive! path)
    (unless (string? path)
      (assertion-violation 'file-create-exclusive! "path must be a string" path))
    (let ((port (guard (e ((i/o-file-already-exists-error? e) #f)
                          ((fs-error? e) (raise e))
                          ((unreadable-entry? e) (raise e))
                          (#t (raise (fs-err 'create path (condition-errno e)))))
                  (open-file-output-port path))))
      (and port
           (begin (note! (list 'create path))
                  (trace-event! 'create path #f)
                  (close-unwritten-port! path port)
                  path))))

  ;; CREATE A DIRECTORY, PARENTS INCLUDED. R6RS has no mkdir; this is
  ;; the platform layer's job like every other name-space change, and it
  ;; is traced for the same reason the others are -- the crash model
  ;; rebuilds surviving entries from the trace.
  (define (mkdir-p! path) (mkdir-p-body! path #t))

  ;; THE SAME, WITHOUT A NOTE, for directories that are administration and
  ;; not the request's writes -- the eval admission's slot directory. It is
  ;; mkdir-p! in every other respect, the failures included.
  (define (mkdir-p-unrecorded! path) (mkdir-p-body! path #f))

  (define (mkdir-p-body! path record?)
    (unless (and (string? path) (> (string-length path) 0))
      (assertion-violation 'mkdir-p! "path must be a non-empty string" path))
    (let loop ((i 1))
      (cond
        ((> i (string-length path))
         (unless (file-is-directory? path)
           (mkdir-one! path record?))
         path)
        ((or (= i (string-length path)) (char=? (string-ref path i) #\/))
         (let ((prefix (substring path 0 i)))
           (unless (or (string=? prefix "") (file-is-directory? prefix))
             (mkdir-one! prefix record?)))
         (loop (+ i 1)))
        (else (loop (+ i 1))))))

  ;; A SWALLOWED FAILURE IS ACCEPTED ONLY IF THE DIRECTORY IS THERE
  ;; AFTERWARDS. The race this tolerates is another process creating the
  ;; same directory first; every other failure -- a permission error, a
  ;; regular file already occupying the name -- was being reported as
  ;; success, so mkdir-p! on "/dev/null" answered "/dev/null".
  ;; NOTE: WITH mkdir(2)'S OWN ERRNO (F100 D1), and noted only when this
  ;; call made the directory. The injected mkdir-fail stands where the
  ;; syscall would and skips it, so it creates nothing and notes nothing.
  (define (mkdir-one! path record?)
    (let-values (((rc code)
                  (let ((injected (mkdir-fault path)))
                    (if injected
                        (values -1 injected)
                        (let ((rc (c-mkdir path #o777)))
                          (values rc (and (< rc 0) (errno))))))))
      (cond
        ((>= rc 0)
         (when record? (note! (list 'mkdir path)))
         (trace-event! 'create path #f))
        ((file-is-directory? path) (void))
        (else (raise (fs-err 'mkdir path code))))))

  ;; ONE DIRECTORY, CREATED BY THIS CALL OR NOT AT ALL. mkdir-p! treats a
  ;; directory that is already there as success; a caller that will remove
  ;; what it made cannot, since it would then remove something it did not
  ;; make. Here EEXIST -- whatever occupies the name, directory or not -- is
  ;; the answer 'exists, an ordinary result like link!'s; 'created means
  ;; this call made it. Any other failure raises durable-error with the
  ;; errno. The parent must exist. The injected mkdir-fail stands where the
  ;; syscall would, as in mkdir-one!.
  ;;
  ;; AN OPTIONAL MODE, #o777 WHEN IT IS NOT GIVEN, so every existing caller
  ;; makes what it made before. mkdir(2) is not variadic and its mode is a
  ;; named argument, which this declaration passes; the process umask still
  ;; applies. A caller making a directory only it may enter passes #o700.
  (define mkdir-exclusive!
    (case-lambda
      ((path) (mkdir-exclusive-mode! path #o777))
      ((path mode) (mkdir-exclusive-mode! path mode))))

  (define (mkdir-exclusive-mode! path mode)
    (unless (and (string? path) (> (string-length path) 0))
      (assertion-violation 'mkdir-exclusive! "path must be a non-empty string" path))
    (unless (and (fixnum? mode) (<= 0 mode #o7777))
      (assertion-violation 'mkdir-exclusive! "mode must be an integer in 0..#o7777" mode))
    (let-values (((rc code)
                  (let ((injected (mkdir-fault path)))
                    (if injected
                        (values -1 injected)
                        (let ((rc (c-mkdir path mode)))
                          (values rc (and (< rc 0) (errno))))))))
      (cond
        ((>= rc 0)
         (note! (list 'mkdir path))
         (trace-event! 'create path #f)
         'created)
        ((eqv? code EEXIST) 'exists)
        (else (raise (fs-err 'mkdir path code))))))

  ;; unlink(2), NOT delete-file: Chez's delete-file answered #f and raised
  ;; nothing when it could not delete (F99), so a failed unlink read as a
  ;; done one. A failure raises durable-error with the errno; success is
  ;; noted.
  ;; The injected unlink-fail stands where the syscall would and skips it,
  ;; so it removes nothing and notes nothing.
  (define (unlink! path)
    (unless (string? path)
      (assertion-violation 'unlink! "path must be a string" path))
    (let-values (((rc code)
                  (let ((injected (unlink-fault path)))
                    (if injected
                        (values -1 injected)
                        (let ((rc (c-unlink path)))
                          (values rc (and (< rc 0) (errno))))))))
      (when (< rc 0) (raise (fs-err 'unlink path code)))
      (note! (list 'unlink path))
      (trace-event! 'unlink path #f)
      path))

  ;; rmdir(2) of an EMPTY directory. A failure raises durable-error with the
  ;; errno, as unlink! does; success is noted.
  (define (rmdir! path)
    (unless (string? path)
      (assertion-violation 'rmdir! "path must be a string" path))
    (let ((rc (c-rmdir path)))
      (when (< rc 0) (fail! 'rmdir path))
      (note! (list 'rmdir path))
      (trace-event! 'rmdir path #f)
      path))

  ;; A REGULAR FILE THIS PROCESS MAY EXECUTE: access(2) with X_OK, and a stat
  ;; that says regular (access alone says yes to a searchable directory).
  ;; It answers #f for anything else, a missing path included, and raises
  ;; nothing: its one caller is a launcher that has to answer before it has
  ;; run anything, and a raise there would read as a different failure.
  (define (path-executable? path)
    (and (string? path)
         (= 0 (c-access path (platform-number 'X_OK)))
         (let ((buf (make-bytevector stat-buffer-size 0)))
           (and (>= (c-stat path buf) 0)
                (= (bitwise-and (st-mode-of buf) S_IFMT) S_IFREG)))))

  (define (process-id) (get-process-id))

  ;; THE MACHINE HOME IS A SEAM AND A HAZARD, and it is read here because
  ;; reading the environment is this layer's job. THEOURGIA_HOME lets a
  ;; fixture keep its own registry -- without it every test on a machine
  ;; would share one water mark. It is also a switch that lets a process
  ;; ignore a rollback, which is exactly what the registry exists to
  ;; catch, so a non-default value announces itself on startup rather
  ;; than being silently in force.
  ;; AN ENVIRONMENT VARIABLE THAT IS SET TO NOTHING IS NOT SET.
  ;;
  ;; NEVER: `(or (getenv "X") default)` GETS THIS WRONG, and the wrong answer
  ;; is not a small one: `""` is a string and therefore true, so `X=` in
  ;; an environment means the empty path rather than the default. Measured
  ;; in `(theourgia client)`: an empty `THEOURGIA_RUN` put the daemon's
  ;; socket directory at the filesystem ROOT, and it panicked at boot
  ;; trying to make it.
  ;;
  ;; NOTE: THREE PLACES TESTED THE LENGTH SEPARATELY BEFORE THIS EXISTED --
  ;; here, `environment-actor` and the run root -- which is three
  ;; suppliers of one rule, and the fourth reader is the one that forgets.
  (define (env-or name . fallback)
    (let ((v (getenv name)))
      (cond
        ((and (string? v) (> (string-length v) 0)) v)
        ((pair? fallback) (car fallback))
        (else #f))))

  (define (machine-home)
    (let ((v (env-or "THEOURGIA_HOME")))
      (if v
          v
          (string-append (or (env-or "HOME") "/tmp") "/.theourgia"))))

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
  ;; NEVER: `fcntl` IS VARIADIC AND THIS BINDING IS NOT. The third argument's
  ;; calling convention for a variadic C function is not the one a fixed
  ;; three-int binding uses, and on macOS ARM64 the two differ -- so
  ;; `F_DUPFD`'s "lowest descriptor at or above N" was not reliably given
  ;; the N written here. `dup` takes one argument and is not variadic, so
  ;; the same result is reached by asking for descriptors until one is
  ;; above the standard three.
  (define c-dup (foreign-procedure "dup" (int) int))
  (define c-flock (foreign-procedure "flock" (int int) int))
  (define c-ftruncate (foreign-procedure "ftruncate" (int integer-64) int))
  (define c-lseek (foreign-procedure "lseek" (int integer-64 int) integer-64))
  (define c-write (foreign-procedure "write" (int u8* size_t) ssize_t))
  (define c-link  (foreign-procedure "link"  (string string) int))
  ;; NOT VARIADIC, UNLIKE open(2): rename(2), unlink(2) and mkdir(2) name
  ;; every argument, so their errno is the syscall's own and a mode passes
  ;; as the second named argument. Chez's rename-file, delete-file and
  ;; mkdir raised their own conditions (or, for delete-file, nothing), and
  ;; the errno was lost at the door (F100 D1).
  (define c-rename (foreign-procedure "rename" (string string) int))
  (define c-unlink (foreign-procedure "unlink" (string) int))
  (define c-rmdir (foreign-procedure "rmdir" (string) int))
  (define c-access (foreign-procedure "access" (string int) int))
  (define c-mkdir (foreign-procedure "mkdir" (string unsigned-16) int))
  (define c-stat  (foreign-procedure "stat"  (string u8*) int))
  (define c-lstat (foreign-procedure "lstat" (string u8*) int))
  (define c-realpath (foreign-procedure "realpath" (string u8*) uptr))
  (define c-socket (foreign-procedure "socket" (int int int) int))
  (define c-connect (foreign-procedure "connect" (int u8* int) int))
  (define c-read (foreign-procedure "read" (int u8* size_t) ssize_t))
  (define c-setsockopt
    (foreign-procedure "setsockopt" (int int int u8* int) int))
  (define c-dup2 (foreign-procedure "dup2" (int int) int))

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

  (define ESRCH (platform-number 'ESRCH))
  (define EPERM (platform-number 'EPERM))

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

  ;; KEY: SO THAT THE EFFECT CAN BE READ SOMEWHERE OTHER THAN THE RETURN
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
  ;;
  ;; The injected kill-fail stands where the syscall would and skips it, so
  ;; nothing is signalled and its errno is the answer.
  (define (signal-pid! pid signum)
    (let ((injected (kill-fault)))
      (if injected
          injected
          (let ((r (c-kill pid signum)))
            (if (= r 0) 0 (errno))))))

  ;; NOTE: EPERM MEANS ALIVE. `kill(pid, 0)` asks the kernel whether a
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
  ;; `struct rlimit`, current then maximum, at the sizes and offsets the
  ;; platform's row gives (two 64-bit values on every measured row). It is
  ;; passed as bytes rather than described to Chez because a record layout
  ;; is what the kernel reads, and this way the two fields cannot be
  ;; swapped by a declaration that looks right.
  (define RLIMIT_CPU (platform-number 'RLIMIT_CPU))

  (define (rlimit-bytes cur max)
    (let ((bv (make-bytevector (platform-struct-size 'rlimit) 0)))
      (row-uint-set! bv 'rlimit 'rlim_cur cur)
      (row-uint-set! bv 'rlimit 'rlim_max max)
      bv))

  (define (setrlimit! resource cur max)
    (let ((r (c-setrlimit resource (rlimit-bytes cur max))))
      (if (= r 0) 0 (errno))))

  (define (getrlimit resource)
    (let ((bv (make-bytevector (platform-struct-size 'rlimit) 0)))
      (if (= 0 (c-getrlimit resource bv))
          (cons (row-uint-ref bv 'rlimit 'rlim_cur) (row-uint-ref bv 'rlimit 'rlim_max))
          (cons #f (errno)))))

  ;; ---- resident size, per platform ----------------------------------------
  ;;
  ;; NOTE: BYTES, ON EVERY PLATFORM. macOS reports bytes, FreeBSD and Linux
  ;; report PAGES, and a reading that forgot to multiply is still a
  ;; positive number that grows with the child's allocations -- it would
  ;; pass any cell that only asks for "positive and rising", and would
  ;; then let a child four thousand times over its limit run free.
  ;;
  ;; NOTE: #f WHEN IT CANNOT BE READ, never 0. "I could not look" and "it is
  ;; using nothing" are opposite facts, and a sampler that treats the
  ;; first as the second reports a healthy child for a process that has
  ;; gone.
  (define c-proc-pidinfo
    (and (foreign-entry? "proc_pidinfo")
         (foreign-procedure "proc_pidinfo" (int int integer-64 u8* int) int)))

  ;; macOS only: PROC_PIDTASKINFO and proc_taskinfo exist on no other row,
  ;; so they are read here, in the one branch that runs there, and never
  ;; when the library is initialised. pti_resident_size's offset and the
  ;; struct's size are the row's; the buffer is at least 256 bytes.
  (define (rss-darwin pid)
    (and c-proc-pidinfo
         (let* ((size (max 256 (platform-struct-size 'proc_taskinfo)))
                (bv (make-bytevector size 0)))
           (let ((n (c-proc-pidinfo pid (platform-number 'PROC_PIDTASKINFO) 0 bv size)))
             (and (> n 0)
                  (row-uint-ref bv 'proc_taskinfo 'pti_resident_size))))))

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
                    ;; NOTE: THE PAGE SIZE IS ASKED FOR, not written down.
                    ;; statm counts pages, and 4096 is only the common
                    ;; case: on a machine with 16 KiB pages a hardcoded
                    ;; 4096 under-reports every reading by four, quietly
                    ;; and everywhere.
                    (let ((pages (string->number (cadr parts))))
                      (and pages (* pages (c-getpagesize))))))))))

  ;; FreeBSD keeps it in `kinfo_proc`, reached through sysctl, and the
  ;; offset of `ki_rssize` is not derivable from a header at runtime.
  ;;
  ;; KEY: SO IT WAS MEASURED, ON THE MACHINE IT HAS TO WORK ON. A probe
  ;; started two children -- one touching a large allocation, one not --
  ;; read `ps -o rss=` for both at the same moment, and scanned the whole
  ;; struct for a slot that reproduced BOTH readings. FreeBSD 15.0-RELEASE,
  ;; page 4096, one candidate and no other:
  ;;
  ;;     offset 264, read as PAGES: big 57465 -> 224 MiB (ps: 224 MiB)
  ;;                                small  5767 ->  23 MiB (ps:  23 MiB)
  ;;
  ;; NOTE: READ AS 64 BITS, AND THE FIRST ANSWER HERE WAS WRONG. This said
  ;; 32 bits, reasoning that the 64-bit read matched only because the
  ;; next field happened to be zero. The header settles it the other way:
  ;; `/usr/include/x86/_types.h` defines `__segsz_t` as `__int64_t` under
  ;; `__LP64__`, so on amd64 `ki_rssize` IS 64 bits and the upper half
  ;; belongs to the value. The probe could not tell the two readings
  ;; apart because every number it saw was small -- agreement between two
  ;; readings is not evidence when both would agree on small values.
  ;; A 32-bit read would truncate above 2^32 pages.
  ;;
  ;; NOTE: AND THE STRUCT SIZE IS CHECKED, because an offset is a claim about
  ;; a layout. sysctl reports 1088 bytes there; a kernel that returns a
  ;; different size is one this offset was never measured against, and
  ;; answering #f is the honest response to that.
  ;;
  ;; The offset, the size and the sysctl names are the FreeBSD row's (the
  ;; probe's reading repeats both measurements above: ki_rssize at 264, 8
  ;; bytes, the struct 1088). They exist on no Linux row, so they are read
  ;; here, in the branch that runs on FreeBSD, and never when the library
  ;; is initialised.

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
           (bytevector-u32-native-set! mib 0 (platform-number 'CTL_KERN))
           (bytevector-u32-native-set! mib 4 (platform-number 'KERN_PROC))
           (bytevector-u32-native-set! mib 8 (platform-number 'KERN_PROC_PID))
           (bytevector-u32-native-set! mib 12 pid)
           (bytevector-u64-native-set! len 0 4096)
           (and (= 0 (c-sysctl mib 4 buf len 0 0))
                (= (bytevector-u64-native-ref len 0) (platform-struct-size 'kinfo_proc))
                (* (row-uint-ref buf 'kinfo_proc 'ki_rssize) (c-getpagesize))))))

  ;; NEVER: AND ZERO IS NOT A READING. Every branch below can answer 0 for a
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

;; ---- detaching: where a daemon's own output goes ------------------------
  ;;
  ;; NEVER: A DAEMON MAY NOT KEEP ITS PARENT'S STDOUT. It outlives the client
  ;; that started it, and a descriptor it holds is one the client's own
  ;; caller is waiting on: a shell that started a client and read its
  ;; output would not see end-of-file until the DAEMON exited too, which
  ;; can be hours.
  ;;
  ;; NEVER: AND IT MAY NOT GO TO /dev/null EITHER, which is what this did
  ;; first. Everything a daemon has to say about why it would not start
  ;; -- the socket path is occupied, the lock is held, the store will not
  ;; open -- is said AFTER this point, so discarding it left a client
  ;; that could only report "it did not come up" and no way for anyone to
  ;; learn why. Measured before it was changed: a foreground `serve` onto
  ;; an occupied path says `(error serve-path-occupied (path ...))`; the
  ;; same run detached said nothing at all, anywhere.
  ;;
  ;; KEY: APPEND, NOT TRUNCATE. Several starts against one store share this
  ;; file, and a start that truncated it would erase the record of the
  ;; failure that made the caller try again.
  ;;
  ;; NOTE: STDIN IS NOT THE LOG. It goes to /dev/null: a daemon that read
  ;; from its own log would be reading whatever it had just written.
  ;; NOTE: THE ORDER MATTERS AND IS DELIBERATE. `/dev/null` is put on 0
  ;; BEFORE the log is put on 1 and 2, so that if the log's descriptor
  ;; happens to be 1 or 2 -- which it can be when those were closed on
  ;; entry -- the copy onto 0 has already been made and nothing is lost.
  ;;
  ;; NEVER: AND THE TWO DESCRIPTORS ARE CLOSED ON EVERY WAY OUT, including the
  ;; failing ones. A raised `dup2` used to leave both open.
;; KEY: BOTH DESCRIPTORS ARE MOVED ABOVE 0,1,2 BEFORE ANY COPYING. Opened
  ;; while some of those three are closed, either can land ON one of the
  ;; targets, and then a later `dup2` onto that number changes what the
  ;; other variable refers to. Reasoning about which arrangements are safe
  ;; is how the first attempt was justified, and a reviewer reported the
  ;; overwrite twice against that reasoning; moving them out of the way
  ;; first removes the question instead of answering it.
  ;;
  ;; NOTE: `F_DUPFD` gives the lowest free descriptor AT OR ABOVE the number
  ;; asked for, which is exactly "somewhere that is not 0, 1 or 2".
  (define F_DUPFD (platform-number 'F_DUPFD))

  ;; NEVER: THE DESCRIPTOR HANDED IN IS THIS PROCEDURE'S TO ACCOUNT FOR. On
  ;; the success path it is closed after being duplicated higher up; when
  ;; `F_DUPFD` failed it used to be left open while the failure was
  ;; raised, and the caller -- which never saw a value -- had nothing to
  ;; close.
  (define (above-stdio fd who)
    (if (> fd 2)
        fd
        ;; Each `dup` answers the lowest free descriptor; the ones at or
        ;; below 2 are held so the next call cannot be given them again,
        ;; and all of them are released once one lands above.
        (let loop ((held '()) (from fd))
          (let ((n (c-dup from)))
            (cond
              ((= n -1)
               (let ((code (errno)))
                 (for-each c-close held)
                 (c-close fd)
                 (raise (fs-err 'dup who code))))
              ((> n 2)
               (for-each c-close held)
               (c-close fd)
               n)
              (else (loop (cons n held) n)))))))

  ;; NEVER: OPENING IS PART OF WHAT CAN FAIL. Both descriptors were acquired
  ;; in the `let` that binds them -- outside the guard below -- so a
  ;; failure to open the SECOND left the first open with nothing holding
  ;; it: measured, the log opened as fd 42 and stayed open when
  ;; `/dev/null` could not be opened. They are acquired inside the guard,
  ;; and whatever has been acquired when something fails is released.
  (define (redirect-stdio! log-path)
    (let ((null-fd #f)
          (log-fd #f))
      (guard (e (#t
                 (when null-fd (fd-close null-fd))
                 (when log-fd (fd-close log-fd))
                 (raise e)))
        (set! null-fd (above-stdio (fd-open "/dev/null" '(read)) "/dev/null"))
        (set! log-fd (above-stdio (fd-open log-path '(write append create)) log-path))
        (when (= -1 (c-dup2 null-fd 0))
          (raise (fs-err 'dup2 "/dev/null" (errno))))
        (for-each (lambda (target)
                    (when (= -1 (c-dup2 log-fd target))
                      (raise (fs-err 'dup2 log-path (errno)))))
                  '(1 2)))
      ;; NOTE: UNCONDITIONAL NOW: both were moved above 2, so neither is one
      ;; of the descriptors just installed.
      ;; NEVER: AND EACH RELEASE STANDS ALONE. Written as two calls in a row, a
      ;; failure closing the first one skipped the second and leaked it --
      ;; the descriptor that could not be released taking with it one that
      ;; could.
      (close-quietly null-fd)
      (close-quietly log-fd)))

  ;; ---- starting a daemon without becoming one -----------------------------
  ;;
  ;; NEVER: NOT `execvp`. `exec-argv!` REPLACES this process, which is right
  ;; for a launcher and wrong for a client: the client has a call to make
  ;; once the daemon is up, and a process that exec'd into the daemon is
  ;; not there to make it.
  ;;
  ;; KEY: MEASURED, 2026-09-18, on both platforms this ships to:
  ;;
  ;;                                    macOS 25.3.0   FreeBSD 15.0-RELEASE
  ;;   sizeof posix_spawnattr_t              8                  8
  ;;   sizeof posix_spawn_file_actions_t     8                  8
  ;;   POSIX_SPAWN_SETSID                 1024            NOT DEFINED
  ;;
  ;; NOTE: SO THE SESSION IS NOT SET HERE. Both attribute types are
  ;; pointer-sized handles and both are passed as NULL, because the one
  ;; attribute this would have wanted does not exist on FreeBSD 15 -- the
  ;; child leaves the session itself, first thing, under `--detach`. The
  ;; window that leaves is described where the child does it.
  ;;
  ;; NOTE: THE ENVIRONMENT IS PASSED EXPLICITLY, because `posix_spawn` has
  ;; no "inherit" and a NULL envp is an EMPTY environment, not the
  ;; caller's -- a daemon started that way would lose THEOURGIA_HOME and
  ;; open a different store than the client asked about, silently.
  ;;
  ;; NOTE: `environ` IS ONLY THERE ONCE libc IS LOADED. `foreign-entry?`
  ;; answers #f for every one of these symbols in a process that has not
  ;; touched the library yet, which reads exactly like "this platform
  ;; does not have it". The library is loaded by the time any of this
  ;; runs; the check below is about the platform, and says so.
  (define (spawn-detached! argv)
    (unless (and (pair? argv) (for-all string? argv))
      (assertion-violation 'spawn-detached! "argv must be a non-empty list of strings" argv))
    (unless (foreign-entry? "posix_spawnp")
      (assertion-violation 'spawn-detached! "no posix_spawnp in libc" (machine-kind)))
    (unless (foreign-entry? "environ")
      (assertion-violation 'spawn-detached! "no environ in libc" (machine-kind)))
    (let* ((c-spawn (foreign-procedure "posix_spawnp"
                                       (u8* string void* void* void* void*) int))
           (width (foreign-sizeof 'void*))
           (cells (foreign-alloc (* width (+ 1 (length argv)))))
           (strings (map c-string argv))
           (pid-out (make-pid-buffer))
           (envp (foreign-ref 'void* (foreign-entry "environ") 0)))
      (do ((ps strings (cdr ps)) (i 0 (+ i 1))) ((null? ps))
        (foreign-set! 'void* cells (* i width) (car ps)))
      (foreign-set! 'void* cells (* (length argv) width) 0)
      (let ((rc (c-spawn pid-out (car argv) 0 0 cells envp)))
        (for-each foreign-free strings)
        (foreign-free cells)
        (if (zero? rc)
            (pid-buffer-ref pid-out)
            (raise (fs-err 'spawn (car argv) rc))))))

  ;; ---- a descriptor a child must not inherit ------------------------------
  ;;
  ;; NEVER: A DESCRIPTOR NOT MARKED IS INHERITED by a child made through a
  ;; spawn that keeps unmarked descriptors -- fork and exec, or this file's
  ;; posix_spawn (spawn-captured!, spawn-detached!), which sets no
  ;; close-on-exec default. libuv's spawn (proc-spawn!) closes what it did
  ;; not set up, measured on macOS and FreeBSD 15.0, but nothing guarantees
  ;; every path does. A lock whose descriptor a child inherits is held for as
  ;; long as that child lives, whatever its parent does -- so a lock that
  ;; must end with its process is marked here.
  ;;
  ;; NOTE: THE MARK IS SET WITH ioctl(FIOCLEX), NOT fcntl(F_SETFD). fcntl is
  ;; variadic and F_SETFD's flags are its variadic argument, which a
  ;; fixed-arity declaration does not pass where the callee reads it on
  ;; every platform (the note on open(2) at the top of this file). FIOCLEX
  ;; takes no third argument and sets the one flag there is. Its value is
  ;; the one the platform's row gives (0x20006601 on macOS and FreeBSD, 0x5451 on
  ;; Linux). The getter asks fcntl(F_GETFD), which also takes no third
  ;; argument.
  ;;
  ;; A failure raises unreadable-entry naming the descriptor's path, like
  ;; every other failure on a descriptor this library opened. The injected
  ;; cloexec-fail stands where the ioctl would and skips it.
  (define FIOCLEX (platform-number 'FIOCLEX))
  (define F_GETFD (platform-number 'F_GETFD))
  (define FD_CLOEXEC (platform-number 'FD_CLOEXEC))

  (define (fd-close-on-exec! fd)
    (let-values (((rc code)
                  (let ((injected (cloexec-fault)))
                    (if injected
                        (values -1 injected)
                        (let ((rc ((foreign-procedure "ioctl" (int unsigned-long) int) fd FIOCLEX)))
                          (values rc (and (< rc 0) (errno))))))))
      (when (< rc 0) (unreadable! (subject-of fd '()) code))
      fd))

  (define (fd-close-on-exec? fd)
    (let ((flags ((foreign-procedure "fcntl" (int int) int) fd F_GETFD)))
      (when (< flags 0) (unreadable! (subject-of fd '()) (errno)))
      (not (zero? (fxand flags FD_CLOEXEC)))))

  ;; THE NUMBER OF PROCESSORS ONLINE, READ IN ONE PLACE: sysconf, which is not
  ;; variadic. _SC_NPROCESSORS_ONLN is the platform row's (58 on macOS and
  ;; FreeBSD, 84 on Linux).
  ;; THEOURGIA_EVAL_SLOTS_DEFAULT, a positive integer, replaces the reading:
  ;; a test seam, so a row can ask for a pool of a known size on any
  ;; machine; empty, it is unset (env-or). -> a positive integer, or #f when
  ;; the seam is set and not one, or the kernel gave no answer.
  (define _SC_NPROCESSORS_ONLN (platform-number '_SC_NPROCESSORS_ONLN))

  (define (online-processors)
    (let ((seam (env-or "THEOURGIA_EVAL_SLOTS_DEFAULT")))
      (if seam
          (let ((n (string->number seam 10)))
            (and n (exact? n) (integer? n) (> n 0) n))
          (let ((n ((foreign-procedure "sysconf" (int) long) _SC_NPROCESSORS_ONLN)))
            (and (> n 0) n)))))

  ;; ---- starting a child whose three streams are files ---------------------
  ;;
  ;; NEVER: THE CHILD'S STREAMS ARE FILES THE CALLER OWNS, NOT THE CALLER'S. fd 0
  ;; is opened from `stdin-path` (or /dev/null), fd 1 and fd 2 are created or
  ;; truncated at `stdout-path` and `stderr-path` with mode 0600, all by the
  ;; spawn's own file actions, before the program runs. A child that
  ;; inherited the caller's fd 0 would read the caller's input, and one that
  ;; inherited fd 1 would write into the caller's answer channel. Files, not
  ;; pipes: nothing here can fill and block while the caller is not reading.
  ;;
  ;; THE ENVIRONMENT IS THE CALLER'S WITH `bindings` REPLACING ITS OWN. Each
  ;; binding is (name . value); an entry of `environ` whose name is one of
  ;; them is left out, and the binding is added, so the child's getenv sees
  ;; the binding whatever the caller's environment held. Appending instead
  ;; would leave two entries of one name, and which one a getenv answers is
  ;; the C library's choice. The entries kept are the caller's own pointers,
  ;; byte for byte.
  ;;
  ;; NOTE: O_CREAT AND O_TRUNC ARE PASSED HERE, and the note at the top of
  ;; this file does not forbid it: that note is about open(2), whose mode
  ;; sits in the variadic part a fixed-arity declaration cannot reach.
  ;; posix_spawn_file_actions_addopen takes its mode as a named argument.
  ;; The flags are the platform row's (O_CREAT 0x200 and O_TRUNC 0x400 on
  ;; macOS and FreeBSD, 0x40 and 0x200 on Linux, where 0x200 is O_TRUNC's
  ;; neighbour's value on the BSDs -- which is why no flag is written
  ;; down here). posix_spawn_file_actions_t is a pointer-sized handle on
  ;; the BSDs and an 80-byte struct on glibc; it is allocated at the row's
  ;; size.
  ;;
  ;; -> (values pid #f) when the program was started, or (values #f errno)
  ;; when posix_spawnp answered non-zero -- a file action that could not be
  ;; carried out included. Nothing ran in that case, and the caller says so.
  (define spawn-O_WRONLY (platform-number 'O_WRONLY))
  (define spawn-O_CREAT (platform-number 'O_CREAT))
  (define spawn-O_TRUNC (platform-number 'O_TRUNC))

  (define (spawn-captured! argv bindings stdin-path stdout-path stderr-path)
    (unless (and (pair? argv) (for-all string? argv))
      (assertion-violation 'spawn-captured! "argv must be a non-empty list of strings" argv))
    (unless (and (list? bindings)
                 (for-all (lambda (b) (and (pair? b) (string? (car b)) (string? (cdr b)))) bindings))
      (assertion-violation 'spawn-captured! "bindings must be a list of (name . value) strings" bindings))
    (unless (and (or (not stdin-path) (string? stdin-path)) (string? stdout-path) (string? stderr-path))
      (assertion-violation 'spawn-captured! "the stream paths must be strings" (list stdin-path stdout-path stderr-path)))
    (unless (foreign-entry? "posix_spawnp")
      (assertion-violation 'spawn-captured! "no posix_spawnp in libc" (machine-kind)))
    (unless (foreign-entry? "environ")
      (assertion-violation 'spawn-captured! "no environ in libc" (machine-kind)))
    ;; EVERY ALLOCATION IS RELEASED ON EVERY PATH, a raise part-way through
    ;; included: each one is recorded as it is made and freed on the way
    ;; out. The kept environ entries are the caller's and are never freed.
    ;; AND THE FILE ACTIONS ARE DESTROYED WHENEVER THEY WERE INITIALISED: libc
    ;; owns copies of their paths, and freeing the handle's storage without
    ;; destroying it would leak those. The destroy is in the same unwind as
    ;; the frees, before them.
    (let ((owned '()) (destroy! #f))
      (define (own! p) (set! owned (cons p owned)) p)
      (dynamic-wind
        (lambda () #f)
        (lambda ()
          (let* ((c-spawn (foreign-procedure "posix_spawnp" (u8* string void* void* void* void*) int))
                 (c-fa-init (foreign-procedure "posix_spawn_file_actions_init" (void*) int))
                 (c-fa-open (foreign-procedure "posix_spawn_file_actions_addopen"
                                               (void* int string int unsigned-int) int))
                 (c-fa-destroy (foreign-procedure "posix_spawn_file_actions_destroy" (void*) int))
                 (width (foreign-sizeof 'void*))
                 (names (map (lambda (b) (string->utf8 (string-append (car b) "="))) bindings))
                 ;; A NULL environ is an empty environment, not an array to read.
                 (environ-at (foreign-ref 'void* (foreign-entry "environ") 0))
                 (kept (if (= environ-at 0)
                           '()
                           (let loop ((i 0) (out '()))
                             (let ((entry (foreign-ref 'void* environ-at (* i width))))
                               (if (= entry 0)
                                   (reverse out)
                                   (loop (+ i 1)
                                         (if (exists (lambda (n) (c-bytes-prefix? entry n)) names)
                                             out
                                             (cons entry out))))))))
                 (added (map (lambda (b) (own! (c-string (string-append (car b) "=" (cdr b))))) bindings))
                 (envp-list (append kept added))
                 (envp (own! (foreign-alloc (* width (+ 1 (length envp-list))))))
                 (strings (map (lambda (a) (own! (c-string a))) argv))
                 (cells (own! (foreign-alloc (* width (+ 1 (length argv))))))
                 ;; AT LEAST THE ROW'S SIZE: a handle on the BSDs (8 bytes), a
                 ;; struct on glibc (80). An allocation smaller than the type
                 ;; lets posix_spawn_file_actions_init write past its end.
                 (actions (own! (foreign-alloc
                                  (max width 16 (platform-struct-value 'posix_spawn 'file-actions-size)))))
                 (pid-out (make-pid-buffer)))
            (do ((ps envp-list (cdr ps)) (i 0 (+ i 1))) ((null? ps))
              (foreign-set! 'void* envp (* i width) (car ps)))
            (foreign-set! 'void* envp (* (length envp-list) width) 0)
            (do ((ps strings (cdr ps)) (i 0 (+ i 1))) ((null? ps))
              (foreign-set! 'void* cells (* i width) (car ps)))
            (foreign-set! 'void* cells (* (length argv) width) 0)
            (let ((rc (let ((init (c-fa-init actions)))
                        (if (not (zero? init))
                            init
                            (begin
                              (set! destroy! (lambda () (c-fa-destroy actions)))
                              (let* ((out-flags (bitwise-ior spawn-O_WRONLY spawn-O_CREAT spawn-O_TRUNC))
                                     (a (c-fa-open actions 0 (or stdin-path "/dev/null") 0 0))
                                     (b (if (zero? a) (c-fa-open actions 1 stdout-path out-flags #o600) a))
                                     (c (if (zero? b) (c-fa-open actions 2 stderr-path out-flags #o600) b)))
                                (if (zero? c) (c-spawn pid-out (car argv) actions 0 cells envp) c)))))))
              (if (zero? rc)
                  (values (pid-buffer-ref pid-out) #f)
                  (values #f rc)))))
        (lambda ()
          (when destroy! (destroy!) (set! destroy! #f))
          (for-each foreign-free owned)
          (set! owned '())))))

  ;; Whether the NUL-terminated C string at `address` starts with the bytes
  ;; of `prefix`.
  (define (c-bytes-prefix? address prefix)
    (let ((n (bytevector-length prefix)))
      (let loop ((i 0))
        (cond ((= i n) #t)
              ((= (foreign-ref 'unsigned-8 address i) (bytevector-u8-ref prefix i)) (loop (+ i 1)))
              (else #f)))))

  ;; NEVER: A PROCESS THIS LIBRARY STARTED IS STILL A CHILD OF THE PROCESS
  ;; THAT STARTED IT. `spawn-detached!` answers a pid and nothing ever
  ;; waits for it, so when that process exits -- and the loser of the
  ;; daemon's lock race exits at once, every time -- the kernel keeps its
  ;; entry until somebody collects it. For a one-shot client that costs
  ;; nothing: it exits and init inherits the entry. A shell that serves a
  ;; whole session does not exit, and the entries accumulate.
  ;;
  ;; NOTE: IT COLLECTS ANY EXITED CHILD rather than a remembered pid. A
  ;; caller keeping a list of "its own" pids would have one more thing to
  ;; keep correct, and every child of these callers is started here.
  ;;
  ;; NOTE: ANSWERS HOW MANY IT COLLECTED, so a row can assert that it did
  ;; something rather than that it did not raise.
  ;;
  ;; KEY: WNOHANG MEASURED: 0x1 on macOS (sys/wait.h, MacOSX.sdk) and on
  ;; FreeBSD 15; it is 1 wherever this ships.
  (define WNOHANG (platform-number 'WNOHANG))

  (define (reap-children!)
    (if (not (foreign-entry? "waitpid"))
        0
        (let ((c-waitpid (foreign-procedure "waitpid" (int u8* int) int))
              (status (make-bytevector (platform-type-size 'int) 0)))
          (let loop ((n 0))
            (let ((r (c-waitpid -1 status WNOHANG)))
              (if (> r 0) (loop (+ n 1)) n))))))

  ;; ONE CHILD, BY ITS PID, WITHOUT WAITING (F100b item 4). The client retains
  ;; the daemon's pid and polls it in its socket wait: #f while it runs,
  ;; `(exit n)` or `(signal n)` once it has ended, which reaps it. NOT
  ;; waitpid(-1): that collects ANY child, and a client with another child
  ;; that exits first would read that child's status as the daemon's (PID2).
  ;; The status word is decoded as <sys/wait.h> does on both platforms: the
  ;; low seven bits are the terminating signal, 0 for a normal exit, whose
  ;; code is the next eight. A pid that is not a child is durable-error
  ;; (op waitpid) with its errno, ECHILD.
  ;; The injected waitpid-fail stands where the syscall would and skips it:
  ;; nothing is collected, and the raise is the one a real failure makes.
  (define (waitpid-status pid)
    (let ((injected (waitpid-fault)))
      (when injected (raise (fs-err 'waitpid pid injected))))
    (let ((c-waitpid (foreign-procedure "waitpid" (int u8* int) int))
          (status (make-bytevector (platform-type-size 'int) 0)))
      (let ((r (c-waitpid pid status WNOHANG)))
        (cond
          ((= r 0) #f)
          ((< r 0) (raise (fs-err 'waitpid pid (errno))))
          (else
           (let* ((w (bytevector-sint-ref status 0 (native-endianness) (platform-type-size 'int)))
                  (sig (fxand w #x7f)))
             (if (= sig 0)
                 (list 'exit (fxand (fxsra w 8) #xff))
                 (list 'signal sig))))))))

  ;; ---- does this filesystem distinguish Foo from foo -----------------------
  ;;
  ;; NEVER: A STORE'S KEY MUST NOT CHANGE WHEN THE STORE APPEARS, and on a
  ;; case-insensitive filesystem it did: the part of the path that does
  ;; not exist yet keeps whatever case the caller typed, and `realpath`
  ;; returns the filesystem's own spelling once it does exist. `Foo` and
  ;; `foo` are one store there, and they were getting two keys -- and one
  ;; of those keys changed the moment the store was created.
  ;;
  ;; KEY: MEASURED, 2026-09-18, on both platforms this ships to:
  ;;
  ;;                            macOS 25.3.0        FreeBSD 15.0-RELEASE
  ;;   _PC_CASE_SENSITIVE            11              not defined at all
  ;;                                                 (33 _PC_ names; 10 and
  ;;                                                  12 are taken, 11 is
  ;;                                                  unassigned; grep of
  ;;                                                  /usr/include: 0 files)
  ;;
  ;; NEVER: SO THE NUMBER IS NOT PASSED WHERE IT MEANS NOTHING. Asking with a
  ;; name a platform never defined is how a platform assumption becomes a
  ;; general one; it is asked only where it was measured, and everywhere
  ;; else the answer is `unknown`.
  ;; #t, #f, or 'unknown -- and a caller that cannot find out must not
  ;; guess, because guessing "insensitive" would fold keys on a
  ;; filesystem where two spellings really are two stores.
  ;; _PC_CASE_SENSITIVE is macOS's alone (absent on every other row), so it
  ;; is read inside the darwin branch, never when the library is
  ;; initialised.
  (define (path-case-sensitive? path)
    (if (or (not (string=? (machine-kind) "darwin"))
            (not (foreign-entry? "pathconf")))
        'unknown
        (let* ((c-pathconf (foreign-procedure "pathconf" (string int) long))
               (r (c-pathconf path (platform-number '_PC_CASE_SENSITIVE))))
          (cond ((> r 0) #t)
                ((= r 0) #f)
                (else 'unknown)))))

  (define (c-string text)
    (let* ((b (string->utf8 text))
           (n (bytevector-length b))
           (p (foreign-alloc (+ n 1))))
      (do ((i 0 (+ i 1))) ((= i n)) (foreign-set! 'unsigned-8 p i (bytevector-u8-ref b i)))
      (foreign-set! 'unsigned-8 p n 0)
      p))

  ;; ---- a unix socket, for a client that must not import a server ----------
  ;;
  ;; NEVER: THE CLIENT MAY NOT REACH THE ACTOR SYSTEM'S NETWORKING. A caller
  ;; that had to load it would pay for the server it is trying to talk
  ;; to, which is the whole point of the split -- so the five calls a
  ;; blocking client needs are here, in the layer that already owns the
  ;; libc surface, and `(theourgia client)` imports nothing else.
  ;;
  ;; KEY: EVERY NUMBER BELOW IS THE PLATFORM ROW'S (theourgia
  ;; platform-numbers), measured by test/probe/layout.c on each platform,
  ;; and none is written here. The shapes differ, which is why: on macOS
  ;; and FreeBSD sockaddr_un is 106 bytes with a one-byte sun_len at 0,
  ;; a one-byte sun_family at 1 and sun_path's 104 bytes at 2, and
  ;; SOL_SOCKET, SO_RCVTIMEO and SO_SNDTIMEO are 65535, 4102 and 4101; on
  ;; Linux it is 110 bytes with no sun_len, a two-byte sun_family at 0 and
  ;; 108 bytes of path at 2, and the three options are 1, 20 and 21. An
  ;; address built from the wrong row connects to the wrong path bytes,
  ;; quietly; that is what the rows end.
  ;;
  ;; NOTE: THE ADDRESS LENGTH IS THE WHOLE STRUCT on every row, and
  ;; sun_len, where the row has one, says the same, as it always did on
  ;; the BSDs; Linux accepts the whole struct. SUN_LEN (the path's own
  ;; length) is not what is passed; the probe's SUN_LEN sample is kept as
  ;; a reading, and a row of the suite checks it against the offset.
  (define AF_UNIX (platform-number 'AF_UNIX))
  (define SOCK_STREAM (platform-number 'SOCK_STREAM))
  (define SOL_SOCKET (platform-number 'SOL_SOCKET))
  (define SO_RCVTIMEO (platform-number 'SO_RCVTIMEO))
  (define SO_SNDTIMEO (platform-number 'SO_SNDTIMEO))
  (define SOCKADDR_UN_SIZE (platform-struct-size 'sockaddr_un))
  (define SUN_PATH_OFFSET (platform-field 'sockaddr_un 'sun_path 'offset))
  (define SUN_PATH_MAX (platform-field 'sockaddr_un 'sun_path 'size))

  (define (sun-path-max) SUN_PATH_MAX)

  ;; NOTE: THE LENGTH LIMIT IS PART OF THE ABI, NOT A STYLE RULE. `sun_path`
  ;; holds SUN_PATH_MAX bytes INCLUDING the terminator (104 on the BSDs,
  ;; 108 on Linux), and a path that does not fit is not truncated by the
  ;; kernel into something harmless -- it binds or connects to a
  ;; different name. Scratch directories are long enough for this to
  ;; happen in practice, so it is refused here, by name, with both
  ;; numbers in the message.
  (define (sockaddr-un path)
    (let* ((bytes (string->utf8 path))
           (n (bytevector-length bytes)))
      (when (>= n SUN_PATH_MAX)
        (assertion-violation 'sockaddr-un
          "socket path does not fit in sun_path" (list path n SUN_PATH_MAX)))
      (let ((sa (make-bytevector SOCKADDR_UN_SIZE 0)))
        (when (platform-field-present? 'sockaddr_un 'sun_len)
          (row-uint-set! sa 'sockaddr_un 'sun_len SOCKADDR_UN_SIZE))
        (row-uint-set! sa 'sockaddr_un 'sun_family AF_UNIX)
        (bytevector-copy! bytes 0 sa SUN_PATH_OFFSET n)
        sa)))

  ;; `struct timeval` at the row's size, each field at its own offset and
  ;; width: tv_usec is 4 bytes on macOS and 8 on FreeBSD and Linux, and
  ;; the fresh buffer's padding stays zero either way.
  (define (timeval-bytes ms)
    (let ((tv (make-bytevector (platform-struct-size 'timeval) 0)))
      (row-uint-set! tv 'timeval 'tv_sec (div ms 1000))
      (row-uint-set! tv 'timeval 'tv_usec (* 1000 (mod ms 1000)))
      tv))

  ;; A DEADLINE ON BOTH DIRECTIONS, so a client cannot be parked forever
  ;; by a daemon that accepted the connection and then stopped. This is
  ;; the transport's own bound; the caller's overall deadline is its own
  ;; business and is enforced above.
  (define (socket-timeout! fd ms)
    (let ((tv (timeval-bytes ms)))
      (for-each
        (lambda (opt)
          (when (= -1 (c-setsockopt fd SOL_SOCKET opt tv (bytevector-length tv)))
            (raise (fs-err 'setsockopt "socket" (errno)))))
        (list SO_RCVTIMEO SO_SNDTIMEO))))

  ;; Answers a connected descriptor, or raises a durable-error carrying
  ;; the errno. NEVER: IT DOES NOT CLASSIFY THE ERRNO: which failures mean
  ;; "no daemon, start one" and which mean "stop" is the client's rule
  ;; and lives with the client, in one list.
  ;; NEVER: THE ADDRESS IS BUILT BEFORE THE DESCRIPTOR EXISTS. `let` does not
  ;; order its bindings, so with the socket opened first a `sockaddr-un`
  ;; that refused -- a path too long for `sun_path` -- left that
  ;; descriptor open with nothing holding it: measured, a 104-character
  ;; path leaked fd 3 per attempt. `let*` puts the step that can refuse
  ;; ahead of the step that allocates, so there is nothing to leak.
  (define (unix-socket-connect path timeout-ms)
    (let* ((sa (sockaddr-un path))
           (fd (c-socket AF_UNIX SOCK_STREAM 0)))
      (when (= fd -1)
        (raise (fs-err 'socket path (errno))))
      ;; NEVER: A DESCRIPTOR THAT HAS BEEN ALLOCATED IS CLOSED ON EVERY WAY OUT.
      ;; `socket-timeout!` raises when `setsockopt` fails, and the socket
      ;; opened two lines above was left open -- a client that retried
      ;; would leak one per attempt.
      (guard (e (#t (c-close fd) (raise e)))
        (socket-timeout! fd timeout-ms))
      (let retry ()
        (let ((r (c-connect fd sa SOCKADDR_UN_SIZE)))
          (cond
            ((= r 0) fd)
            ((= (errno) EINTR) (retry))
            (else
             (let ((code (errno)))
               (c-close fd)
               (raise (fs-err 'connect path code)))))))))

  ;; Reads up to `want` bytes. Answers a bytevector, which is EMPTY at
  ;; end of file -- the caller decides whether an early EOF is an
  ;; answer or a loss, because only it knows what a whole answer is.
  (define (fd-read fd want)
    (let ((buf (make-bytevector want)))
      (let retry ()
        (let ((n (c-read fd buf want)))
          (cond
            ((and (< n 0) (= (errno) EINTR)) (retry))
            ((< n 0) (raise (fs-err 'read "socket" (errno))))
            ((= n 0) (bytevector))
            ((= n want) buf)
            (else
             (let ((out (make-bytevector n)))
               (bytevector-copy! buf 0 out 0 n)
               out)))))))

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

  ;; NEVER: O_APPEND IS NOT 8 EVERYWHERE. It is 8 on macOS and FreeBSD and
  ;; 1024 on Linux, where 8 is no flag at all: a segment opened for append
  ;; with the BSD value writes every record at offset 0, over the start of
  ;; the segment. Every number here is the platform row's for that reason.
  (define O_RDONLY (platform-number 'O_RDONLY))
  (define O_WRONLY (platform-number 'O_WRONLY))
  (define O_RDWR (platform-number 'O_RDWR))
  (define O_APPEND (platform-number 'O_APPEND))
  (define LOCK_SH (platform-number 'LOCK_SH))
  (define LOCK_EX (platform-number 'LOCK_EX))
  (define LOCK_NB (platform-number 'LOCK_NB))
  (define LOCK_UN (platform-number 'LOCK_UN))
  (define SEEK_SET (platform-number 'SEEK_SET))
  (define SEEK_CUR (platform-number 'SEEK_CUR))
  (define SEEK_END (platform-number 'SEEK_END))
  (define EINTR (platform-number 'EINTR))
  (define EIO (platform-number 'EIO))
  (define EEXIST (platform-number 'EEXIST))
  ;; flock's "somebody else holds it" under LOCK_NB: 35 on macOS and
  ;; FreeBSD (EAGAIN), 11 on Linux, as the rows read it.
  (define EWOULDBLOCK (platform-number 'EWOULDBLOCK))
  ;; THE THREE WAYS AN OPEN FAILS THAT A BARRIER HAS TO SURVIVE,
  ;; named because an injected fault says which one it is: the file is
  ;; gone, this process may not read it, or this process has no
  ;; descriptor left. The first two are reachable from a fixture with
  ;; rm and chmod; the third is not reachable at all without this seam,
  ;; which is why the seam exists.
  (define ENOENT (platform-number 'ENOENT))
  (define EACCES (platform-number 'EACCES))
  ;; A write on a descriptor opened read-only: the kernel refuses it, so the
  ;; syscall ran (the record's real-failure case). 9 on macOS, FreeBSD and
  ;; Linux; measured here by facade-ffi's real-syscall row.
  (define EBADF (platform-number 'EBADF))
  ;; waitpid(2) on a pid that is not a child of this process: 10 on macOS,
  ;; FreeBSD and Linux (sys/errno.h). Exported so a row reads it by name
  ;; (F100b Q7, H4).
  (define ECHILD (platform-number 'ECHILD))
  (define EMFILE (platform-number 'EMFILE))
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

  ;; ---- the mutation record (F100 D1') --------------------------------------
  ;;
  ;; KEY: WHAT THIS PROCESS CHANGED THROUGH THE DOOR, per request. A scope
  ;; opened by `with-mutation-record` collects, in order, one entry per
  ;; content change a mutating primitive made: (create p) (mkdir p)
  ;; (write p) (truncate p) (rename from to) (unlink p) (link from to).
  ;; Outside a scope nothing is noted, and `mutation-record` answers ().
  ;; IT IS AN OPERATION HISTORY OF POSSIBLE MUTATION, not proof of changed
  ;; bytes: a same-byte write is noted, and a digest is what answers for
  ;; bytes. Process output (stdout, stderr, a daemon's serve.log) is not a
  ;; change made through the door and is outside it.
  ;;
  ;; WHEN A PRIMITIVE NOTES ITSELF is decided per primitive from the
  ;; syscall's contract: mkdir, create, create-exclusive, ftruncate, unlink
  ;; and link after success only (a failed call leaves the tree as it
  ;; was); rename after success and on a failure with errno EIO, the one
  ;; case in which it may have partly happened; a write when any byte was
  ;; reported written, and on any final failure of a real write syscall;
  ;; fsync, fullfsync and close never. A composite notes each step as it
  ;; succeeds.
  ;;
  ;; NEVER: KEYED BY WHO IS RUNNING, NOT HELD IN A PARAMETER. A Chez
  ;; parameter belongs to the OS thread, and every actor of a scheduler
  ;; shares one, so a daemon request that yields would lend its record to
  ;; whichever actor runs next. AND ONE KEY FOR TWO ACTORS LEAKS A SCOPE
  ;; PAST ITS ACTOR: measured with a shared key (F100a r2's mutant), A's
  ;; scope exits and deletes the key, B's then restores the box it saved,
  ;; A's, and a stale scope stays installed for the process, noting what
  ;; later scopeless actors do (facade-record (a) and (b) both go red). The key is asked of a thunk, two cases:
  ;;   - no scheduler started (whether or not the scheduler library is
  ;;     loaded: the CLI, core.sc's local route, the eval worker): the
  ;;     default thunk answers `process-key`, one object for the process;
  ;;   - a scheduler started through (theourgia sched): its start-scheduler
  ;;     installs (lambda () self) first, so the key is the running actor.
  ;; ffi imports nothing of the scheduler for this: importing igropyr's
  ;; actor library here would load libuv into every program (F0-2).
  ;; The table is WEAK, so an actor killed without unwinding takes its
  ;; entry with it. igropyr's scheduler saves and restores each process's
  ;; winders on a switch and never runs them (actor.sc:459-474, :503), so a
  ;; scope is installed once and stays installed across its actor's
  ;; yields; a killed actor's after-thunk never runs (:701-705), and the
  ;; weak table lets its entry go with the PCB.
  (define process-key (list 'process-key))
  (define mutation-self (lambda () process-key))
  (define (mutation-set-self! thunk)
    (unless (procedure? thunk)
      (assertion-violation 'mutation-set-self! "not a procedure" thunk))
    (set! mutation-self thunk))
  (define mutation-records (make-weak-eq-hashtable))

  ;; A SCOPE SAVES AND RESTORES THE ONE BEFORE IT, and the key is taken
  ;; once, at entry: the winders may run when another actor is current,
  ;; and they must still name this one. INITIAL ENTRIES are how a scope
  ;; that hands its work to another actor carries what it had already
  ;; done: the spawned actor's record is those entries followed by its
  ;; own, and the spawner reads that whole record back.
  (define (with-mutation-record thunk . initial)
    (unless (procedure? thunk)
      (assertion-violation 'with-mutation-record "not a procedure" thunk))
    (let* ((key (mutation-self))
           (entries (box (if (pair? initial) (reverse (car initial)) '())))
           (before #f))
      (dynamic-wind
        (lambda ()
          (set! before (hashtable-ref mutation-records key #f))
          (hashtable-set! mutation-records key entries))
        thunk
        (lambda ()
          (if before
              (hashtable-set! mutation-records key before)
              (hashtable-delete! mutation-records key))))))

  (define (mutation-record)
    (let ((b (hashtable-ref mutation-records (mutation-self) #f)))
      (if b (reverse (unbox b)) '())))

  (define (note! entry)
    (let ((b (hashtable-ref mutation-records (mutation-self) #f)))
      (when b (set-box! b (cons entry (unbox b))))))

  ;; CHEZ'S OWN I/O CONDITIONS, MAPPED TO AN ERRNO where Chez did the
  ;; syscall (file-ensure!'s and file-create-exclusive!'s create, which
  ;; cannot pass a mode through open(2)'s variadic argument). A condition
  ;; with no class here keeps errno #f.
  (define EROFS (platform-number 'EROFS))
  (define (condition-errno e)
    ;; THE SUBTYPE FIRST: a read-only-file error is also a protection error,
    ;; and asked the other way round it read as EACCES (F100a review r1).
    (cond ((i/o-file-does-not-exist-error? e) ENOENT)
          ((i/o-file-is-read-only-error? e) EROFS)
          ((i/o-file-protection-error? e) EACCES)
          ((i/o-file-already-exists-error? e) EEXIST)
          (else #f)))

  ;; CHEZ'S CONDITION FROM A NON-MUTATING STEP, as unreadable-entry naming
  ;; the path, the errno mapped where the class names one.
  (define (chez-unreadable! path e)
    (let ((code (and (condition? e) (condition-errno e))))
      (raise (make-unreadable-entry path
                                    (if code (c-strerror code) "the step failed")
                                    (and code (errno-reason code))))))

  ;; THE CLOSE OF A PORT CHEZ OPENED TO CREATE A FILE AND NEVER WROTE: a
  ;; non-mutating step (F100 D1), so its failure is unreadable-entry.
  (define (close-unwritten-port! path port)
    (guard (e ((unreadable-entry? e) (raise e))
              (#t (chez-unreadable! path e)))
      (close-port port)))

  ;; ---- reading an entry, and saying why it could not be read -------------
  ;;
  ;; KEY: ABSENCE IS AN ERRNO RETURNED BY THE OPERATION, NOT A PREDICATE
  ;; ASKED BEFORE IT. A directory with modes 000, r-- or --x still answers
  ;; stat, and `file-exists?` on a child of it answers #f -- the same answer
  ;; as a child that is not there. What fails is the listing or the read
  ;; that follows. So each of the three operations below does the thing
  ;; itself and answers `absent` when, and only when, the system said
  ;; ENOENT or ENOTDIR; every other failure -- permission, i/o, a symlink
  ;; loop, an overflow -- raises `unreadable-entry`, carrying the path that
  ;; failed and the system's reason. Nothing here decides that a failure is
  ;; an absence.
  ;;
  ;; A LISTING IS COMPLETE OR IT RAISES, and so is a read: end-of-directory
  ;; and end-of-file are the only ends. A failure part way is
  ;; `unreadable-entry` naming the path, never a shorter list or a shorter
  ;; buffer.

  (define ENOTDIR (platform-number 'ENOTDIR))
  (define EISDIR (platform-number 'EISDIR))
  (define ELOOP (platform-number 'ELOOP))
  (define EOVERFLOW (platform-number 'EOVERFLOW))
  (define ENAMETOOLONG (platform-number 'ENAMETOOLONG))

  ;; TWO FIELDS, TWO READERS. The REASON is the system's own message, for a
  ;; person reading `check` -- which file, and why, in the words the system
  ;; used. The ERRNO is a name when this file knows one and the number
  ;; otherwise, for code comparing a failure with the one it caused, so
  ;; that no errno is ever reported as nothing.
  (define c-strerror (foreign-procedure "strerror" (int) string))
  ;; THE REASON A durable-error IS ANSWERED WITH (F100b): the same text an
  ;; unreadable-entry already carries for the same errno.
  ;; #f HAS NO TEXT (F100b M2a reviews r1 F3, r2 F3): a durable-error may
  ;; carry no errno -- a short write, or a Chez condition condition-errno
  ;; cannot map (a create, say) -- and handing #f to strerror raised from
  ;; the table itself. What a missing errno MEANS depends on the step; the
  ;; table in (theourgia answers) says so for a write.
  (define (errno-text code) (if code (c-strerror code) "no errno"))
  (define (errno-reason code)
    (cond
      ((eqv? code EACCES) 'EACCES)
      ((eqv? code EIO) 'EIO)
      ((eqv? code ELOOP) 'ELOOP)
      ((eqv? code EOVERFLOW) 'EOVERFLOW)
      ((eqv? code ENAMETOOLONG) 'ENAMETOOLONG)
      ((eqv? code EMFILE) 'EMFILE)
      ((eqv? code EPERM) 'EPERM)
      ((eqv? code ENOENT) 'ENOENT)
      ((eqv? code ENOTDIR) 'ENOTDIR)
      ((eqv? code EISDIR) 'EISDIR)
      (else code)))

  (define (absence-errno? code) (or (eqv? code ENOENT) (eqv? code ENOTDIR)))

  (define-condition-type &unreadable-entry &error
    make-unreadable-entry-condition unreadable-entry?
    (path unreadable-entry-path)
    (reason unreadable-entry-reason)
    (errno unreadable-entry-errno))

  ;; THE CONDITION, BUILT IN ONE PLACE: a path and a reason, with a message
  ;; so that a handler printing conditions has something to print. The log
  ;; raises the same condition from a discovery that is already a fact.
  (define (make-unreadable-entry path reason errno)
    (condition (make-unreadable-entry-condition path reason errno)
               (make-message-condition "the entry cannot be read")
               (make-irritants-condition (list path reason))))

  (define (unreadable! path code)
    (raise (make-unreadable-entry path (c-strerror code) (errno-reason code))))

  ;; -> directory | regular | other | absent
  ;; The TYPE of what the path names, following symlinks. A type is not a
  ;; promise that the object can be read; the read says that.
  (define (entry-type path)
    (unless (string? path)
      (assertion-violation 'entry-type "path must be a string" path))
    (let ((buf (make-bytevector stat-buffer-size 0)))
      (let-values (((rc code)
                    (if (stat-fault? path)
                        (values -1 (stat-fault-errno))
                        (let ((rc (c-stat path buf))) (values rc (and (< rc 0) (errno)))))))
        (cond
          ((>= rc 0)
           (let ((kind (bitwise-and (st-mode-of buf) S_IFMT)))
             (cond ((= kind S_IFDIR) 'directory)
                   ((= kind S_IFREG) 'regular)
                   (else 'other))))
          ((absence-errno? code) 'absent)
          (else (unreadable! path code))))))

  ;; -> directory | regular | link | other | absent
  ;; The TYPE of the directory ENTRY the path names, NOT following a
  ;; symbolic link at its last component (lstat): a link answers `link`,
  ;; whatever it points at and whether it points at anything. A walk that
  ;; must stay inside the tree it walks -- removing a directory another
  ;; program wrote into -- asks this one: entry-type follows the link and
  ;; names the target's type, so a walk that asked it would enter the
  ;; target. ENOENT and ENOTDIR answer absent; any other failure raises
  ;; unreadable-entry naming the path. stat-fail reaches it.
  (define (entry-name-type path)
    (unless (string? path)
      (assertion-violation 'entry-name-type "path must be a string" path))
    (let ((buf (make-bytevector stat-buffer-size 0)))
      (let-values (((rc code)
                    (if (stat-fault? path)
                        (values -1 (stat-fault-errno))
                        (let ((rc (c-lstat path buf))) (values rc (and (< rc 0) (errno)))))))
        (cond
          ((>= rc 0)
           (let ((kind (bitwise-and (st-mode-of buf) S_IFMT)))
             (cond ((= kind S_IFDIR) 'directory)
                   ((= kind S_IFREG) 'regular)
                   ((= kind S_IFLNK) 'link)
                   (else 'other))))
          ((absence-errno? code) 'absent)
          (else (unreadable! path code))))))

  ;; -> the size in bytes of what the path names, or absent. An R1 query
  ;; (F100 D1): a stat, so it needs no read permission on the file itself,
  ;; and it answers `absent` for ENOENT and ENOTDIR and raises
  ;; unreadable-entry for any other failure. stat-fail reaches it.
  ;;
  ;; st_size's offset, per platform, from the field lists given at
  ;; path-device-inode:
  ;;   macOS 25.3 arm64   96, eight bytes: MEASURED 2026-09-25, offsetof in
  ;;                      a C program against the system header
  ;;                      (sizeof(struct stat) 144, as stated below).
  ;;   FreeBSD 15         112, eight bytes: from sys/stat.h's field order
  ;;                      (dev 8, ino 8, nlink 8, mode 2, padding0 2, uid 4,
  ;;                      gid 4, padding1 4, rdev 8, four timespecs 64), whose
  ;;                      total is the 224 measured below.
  ;;   Linux x86-64       48 (glibc's field list).
  ;; The offset is the platform row's (96 on macOS, 48 on both Linux, 112
  ;; on FreeBSD), read as off_t is: signed, at its width. test/facade-ffi.sc's
  ;; row "size-entry answers absent for ENOENT and the size otherwise" is
  ;; still the check of it on whichever platform the suite runs.
  (define (size-entry path)
    (unless (string? path)
      (assertion-violation 'size-entry "path must be a string" path))
    (let ((buf (make-bytevector stat-buffer-size 0)))
      (let-values (((rc code)
                    (if (stat-fault? path)
                        (values -1 (stat-fault-errno))
                        (let ((rc (c-stat path buf))) (values rc (and (< rc 0) (errno)))))))
        (cond
          ((>= rc 0) (row-sint-ref buf 'stat 'st_size))
          ((absence-errno? code) 'absent)
          (else (unreadable! path code))))))

  ;; REPLACE A FILE'S CONTENTS IN PLACE, keeping its inode. The door's form
  ;; of the base's truncating open-file-output-port (F100 D1): a hard link
  ;; to the file sees the new bytes, as it did. Create if absent, then open
  ;; for writing, truncate and write; each step is noted as it succeeds, by
  ;; the primitive that takes it: (create p) (truncate p) (write p) for a
  ;; fresh path, (truncate p) (write p) for an existing file, and no
  ;; (write p) for empty bytes. It notes nothing itself.
  ;; NEVER: THE TRUNCATE IS UNCONDITIONAL. Deciding it from a presence test
  ;; taken before the open let another process create the file in between
  ;; and keep its tail: "x" over its "abcdef" left "xbcdef", where the
  ;; base's truncating port left "x" (F100a review r1).
  ;; No flush: the base's sites did not flush, and this keeps their bytes.
  (define (overwrite-entry! path bv)
    (unless (bytevector? bv)
      (assertion-violation 'overwrite-entry! "not a bytevector" bv))
    (let ((fd (fd-open path '(write create))))
      (let ((done (box #f)))
        (dynamic-wind
          void
          (lambda ()
            (ftruncate! fd 0)
            (write-all! fd bv)
            (set-box! done #t)
            (fd-close fd))
          (lambda ()
            (unless (unbox done)
              (set-box! done #t)
              (close-quietly fd)))))
      (void)))

  ;; A DESCRIPTOR THAT WAS NEVER WRITTEN, CLOSED BY ITS READER. On the
  ;; normal path the close is checked, and its failure (or an injected
  ;; close-fail) raises unreadable-entry naming the path -- the close of an
  ;; unwritten descriptor is a non-mutating primitive (F100 D1). On an escape
  ;; it is closed quietly, so the failure already on its way out is the one
  ;; reported: the header's two obligations around cleanup.
  (define (read-closing path close-rc fault? thunk)
    (let ((closed (box #f)))
      (dynamic-wind
        void
        (lambda ()
          (let ((v (thunk)))
            (set-box! closed #t)
            (let ((rc (close-rc)))
              (when (< rc 0) (unreadable! path (errno)))
              (when (and fault? (close-fault?)) (unreadable! path EIO)))
            v))
        (lambda ()
          (unless (unbox closed)
            (set-box! closed #t)
            (close-rc))))))

  ;; -> the file's bytes, or #(absent <errno>) for ENOENT and ENOTDIR. The
  ;; errno is kept so a caller for which absence is a failure can name it
  ;; (entry-bytes); read-entry answers the bare `absent`.
  (define (read-entry/errno path)
    (unless (string? path)
      (assertion-violation 'read-entry "path must be a string" path))
    (let ((fd (let ((injected (open-fault path)))
                (if injected
                    (begin (errno-set! injected) -1)
                    (c-open path O_RDONLY)))))
      (cond
        ((< fd 0)
         (let ((code (errno)))
           (if (absence-errno? code) (vector 'absent code) (unreadable! path code))))
        (else
         (let ((chunk (make-bytevector 65536)))
           (read-closing path (lambda () (c-close fd)) #t
             (lambda ()
               (let-values (((out collect) (open-bytevector-output-port)))
                 (let loop ((produced 0))
                   (let ((injected (read-fault path produced)))
                     (when injected (unreadable! path injected)))
                   (let ((n (c-read fd chunk 65536)))
                     (cond
                       ((< n 0)
                        (let ((code (errno)))
                          (if (eqv? code EINTR)
                              (loop produced)
                              (unreadable! path code))))
                       ((= n 0) (collect))
                       (else
                        (put-bytevector out chunk 0 n)
                        (loop (+ produced n))))))))))))))

  ;; -> the file's bytes, or absent
  (define (read-entry path)
    (let ((r (read-entry/errno path)))
      (if (vector? r) 'absent r)))

  ;; -> the file's bytes. NOT AN R1 QUERY: absence raises unreadable-entry
  ;; with ITS errno, ENOENT or ENOTDIR, like any other failure, for a caller
  ;; whose file must be there and that has no next step for `absent`
  ;; (F100a; the base's native read raised too). directory-entries is the
  ;; same shape for a listing.
  (define (entry-bytes path)
    (let ((r (read-entry/errno path)))
      (if (vector? r) (unreadable! path (vector-ref r 1)) r)))

  ;; -> at most COUNT bytes of the file from OFFSET: fewer only at its end.
  ;; An offset at or past the end answers an empty bytevector, a range that
  ;; crosses the end the bytes up to it -- found by a further read that
  ;; answers 0, and read-fail-after stands before every read after the
  ;; first that produced something, as it does in read-entry.
  ;; Non-mutating throughout (open, lseek, read, the close), so any failure,
  ;; absence included, raises unreadable-entry naming the path (F100a). For
  ;; a caller that reads one record out of a segment, where reading the
  ;; whole file to slice it would cost the file's size per record.
  (define (read-entry-range path offset count)
    (unless (and (string? path) (integer? offset) (exact? offset) (>= offset 0)
                 (integer? count) (exact? count) (>= count 0))
      (assertion-violation 'read-entry-range "path, offset and count" path offset count))
    (let ((fd (let ((injected (open-fault path)))
                (if injected
                    (unreadable! path injected)
                    (c-open path O_RDONLY)))))
      (when (< fd 0) (unreadable! path (errno)))
      (read-closing path (lambda () (c-close fd)) #t
        (lambda ()
          (when (< (c-lseek fd offset SEEK_SET) 0) (unreadable! path (errno)))
          (let ((buf (make-bytevector count)))
            (let loop ((got 0))
              (if (= got count)
                  buf
                  (let ((injected (read-fault path got)))
                    (when injected (unreadable! path injected))
                    (let* ((want (- count got))
                           (chunk (make-bytevector want))
                           (n (c-read fd chunk want)))
                      (cond
                        ((< n 0)
                         (let ((code (errno)))
                           (if (eqv? code EINTR) (loop got) (unreadable! path code))))
                        ((= n 0)
                         (let ((out (make-bytevector got)))
                           (bytevector-copy! buf 0 out 0 got)
                           out))
                        (else
                         (bytevector-copy! chunk 0 buf got n)
                         (loop (+ got n)))))))))))))

  ;; -> the names in the directory, without "." and "..", or absent
  ;; d_name's offset is the platform row's: 21 on macOS, 19 on Linux, 24
  ;; on FreeBSD.
  (define dirent-name-offset (platform-field 'dirent 'd_name 'offset))
  (define x86-macos? (and macos? (memq (machine-type) '(a6osx ta6osx)) #t))
  (define c-opendir
    (foreign-procedure (if x86-macos? "opendir$INODE64" "opendir") (string) uptr))
  (define c-readdir
    (foreign-procedure (if x86-macos? "readdir$INODE64" "readdir") (uptr) uptr))
  (define c-closedir (foreign-procedure "closedir" (uptr) int))
  (define (errno-set! code) (foreign-set! 'int (c-errno-location) 0 code))

  (define (dirent-name entry)
    (let loop ((i 0) (acc '()))
      (let ((b (foreign-ref 'unsigned-8 entry (+ dirent-name-offset i))))
        (if (= b 0)
            (utf8->string (u8-list->bytevector (reverse acc)))
            (loop (+ i 1) (cons b acc))))))

  ;; -> the names, or #(absent <errno>); list-entries answers the bare
  ;; `absent`, directory-entries raises with the errno kept.
  (define (list-entries/errno path)
    (unless (string? path)
      (assertion-violation 'list-entries "path must be a string" path))
    (let ((dir (c-opendir path)))
      (if (= dir 0)
          (let ((code (errno)))
            (if (absence-errno? code) (vector 'absent code) (unreadable! path code)))
          (read-closing path (lambda () (c-closedir dir)) #f
            (lambda ()
              (let loop ((names '()))
                (let ((injected (readdir-fault path (length names))))
                  (when injected (unreadable! path injected)))
                (errno-set! 0)
                (let ((entry (c-readdir dir)))
                  (if (= entry 0)
                      (let ((code (errno)))
                        (if (= code 0)
                            (reverse names)
                            (unreadable! path code)))
                      (let ((name (dirent-name entry)))
                        (loop (if (or (string=? name ".") (string=? name ".."))
                                  names
                                  (cons name names))))))))))))

  (define (list-entries path)
    (let ((r (list-entries/errno path)))
      (if (vector? r) 'absent r)))

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

  ;; WHETHER A DESCRIPTOR WAS WRITTEN (a write or a truncation was asked
  ;; of it), which decides the class of its close's failure.
  (define fd-written (make-eqv-hashtable))
  (define (written! fd) (hashtable-set! fd-written fd #t))

  (define (forget-fd! fd)
    (hashtable-delete! fd-paths fd)
    (hashtable-delete! fd-written fd))

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
    ;; `client` is here for the same reason as `conn`: the thin client's
    ;; close-after-the-answer has to be made to fail before anything can
    ;; ask what it does then, and a failure mode no row can arm is one
    ;; that nothing checks.
    ;; `presence` and `presence-decision` are adopt's two questions about the
    ;; name instance.sexp: the inventory's (answered by the preflight) and the
    ;; decision's (answered by the request's failure table). Each is its own
    ;; stage so a fault can be aimed at either one without the other, or
    ;; verify-instance's earlier stat, consuming it.
    ;; `mcp-wait` and `mcp-signal` are the MCP shell's two calls on a child
    ;; it runs for `eval`: the poll that asks whether it has ended, and the
    ;; signal sent at its deadline. Each is a stage so a fault can be aimed
    ;; at the one call and not at the daemon starts that also wait and signal.
    ;; `derived` is the write of a table of facts an editor supplied, beside
    ;; the store and never in it: a fault aimed there shows that a failed
    ;; write leaves the table a reader had.
    '(deliver-barrier commit registry publish snapshot repair report working index conn client
      eval-cleanup presence presence-decision mcp-wait mcp-signal admission derived))

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
         read-fail-after readdir-fail-after
         conn-raise store-raise writer-raise writer-raise-late
         writer-hold writer-hold-long conn-hold conn-hold-long close-fail
         lseek-fail mkdir-fail client-extra-child store-raise-early
         reload-raise probe-raise unlink-fail waitpid-fail kill-fail cloexec-fail))

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
     ;; IT IS READ FOR THE FAULTS THAT FAIL WITH AN ERRNO (`errno-faults`)
     ;; AND FOR NOTHING ELSE. Stripping it
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

     ;; The faults that fail with an errno. stat-fail predates the
     ;; qualifier and reports EIO without one; the other three require it.
     ;; lseek-fail and mkdir-fail are stat-fail's kind: EIO unless :errno=
     ;; names another (F100a amendment 6).
     ;; waitpid-fail and kill-fail are mkdir-fail's kind as well: EIO unless
     ;; :errno= names another.
     (define errno-faults '(open-fail stat-fail read-fail-after readdir-fail-after lseek-fail mkdir-fail
                            waitpid-fail kill-fail cloexec-fail))
     (define errno-required-faults '(open-fail read-fail-after readdir-fail-after))

     ;; THE PATHLESS FAULTS TAKE NO PATH, so their whole argument is the
     ;; qualifier: `waitpid-fail@mcp-wait:errno=EACCES`. Read with the colon
     ;; the others carry before it, it was missed and EIO was injected
     ;; instead of the errno named; anything else after the stage is refused
     ;; at load, as an unknown qualifier is.
     (define pathless-faults '(waitpid-fail kill-fail cloexec-fail))

     (define-values (fault-arg-head fault-errno-text)
       (cond
         ((and (memq fault-name pathless-faults) fault-arg)
          (split-errno (string-append ":" fault-arg)))
         ((memq fault-name errno-faults) (split-errno fault-arg))
         (else (values fault-arg #f))))

     (define pathless-fault-checked
       (when (and (memq fault-name pathless-faults) fault-arg
                  (not (and (string? fault-arg-head) (string=? fault-arg-head ""))))
         (assertion-violation 'theourgia-ffi
           "this fault takes no path: <fault>@<stage> or <fault>@<stage>:errno=<name>"
           fault-spec)))

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
                  ((string=? fault-errno-text "ENOTDIR") ENOTDIR)
                  ((string=? fault-errno-text "EIO") EIO)
                  ((string=? fault-errno-text "ELOOP") ELOOP)
                  ((string=? fault-errno-text "EOVERFLOW") EOVERFLOW)
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
       (begin
         (when (memq fault-name errno-required-faults)
           (when (or (not fault-errno) (eq? fault-errno 'unknown))
             (assertion-violation 'theourgia-ffi
               "this fault needs :errno=<name> (EMFILE EACCES ENOENT ENOTDIR EIO ELOOP EOVERFLOW) or a positive integer"
               fault-spec)))
         (when (and (memq fault-name '(stat-fail lseek-fail mkdir-fail waitpid-fail kill-fail cloexec-fail)) (eq? fault-errno 'unknown))
           (assertion-violation 'theourgia-ffi
             "this fault's :errno= names no errno this file knows"
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
       (when (and (memq fault-name errno-required-faults) (eq? fault-kind 'dir))
         (assertion-violation 'theourgia-ffi
           "this fault takes file=<substring>; it matches on the path alone"
           fault-spec)))

     (define fault-argument-checked
       (when (memq fault-name '(fsync-fail no-log-fsync open-fail read-fail-after readdir-fail-after
                                lseek-fail mkdir-fail))
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
       ;; NEVER: SO NOTHING IN HERE IS COVERED BY A RUN WITH INJECTION OFF.
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
;; ---- the hold seam (F100b item 7) -----------------------------------
     ;;
     ;; ONE MECHANISM, SEPARATE FROM THEOURGIA_FAULT so a hold and a fault can
     ;; be armed together. THEOURGIA_HOLD is `<stage>:<release>` or several
     ;; of them joined by `;`. At a named stage the process creates
     ;; `<release>.held` -- the acknowledgement a row polls for -- and waits,
     ;; polling every 20 ms, until `<release>` exists. After THEOURGIA_HOLD_MS
     ;; (default 30000) it goes on and says `(theourgia hold-expired <stage>)`
     ;; on stderr. Once the release exists, later passes do not wait.
     ;; AN UNKNOWN STAGE OR A MALFORMED ENTRY IS REFUSED AT LOAD, like an
     ;; unknown fault: a hold aimed at nothing would read, in a test log,
     ;; exactly like a hold that never came.
     (define known-hold-stages
       '(client-scan report-write bind write-after-create publish-after-link store-start
         after-discovery after-barrier mcp-child-wait eval-admission))
     (define (split-at-semicolons s)
       (let loop ((i 0) (from 0) (out '()))
         (cond
           ((= i (string-length s)) (reverse (cons (substring s from i) out)))
           ((char=? (string-ref s i) #\;) (loop (+ i 1) (+ i 1) (cons (substring s from i) out)))
           (else (loop (+ i 1) from out)))))
     (define holds
       (let ((v (getenv "THEOURGIA_HOLD")))
         (if (not (and (string? v) (> (string-length v) 0)))
             '()
             (map (lambda (entry)
                    (let-values (((stage release) (split-at-colon entry)))
                      (unless (and release (> (string-length release) 0)
                                   (memq (string->symbol stage) known-hold-stages))
                        (assertion-violation 'theourgia-ffi
                          "THEOURGIA_HOLD must be <stage>:<path>[;<stage>:<path>...] with a known stage"
                          v known-hold-stages))
                      (cons (string->symbol stage) release)))
                  (split-at-semicolons v)))))
     ;; AN EXACT NON-NEGATIVE INTEGER OF MILLISECONDS, OR REFUSED AT LOAD
     ;; like a stage (M2b1 review r1, F3): +inf.0 or +nan.0 would never
     ;; expire, and a complex number would raise inside the wait.
     (define hold-ms
       (let ((v (getenv "THEOURGIA_HOLD_MS")))
         (if (not v)
             30000
             (let ((n (string->number v 10)))
               (unless (and n (exact? n) (integer? n) (>= n 0))
                 (assertion-violation 'theourgia-ffi
                   "THEOURGIA_HOLD_MS must be an exact non-negative integer of milliseconds" v))
               n))))
     ;; THE WAIT IS SETTABLE, ONCE, NEVER PARAMETERIZED (F100b M2 Q3). The
     ;; default blocks the OS thread, which is right for the thin client (no
     ;; scheduler). A daemon sets igropyr's sleep-ms, which yields, before any
     ;; actor exists -- a hold that blocked the thread would stop every actor,
     ;; and a second held actor could never be reached (A-record).
     (define hold-sleeper
       (lambda (ms) (sleep (make-time 'time-duration (* ms 1000000) 0))))
     (define (hold-sleeper-set! proc) (set! hold-sleeper proc))
     (define (hold-point! stage)
       (let ((h (assq stage holds)))
         (when h
           (let ((release (cdr h)))
             (trace-event! 'hold stage #f)
             (file-ensure-unrecorded! (string-append release ".held"))
             (let loop ((waited 0))
               (cond
                 ((file-exists? release) (void))
                 ((>= waited hold-ms)
                  (let ((p (current-error-port)))
                    (put-string p "(theourgia hold-expired ")
                    (put-string p (symbol->string stage))
                    (put-string p ")\n")
                    (flush-output-port p)))
                 (else (hold-sleeper 20) (loop (+ waited 20)))))))))

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

     ;; NOTE: NOT ONE-SHOT. What this exists to ask is whether a close
     ;; failure changes anything, and a caller that closed twice on its
     ;; way out would get one failure and one success from a one-shot
     ;; fault -- which is the shape that hid the defect in the first
     ;; place.
     (define (close-fault?)
       (and fault-name
            (eq? fault-name 'close-fail)
            (in-fault-stage?)))

     (define (stat-fault-errno) (or fault-errno EIO))

     ;; -> an errno, or #f. A READ OR A LISTING FAILS PART WAY, which is
     ;; what these two exist to produce: they fire only once something has
     ;; been produced, so a caller that answered with what it had so far
     ;; would be seen doing it. One-shot within the stage, like open-fail.
     (define (read-fault path produced)
       (and fault-name
            (eq? fault-name 'read-fail-after)
            (in-fault-stage?)
            (fault-path-match? #f path)
            (> produced 0)
            (eq? (unbox fault-state) 'fresh)
            (begin (set-box! fault-state 'done) fault-errno)))

     (define (readdir-fault path produced)
       (and fault-name
            (eq? fault-name 'readdir-fail-after)
            (in-fault-stage?)
            (fault-path-match? #f path)
            (> produced 0)
            (eq? (unbox fault-state) 'fresh)
            (begin (set-box! fault-state 'done) fault-errno)))

     (define (stat-fault? path)
       (and fault-name
            (eq? fault-name 'stat-fail)
            (in-fault-stage?)
            (or (not fault-arg) (fault-path-match? #f path))
            (eq? (unbox fault-state) 'fresh)
            (begin (set-box! fault-state 'done) #t)))

     ;; ONE-SHOT, PATH-SCOPED, LIKE stat-fail (F100a): an lseek on a
     ;; descriptor whose path matches, and a mkdir of a path that matches,
     ;; report EIO or the :errno= given, without the syscall. -> an errno,
     ;; or #f.
     (define (lseek-fault subject)
       (and fault-name
            (eq? fault-name 'lseek-fail)
            (in-fault-stage?)
            (fault-path-match? #f subject)
            (eq? (unbox fault-state) 'fresh)
            (begin (set-box! fault-state 'done) (or fault-errno EIO))))

     ;; ONE SHOT in its stage: the first unlink there fails with EIO, which is
     ;; all a cleanup needs in order to be seen failing.
     (define (unlink-fault path)
       (and fault-name
            (eq? fault-name 'unlink-fail)
            (in-fault-stage?)
            (eq? (unbox fault-state) 'fresh)
            (begin (set-box! fault-state 'done) EIO)))

     (define (mkdir-fault path)
       (and fault-name
            (eq? fault-name 'mkdir-fail)
            (in-fault-stage?)
            (fault-path-match? #f path)
            (eq? (unbox fault-state) 'fresh)
            (begin (set-box! fault-state 'done) (or fault-errno EIO))))

     ;; ONE SHOT in its stage, for a process, not a path: the first waitpid
     ;; (or kill) there fails with EIO or the :errno= given, without the
     ;; syscall. -> an errno, or #f.
     (define (waitpid-fault)
       (and fault-name
            (eq? fault-name 'waitpid-fail)
            (in-fault-stage?)
            (eq? (unbox fault-state) 'fresh)
            (begin (set-box! fault-state 'done) (or fault-errno EIO))))
     (define (kill-fault)
       (and fault-name
            (eq? fault-name 'kill-fail)
            (in-fault-stage?)
            (eq? (unbox fault-state) 'fresh)
            (begin (set-box! fault-state 'done) (or fault-errno EIO))))
     ;; The descriptor refused as an invalid one would be: EBADF unless
     ;; :errno= names another.
     (define (cloexec-fault)
       (and fault-name
            (eq? fault-name 'cloexec-fail)
            (in-fault-stage?)
            (eq? (unbox fault-state) 'fresh)
            (begin (set-box! fault-state 'done) (or fault-errno EBADF))))

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
     ;; -> the count, the errno, and whether the syscall was SKIPPED by an
     ;; injected failure. write-all! notes a final write failure only when
     ;; write(2) really ran (F100 D1'), and the flag travels with the
     ;; answer: a process-global flag was overwritten by another actor's
     ;; write between the two (F100a review r1).
     (define (write-once fd bv count subject)
       (if (and (in-fault-stage?)
                (or (not fault-arg) (fault-path-match? fd subject)))
           (staged-write-once fd bv count)
           (real-write/skipped fd bv count)))

     (define (skipped-failure code) (values -1 code #t))

     (define (staged-write-once fd bv count)
       (case fault-name
         ((eintr-once)
          (if (eq? (unbox fault-state) 'fresh)
              (begin (set-box! fault-state 'done) (skipped-failure EINTR))
              (real-write/skipped fd bv count)))
         ((short-write)
          (if (eq? (unbox fault-state) 'fresh)
              (let-values (((n code) (real-write fd bv (min short-count count))))
                (when (> n 0) (set-box! fault-state 'done))
                (values n code #f))
              (real-write/skipped fd bv count)))
         ((write-eio-after-partial)
          (if (eq? (unbox fault-state) 'fresh)
              (let-values (((n code) (real-write fd bv (min short-count count))))
                (when (> n 0) (set-box! fault-state 'partial))
                (values n code #f))
              (skipped-failure EIO)))
         ;; FAILS BEFORE THE FIRST BYTE. write-eio-after-partial cannot
         ;; produce this: it fails only after its partial has landed, so
         ;; the outcome that means "reserved and nothing written" had no
         ;; way to be reached, and a caller could not be shown that a
         ;; retry is safe there.
         ((write-eio-first)
          (if (eq? (unbox fault-state) 'fresh)
              (begin (set-box! fault-state 'done) (skipped-failure EIO))
              (real-write/skipped fd bv count)))
         (else (real-write/skipped fd bv count)))))

    (else
     (define theourgia-stage (make-parameter #f))
     ;; NOTHING ABOVE EXISTS IN THIS BUILD. These two are the whole of
     ;; what the rest of the library calls, and they say "no fault" in a
     ;; form the compiler can fold away.
     (define (theourgia-fault) #f)
     (define (theourgia-fault-armed?) #f)
     (define (stat-fault? path) #f)
     (define (stat-fault-errno) EIO)
     (define (lseek-fault subject) #f)
     (define (mkdir-fault path) #f)
     (define (waitpid-fault) #f)
     (define (kill-fault) #f)
     (define (cloexec-fault) #f)
     (define (unlink-fault path) #f)
     (define (read-fault path produced) #f)
     (define (readdir-fault path produced) #f)
     (define (close-fault?) #f)
     (define (report-fault?) #f)
     (define (open-fault path) #f)
     (define (fsync-fault fd subject kind) #f)
     (define (barrier! name) (void))
     (define (hold-point! stage) (void))
     (define (hold-sleeper-set! proc) (void))
     (define no-flock? #f)
     (define (write-once fd bv count subject) (real-write/skipped fd bv count))))

  ;; ---- opening ----------------------------------------------------------

  ;; Creating the file is Chez's job, for the reason given at the top:
  ;; O_CREAT would need a mode and the mode cannot be passed. no-fail
  ;; makes an existing file acceptable and no-truncate leaves its
  ;; contents alone, so this is create-if-absent and nothing else. It is
  ;; separate and exported because a caller sometimes wants only this --
  ;; the lock file has to exist before anyone opens it read-only.
  (define (file-ensure! path) (file-ensure-body! path #t))

  ;; THE HOLD SEAM'S MARKER IS CREATED THROUGH THE DOOR, WITHOUT A NOTE (F100b
  ;; item 7, Q8). A hold inside an open scope must not add `<path>.held` to
  ;; that scope's record. The flag is LEXICAL -- an argument -- and NOT a
  ;; parameter: under igropyr a parameterize is one global cell that a
  ;; preemption hands to every other actor (actor.sc:459-469), so it would
  ;; suppress the notes of whatever actor ran during the hold. Its callers
  ;; are hold-point!, which exists only in an injection build, and the eval
  ;; admission's slot files (eval-admission.sc eval-admit!), which are
  ;; administration and not the request's writes.
  (define (file-ensure-unrecorded! path) (file-ensure-body! path #f))

  (define (file-ensure-body! path record?)
    (unless (string? path)
      (assertion-violation 'file-ensure! "path must be a string" path))
    ;; THE PRESENCE TEST RAISES FIRST (entry-type): under a parent that
    ;; cannot be searched it is unreadable-entry, before anything is made.
    ;; The create's own failure is durable-error with the errno mapped from
    ;; Chez's condition (the create is Chez's, see the note on open(2)). The
    ;; create is NOTED AS SOON AS IT SUCCEEDED, before the close: a close
    ;; that then fails must not take the create out of the record (F100a
    ;; review r1). That close, of a port never written, is its own step.
    (when (eq? (entry-type path) 'absent)
      (let ((port (guard (e ((fs-error? e) (raise e))
                            ((unreadable-entry? e) (raise e))
                            (#t (raise (fs-err 'create path (and (condition? e) (condition-errno e))))))
                    (open-file-output-port path (file-options no-fail no-truncate)))))
        (when record? (note! (list 'create path)))
        (trace-event! 'create path #f)
        (close-unwritten-port! path port)))
    path)

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
      ;; NOTE: THE OPEN ITSELF IS NON-MUTATING (the create, if any, is
      ;; file-ensure!'s and already happened), so its failure is
      ;; unreadable-entry with the errno, ENOENT and ENOTDIR included (F100
      ;; D1): only the R1 queries answer `absent`.
      (let ((injected (open-fault path)))
        (when injected (unreadable! path injected)))
      (let ((fd (c-open path (flags->int 'fd-open flags))))
        (when (< fd 0) (unreadable! path (errno)))
        (hashtable-set! fd-paths fd path)
        fd)))

  ;; The entry is dropped BEFORE the close, so that a close which fails
  ;; cannot leave a descriptor number mapped to a path it no longer
  ;; refers to -- the number is reusable either way, and a stale mapping
  ;; would put the wrong path in the next trace.
  ;; NOTE: THE CLOSE OF A WRITTEN DESCRIPTOR IS MUTATING, the close of one
  ;; never written is not (F100 D1): a failed close after a write may
  ;; have lost bytes, a failed close of a read loses nothing. It notes
  ;; nothing either way.
  (define (fd-close fd)
    (let ((subject (subject-of fd '()))
          (written? (hashtable-ref fd-written fd #f)))
      (forget-fd! fd)
      (let* ((rc (c-close fd))
             (code (and (< rc 0) (errno))))
        (define (refuse! code)
          (if written?
              (raise (fs-err 'close subject code))
              (raise (make-unreadable-entry subject (c-strerror code) (errno-reason code)))))
        ;; NOTE: THE DESCRIPTOR IS REALLY CLOSED, and the failure is injected
        ;; after: a fault that skipped the close would leak one per call,
        ;; and the leak rather than the refusal is what a row would end up
        ;; measuring.
        (when (close-fault?) (refuse! EIO))
        (when code (refuse! code))
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
       ;; F_FULLFSYNC is macOS's alone (absent on every other row), so it is
       ;; read here, in the branch that runs there.
       (when macos?
         (let ((rc (c-fcntl fd (platform-number 'F_FULLFSYNC) 0)))
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
      ;; THE DIRECTORY'S OPEN IS NON-MUTATING (F100 D1): unreadable-entry.
      (let ((fd (c-open path O_RDONLY)))
        (when (< fd 0) (unreadable! subject (errno)))
        ;; THE DIRECTORY'S DESCRIPTOR IS NEVER WRITTEN, so a close that fails
        ;; on the normal path is unreadable-entry; close-fail, aimed at
        ;; fd-close's descriptors, is not armed here.
        (read-closing subject (lambda () (c-close fd)) #f
          (lambda () (flush! fd subject 'dir-fsync 'dir-fullfsync 'dir))))
      (void))))

  ;; ---- size, position, truncation ---------------------------------------

  (define (whence->int who w)
    (case w
      ((set start) SEEK_SET)
      ((current cur) SEEK_CUR)
      ((end) SEEK_END)
      (else (assertion-violation who "unknown whence" w))))

  ;; lseek IS NON-MUTATING (F100 D1): its failure is unreadable-entry naming
  ;; the descriptor's path (fd-paths), the number only when none. The
  ;; injected lseek-fail stands where the first lseek would and skips it.
  (define (lseek-or-refuse fd offset whence subject)
    (let ((injected (lseek-fault subject)))
      (when injected (unreadable! subject injected)))
    (let ((pos (c-lseek fd offset whence)))
      (when (< pos 0) (unreadable! subject (errno)))
      pos))

  (define (fd-seek! fd offset whence . opts)
    (lseek-or-refuse fd offset (whence->int 'fd-seek! whence) (subject-of fd opts)))

  (define (fd-size fd . opts)
    (let* ((subject (subject-of fd opts))
           (here (lseek-or-refuse fd 0 SEEK_CUR subject))
           (end (lseek-or-refuse fd 0 SEEK_END subject)))
      (lseek-or-refuse fd here SEEK_SET subject)
      end))

  ;; By seeking rather than by stat: a stat structure's layout differs
  ;; between these platforms and would have to be measured field by
  ;; field, while lseek returns one integer that means the same thing
  ;; everywhere.
  ;;
  ;; A COMPOSITE (open then lseek), NON-MUTATING throughout: whichever step
  ;; fails raises unreadable-entry naming the path, absence included (F100
  ;; D1). Its own classifying guard is gone; fd-open's failure is already
  ;; the right class.
  (define (file-size path)
    (when (stat-fault? path) (unreadable! path (stat-fault-errno)))
    (let ((fd (fd-open path '(read))))
      (read-closing path (lambda () (forget-fd! fd) (c-close fd)) #t
        (lambda () (fd-size fd path)))))

  (define (ftruncate! fd length . opts)
    (unless (and (integer? length) (exact? length) (>= length 0))
      (assertion-violation 'ftruncate! "length must be a non-negative exact integer" length))
    (let ((subject (subject-of fd opts)))
      (written! fd)
      (let ((rc (c-ftruncate fd length)))
        (when (< rc 0) (fail! 'ftruncate subject))
        (note! (list 'truncate subject))
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

  ;; The same, with write-once's third answer: the syscall ran.
  (define (real-write/skipped fd bv count)
    (let-values (((n code) (real-write fd bv count)))
      (values n code #f)))

  (define short-count 7)

  ;; ONE write(2), NEVER RETRIED (F100b item 3, E4). A startup report is one
  ;; line that a reader selects whole or not at all; write-all!'s retry of a
  ;; short write would join a second syscall's bytes to the first, and a
  ;; reader between the two sees half a line that parses as nothing. So a
  ;; short write is a failure here: durable-error with op write and errno
  ;; #f, as write-all! answers a zero-length write. The write is noted once,
  ;; when write(2) really ran, as in write-all!.
  (define (write-one! fd bv . opts)
    (unless (bytevector? bv)
      (assertion-violation 'write-one! "not a bytevector" bv))
    (let ((subject (subject-of fd opts))
          (total (bytevector-length bv)))
      (let-values (((n code skipped) (begin (written! fd)
                                            (write-once fd bv total subject))))
        (unless skipped (note! (list 'write subject)))
        (cond
          ((< n 0) (raise (fs-err 'write subject code)))
          ((< n total) (raise (fs-err 'write subject #f)))
          (else (trace-event! 'write subject n) total)))))

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
           (total (bytevector-length bv))
           ;; THE WRITE IS NOTED ONCE PER CALL: when a byte is reported
           ;; written, or when a real write syscall fails finally (POSIX
           ;; gives no unchanged-file guarantee for a physical write error).
           ;; The EINTR retried without progress is not a final failure, and
           ;; write-eio-first skips the syscall, so it notes nothing.
           (noted #f)
           (note-write! (lambda () (unless noted (set! noted #t) (note! (list 'write subject))))))
      (let loop ((pos 0) (chunk bv))
        (if (>= pos total)
            total
            ;; MARKED WRITTEN WHERE A WRITE IS ATTEMPTED, not on entry: an
            ;; empty bytevector attempts none, and its descriptor's close
            ;; keeps the class of an unwritten one (F100a review r1).
            (let-values (((n code skipped) (begin (written! fd)
                                                   (write-once fd chunk (- total pos) subject))))
              (cond
                ((and (< n 0) (= code EINTR))
                 (loop pos chunk))
                ((< n 0)
                 (unless skipped (note-write!))
                 (raise (fs-err 'write subject code)))
                ((= n 0)
                 (note-write!)
                 (raise (fs-err 'write subject #f)))
                (else
                 (note-write!)
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
  ;; EVERY PLATFORM'S OFFSETS AND WIDTHS ARE ITS ROW'S (theourgia
  ;; platform-numbers), measured by compiling test/probe/layout.c there;
  ;; a platform without a row is refused before this runs. The buffer is
  ;; at least 512 bytes, over every measured struct stat (128 to 224), so
  ;; a wrong row cannot smash the heap -- but an oversized buffer does not
  ;; make wrong offsets right, which is why the offsets are measured.
  (define stat-buffer-size (max 512 (platform-struct-size 'stat)))

  ;; THE ONE NAME A PATH HAS. Several spellings reach one file --
  ;; a trailing slash, a `.`, a symlink, a relative path -- and anything
  ;; deriving an identity from the spelling gives that one file several
  ;; identities.
  ;;
  ;; KEY: MEASURED, on the key the daemon used to compute: `/tmp/x`,
  ;; `/tmp/x/`, a symlink to it and `/tmp/./x` produced FOUR different
  ;; keys, although `stat` reported the same device and inode for all
  ;; four -- because the spelling was concatenated in front of them.
  ;;
  ;; NOTE: IT ANSWERS #f FOR A PATH THAT IS NOT THERE, rather than raising:
  ;; a caller asking "what is this really called" about something that
  ;; does not exist has an answer to give, and it is not an error.
  ;; `PATH_MAX` is the row's (1024 on macOS and FreeBSD, 4096 on Linux);
  ;; the buffer is at least 4096 and at least PATH_MAX.
  (define (real-path path)
    (guard (e (#t #f))
      (let* ((size (max 4096 (platform-number 'PATH_MAX)))
             (buf (make-bytevector size 0))
             (rc (c-realpath path buf)))
        (and (not (eqv? rc 0))
             (let loop ((i 0))
               (cond ((>= i size) #f)
                     ((zero? (bytevector-u8-ref buf i))
                      (utf8->string (let ((out (make-bytevector i)))
                                      (bytevector-copy! buf 0 out 0 i)
                                      out)))
                     (else (loop (+ i 1)))))))))

  ;; IS THIS PATH A SOCKET? Read from `stat`'s type bits, because the
  ;; question the daemon asks before unlinking is exactly "is this a
  ;; socket" -- and Chez answers "regular file?" and "directory?" but not
  ;; this one.
  ;;
  ;; NEVER: THE OFFSET IS PER PLATFORM AND IS NOT DERIVABLE AT RUN TIME, the
  ;; same difficulty `rss-freebsd` states above. Each one is the field
  ;; order that platform's `sys/stat.h` declares, and the order is the
  ;; provenance -- an offset with no field list behind it is a number
  ;; nobody can check:
  ;;
  ;;   macOS (struct stat, __DARWIN_64_BIT_INO_T):
  ;;     dev_t st_dev (int32, 4) | mode_t st_mode (uint16, 2) | ...
  ;;     ⇒ st_mode at 4, 16 bits.
  ;;
  ;;   Linux x86-64 (glibc struct stat):
  ;;     __dev_t st_dev (8) | __ino_t st_ino (8) | __nlink_t st_nlink (8)
  ;;     | __mode_t st_mode (4) | ...
  ;;     ⇒ st_mode at 24, 32 bits.
  ;;
  ;;   FreeBSD 12 and later (struct stat, ino64):
  ;;     dev_t st_dev (8) | ino_t st_ino (8) | nlink_t st_nlink (8)
  ;;     | mode_t st_mode (uint16, 2) | ...
  ;;     ⇒ st_mode at 24, 16 bits.
  ;;
  ;; NOTE: `st_ino` AT 8 ON ALL THREE is what `path-device-inode` already
  ;; relies on, so two of these three field lists were load bearing
  ;; before this predicate existed.
  ;;
  ;; KEY: MEASURED ON TWO OF THE THREE, 2026-09-18, by statting a socket, a
  ;; fifo, a regular file and a directory and reading every candidate
  ;; offset:
  ;;
  ;;   macOS 25.3.0 arm64   offset 4, 16 bits:  C1ED 11A4 81A4 41ED  ok
  ;;                        offset 24:          reads 0
  ;;   FreeBSD 15.0-RELEASE offset 24, 16 bits: C1ED 11A4 81A4 41ED  ok
  ;;                        offset 4:           reads 0
  ;;
  ;; LINUX IS NOW MEASURED, and the field list above was right for x86_64
  ;; only: the probe's rows read st_mode at 24 (4 bytes) on x86_64 and at
  ;; 16 (4 bytes) on aarch64, whose glibc uses the generic layout. A rule
  ;; keyed on "linux" alone read st_uid on aarch64. The offset and width
  ;; now come from the platform's row, key (system, machine, libc).
  ;;
  ;; NOTE: AND ON FreeBSD A 32-BIT READ AT 24 ALSO MATCHED, because the two
  ;; bytes after the 16-bit field happen to be zero for these four kinds.
  ;; So that platform's WIDTH is not pinned by this measurement -- only
  ;; its offset is. A width wrong in the other direction would show on a
  ;; mode whose neighbouring bytes are not zero.
  ;;
  ;; NOTE: SO IT IS CHECKED BY A CELL RATHER THAN TRUSTED. `daemon-socket.sc`
  ;; makes a socket, a fifo, a regular file and a directory on one path in
  ;; turn and asks this predicate about each; an offset wrong on some
  ;; platform fails there, rather than showing up as a daemon unlinking
  ;; something it does not own.
  (define S_IFMT (platform-number 'S_IFMT))
  (define S_IFSOCK (platform-number 'S_IFSOCK))
  (define S_IFDIR (platform-number 'S_IFDIR))
  (define S_IFREG (platform-number 'S_IFREG))
  (define S_IFLNK (platform-number 'S_IFLNK))

  ;; st_mode out of a filled stat buffer; the offsets are the ones stated
  ;; above for each platform.
  ;; st_mode at the row's offset and width: 4 and 2 bytes on macOS, 24
  ;; and 4 on Linux x86_64, 16 and 4 on Linux aarch64, 24 and 2 on
  ;; FreeBSD. The aarch64 offset is the one a system-wide "linux" rule got
  ;; wrong: it read st_uid, and an existing directory was not one.
  (define (st-mode-of buf)
    (row-uint-ref buf 'stat 'st_mode))

  (define (st-mode path)
    (let ((buf (make-bytevector stat-buffer-size 0)))
      (let ((rc (c-stat path buf)))
        (and (>= rc 0) (st-mode-of buf)))))

  (define (file-is-socket? path)
    (guard (e (#t #f))
      (let ((mode (st-mode path)))
        (and mode (= S_IFSOCK (bitwise-and mode S_IFMT))))))

  (define (path-device-inode path)
    (unless (string? path)
      (assertion-violation 'path-device-inode "path must be a string" path))
    ;; stat IS NON-MUTATING (F100 D1): unreadable-entry with the errno,
    ;; absence included, and stat-fail reaches it (it called c-stat
    ;; directly, where the fault could not).
    (when (stat-fault? path) (unreadable! path (stat-fault-errno)))
    (let ((buf (make-bytevector stat-buffer-size 0)))
      (let ((rc (c-stat path buf)))
        (when (< rc 0) (unreadable! path (errno)))
        (values (row-uint-ref buf 'stat 'st_dev)
                (row-uint-ref buf 'stat 'st_ino)))))

  ;; Version hints invalidate derived indexes; they never prove a record.
  ;; ctime catches replacements and same-length edits even if mtime is restored.
  (define (path-version path)
    ;; THE TIMES ARE A STAT, NON-MUTATING (F100 D1): Chez's own condition
    ;; becomes unreadable-entry, its errno read from the condition class.
    (let-values (((device inode) (path-device-inode path)))
      (let-values (((modified changed)
                    (guard (e ((unreadable-entry? e) (raise e))
                              (#t (let ((code (and (condition? e) (condition-errno e))))
                                    (raise (make-unreadable-entry
                                             path
                                             (if code (c-strerror code) "the entry's times cannot be read")
                                             (and code (errno-reason code)))))))
                      (values (file-modification-time path) (file-change-time path)))))
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
        ((>= rc 0) (note! (list 'link from to)) (trace-event! 'link to #f) 'linked)
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
      ;; THE OPEN AND THE flock ARE NON-MUTATING (F100 D1): unreadable-entry.
      (let ((fd (c-open path O_RDONLY)))
        (when (< fd 0) (unreadable! path (errno)))
        (hashtable-set! fd-paths fd path)
        (unless no-flock?
          (guard (e (#t (close-quietly fd) (raise e)))
            (let* ((rc (c-flock fd (+ m LOCK_NB)))
                   (code (and (< rc 0) (errno))))
              ;; ONLY CONTENTION WAITS (F100 D1). A non-blocking attempt that
              ;; failed for another reason is unreadable-entry: waiting on it
              ;; hid an EIO behind a later success (F100a review r1).
              (when (and code (not (memv code (list EWOULDBLOCK EINTR))))
                (unreadable! path code))
              (when code
                (trace-event! 'lock-wait subject #f)
                (let retry ()
                  (let ((rc (c-flock fd m)))
                    (when (< rc 0)
                      (let ((code (errno)))
                        (if (= code EINTR)
                            (retry)
                            (unreadable! path code)))))))))
          (trace-event! 'flock subject #f))
        (make-lock-handle path mode fd #t))))

  ;; NEVER: THE ATTEMPT THAT NEVER WAITS. `lock-acquire!` blocks when the lock
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
    ;; CONTENTION IS AN ANSWER, #f; every other failure of the open or the
    ;; flock is unreadable-entry (F100 D1). An earlier version answered #f
    ;; for any flock failure, so a lock that could not be asked about read
    ;; as a lock somebody held.
    (let ((m (mode->int 'lock-try-acquire! mode)))
      (let ((fd (c-open path O_RDONLY)))
        (when (< fd 0) (unreadable! path (errno)))
        (hashtable-set! fd-paths fd path)
        (cond
          (no-flock? (make-lock-handle path mode fd #t))
          (else
           (let* ((rc (guard (e (#t (close-quietly fd) (raise e)))
                        (c-flock fd (+ m LOCK_NB))))
                  (code (and (< rc 0) (errno))))
             (cond
               ;; CONTENTION IS AN ANSWER, so its close is on the normal path
               ;; and is checked: a failure is unreadable-entry, never #f
               ;; (F100a review r2; the census's `close` section pins it).
               ((and code (eqv? code EWOULDBLOCK))
                (forget-fd! fd)
                (let ((rc (c-close fd)))
                  (when (< rc 0) (unreadable! path (errno))))
                (when (close-fault?) (unreadable! path EIO))
                #f)
               (code (close-quietly fd) (unreadable! path code))
               (else (trace-event! 'flock (cons path mode) #f)
                     (make-lock-handle path mode fd #t)))))))))

  ;; Idempotent, because the caller owning the release will sometimes
  ;; release on two paths out of the same region and must not have to
  ;; track which one ran.
  (define (lock-release! l)
    (unless (lock-handle? l)
      (assertion-violation 'lock-release! "not a lock" l))
    ;; NOTE: A FAILED UNLOCK OR CLOSE RAISES (F100 D1): the descriptor was
    ;; never written, so either is unreadable-entry naming the lock's path;
    ;; close-fail reaches the close. Both are attempted before either is
    ;; reported, so a failed unlock does not leak the descriptor. A caller in
    ;; an unwind wants it quiet: call-with-lock releases quietly on an
    ;; escape and checked on a normal exit.
    (when (lock-handle-held l)
      (lock-handle-held-set! l #f)
      (let* ((fd (lock-handle-fd l))
             (path (lock-handle-path l))
             (subject (cons path (lock-handle-mode-name l)))
             (unlock-rc (c-flock fd LOCK_UN))
             (unlock-code (and (< unlock-rc 0) (errno))))
        (forget-fd! fd)
        (let* ((close-rc (c-close fd))
               (close-code (and (< close-rc 0) (errno))))
          (unless unlock-code (trace-event! 'unlock subject #f))
          (cond (unlock-code (unreadable! path unlock-code))
                (close-code (unreadable! path close-code))
                ((close-fault?) (unreadable! path EIO))))))
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
  ;; NEVER: HOW A LOCK IS TAKEN IS THE PROGRAM'S DECISION, NOT THIS FILE'S.
  ;; The default is the blocking acquire, so every existing caller --
  ;; the CLI above all -- behaves exactly as before. A daemon sets this
  ;; once, for its whole lifetime, to an attempt that never waits: an
  ;; actor parked in `flock` runs on the scheduler's own thread and
  ;; stops every other process in the VM, including the one that would
  ;; have released the lock.
  ;;
  ;; NOTE: AND IT IS SET, NOT PARAMETERIZED AROUND A PROCESS. Measured:
  ;; Chez parameters are per OS THREAD, and green threads share one --
  ;; so a `parameterize` in one actor is visible to every actor that
  ;; runs during its extent, and gone again afterwards. Neither half is
  ;; what a per-process override would need. What keeps the CLI on the
  ;; default is that nothing in the CLI ever sets this.
  (define current-lock-acquire (make-parameter lock-acquire!))

  ;; NEVER: THE PAIR IS OVERRIDDEN TOGETHER OR NOT AT ALL. A strategy in
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
    ;; THE HEADER'S TWO OBLIGATIONS: on a normal exit the release is checked
    ;; and its failure raises; on an escape it is released quietly, so the
    ;; failure already on its way out is the one reported.
    (let ((l ((current-lock-acquire) path mode-name))
          (released (box #f)))
      (dynamic-wind
        void
        (lambda ()
          (call-with-values
            (lambda () (proc (lock-fd l)))
            (lambda vs
              (set-box! released #t)
              ((current-lock-release) l)
              (apply values vs))))
        (lambda ()
          (unless (unbox released)
            (set-box! released #t)
            (guard (e (#t (void))) ((current-lock-release) l)))))))

  (define (with-exclusive-lock path proc)
    (call-with-lock 'with-exclusive-lock path 'exclusive proc))

  (define (with-shared-lock path proc)
    (call-with-lock 'with-shared-lock path 'shared proc)))
