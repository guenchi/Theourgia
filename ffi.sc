#!chezscheme
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
;;;                       EINTR 4  F_FULLFSYNC 51  sizeof(off_t) 8
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
;;; ftruncate, write, fsync. open and close emit nothing. fsync emits
;;; ONE line even on macOS, where it is two syscalls -- a
;;; platform-conditional second event would make the same run read
;;; differently on the two platforms this ships to.
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
;;;   fsync-fail:<substr>       fsync on a matching path reports EIO
;;;   no-log-fsync:<substr>     fsync on a matching path is SKIPPED and
;;;                             reports success
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
;;; ARMED AT RUN TIME, AND IT SAYS SO. The switch is read once when the
;;; library is initialised, so an unset variable costs one boolean test
;;; per syscall and nothing else -- but it also means a released build
;;; can still be told to skip a flush by its environment. That is a real
;;; hazard for a durability library and the reason (igropyr inject) gates
;;; itself at EXPANSION time instead, leaving no injection code in a
;;; compiled object at all. Until this follows suit, an armed process
;;; announces itself on stderr at startup, so the state is at least never
;;; silent.

(library (theourgia ffi)
  (export fd-open fd-close
          with-exclusive-lock with-shared-lock
          ftruncate! fsync! fsync-dir! write-all!
          fd-seek! fd-size file-size file-ensure! fd-path
          fs-error? fs-error-op fs-error-target fs-error-errno
          theourgia-fault theourgia-fault-armed?
          theourgia-trace? trace-event!)
  (import (chezscheme)
          (theourgia trace)
          (only (igropyr platform)
                platform-os ensure-supported-platform!
                load-first-shared-object!))

  (define platform-checked (begin (ensure-supported-platform!) #t))

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

  (define fault-spec (getenv "THEOURGIA_FAULT"))

  (define (split-at-colon s)
    (let ((n (string-length s)))
      (let loop ((i 0))
        (cond
          ((= i n) (values (string->symbol s) #f))
          ((char=? (string-ref s i) #\:)
           (values (string->symbol (substring s 0 i))
                   (substring s (+ i 1) n)))
          (else (loop (+ i 1)))))))

  (define-values (fault-name fault-arg)
    (if fault-spec (split-at-colon fault-spec) (values #f #f)))

  (define fault-armed? (and fault-name #t))

  ;; A PATH-SCOPED FAULT WITH NO PATH IS REFUSED AT STARTUP. Both of
  ;; these exist to hit one file and not the other -- the log's flush and
  ;; the registry's -- so a missing substring would make the injection
  ;; either inert or global, and both of those read in a test log exactly
  ;; like the fault having worked. An empty substring is the same
  ;; mistake: it matches every path.
  (define fault-argument-checked
    (when (memq fault-name '(fsync-fail no-log-fsync))
      (unless (and (string? fault-arg) (> (string-length fault-arg) 0))
        (assertion-violation 'theourgia-ffi
                             "this fault needs a non-empty path substring"
                             fault-spec))))

  (define (theourgia-fault) fault-name)
  (define (theourgia-fault-armed?) fault-armed?)

  ;; ANNOUNCED, NOT ASSUMED ABSENT. See the note at the top: this switch
  ;; survives into a release, so a process running with it set says so
  ;; before it does anything. One line, on stderr, whether or not the
  ;; trace is on.
  (define fault-announced
    (when fault-armed?
      (guard (e (#t (void)))
        (let ((p (current-error-port)))
          (fprintf p "(theourgia fault-injection-armed ~a)\n" fault-spec)
          (flush-output-port p)))))

  ;; One box for all of them, because exactly one fault is ever armed:
  ;; fresh -> done for the two that fire once, fresh -> partial -> EIO
  ;; for the one that fires twice.
  (define fault-state (box 'fresh))

  (define (string-contains? s sub)
    (let ((n (string-length s)) (m (string-length sub)))
      (let loop ((i 0))
        (cond
          ((> (+ i m) n) #f)
          ((string=? (substring s i (+ i m)) sub) #t)
          (else (loop (+ i 1)))))))

  ;; The path a fault is aimed at, taken from the subject when it is one
  ;; and from the registry otherwise. A fault with no argument matches
  ;; nothing here, so fsync-fail without a substring is inert rather than
  ;; global -- an injection that hit every flush at once could not
  ;; distinguish the log's from the registry's, which is the whole reason
  ;; these two take an argument.
  (define (fault-path-match? fd subject)
    (let ((p (cond ((string? subject) subject)
                   ((and (pair? subject) (string? (car subject))) (car subject))
                   (else (fd-path fd)))))
      (and (string? p) (string? fault-arg) (string-contains? p fault-arg))))

  (define (fsync-skipped? fd subject)
    (and fault-armed? (eq? fault-name 'no-log-fsync)
         (fault-path-match? fd subject)))

  (define (fsync-forced-failure? fd subject)
    (and fault-armed? (eq? fault-name 'fsync-fail)
         (fault-path-match? fd subject)))

  ;; ---- trace ------------------------------------------------------------

  ;; The switch and the printer live in (theourgia trace) so that the
  ;; record codec can report events without importing libc; they are
  ;; re-exported here because a caller of this library should not have
  ;; to know that.

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
          (open-file-output-port path (file-options no-fail no-truncate))))
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
  (define (fsync! fd . opts)
    (let ((subject (subject-of fd opts)))
      (cond
        ;; Skipped AND unreported: see the note at the top on why this
        ;; one must not print a line. Both flush faults are behind ONE
        ;; test of the switch, so an unarmed process pays for the feature
        ;; exactly once per flush.
        ((and fault-armed?
              (cond ((fsync-skipped? fd subject) (void) #t)
                    ((fsync-forced-failure? fd subject)
                     (raise (fs-err 'fsync subject EIO)))
                    (else #f)))
         (void))
        (else
         (let ((rc (c-fsync fd)))
           (when (< rc 0) (fail! 'fsync subject)))
         (when macos?
           (let ((rc (c-fcntl fd F_FULLFSYNC 0)))
             (when (< rc 0) (fail! 'fullfsync subject))))
         (trace-event! 'fsync subject #f)
         (void)))))

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
            (lambda ()
              (let ((rc (c-fsync fd)))
                (when (< rc 0) (fail! 'dir-fsync subject)))
              (when macos?
                (let ((rc (c-fcntl fd F_FULLFSYNC 0)))
                  (when (< rc 0) (fail! 'dir-fullfsync subject))))
              (trace-event! 'fsync subject #f))
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
    (let ((fd (fd-open path '(read))))
      (let ((done (box #f)))
        (dynamic-wind
          void
          (lambda () (fd-size fd path))
          (lambda ()
            (unless (unbox done)
              (set-box! done #t)
              (close-quietly fd)))))))

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
  ;; the count accepted and #f, or -1 and the errno to report.
  (define (real-write fd bv count)
    (let ((r (c-write fd bv count)))
      (if (< r 0) (values -1 (errno)) (values r #f))))

  (define short-count 7)

  (define (write-once fd bv count)
    (if (not fault-armed?)
        (real-write fd bv count)
        (case fault-name
          ;; Skipped, not performed-then-relabelled: EINTR means nothing
          ;; was written, and it has to actually be true.
          ((eintr-once)
           (if (eq? (unbox fault-state) 'fresh)
               (begin (set-box! fault-state 'done) (values -1 EINTR))
               (real-write fd bv count)))
          ;; A genuinely short write: the kernel is offered fewer bytes,
          ;; so the file really does hold only those.
          ;; THE STATE MOVES ONLY IF THE WRITE ACTUALLY HAPPENED. Marking
          ;; the fault spent before the call would let a real EINTR from
          ;; the kernel consume it, and the run would then look like a
          ;; short write that never occurred -- and for the EIO fault,
          ;; "after partial" would be a lie: the retry would be refused
          ;; with nothing on disk.
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
          (else (real-write fd bv count)))))

  (define (write-all! fd bv . opts)
    (unless (bytevector? bv)
      (assertion-violation 'write-all! "not a bytevector" bv))
    (let ((subject (subject-of fd opts))
          (total (bytevector-length bv)))
      (let loop ((pos 0) (chunk bv))
        (if (>= pos total)
            total
            (let-values (((n code) (write-once fd chunk (- total pos))))
              (cond
                ((and (< n 0) (= code EINTR))
                 (loop pos chunk))
                ((< n 0)
                 (raise (fs-err 'write subject code)))
                ((= n 0)
                 (raise (fs-err 'write subject #f)))
                (else
                 (trace-event! 'write subject n)
                 (let ((pos (+ pos n)))
                   (if (>= pos total)
                       (loop pos chunk)
                       (let ((tail (make-bytevector (- total pos))))
                         (bytevector-copy! bv pos tail 0 (- total pos))
                         (loop pos tail)))))))))))

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
  (define (call-with-lock who path mode mode-name proc)
    (unless (procedure? proc)
      (assertion-violation who "not a procedure" proc))
    ;; Which lock was taken is not recoverable from the outcome -- a
    ;; shared and an exclusive acquisition of a free lock look identical
    ;; afterwards -- so the mode travels with the path.
    (let ((subject (cons path mode-name)))
      (file-ensure! path)
      (let ((fd (c-open path O_RDONLY)))
        (when (< fd 0) (fail! 'open path))
        (hashtable-set! fd-paths fd path)
        (let ((held (box #f)) (closed (box #f)))
          (dynamic-wind
            ;; A LOCKED REGION IS NOT RE-ENTRANT, AND THAT IS A RULE FOR
            ;; CALLERS RATHER THAN A CHECK HERE. dynamic-wind will run
            ;; this thunk again if proc captured a continuation, escaped
            ;; -- releasing the lock and closing the descriptor on the
            ;; way out -- and was then re-invoked; the body would resume
            ;; holding nothing, against a descriptor number the kernel
            ;; may since have handed to another file.
            ;;
            ;; A GUARD HERE WAS WRITTEN AND THEN TAKEN OUT, because it
            ;; fires on ordinary exception propagation. R6RS `guard`
            ;; re-raises in the dynamic environment of the original
            ;; raise, so a guard clause that DECLINES re-enters every
            ;; dynamic-wind it unwound: measured as (in out in out) on a
            ;; bare dynamic-wind, and with the check installed an
            ;; ordinary #(durable-error write ...) raised inside proc
            ;; reached the outer handler as an assertion violation
            ;; instead. Replacing a real failure with a false one is a
            ;; worse defect than the re-entry it was guarding, and no
            ;; test here can tell the two re-entries apart.
            void
            (lambda ()
              (let ((rc (c-flock fd (+ mode LOCK_NB))))
                (when (< rc 0)
                  (trace-event! 'lock-wait subject #f)
                  (let retry ()
                    (let ((rc (c-flock fd mode)))
                      (when (< rc 0)
                        (if (= (errno) EINTR)
                            (retry)
                            (fail! 'flock path)))))))
              (set-box! held #t)
              (trace-event! 'flock subject #f)
              (proc fd))
            (lambda ()
              (unless (unbox closed)
                (set-box! closed #t)
                ;; UNLOCK, CLOSE, THEN REPORT. Reporting last means an
                ;; escape from inside trace-event! cannot leave the
                ;; descriptor open, and the line is only printed if
                ;; LOCK_UN actually returned success -- a trace that
                ;; announced an unlock which failed would be forging the
                ;; evidence a test reads. The close releases the lock in
                ;; either case, which is why a failed LOCK_UN is not
                ;; escalated here.
                (let ((released (and (unbox held) (>= (c-flock fd LOCK_UN) 0))))
                  (close-quietly fd)
                  (when released (trace-event! 'unlock subject #f))))))))))

  (define (with-exclusive-lock path proc)
    (call-with-lock 'with-exclusive-lock path LOCK_EX 'exclusive proc))

  (define (with-shared-lock path proc)
    (call-with-lock 'with-shared-lock path LOCK_SH 'shared proc)))
