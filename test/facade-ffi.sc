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

;; THE PROCESS CALLS, ASSERTED BY THEIR EFFECTS.
;;
;; NEVER: "THE CALL RETURNED" IS NOT A READING. Swapping two valid resource
;; limits raises nothing; naming the wrong field of a kernel struct
;; returns a plausible number; signalling the wrong pid succeeds. Every
;; row here asks for something the call is supposed to have CHANGED, or
;; for a value whose shape only the right answer has.

(import (chezscheme) (theourgia ffi))

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
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

;; ---- kill(pid, 0): alive, absent, and the difference that matters -------

(want "FF-01 this process is alive to signal 0"
      (process-alive-signal0? (get-process-id)) #t)

;; NOTE: A PID THAT CANNOT EXIST. Not a large number that might: pid 1 is
;; init and answers EPERM, which is the case the predicate must call
;; ALIVE. This one is chosen to be absent, and the row below reads the
;; errno rather than the verdict, so "absent" is distinguished from
;; "present and not ours".
(define absent-pid 4194303)

(want "FF-01 an absent pid answers ESRCH, not merely false"
      (signal-pid! absent-pid 0) 3)
(want "FF-01 and the predicate calls it dead"
      (process-alive-signal0? absent-pid) #f)

;; KEY: EPERM IS ALIVE, AND THIS IS THE ROW THAT SAYS SO. pid 1 exists and
;; is not ours; a predicate that read every failure as absence would
;; answer #f here and would report a running child as dead the moment
;; the child changed user.
(want "FF-01 pid 1 exists and is not ours: EPERM"
      (let ((r (signal-pid! 1 0))) (if (= r 1) 'eperm r)) 'eperm)
(want "FF-01 TWIN: and the predicate still calls it alive"
      (process-alive-signal0? 1) #t)

;; ---- setrlimit: the two fields are not interchangeable ------------------
;;
;; KEY: TWO DISTINCT VALUES, read back separately. Written with one value
;; for both, an implementation that filled the struct backwards -- or
;; that wrote the same field twice -- would pass.

(define before (getrlimit RLIMIT_CPU))
(want "FF-02 the CPU limit reads back as a pair" (pair? before) #t)
(want "FF-02 setting distinct soft and hard limits succeeds"
      (setrlimit! RLIMIT_CPU 3600 7200) 0)
(want "FF-02 and both fields come back, in that order"
      (getrlimit RLIMIT_CPU) '(3600 . 7200))

;; NEVER: AND IT IS A ONE-WAY DOOR, which is why this runs last of the pair:
;; a hard limit cannot be raised again by an unprivileged process, so a
;; fixture that lowered it early would break every later row that wanted
;; a bigger one. Named here because the next person will want to move it.
(want "FF-02 TWIN: raising the hard limit again is refused"
      (> (setrlimit! RLIMIT_CPU 3600 9000) 0) #t)

;; ---- resident size: bytes, and the right field --------------------------

(define rss (process-rss-bytes (get-process-id)))

(want "FF-03 this process has a resident size" (and (number? rss) (> rss 0)) #t)

;; KEY: BYTES, NOT PAGES AND NOT KiB. A Chez process is tens of megabytes;
;; the same number in KiB would be tens of thousands, and in pages tens
;; of thousands too. The window below is wide enough for any machine and
;; still excludes both wrong units by three orders of magnitude.
(want "FF-03 and it is in BYTES"
      (list (> rss 4000000) (< rss 4000000000)) '(#t #t))

;; KEY: THE FIELD BESIDE IT IS THE ONE THAT PROVES THE OFFSET. A reader
;; that took the first field of the kernel's struct would report the
;; VIRTUAL size, which on this machine is four hundred gigabytes -- a
;; number that is positive, that grows with allocation, and that would
;; let a child run a thousand times past its memory limit. Measured
;; here: virtual 445,774,053,376 against resident 22,495,232.
(want "FF-03 TWIN: it is not the virtual size"
      (< rss 100000000000) #t)

;; SCRATCH PATHS LIVE UNDER THE RUNNER'S TWO ROOTS (F71): files and
;; directories under THEOURGIA_TEST_ROOT, socket paths under
;; THEOURGIA_TEST_SOCK, which is short enough for one. Run alone, without
;; them, a path falls back to /tmp as it always did.
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

;; ---- the same reading, on the platform that is not this one ------------
;;
;; NOTE: THIS ROW ONLY RUNS ON FreeBSD, and it says so rather than passing
;; quietly elsewhere. Both production machines are FreeBSD 15; the
;; reading there comes out of `kinfo_proc` at an offset that was
;; MEASURED on that machine (see `ffi.sc`), not read out of a header --
;; and an offset is a claim about a layout, so it is worth a row that
;; fails if the layout moves.
;;
;; NEVER: WHAT IT IS NOT: a row that is green here. On this machine it
;; announces that it did not run. A cell that reported success for a
;; platform it never touched would be the most comfortable kind of lie.
;; ---- FF-05 setsid, called for real ---------------------------------
;;
;; NEVER: NOTHING IN THIS FILE CALLED IT. Every `signal-pid!` here used signal
;; 0, and `setsid!` appeared in no row at all -- so an implementation that
;; returned a plausible number without asking the kernel for anything had
;; nothing to fail. It matters because the eval guardian kills a whole
;; process group: if the child never got its own session, `kill(-pgid)`
;; reaches the wrong processes, or none.
;;
;; NOTE: IN A CHILD, BECAUSE A PROCESS THAT LEADS A GROUP CANNOT. `setsid`
;; fails with EPERM when the caller is already a process-group leader,
;; and this fixture may or may not be one depending on how it was
;; started. A fresh child is never one.
(define setsid-out (string-append scratch-base "/ffi-setsid-" (number->string (get-process-id))))
(define setsid-src (string-append setsid-out ".sc"))
(call-with-output-file setsid-src
  (lambda (port)
    (display "(import (chezscheme) (theourgia ffi))" port) (newline port)
    (display "(let* ((pid (get-process-id))" port) (newline port)
    (display "       (sid-before (session-id 0))" port) (newline port)
    (display "       (sid (setsid!))" port) (newline port)
    (display "       (sid-after (session-id 0)))" port) (newline port)
    (display "  (display (list pid sid-before sid sid-after)))" port) (newline port))
  'replace)
(system (string-append "scheme --script " setsid-src " > " setsid-out " 2>&1"))
(define setsid-reading
  (guard (e (#t #f))
    (call-with-input-file setsid-out read)))

(want "FF-05 setsid in a child answers a session id, and it is that child's pid"
      (and (pair? setsid-reading)
           (let ((pid (car setsid-reading)) (sid (caddr setsid-reading)))
             (list (and (number? sid) (> sid 0)) (= pid sid))))
      '(#t #t))

;; NEVER: AND THE ROW ABOVE, ALONE, IS SATISFIED BY A LIE. `setsid!` answers
;; the new session id, which is the caller's own pid -- so
;; `(define (setsid!) (get-process-id))` passes it, having created no
;; session at all. The two readings below come from the kernel, through
;; `getsid`, and they are of a DIFFERENT quantity than the return value:
;;
;;   * before the call the child is in the session it inherited, which is
;;     not its own pid -- it is the shell's, or this fixture's;
;;   * after the call it is in its own.
;;
;; A `setsid!` that does nothing leaves the two readings equal, and the
;; inherited one is what it stays at.
(want "FF-05 TWIN: and the kernel agrees that the session CHANGED"
      (and (pair? setsid-reading)
           (let ((pid (car setsid-reading))
                 (before (cadr setsid-reading))
                 (after (cadddr setsid-reading)))
             (list (and (number? before) (not (= before pid)))
                   (equal? after pid)
                   (not (equal? before after)))))
      '(#t #t #t))

(define here (symbol->string (machine-type)))
(define (freebsd?)
  (let loop ((i 0))
    (cond ((> (+ i 2) (string-length here)) #f)
          ((string=? "fb" (substring here i (+ i 2))) #t)
          (else (loop (+ i 1))))))

(if (freebsd?)
    (let* ((mine (get-process-id))
           (rss-here (process-rss-bytes mine)))
      (want "FF-04 FreeBSD: this process has a resident size in bytes"
            (and (number? rss-here) (> rss-here 4000000) (< rss-here 4000000000)) #t)
      ;; KEY: AGAINST `ps`, TAKEN NOW. The offset was found by agreeing with
      ;; ps on two processes; the row that keeps it honest is the same
      ;; agreement, on this one, at this moment.
      (let* ((out (string-append scratch-base "/ffi-ps-" (number->string mine)))
             (_ (system (string-append "ps -o rss= -p " (number->string mine) " > " out)))
             (ps-kib (guard (e (#t #f))
                       (string->number
                         (let loop ((cs (string->list (call-with-input-file out get-line))) (o '()))
                           (cond ((null? cs) (list->string (reverse o)))
                                 ((char=? (car cs) #\space) (loop (cdr cs) o))
                                 (else (loop (cdr cs) (cons (car cs) o)))))))))
        (want "FF-04 FreeBSD: and it agrees with ps within 20%"
              (and ps-kib
                   (let ((ps-bytes (* ps-kib 1024)))
                     (and (> rss-here (* 4/5 ps-bytes)) (< rss-here (* 6/5 ps-bytes)))))
              #t)))
    (printf "SKIP FF-04 the FreeBSD resident-size reading: this machine is ~a.
~a
"
            here
            "     It is measured where it matters (both production machines are FreeBSD 15);
     the offset 264/pages came from a probe run there, and ffi.sc refuses to
     answer at all if the struct size is not the 1088 that probe saw."))

;; F100-A rows for test/facade-ffi.sc (main session's cells, rewritten by the
;; code session to the amendments of 2026-09-25 in briefs/f100a-task.md).
;; Inserted before the fixture's closing `rows:` line; it uses the fixture's
;; `want`, `caught`, `rows`, `bad` and `scratch-base`. Names introduced by
;; F100a: size-entry, overwrite-entry!, with-mutation-record, mutation-record,
;; and the exported errno constants EIO ENOENT EACCES ENOTDIR.
;;
;; THE SCRATCH DIRECTORY AND THE FAULT SUBSTRINGS ARE CHOSEN TOGETHER. A
;; fault's path qualifier is matched against the whole path, so every
;; qualifier below names a leaf ("f100a-..." ) that no enclosing directory
;; contains (ffi.sc's header, on the substring trap).

(define f100a-dir (string-append scratch-base "/ffi-f100a-" (number->string (get-process-id))))
(mkdir-p! f100a-dir)
(define (under name) (string-append f100a-dir "/" name))
(define (touch! path text) (overwrite-entry! path (string->utf8 text)))
(define (chmod! path mode) (system (string-append "chmod " mode " " path)))
(define (bytes-of path) (utf8->string (entry-bytes path)))
(define (class-of thunk)
  ;; THE CLASS AND THE ERRNO, NOT THE MESSAGE: the row asks which condition
  ;; type and which errno, which is what D1 promises.
  (guard (e ((unreadable-entry? e) (list 'unreadable (unreadable-entry-errno e)))
            ((fs-error? e) (list 'durable (fs-error-errno e)))
            (#t (list 'other e)))
    (thunk) 'returned))
(define (recorded thunk)
  ;; THE RECORD AS THE ROW SEES IT: the entries noted inside one scope,
  ;; whatever the thunk raised -- except make-one!'s failure marker, which
  ;; is the row's own tree assertion and goes on to `want`.
  (with-mutation-record
    (lambda ()
      (guard (e ((make-one!-failed? e) (raise e))
                (#t #f))
        (thunk))
      (mutation-record))))
(define (make-one!-failed? e)
  (and (pair? e) (eq? (car e) 'make-one!-failed)))

;; ---- non-mutating primitives: unreadable-entry, errno kept ---------------

(touch! (under "secret") "x\n")
(chmod! (under "secret") "000")
(want "F100-A fd-open of a 000 file is unreadable-entry EACCES"
      (class-of (lambda () (fd-open (under "secret") '(read)))) (list 'unreadable 'EACCES))
(mkdir-p! (under "closed"))
(touch! (under "closed/inner") "y\n")
(chmod! (under "closed") "000")
(want "F100-A fd-open under a 000 directory is unreadable-entry EACCES"
      (class-of (lambda () (fd-open (under "closed/inner") '(read)))) (list 'unreadable 'EACCES))
(want "F100-A fd-open of an absent path is unreadable-entry ENOENT"
      (class-of (lambda () (fd-open (under "nothing") '(read)))) (list 'unreadable 'ENOENT))
(want "F100-A path-device-inode of an absent path is unreadable-entry ENOENT"
      (class-of (lambda () (path-device-inode (under "nothing")))) (list 'unreadable 'ENOENT))
(want "F100-A entry-type answers absent, never raises, for ENOENT"
      (entry-type (under "nothing")) 'absent)
(want "F100-A size-entry answers absent for ENOENT and the size otherwise"
      (list (size-entry (under "nothing")) (size-entry (under "secret"))) (list 'absent 2))
(touch! (under "lock") "")
(chmod! (under "lock") "000")
(want "F100-A lock-acquire! on a 000 lock file is unreadable-entry EACCES"
      (class-of (lambda () (lock-acquire! (under "lock") 'exclusive))) (list 'unreadable 'EACCES))
(touch! (under "held") "")
(define holder (lock-acquire! (under "held") 'exclusive))
(want "F100-A lock-try-acquire! contention answers #f, no condition"
      (lock-try-acquire! (under "held") 'exclusive) #f)
(lock-release! holder)

;; ---- mutating primitives: durable-error, record noted per the rule -------

(want "F100-A fd-open with create under an absent parent: durable-error ENOENT, record empty, nothing created"
      (list (class-of (lambda () (fd-open (under "gone/child") '(write create))))
            (recorded (lambda () (fd-open (under "gone/child") '(write create))))
            (entry-type (under "gone")))
      (list (list 'durable ENOENT) '() 'absent))
;; THE TREE BESIDE THE RECORD (ruling H2, RR3; review r3): a 000 parent
;; hides its children from entry-type too, so the tree is read with the
;; parent opened for that one question and closed again.
(define (entry-type-inside dir path)
  (chmod! dir "700")
  (let ((t (entry-type path)))
    (chmod! dir "000")
    t))
(want "F100-A fd-open with create under a 000 parent: the child's entry-type raises first, record empty, nothing created"
      (list (class-of (lambda () (fd-open (under "closed/new") '(write create))))
            (recorded (lambda () (fd-open (under "closed/new") '(write create))))
            (entry-type-inside (under "closed") (under "closed/new")))
      (list (list 'unreadable 'EACCES) '() 'absent))
(want "F100-A mkdir-p! under a 000 parent: unreadable-entry EACCES from its directory check, record empty, nothing created"
      (list (class-of (lambda () (mkdir-p! (under "closed/d"))))
            (recorded (lambda () (mkdir-p! (under "closed/d"))))
            (entry-type-inside (under "closed") (under "closed/d")))
      (list (list 'unreadable 'EACCES) '() 'absent))
(touch! (under "movable") "m\n")
(want "F100-A rename-over! into a 000 directory: durable-error EACCES, record empty, the source still there"
      (list (class-of (lambda () (rename-over! (under "movable") (under "closed/moved"))))
            (recorded (lambda () (rename-over! (under "movable") (under "closed/moved"))))
            (bytes-of (under "movable"))
            (entry-type-inside (under "closed") (under "closed/moved")))
      (list (list 'durable EACCES) '() "m\n" 'absent))
(want "F100-A link! into a 000 directory: durable-error EACCES, record empty, the source still there"
      (list (class-of (lambda () (link! (under "movable") (under "closed/linked"))))
            (recorded (lambda () (link! (under "movable") (under "closed/linked"))))
            (bytes-of (under "movable"))
            (entry-type-inside (under "closed") (under "closed/linked")))
      (list (list 'durable EACCES) '() "m\n" 'absent))
(want "F100-A file-create-exclusive! on an existing name answers #f and leaves the file as it was"
      (list (file-create-exclusive! (under "movable")) (bytes-of (under "movable")))
      (list #f "m\n"))
;; FFI PRIMITIVES ONLY: atomic-write! is (theourgia log)'s and its
;; temporary's entries are a log-level row, deferred to F100b (amendment 4).
;; fsync and close note nothing, so the four entries are the whole record.
(want "F100-A a successful mkdir, create, write and rename are noted in order, and the tree is what they made"
      (list (recorded (lambda ()
                        (mkdir-p! (under "made"))
                        (let ((fd (fd-open (under "made/f") '(write create))))
                          (write-all! fd (string->utf8 "z\n"))
                          (fsync! fd (under "made/f") 'publish)
                          (fd-close fd))
                        (rename-over! (under "made/f") (under "made/g"))))
            (entry-type (under "made")) (entry-type (under "made/f")) (bytes-of (under "made/g")))
      (list (list (list 'mkdir (under "made"))
                  (list 'create (under "made/f"))
                  (list 'write (under "made/f"))
                  (list 'rename (under "made/f") (under "made/g")))
            'directory 'absent "z\n"))

;; THE TREE BY CONSTRUCTION (rulings J and K, RR3): make-one! reads back
;; what it wrote, and ANY failure -- the open, the write, the close, the
;; read-back, or bytes that differ -- raises its one marker,
;; (make-one!-failed path reason). `recorded` passes that marker on, so a
;; row that writes through make-one! asserts the tree through the marker,
;; without a projection of its own. A row that needs a write without the
;; read-back says so; none does today.
(define (make-one! path)
  (let ((got (guard (e (#t (list 'raised (if (and (condition? e) (message-condition? e))
                                             (condition-message e)
                                             e))))
               (let ((fd (fd-open path '(write create))))
                 (write-all! fd (string->utf8 "s\n"))
                 (fd-close fd))
               (bytes-of path))))
    (unless (equal? got "s\n")
      (raise (list 'make-one!-failed path got)))))
;; THE MARKER REACHES THE ROW (ruling K; review r5 found `recorded`
;; swallowing every make-one! failure but the read-back's): a make-one!
;; that cannot open its file, inside `recorded`, is the marker at `want`.
(want "F100a a make-one! that fails before its read-back reaches the row through recorded"
      (let ((r (caught (recorded (lambda () (make-one! (under "closed/made-one")))))))
        (and (pair? r) (eq? (car r) 'RAISED) (pair? (cadr r)) (car (cadr r))))
      'make-one!-failed)

;; OUTSIDE A SCHEDULER THE KEY IS ONE OBJECT FOR THE PROCESS (ffi's header):
;; two scopes in sequence share it, and each scope's record still holds
;; only what happened inside it, because a scope starts empty and restores
;; the one before it.
(want "F100-A outside a scheduler, two scopes in sequence each record only their own entries, and none is left after"
      (list (recorded (lambda () (make-one! (under "seq-1"))))
            (recorded (lambda () (make-one! (under "seq-2"))))
            (mutation-record))
      (list (list (list 'create (under "seq-1")) (list 'write (under "seq-1")))
            (list (list 'create (under "seq-2")) (list 'write (under "seq-2")))
            '()))

;; ---- fault-armed rows, one child process each -----------------------------
;;
;; INJECTION EXISTS ONLY IN A BUILD EXPANDED WITH THEOURGIA_INJECT=on, and a
;; fault fires only inside the stage its spec names, so the child sets both
;; (amendment 1, as test/stagefault.sc and test/unreadable-writer.sc do).
;; STDERR APART: an armed expansion announces itself on stderr, and `read`
;; would take the banner for the answer. The child writes one datum on stdout.
(define armed-n 0)
(define (armed fault expr)
  (set! armed-n (+ armed-n 1))
  (let* ((tag (number->string armed-n))
         (script (under (string-append "armed-" tag ".ss")))
         (out (under (string-append "armed-" tag ".out")))
         (err (under (string-append "armed-" tag ".err"))))
    (call-with-port (open-file-output-port script (file-options no-fail) (buffer-mode block) (native-transcoder))
      (lambda (p)
        (for-each (lambda (f) (write f p) (newline p))
          (list '(import (chezscheme) (theourgia ffi))
                '(define (class-of thunk)
                   (guard (e ((unreadable-entry? e) (list 'unreadable (unreadable-entry-errno e)))
                             ((fs-error? e) (list 'durable (fs-error-errno e)))
                             (#t (list 'other)))
                     (thunk) 'returned))
                '(define (recorded thunk)
                   (with-mutation-record (lambda () (guard (e (#t #f)) (thunk)) (mutation-record))))
                '(define (class-and-record thunk)
                   (with-mutation-record (lambda () (let ((c (class-of thunk))) (list c (mutation-record))))))
                `(write (parameterize ((theourgia-stage 'publish)) ,expr))
                '(newline)))))
    (system (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT='" fault "' scheme --script "
                           script " > " out " 2> " err))
    (guard (e (#t (list 'no-answer (call-with-port (open-input-file err) get-string-all))))
      (let ((d (call-with-port (open-input-file out) read)))
        (if (eof-object? d)
            (list 'no-answer (call-with-port (open-input-file err) get-string-all))
            d)))))

(touch! (under "f100a-lseek-target") "abcdef\n")
(want "F100-A fd-size with lseek-fail is unreadable-entry naming the descriptor's path, EIO"
      (armed "lseek-fail@publish:f100a-lseek-target"
             `(let ((fd (fd-open ,(under "f100a-lseek-target") '(read))))
                (guard (e ((unreadable-entry? e)
                           (list 'unreadable (unreadable-entry-path e) (unreadable-entry-errno e))))
                  (fd-size fd) 'returned)))
      (list 'unreadable (under "f100a-lseek-target") 'EIO))
(touch! (under "f100a-stat-target") "s\n")
(want "F100-A path-device-inode with stat-fail is unreadable-entry EIO"
      (armed "stat-fail@publish:f100a-stat-target"
             `(class-of (lambda () (path-device-inode ,(under "f100a-stat-target")))))
      (list 'unreadable 'EIO))
(want "F100-A mkdir-p! a/b with b refused by mkdir-fail: durable-error EIO, record ((mkdir a))"
      (armed "mkdir-fail@publish:f100a-mkdir-leaf"
             `(list (class-and-record (lambda () (mkdir-p! ,(under "f100a-mk/f100a-mkdir-leaf"))))
                    (entry-type ,(under "f100a-mk")) (entry-type ,(under "f100a-mk/f100a-mkdir-leaf"))))
      (list (list (list 'durable EIO) (list (list 'mkdir (under "f100a-mk")))) 'directory 'absent))
;; ONE CALL PER ROW, CLASS AND RECORD TOGETHER. A one-shot fault is spent by
;; the first qualifying operation in its stage, so a row that called the
;; operation once for the class and again for the record would read the
;; record of an unfaulted second call.
;; THE DESCRIPTOR IS OPENED OUTSIDE THE SCOPE (amendment 5), so the record
;; holds only what the write did: nothing for write-eio-first, which skips
;; the syscall, and the write for write-eio-after-partial.
(want "F100-A write-all! with write-eio-first: durable-error EIO, record empty"
      (armed "write-eio-first@publish:f100a-w-first"
             `(let ((fd (fd-open ,(under "f100a-w-first") '(write create))))
                (list (class-and-record (lambda () (write-all! fd (string->utf8 "abc"))))
                      (utf8->string (entry-bytes ,(under "f100a-w-first"))))))
      (list (list (list 'durable EIO) '()) ""))
(want "F100-A write-all! with write-eio-after-partial: durable-error EIO, record ((write p))"
      (armed "write-eio-after-partial@publish:f100a-w-partial"
             `(let ((fd (fd-open ,(under "f100a-w-partial") '(write create))))
                (list (class-and-record (lambda () (write-all! fd (string->utf8 "abcdefghijklmnop"))))
                      (utf8->string (entry-bytes ,(under "f100a-w-partial"))))))
      (list (list (list 'durable EIO) (list (list 'write (under "f100a-w-partial")))) "abcdefg"))
;; fsync changes no entry: the bytes are the ones written before it, read
;; in the parent after the child has gone (RR3).
(touch! (under "f100a-fsync-target") "s\n")
(want "F100-A fsync-fail is durable-error EIO, notes nothing, and leaves the bytes as they were"
      (list (armed "fsync-fail@publish:file=f100a-fsync-target"
                   `(let ((fd (fd-open ,(under "f100a-fsync-target") '(write create))))
                      (class-and-record (lambda () (fsync! fd ,(under "f100a-fsync-target") 'publish)))))
            (bytes-of (under "f100a-fsync-target")))
      (list (list (list 'durable EIO) '()) "s\n"))
(touch! (under "f100a-close-ro") "r\n")
(want "F100-A fd-close of a read-only descriptor with close-fail is unreadable-entry EIO"
      (armed "close-fail@publish:f100a-close-ro"
             `(let ((fd (fd-open ,(under "f100a-close-ro") '(read))))
                (class-of (lambda () (fd-close fd)))))
      (list 'unreadable 'EIO))
;; ONE SCOPE AROUND THE WHOLE LIFE OF A WRITTEN DESCRIPTOR: its create and
;; its write are noted, its failed close is not (close notes nothing).
(want "F100-A fd-close of a written descriptor with close-fail is durable-error EIO, record ((create p) (write p))"
      (armed "close-fail@publish:f100a-close-rw"
             `(let ((class #f))
                (let ((rec (with-mutation-record
                             (lambda ()
                               (let ((fd (fd-open ,(under "f100a-close-rw") '(write create))))
                                 (write-all! fd (string->utf8 "w\n"))
                                 (set! class (class-of (lambda () (fd-close fd)))))
                               (mutation-record)))))
                  (list class rec))))
      (list (list 'durable EIO)
            (list (list 'create (under "f100a-close-rw")) (list 'write (under "f100a-close-rw")))))
;; THE BYTES ARE READ HERE, OUT OF THE ARMED CHILD: close-fail holds for
;; the child's whole stage, so a read there would have its own close failed.
(want "F100-A the written descriptor's bytes are on disk though its close failed"
      (bytes-of (under "f100a-close-rw")) "w\n")

;; ---- the two door primitives added by the code session (F100a rulings) ---
;;
;; AN UNREADABLE ROW WANTS THE ERRNO'S NAME, A DURABLE ROW ITS NUMBER: the
;; two conditions carry it in those two forms (ffi.sc's header), and the rows
;; follow each field's documented form.
;;
;; entry-bytes is read-entry where absence raises; read-entry-range reads a
;; slice. Both are non-mutating, so every failure is unreadable-entry with
;; the errno. The range's end contract is the header's: an offset at or
;; past the end is empty, a range that crosses it is shorter.
(want "F100a entry-bytes of an absent path is unreadable-entry ENOENT"
      (class-of (lambda () (entry-bytes (under "nothing")))) (list 'unreadable 'ENOENT))
(want "F100a entry-bytes of a 000 file is unreadable-entry EACCES"
      (class-of (lambda () (entry-bytes (under "secret")))) (list 'unreadable 'EACCES))
(want "F100a read-entry-range of an absent path is unreadable-entry ENOENT"
      (class-of (lambda () (read-entry-range (under "nothing") 0 4))) (list 'unreadable 'ENOENT))
(touch! (under "f100a-range") "0123456789")
(want "F100a read-entry-range inside the file answers exactly the range"
      (utf8->string (read-entry-range (under "f100a-range") 3 4)) "3456")
(want "F100a read-entry-range from an offset at the end is empty"
      (read-entry-range (under "f100a-range") 10 4) (make-bytevector 0))
(want "F100a read-entry-range from an offset past the end is empty"
      (read-entry-range (under "f100a-range") 12 4) (make-bytevector 0))
(want "F100a read-entry-range across the end answers the bytes up to it"
      (utf8->string (read-entry-range (under "f100a-range") 8 5)) "89")
;; A FILE LONGER THAN ONE CHUNK, so read-entry's second read happens and
;; read-fail-after (which fires only once something was produced) has a
;; read to stand before. read-entry-range has no second read and is not
;; armed here (its header says why).
(touch! (under "f100a-big-read") (make-string 70000 #\a))
(want "F100a entry-bytes with read-fail-after is unreadable-entry with the errno given"
      (armed "read-fail-after@publish:file=f100a-big-read:errno=EIO"
             `(class-of (lambda () (entry-bytes ,(under "f100a-big-read")))))
      (list 'unreadable 'EIO))

;; ---- every noting primitive, succeeding inside one scope (code session) ---
;;
;; THE RECORD'S MUTANTS NEED A ROW EACH. The in-order row covers mkdir,
;; create, write and rename; these cover the rest, each wanting its exact
;; record, so removing any one primitive's `note!` turns its row red (NOTES
;; records the mutant runs).
(touch! (under "f100a-link-src") "l\n")
(want "F100a a successful link! is noted as (link from to), and the new name reads the file"
      (list (recorded (lambda () (link! (under "f100a-link-src") (under "f100a-link-dst"))))
            (bytes-of (under "f100a-link-dst")))
      (list (list (list 'link (under "f100a-link-src") (under "f100a-link-dst"))) "l\n"))
(touch! (under "f100a-gone") "u\n")
(want "F100a a successful unlink! is noted as (unlink p), and the name is gone"
      (list (recorded (lambda () (unlink! (under "f100a-gone"))))
            (entry-type (under "f100a-gone")))
      (list (list (list 'unlink (under "f100a-gone"))) 'absent))
(touch! (under "f100a-trunc") "abcdef\n")
(want "F100a a successful ftruncate! is noted as (truncate p)"
      (let ((fd (fd-open (under "f100a-trunc") '(write))))
        (let ((rec (recorded (lambda () (ftruncate! fd 2)))))
          (fd-close fd)
          (list rec (bytes-of (under "f100a-trunc")))))
      (list (list (list 'truncate (under "f100a-trunc"))) "ab"))
(want "F100a overwrite-entry! of a fresh path is noted as (create p) (truncate p) (write p), and holds the bytes"
      (list (recorded (lambda () (overwrite-entry! (under "f100a-over-new") (string->utf8 "n\n"))))
            (bytes-of (under "f100a-over-new")))
      (list (list (list 'create (under "f100a-over-new")) (list 'truncate (under "f100a-over-new"))
                  (list 'write (under "f100a-over-new")))
            "n\n"))
(want "F100a overwrite-entry! of an existing file is noted as (truncate p) (write p), and keeps its inode"
      (let-values (((d0 i0) (path-device-inode (under "f100a-over-new"))))
        (let ((rec (recorded (lambda () (overwrite-entry! (under "f100a-over-new") (string->utf8 "second\n"))))))
          (let-values (((d1 i1) (path-device-inode (under "f100a-over-new"))))
            (list rec (= i0 i1) (utf8->string (entry-bytes (under "f100a-over-new")))))))
      (list (list (list 'truncate (under "f100a-over-new")) (list 'write (under "f100a-over-new")))
            #t "second\n"))
(want "F100a a successful file-create-exclusive! is noted as (create p), and the file is there, empty"
      (list (recorded (lambda () (file-create-exclusive! (under "f100a-excl"))))
            (bytes-of (under "f100a-excl")))
      (list (list (list 'create (under "f100a-excl"))) ""))

;; ---- F100a review r1: the rows the review found missing (code session) ---
(define (class-and-record thunk)
  (with-mutation-record (lambda () (let ((c (class-of thunk))) (list c (mutation-record))))))
;; A NESTED SCOPE records its own entries and then gives the outer scope
;; back: the outer record goes on from where it was (R3).
(want "F100a a nested scope records only its own entries, and the outer scope continues after it"
      (with-mutation-record
        (lambda ()
          (make-one! (under "nest-a"))
          (let ((inner (recorded (lambda () (make-one! (under "nest-b"))))))
            (make-one! (under "nest-c"))
            (list inner (mutation-record)))))
      (list (list (list 'create (under "nest-b")) (list 'write (under "nest-b")))
            (list (list 'create (under "nest-a")) (list 'write (under "nest-a"))
                  (list 'create (under "nest-c")) (list 'write (under "nest-c")))))
;; A REAL write(2) THAT FAILS WITH NO BYTE WRITTEN is noted (R4): a
;; read-only descriptor refuses the write in the kernel, so the syscall ran.
(touch! (under "f100a-ro-write") "r\n")
(want "F100a a real write syscall that fails before any byte is durable-error and noted (write p)"
      (let ((fd (fd-open (under "f100a-ro-write") '(read))))
        (let ((r (class-and-record (lambda () (write-all! fd (string->utf8 "abc"))))))
          (guard (e (#t #f)) (fd-close fd))
          (list (car r) (cadr r) (bytes-of (under "f100a-ro-write")))))
      (list (list 'durable EBADF) (list (list 'write (under "f100a-ro-write"))) "r\n"))
;; A COLLISION INSIDE A SCOPE notes nothing (R5).
(want "F100a file-create-exclusive! on an existing name inside a scope answers #f, notes nothing, and leaves the bytes"
      (list (with-mutation-record
              (lambda () (list (file-create-exclusive! (under "movable")) (mutation-record))))
            (bytes-of (under "movable")))
      (list (list #f '()) "m\n"))
;; ENOTDIR IS KEPT (D3): a path under a regular file is absent by ENOTDIR,
;; and the two non-R1 forms raise with it.
(want "F100a entry-bytes and directory-entries under a regular file raise ENOTDIR"
      (list (class-of (lambda () (entry-bytes (under "movable/child"))))
            (class-of (lambda () (directory-entries (under "movable/child"))))
            (read-entry (under "movable/child")))
      (list (list 'unreadable 'ENOTDIR) (list 'unreadable 'ENOTDIR) 'absent))
;; EMPTY BYTES WRITE NOTHING (R7): no (write p), and a fresh path's
;; record is its create and its truncate.
(want "F100a overwrite-entry! of empty bytes on a fresh path is noted as (create p) (truncate p), and is empty"
      (list (recorded (lambda () (overwrite-entry! (under "f100a-over-empty") (make-bytevector 0))))
            (bytes-of (under "f100a-over-empty")))
      (list (list (list 'create (under "f100a-over-empty")) (list 'truncate (under "f100a-over-empty"))) ""))
;; THE BYTES, NOT ONLY THE RECORD (ruling H2, RR3): a truncate that did
;; nothing would still be noted, so these read what is left. Shorter and
;; empty bytes over a longer file.
(touch! (under "f100a-over-long") "abcdef")
(want "F100a overwrite-entry! of shorter bytes over a longer file leaves exactly the new bytes"
      (list (recorded (lambda () (overwrite-entry! (under "f100a-over-long") (string->utf8 "x"))))
            (bytes-of (under "f100a-over-long")))
      (list (list (list 'truncate (under "f100a-over-long")) (list 'write (under "f100a-over-long"))) "x"))
(touch! (under "f100a-over-long2") "abcdef")
(want "F100a overwrite-entry! of empty bytes over a longer file leaves it empty"
      (list (recorded (lambda () (overwrite-entry! (under "f100a-over-long2") (make-bytevector 0))))
            (bytes-of (under "f100a-over-long2")))
      (list (list (list 'truncate (under "f100a-over-long2"))) ""))
;; THE NORMAL-PATH CLOSE OF A READ IS CHECKED (D4): under close-fail each
;; reader that closes a descriptor raises unreadable-entry EIO instead of
;; answering what it read.
(touch! (under "f100a-close-read") "0123456789")
(want "F100a under close-fail, entry-bytes, read-entry-range and file-size raise unreadable-entry EIO"
      (armed "close-fail@publish"
             `(list (class-of (lambda () (entry-bytes ,(under "f100a-close-read"))))
                    (class-of (lambda () (read-entry-range ,(under "f100a-close-read") 0 4)))
                    (class-of (lambda () (file-size ,(under "f100a-close-read"))))))
      (list (list 'unreadable 'EIO) (list 'unreadable 'EIO) (list 'unreadable 'EIO)))
;; AN EMPTY WRITE DOES NOT MARK THE DESCRIPTOR WRITTEN (D5): its close
;; keeps the class of an unwritten one.
(want "F100a after an empty write-all!, a close under close-fail is unreadable-entry, not durable"
      (armed "close-fail@publish"
             `(let ((fd (fd-open ,(under "f100a-close-read") '(read))))
                (write-all! fd (make-bytevector 0))
                (class-of (lambda () (fd-close fd)))))
      (list 'unreadable 'EIO))
;; THE LOCK'S RELEASE (D7): on a normal exit its close is checked, and on
;; an escape the failure already leaving is the one reported.
(touch! (under "f100a-lock") "")
(want "F100a with-exclusive-lock under close-fail: unreadable-entry EIO on a normal exit, the body's own raise on an escape"
      (armed "close-fail@publish"
             `(list (class-of (lambda () (with-exclusive-lock ,(under "f100a-lock") (lambda (fd) 'done))))
                    (guard (e ((symbol? e) (list 'escaped e)) (#t (list 'other)))
                      (with-exclusive-lock ,(under "f100a-lock") (lambda (fd) (raise 'boom))))))
      (list (list 'unreadable 'EIO) (list 'escaped 'boom)))
;; read-fail-after REACHES A RANGE THAT CROSSES THE END (D9): its second
;; read, the one that finds the end, stands after a read that produced.
(want "F100a read-entry-range across the end with read-fail-after is unreadable-entry EIO"
      (armed "read-fail-after@publish:file=f100a-range:errno=EIO"
             `(class-of (lambda () (read-entry-range ,(under "f100a-range") 8 5))))
      (list 'unreadable 'EIO))

;; ---- THE EXIT MATRIX (ruling J, RR4 and RR5) ------------------------------
;;
;; A scope is left in one of three ways -- a normal return, a raise caught
;; outside it, a continuation escape -- with no scope around it or inside
;; an outer one. Each of the six cells asserts what the scope gives back:
;; with none around it, the record that was there before (the key is the
;; process's, so the cell compares with what it found rather than assuming
;; it found nothing); inside an outer scope, the outer record reading
;; outer-before then outer-after, without the inner's entries. The table is
;; data: a new way to leave a scope is a new entry in `scope-exits`.
;; LAST IN THIS FILE: a broken exit leaves a scope installed under the
;; process's key, and no row after the matrix is there to read it.
(define scope-exits '(normal raise escape))
(define scope-enclosings '(none outer))
(define (cell-path tag part) (under (string-append tag "-" part)))
(define (scope-cell exit enclosing tag)
  (define (inner)
    (case exit
      ((normal)
       (with-mutation-record (lambda () (make-one! (cell-path tag "inner")))))
      ((raise)
       (guard (e ((eq? e 'cell-boom) 'caught))
         (with-mutation-record
           (lambda () (make-one! (cell-path tag "inner")) (raise 'cell-boom)))))
      ((escape)
       (call/cc
         (lambda (k)
           (with-mutation-record
             (lambda () (make-one! (cell-path tag "inner")) (k 'out))))))))
  (case enclosing
    ((none)
     (let ((pre (mutation-record)))
       (inner)
       (given-back pre (mutation-record))))
    ((outer)
     ;; THE OUTER RECORD, AND WHAT IS LEFT AFTER THE OUTER SCOPE HAS GONE
     ;; (ruling K; review r5): read inside, the outer record cannot see an
     ;; outer scope whose own exit leaves it installed.
     (let* ((pre (mutation-record))
            (rec (with-mutation-record
                   (lambda ()
                     (make-one! (cell-path tag "before"))
                     (inner)
                     (make-one! (cell-path tag "after"))
                     (mutation-record)))))
       (list rec (given-back pre (mutation-record)))))))
;; The process's key may hold a record from before the cell; the cell
;; compares with it. The row before the matrix asserts there is none.
(define (given-back pre post)
  (if (equal? post pre) '() (list 'left-behind post)))
(define (scope-cell-expect enclosing tag)
  (case enclosing
    ((none) '())
    ((outer)
     (list (list (list 'create (cell-path tag "before")) (list 'write (cell-path tag "before"))
                 (list 'create (cell-path tag "after")) (list 'write (cell-path tag "after")))
           '()))))
(want "F100a the exit matrix starts clean: no scope is left installed by the rows before it"
      (mutation-record)
      '())
(for-each
  (lambda (exit)
    (for-each
      (lambda (enclosing)
        (let ((tag (string-append "cell-" (symbol->string exit) "-" (symbol->string enclosing))))
          (want (string-append "F100a exit matrix [" (symbol->string exit) " x "
                               (symbol->string enclosing) "]: the scope gives back what was there before")
                (scope-cell exit enclosing tag)
                (scope-cell-expect enclosing tag))))
      scope-enclosings))
  scope-exits)

(system (string-append "chmod -R u+rwx " f100a-dir " && rm -rf " f100a-dir))

(printf "rows: ~a\n~a failures\nfacade-ffi complete\n" rows bad)
