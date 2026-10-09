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

;; A blocking fifo with a draining reader: whether write(2) splits a
;; large transfer is the kernel's choice, so this measures rather than
;; predicts. What it does settle either way is byte fidelity -- the
;; bytes that come out the far end are compared with the bytes offered.
(import (chezscheme) (theourgia ffi) (only (theourgia platform-numbers) platform-number))

;; THE WORK DIRECTORY IS DECIDED AT RUN TIME. Every fixture used to name
;; an absolute path under one session's scratchpad. That is green only
;; while that particular directory happens to still exist: tmp is swept,
;; and another machine has no such path at all -- so the whole suite
;; would go red for a reason with nothing to do with the code under test.
;; THEOURGIA_TEST_ROOT overrides the default; the pid keeps two runs, or
;; two fixtures, out of each other's way. Directories are left behind
;; deliberately, as evidence.
(define (test-dir name)
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/" name "-" (number->string (get-process-id)))))
    ;; A ROOT THAT DOES NOT SURVIVE THE ROUND TRIP IS REFUSED HERE. Trace
    ;; lines are written with display and read back as data, and paths go
    ;; into generated scripts and shell commands unquoted -- so a root
    ;; with a space or a bracket in it makes the crash device read no
    ;; events at all and rewrite nothing, which reads exactly like a tree
    ;; that needed no rewriting. Refusing is the one answer that cannot
    ;; be mistaken for success.
    (let loop ((i 0))
      (when (< i (string-length path))
        (let ((c (string-ref path i)))
          (unless (or (char-alphabetic? c) (char-numeric? c)
                      (memv c '(#\/ #\. #\- #\_)))
            (assertion-violation 'test-dir
              "THEOURGIA_TEST_ROOT may use only letters, digits, / . - and _"
              root)))
        (loop (+ i 1))))
    ;; THE SAME CHECK NOW GUARDS A REMOVAL, so it asks for two more
    ;; things a creation did not need: an absolute path, and no `..`
    ;; anywhere in it.
    (unless (and (> (string-length path) 0) (char=? #\/ (string-ref path 0)))
      (assertion-violation 'test-dir
        "THEOURGIA_TEST_ROOT must be an absolute path" root))
    (let loop ((i 0))
      (when (< (+ i 1) (string-length path))
        (when (and (char=? #\. (string-ref path i))
                   (char=? #\. (string-ref path (+ i 1))))
          (assertion-violation 'test-dir
            "THEOURGIA_TEST_ROOT may not contain .." root))
        (loop (+ i 1))))
    ;; AND THE DIRECTORY IS MADE FRESH, NOT ASSUMED FRESH. The name
    ;; carries the process id, which reads like a unique name and is not
    ;; one: the pid space wraps, the scratch root outlives the run, and a
    ;; directory left by an earlier run holding the same pid is handed to
    ;; this one already populated. Counted in the default root on
    ;; 2026-09-11: 4260 leftover directories over 1686 distinct pids, so
    ;; about one run in twenty inherited an older run's store. It showed
    ;; up once as a crash -- an init answering already-initialised to a
    ;; fixture that expected a store id -- and the crash is the harmless
    ;; form. The form that matters is an assertion passing against data
    ;; the run did not write. The sibling `-home` goes with it, because
    ;; the machine registry is keyed by store identity and a stale one
    ;; makes a fresh store look like a rollback.
    ;; AND THE LEAF IS NEVER THE ROOT. Removal only ever names
    ;; <root>/<name>-<pid>; a name that collapsed to nothing would aim it
    ;; at the scratch root itself, which holds every other run.
    (unless (and (> (string-length path) (+ 1 (string-length root)))
                 (string=? root (substring path 0 (string-length root)))
                 (char=? #\/ (string-ref path (string-length root))))
      (assertion-violation 'test-dir
        "the directory must lie strictly inside the root" (list root path)))
    ;; A CLEAN THAT FAILED MUST NOT READ AS A CLEAN THAT WORKED. If the
    ;; removal fails -- contents that cannot be unlinked, a busy mount --
    ;; `mkdir -p` then succeeds on the directory that is already there and
    ;; hands back exactly the populated directory this is here to
    ;; prevent. Both commands are checked, and a failure stops the run
    ;; rather than quietly weakening it.
    (let ((must! (lambda (command)
                   (let ((status (system command)))
                     (unless (eqv? 0 status)
                       (assertion-violation 'test-dir
                         "could not prepare the scratch directory"
                         (list command status)))))))
      (must! (string-append "rm -rf " path " " path "-home"))
      (must! (string-append "mkdir -p " path)))
    path))

;; The trace switch is injected now, not a parameter: (theourgia trace)
;; takes neither getenv nor make-parameter so that it stays portable.
(define theourgia-trace?
  (make-parameter #f (lambda (v) (trace-enable! v) v)))
;; ---- the rows ----------------------------------------------------------------------
;;
;; A NON-BLOCKING PIPE ANSWERS ONE OF TWO THINGS, and which one is a race:
;; with room, a short write (write-all! goes on with the rest); with none,
;; -1 EAGAIN, which write-all! raises on purpose (see its comment: the one
;; product caller that can see EAGAIN is a blocking socket whose send
;; timed out). A single run that raced a background reader was green where
;; the reader kept up and died with rc 255 where it did not. So there are
;; two rows: one that makes the pipe full and asserts the raise, and the
;; race itself, where both outcomes are named and neither is red.
(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
;; A ROW THAT RAISES IS A FAIL LINE, and the rows after it still run.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (want-1 name (caught got) expected))))

(define EAGAIN (platform-number 'EAGAIN))
;; NEVER: fcntl IS VARIADIC, AND ITS THIRD ARGUMENT IS PASSED AS ONE. A fixed
;; (int int int) binding passes it where a non-variadic argument goes, and on
;; macOS ARM64 that is not where fcntl reads it: F_SETFL then set the flags
;; to whatever it found (measured: O_WRONLY becomes O_WRONLY|O_APPEND, and the
;; call answers 0), so this fixture's pipe was never non-blocking there and
;; it raced nothing. On x86_64 the two conventions agree, the pipe was
;; non-blocking, and EAGAIN appeared. The flags are read back with F_GETFL
;; (no third argument) before anything is written.
;; THE LIBRARY BODY RUNS FIRST: importing (theourgia ffi) does not run it, and
;; it is what loads the C library these two bindings are looked up in.
(define ffi-loaded (procedure? fd-open))
(define c-fcntl (foreign-procedure (__varargs_after 2) "fcntl" (int int int) int))
(define c-getfl (foreign-procedure "fcntl" (int int) int))
(define F_SETFL (platform-number 'F_SETFL))
(define F_GETFL (platform-number 'F_GETFL))
(define O_NONBLOCK (platform-number 'O_NONBLOCK))
;; -> #t when the descriptor reads back non-blocking.
(define (make-nonblocking! fd)
  (let* ((before (c-getfl fd F_GETFL))
         (rc (c-fcntl fd F_SETFL (bitwise-ior before O_NONBLOCK)))
         (after (c-getfl fd F_GETFL)))
    (and (= rc 0) (not (zero? (bitwise-and after O_NONBLOCK))))))
(define n 4000000)
(define payload
  (let ((bv (make-bytevector n)))
    (let loop ((i 0) (x 987654321))
      (if (= i n) bv
          (let ((x (modulo (+ (* x 1103515245) 12345) 2147483648)))
            (bytevector-u8-set! bv i (modulo (quotient x 65536) 256))
            (loop (+ i 1) x))))))

;; write-all! on a non-blocking descriptor, with the bytes reported as it
;; goes. -> (all <n>) when it returned, (raised <errno> <bytes-before>) for
;; a filesystem error, or SPUN when it ran out of fuel: a loop that retried
;; EAGAIN would spin, and an engine's fuel ends it where a blocked write
;; could not have been reached at all (the descriptor is non-blocking).
(define (write-nonblocking fd)
  (let ((sent 0))
    ((make-engine
       (lambda ()
         (guard (e ((fs-error? e) (list 'raised (fs-error-errno e) sent)))
           (list 'all (write-all! fd payload "pipe" (lambda (k) (set! sent k)))))))
     100000000
     (lambda (ticks value) value)
     (lambda (engine) 'SPUN))))

;; ---- NB1: the pipe is full -- a reader that holds the fifo open and reads nothing ----
(define dir1 (test-dir "pipe-full"))
(system (string-append "mkfifo " dir1 "/f"))
;; The holder opens the fifo for reading (so the writer's open returns) and
;; never reads; it is ended below, by the exact command line it was given.
(define holder (string-append "exec 3< " dir1 "/f; sleep 30"))
(system (string-append "sh -c '" holder "' &"))
(define fd1 (fd-open (string-append dir1 "/f") (list 'write)))
(define nb1 (make-nonblocking! fd1))
(define t0 (real-time))
;; NOT WRITTEN AT ALL unless the descriptor is non-blocking: a blocking
;; write into a pipe nobody reads would wait for the holder to exit.
(define full (if nb1 (write-nonblocking fd1) 'NOT-NONBLOCKING))
(define full-ms (- (real-time) t0))
(fd-close fd1)
(system (string-append "pkill -f '" holder "' 2>/dev/null"))
(printf "NB1 reading: ~s in ~a ms (non-blocking: ~a)\n" full full-ms nb1)
(want "NB1 a full non-blocking pipe: write-all! raises the filesystem error with errno EAGAIN, promptly, after writing what fitted"
      (list (if (pair? full) (car full) full)
            (and (pair? full) (eq? (car full) 'raised) (eqv? (cadr full) EAGAIN))
            (and (pair? full) (eq? (car full) 'raised) (< 0 (caddr full) n))
            (< full-ms 5000))
      '(raised #t #t #t))

;; ---- NB2: the race, both outcomes named -------------------------------------------------
(define dir2 (test-dir "pipework"))
(system (string-append "mkfifo " dir2 "/f"))
;; THE READER IS JOINED before the far end is read: it marks done once cat
;; has seen the end of the pipe, and the row waits for that mark, up to 30 s.
;; A reader not done by then makes the row VOID (counted, printed with the
;; reason): the far end is not read, and nothing is said about the product.
(system (string-append "sh -c 'cat " dir2 "/f > " dir2 "/out.dat; touch " dir2 "/cat.done' > /dev/null 2>&1 &"))
(define fd2 (fd-open (string-append dir2 "/f") (list 'write)))
(define nb2 (make-nonblocking! fd2))
(define raced (if nb2 (write-nonblocking fd2) 'NOT-NONBLOCKING))
(fd-close fd2)
(define cat-done
  (let wait ((k 0))
    (cond ((file-exists? (string-append dir2 "/cat.done")) #t)
          ((>= k 300) #f)
          (else (sleep (make-time 'time-duration 100000000 0)) (wait (+ k 1))))))
(define got
  (and cat-done
       (let ((p (open-file-input-port (string-append dir2 "/out.dat"))))
         (let ((b (get-bytevector-all p))) (close-port p) (if (eof-object? b) (make-bytevector 0) b)))))
(printf "NB2 reading: ~s (non-blocking: ~a); out.dat ~a bytes; reader done: ~a\n" raced nb2 (and got (bytevector-length got)) cat-done)
(if (not cat-done)
    (begin (set! rows (+ rows 1))
           (printf "VOID NB2 a non-blocking pipe with a reader: the reader was not done in 30 s, so the far end was not read\n"))
(want "NB2 a non-blocking pipe with a reader: either every byte, through short writes and identical at the far end, or EAGAIN as in NB1; never anything else"
      (cond
        ((and (pair? raced) (eq? (car raced) 'all))
         (list 'all-written (= (cadr raced) n) (equal? got payload)))
        ((and (pair? raced) (eq? (car raced) 'raised) (eqv? (cadr raced) EAGAIN))
         ;; THE FAR END HOLDS EXACTLY WHAT WAS WRITTEN: as many bytes as the
         ;; write reported, and they are the payload's first ones.
         (list 'eagain
               (and (number? (caddr raced)) (= (bytevector-length got) (caddr raced)))
               (equal? got (let ((b (make-bytevector (bytevector-length got))))
                             (bytevector-copy! payload 0 b 0 (bytevector-length got)) b))))
        (else (list 'NEITHER raced)))
      (if (and (pair? raced) (eq? (car raced) 'all))
          '(all-written #t #t)
          '(eagain #t #t))))

;; A run that did not reach here is not a pass. The runner requires
;; this line AND a zero failure count: they are two propositions.
(printf "\n~a failures\nrows: ~a\nsmoke-shortwrite-nb complete\n" bad rows)
(exit (if (= bad 0) 0 1))
