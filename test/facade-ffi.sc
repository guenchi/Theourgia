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
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

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
(define setsid-out (string-append "/tmp/ffi-setsid-" (number->string (get-process-id))))
(define setsid-src (string-append setsid-out ".ss"))
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
      (let* ((out (string-append "/tmp/ffi-ps-" (number->string mine)))
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

(printf "rows: ~a\n~a failures\nfacade-ffi complete\n" rows bad)
