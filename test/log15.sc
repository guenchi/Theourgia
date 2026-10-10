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

;; Rotation across a crash (plan L10, design 4.4 and 13').
;;
;; Every point in rotation's sequence is a place the machine can stop,
;; and the sequence exists so that each of those places is recoverable:
;; either N+1 is absent and N is still the current segment, or N+1 is
;; present and is the current segment, possibly empty. Both are legal.
;;
;; TWO STATES PER POINT. Killing the process leaves everything it had
;; written, buffered or not; the durable-only state keeps only what was
;; fsynced. A store must come back from both, and "a record that was
;; reported committed exists exactly once" has to hold in both -- that
;; pair is what says the flushes are in the right places rather than
;; merely present.
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia ffi))

;; THE WORK DIRECTORY IS DECIDED AT RUN TIME. Naming an absolute path
;; under one session's scratchpad is green only while that exact
;; directory survives: tmp is swept, and another machine has no such path
;; at all -- the whole suite would then be red for a reason with nothing
;; to do with the code under test. THEOURGIA_TEST_ROOT overrides the
;; default; the pid keeps two runs, or two fixtures, out of each other's
;; way. The directories are left behind deliberately, as evidence.
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

(define here
  (let* ((script (car (command-line)))
         (n (let loop ((i (- (string-length script) 1)))
              (cond ((< i 0) #f)
                    ((char=? (string-ref script i) #\/) i)
                    (else (loop (- i 1)))))))
    (if n (substring script 0 n) ".")))
(load (string-append here "/crash.sc"))
(define bad 0)
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))

;; A ROW THAT RAISES IS A FAILED ROW, NOT A FAILED FILE. Rows read an
;; answer apart, and a seeded defect that changes the answer's SHAPE
;; makes the accessor raise while the row is being computed -- outside
;; anything that was catching. The file then ends where it stood, every
;; row below goes unrun, and the runner sees no `FAIL` at all: a round
;; scored three such defects as crashes with no failures, for answers
;; the store had in fact got right and said plainly.
;;
;; BOTH SIDES, BECAUSE EITHER CAN RAISE. A row whose EXPECTATION is
;; derived from the program's own answer raises while the expectation
;; is built, and ends the file just the same.
;;
;; IT IS A MACRO FOR ONE REASON: an argument is evaluated before the
;; call, so a procedure could not have guarded either side.
;;
;; IT DOES NOT COVER EVERYTHING. Top-level definitions between rows are
;; outside it, and a raise there still ends the file.
;; HOW MANY ROWS ACTUALLY RAN. A file that ends early still
;; reports the failures it had already found, so a seeded defect
;; that kills the file after a few rows is scored as caught while
;; the rows below it never ran. The count is the only thing that
;; tells those apart, and it has to be compared against the same
;; file's count on unmutated code -- there is no static number to
;; compare it with, because rows are written inside loops and case
;; tables as well as one at a time.
(define rows-run 0)

(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) (caught x)))))))

(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))

(define base (test-dir "log15"))
(define d (string-append base "/store"))
(define home (string-append base "/home"))
(define W "wwwe5q2a")
(define fixed-ts 1757300000003)
(define (rec seq ts deps payload)
  (encode-record seq ts "agent:claude" deps (storable-encode payload)))
(define (r n) (rec n (+ 1757300000000 n) '() (list 'put (string-append "w." (number->string n)) '())))
(define R (bytevector-length (r 1)))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (slurp path)
  (guard (e (#t 'no-such-file))
    (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
      (if (eof-object? b) (make-bytevector 0) b))))
(define (text-of path) (let ((b (slurp path))) (if (bytevector? b) (utf8->string b) "")))
(define (wpath n) (string-append d "/writers/" W "/" (segment-file-name n)))
(define before-image (string-append base "/before"))
(define (snapshot-before!)
  ;; THE DEVICE NEEDS WHAT THE DISK HELD BEFORE THE RUN. A truncation
  ;; after the last flush, an unlink, a rename over an existing file --
  ;; none of those can be undone from the trace and the crashed tree
  ;; alone, and the device used to answer them with empty files.
  (system (string-append "rm -rf " before-image "; cp -R " d " " before-image)))
(define (build!)
  (system (string-append "rm -rf " base "; mkdir -p " d "/writers/" W " " d "/snap " home))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"e5\"))\n"))
  (file-ensure! (string-append d "/lock"))
  (putenv "THEOURGIA_HOME" home)
  ;; A segment already old enough to rotate, so the append under test
  ;; takes the rotation path rather than an ordinary write.
  (put! (wpath 1) (cat (r 1) (r 2)))
  (let ((n (instance-install! d))) (owner-install! d W n))
  (snapshot-before!))

;; ---- the barrier controller ------------------------------------------------
;; THE PRODUCT ANNOUNCES ITSELF BEFORE IT PARKS. barrier! emits its trace
;; line and only then blocks on the fifo, so waiting for that line is
;; waiting for a real event -- not sleeping and hoping. `sleep` appears
;; only as the interval between looks at a file that the child is
;; writing; nothing is synchronised by elapsed time.
(define (wait-for-barrier trace-path name limit)
  (let loop ((n 0))
    (cond
      ((> n limit) #f)
      ((let ((t (text-of trace-path)))
         (crash-has-substring? t (string-append "(trace barrier " name " #f)")))
       #t)
      (else (system "sleep 0.05") (loop (+ n 1))))))

(define child-path (string-append base "/child.sc"))
(define (write-child!)
  (put! child-path
        (string->utf8
          (string-append
            "#!chezscheme\n(import (chezscheme) (theourgia log) (theourgia ffi))\n"
            "(putenv \"THEOURGIA_HOME\" \"" home "\")\n"
            "(parameterize ((log-clock (lambda () " (number->string (+ 1757300000000 3600000 5000)) ")))\n"
            "  (let* ((s (log-begin \"" d "\" (lambda args 'applied)))\n"
            "         (v (session-view s))\n"
            "         (res (session-append! s (make-frame (view-revision v) (view-epoch v)\n"
            "                                             (view-writer v) (view-expect-seq v)\n"
            "                                             \"agent:claude\" '() '(put \"w.3\" ())))))\n"
            ;; THE PROMISE IS THE REQUEST'S, SO THE CHILD MAKES IT THE
            ;; WAY A REQUEST DOES: append, then the commit barrier, then
            ;; answer. An append no longer fsyncs -- durability is per
            ;; request -- so a child that answered before the barrier
            ;; would be reporting `committed` for a record nothing had
            ;; promised, and the row below would then be asking a
            ;; durable-only rewrite to keep something that was never
            ;; durable.
            "    (let ((c (guard (e (#t 'barrier-failed)) (session-commit! s))))\n"
            "      (log-end! s)\n"
            "      (call-with-port (open-file-output-port \"" base "/answer\" (file-options no-fail))\n"
            "        (lambda (p) (put-bytevector p (string->utf8\n"
            "                                        (format \"~s\\n\" (list (car res) c)))))))))\n"))))

;; Runs the child up to BARRIER, then either kills it or releases it.
;; Returns the trace it produced.
(define (run-to-barrier barrier action)
  (let ((fifo (string-append base "/fifo"))
        (trace (string-append base "/trace"))
        (pidf (string-append base "/pid")))
    (system (string-append "rm -f " fifo " " trace " " pidf " " base "/answer"))
    (system (string-append "mkfifo " fifo))
    (write-child!)
;; THE BARRIER ONLY EXISTS IN AN INJECTION BUILD. Without
    ;; THEOURGIA_INJECT the barrier procedure compiles to a no-op, the
    ;; child runs straight through, and every row reads "never parked" --
    ;; which is at least loud, unlike a child that silently completed.
    (system (string-append "THEOURGIA_INJECT=on THEOURGIA_TRACE=1 "
                           "THEOURGIA_BARRIER=" barrier ":" fifo " "
                           "scheme --script " child-path " > /dev/null 2> " trace
                           " & echo $! > " pidf))
    (let ((reached (wait-for-barrier trace barrier 200)))
      (cond
        ;; NOT PARKED MEANS NOT RELEASED. Writing to a fifo with no
        ;; reader blocks forever, so a child that never reached the
        ;; barrier is killed and reported -- the case then goes red on a
        ;; reading instead of hanging the suite.
        ((not reached)
         (system (string-append "kill -9 $(cat " pidf ") 2>/dev/null"))
         'never-parked)
        ((eq? action 'kill)
         (system (string-append "kill -9 $(cat " pidf ") 2>/dev/null"))
         (system "sleep 0.1")
         (text-of trace))
        (else
         (system (string-append "printf x > " fifo))
         (if (wait-for-file (string-append base "/answer") 200)
             (text-of trace)
             'no-answer))))))

;; WAITS FOR THE CHILD'S ANSWER, which is the only thing that says it
;; finished. Sleeping a fixed interval and hoping is a bet on the
;; scheduler: lose it and the row is red for a reason that has nothing to
;; do with the code.
(define (wait-for-file path limit)
  (let loop ((n 0))
    (cond ((file-exists? path) #t)
          ((> n limit) #f)
          (else (system "sleep 0.05") (loop (+ n 1))))))

;; What a store looks like to a fresh reader, which is the only question
;; recovery has to answer.
(define (recovered-state)
  (parameterize ((log-clock (lambda () fixed-ts)))
    (let* ((ls (log-open d))
           (p (load-prefix ls W))
           (seen '()))
      (load-deliver! ls '() (lambda (w seg off seq . rest) (set! seen (cons seq seen))))
      (let ((out (list (discovery-end-seq p)
                       (reverse seen)
                       (map (lambda (e) (log-error-kind (cdr e))) (load-integrity ls)))))
        (load-commit! ls)
        out))))

(printf "== the device is trusted only after its own self-check ==\n")
;; crash.sc checks itself against hand-computed answers when it is run as
;; a script; this row is here so a case that USES it fails loudly if that
;; ever stops being true.
;; NOT MERELY procedure? -- that accepts a device that does nothing at
;; all. The device is run here on a hand-made case with a hand-computed
;; answer, so a case that uses it cannot be presided over by a judge that
;; has silently stopped working.
(want "the device still gives the hand-computed answer on a known case"
      (let ((t (string-append base "/devcheck"))
            (bt (string-append base "/devcheck-before")))
        (system (string-append "rm -rf " t " " bt "; mkdir -p " t))
        (put! (string-append t "/a") (string->utf8 "AAAA"))
        (system (string-append "cp -R " t " " bt))
        (put! (string-append t "/a") (string->utf8 "AAAABBBBCCCC"))
        (crash-durable-only! t bt
          (string-append "(trace write " t "/a 4)\n"
                         "(trace fsync " t "/a #f)\n"
                         "(trace write " t "/a 4)\n"))
        (text-of (string-append t "/a")))
      ;; THE ANSWER DIFFERS FROM BOTH INPUTS. An earlier version expected
      ;; exactly what the crashed tree already held, so a device that did
      ;; nothing at all passed this check -- the one thing it exists to
      ;; rule out.
      "AAAABBBB")

(printf "== L10: the six points, killed ==\n")
(define points '("before-rotate-write" "after-current-fsync" "after-create-next"
                 "after-next-fsync" "after-dir-fsync" "after-first-write-next"))
(define (kill-at point)
  (build!)
  (let ((trace (run-to-barrier point 'kill)))
    (if (eq? trace 'never-parked)
        'never-parked
        (recovered-state))))
(for-each
  (lambda (point)
    (let ((got (kill-at point)))
      ;; THE RECORD UNDER TEST WAS NEVER REPORTED COMMITTED -- the child
      ;; was killed before it could return -- so the only requirement is
      ;; that the store comes back readable with its first two records
      ;; and no integrity error. A third record MAY be there (the kill
      ;; state keeps whatever was written); what must never happen is a
      ;; store that will not load or that lost history.
      (want (string-append "killed at " point ": history intact, no integrity error")
            (if (eq? got 'never-parked)
                'never-parked
                (list (>= (car got) 2) (list-head (cadr got) 2) (caddr got)))
            (list #t '(1 2) '()))))
  points)

(printf "== L10: the six points, only what was fsynced ==\n")
(define (durable-at point)
  (build!)
  (let ((trace (run-to-barrier point 'kill)))
    (if (eq? trace 'never-parked)
        'never-parked
        (begin (crash-durable-only! d before-image trace) (recovered-state)))))
(for-each
  (lambda (point)
    (let ((got (durable-at point)))
      (want (string-append "durable-only at " point ": history intact, no integrity error")
            (if (eq? got 'never-parked)
                'never-parked
                (list (>= (car got) 2) (list-head (cadr got) 2) (caddr got)))
            (list #t '(1 2) '()))))
  points)

(printf "== released rather than killed, the record is committed exactly once ==\n")
(build!)
(define release-trace (run-to-barrier "after-dir-fsync" 'release))
(want "letting it through commits, and the record exists exactly once"
      (if (eq? release-trace 'never-parked)
          'never-parked
          (let ((st (recovered-state)))
            (list (car st) (cadr st) (caddr st))))
      (list 3 '(1 2 3) '()))

(printf "== L10: what the tree looks like at each boundary ==\n")
;; "It loads and the first two records are there" is satisfied at every
;; one of the six points, which is why it could not tell a rotation with
;; its flushes in the wrong order from one with them in the right order.
;; These assert the tree itself, worked out from section 4.4's sequence:
;;
;;   before-rotate-write   N+1 absent
;;   after-current-fsync   N+1 absent, N flushed
;;   after-create-next     N+1 present and empty
;;   after-next-fsync      N+1 present and empty
;;   after-dir-fsync       N+1 present and empty, its entry durable
;;   after-first-write-next N+1 holds record 3
;;
;; In the durable-only state the same points differ in one place: until
;; the directory is flushed, N+1's entry does not survive at all.
(define (segments-after point state)
  (build!)
  (let ((trace (run-to-barrier point 'kill)))
    (if (memq trace '(never-parked no-answer))
        trace
        (begin
          (when (eq? state 'durable) (crash-durable-only! d before-image trace))
          (list (enumerate-segment-files d W)
                (let ((b (slurp (wpath 2))))
                  (cond ((eq? b 'no-such-file) 'absent)
                        ((= 0 (bytevector-length b)) 'empty)
                        ((equal? b (rec 3 (+ 1757300000000 3600000 5000) '()
                                        '(put "w.3" ())))
                         'record-3)
                        (else (list 'other (bytevector-length b))))))))))
(for-each
  (lambda (point expect)
    (want (string-append "killed at " point ": the tree is exactly this")
          (segments-after point 'kill) expect))
  points
  (list (list '(1) 'absent)
        (list '(1) 'absent)
        (list '(1 2) 'empty)
        (list '(1 2) 'empty)
        (list '(1 2) 'empty)
        (list '(1 2) 'record-3)))
(for-each
  (lambda (point expect)
    (want (string-append "durable-only at " point ": the tree is exactly this")
          (segments-after point 'durable) expect))
  points
  (list (list '(1) 'absent)
        (list '(1) 'absent)
        ;; The entry for N+1 is not durable until the directory is
        ;; flushed, so the first three points lose it entirely.
        (list '(1) 'absent)
        (list '(1) 'absent)
        (list '(1 2) 'empty)
        ;; AND HERE THE TWO STATES DIFFER, which is the point of having
        ;; both. The record has been written into N+1 and not yet
        ;; flushed: killing the process keeps it (the bytes reached the
        ;; OS), and the durable-only state discards it. Nothing promised
        ;; it -- the append had not returned -- so losing it is correct,
        ;; and the row below is where a record that WAS promised has to
        ;; survive the same rewrite.
        (list '(1 2) 'empty)))

(printf "== a committed record survives the durable-only state ==\n")
;; THIS IS THE CRITERION THE DESIGN NAMES, and the rows above cannot
;; reach it: they kill the child before it returns, so no record was ever
;; reported committed and losing one is legal. Here the append is allowed
;; to finish -- the caller has been told `committed` -- and only then is
;; the durable-only rewrite applied. A rotation that creates the new
;; segment without flushing its directory loses that entry, and with it a
;; record the caller was promised.
(build!)
(define committed-trace
  (let ((trace (string-append base "/full-trace")))
    (system (string-append "rm -f " trace " " base "/answer"))
    (write-child!)
    (system (string-append "THEOURGIA_INJECT=on THEOURGIA_TRACE=1 scheme --script "
                           child-path " > /dev/null 2> " trace))
    (text-of trace)))
(want "the child reported the record committed"
      (crash-has-substring? (text-of (string-append base "/answer")) "committed") #t)
(want "and after keeping only what was fsynced, it is still there exactly once"
      (begin (crash-durable-only! d before-image committed-trace)
             (let ((st (recovered-state)))
               (list (car st) (cadr st) (caddr st))))
      (list 3 '(1 2 3) '()))

(printf "== L19(c): two stores racing for the machine lock ==\n")
;; Two stores, one machine registry. P is held inside the registry's
;; critical section -- it has read the marks and not yet written them
;; back -- while Q is started. A correct implementation makes Q wait on
;; the machine lock; a broken one lets Q into the section, and then the
;; two read-modify-writes can lose each other.
;;
;; THE CONTROLLER WAITS FOR EITHER OUTCOME, not only for the good one.
;; Waiting only for Q's lock-wait would hang forever exactly when the
;; product is wrong, and a hang is the failure a suite reports worst.
(define d2 (string-append base "/store2"))
(define (build-two!)
  (system (string-append "rm -rf " base "; mkdir -p " d "/writers/" W " " d "/snap "
                         d2 "/writers/" W " " d2 "/snap " home))
  (for-each
    (lambda (root id)
      (put! (string-append root "/meta.sexp")
            (string->utf8 (string-append "((format 1) (store-id \"" id "\"))\n")))
      (file-ensure! (string-append root "/lock"))
      (put! (string-append root "/writers/" W "/" (segment-file-name 1))
            (cat (r 1) (r 2))))
    (list d d2) (list "e5" "e6"))
  (putenv "THEOURGIA_HOME" home)
  (let ((n (instance-install! d))) (owner-install! d W n))
  (let ((n (instance-install! d2))) (owner-install! d2 W n)))

(define (racer-child store out)
  (let ((path (string-append base "/racer-" out ".sc")))
    (put! path
          (string->utf8
            (string-append
              "#!chezscheme\n(import (chezscheme) (theourgia log) (theourgia ffi))\n"
              "(putenv \"THEOURGIA_HOME\" \"" home "\")\n"
              "(parameterize ((log-clock (lambda () " (number->string fixed-ts) ")))\n"
              "  (let* ((s (log-begin \"" store "\" (lambda args 'applied)))\n"
              "         (v (session-view s))\n"
              "         (res (session-append! s (make-frame (view-revision v) (view-epoch v)\n"
              "                                             (view-writer v) (view-expect-seq v)\n"
              "                                             \"agent:claude\" '() '(put \"w.3\" ())))))\n"
              "    (log-end! s)\n"
              "    (call-with-port (open-file-output-port \"" base "/" out "\" (file-options no-fail))\n"
              "      (lambda (p) (put-bytevector p (string->utf8 (format \"~s\\n\" res)))))))\n")))
    path))

(build-two!)
(define p-fifo (string-append base "/p-fifo"))
(define p-trace (string-append base "/p-trace"))
(define q-trace (string-append base "/q-trace"))
(system (string-append "rm -f " p-fifo "; mkfifo " p-fifo))
(define p-script (racer-child d "p-answer"))
(define q-script (racer-child d2 "q-answer"))
;; P stops after reading the registry and before writing it back.
(system (string-append "THEOURGIA_INJECT=on THEOURGIA_TRACE=1 THEOURGIA_BARRIER=registry-read:"
                       p-fifo " scheme --script " p-script " > /dev/null 2> " p-trace
                       " & echo $! > " base "/p-pid"))
(define p-parked (wait-for-barrier p-trace "registry-read" 200))
;; Q now runs to completion or blocks; either way its trace tells us which.
(system (string-append "THEOURGIA_TRACE=1 scheme --script " q-script
                       " > /dev/null 2> " q-trace " & echo $! > " base "/q-pid"))
(define q-verdict
  (let loop ((n 0))
    (let ((t (text-of q-trace)))
      (cond
        ((crash-has-substring? t "(trace registry-write") 'entered-the-section)
        ;; NAMED, NOT ANY WAIT. "some lock-wait appeared" is satisfied by
        ;; contention on something else entirely.
        ((crash-has-substring? t (string-append "(trace lock-wait (" home "/lock . exclusive)"))
         'waiting)
        ((> n 200) 'neither)
        (else (system "sleep 0.05") (loop (+ n 1)))))))
(want "P parked inside the registry's critical section" p-parked #t)
(want "and Q waits on the machine lock rather than entering it"
      q-verdict 'waiting)
;; Release P, let both finish, and check the marks.
;; RELEASE ONLY IF SOMEONE IS PARKED, then wait for both answers. The
;; unconditional write blocked forever whenever P failed to reach its
;; barrier, and `sleep 1` was a bet that Q would be scheduled in time.
(when p-parked (system (string-append "printf x > " p-fifo)))
(define p-done (wait-for-file (string-append base "/p-answer") 400))
(define q-done (wait-for-file (string-append base "/q-answer") 400))
(want "both children finished, so the marks below are their final ones"
      (list p-done q-done) (list #t #t))
(want "both stores committed, and each mark is exactly its own last sequence"
      (let* ((reg (text-of (string-append home "/instances.sexp")))
             (entries (guard (e (#t 'unreadable)) (read (open-string-input-port reg)))))
        (list (crash-has-substring? (text-of (string-append base "/p-answer")) "committed")
              (crash-has-substring? (text-of (string-append base "/q-answer")) "committed")
              (and (list? entries)
                   (list-sort (lambda (a b) (string<? (car a) (car b)))
                              (map (lambda (e) (cons (car e) (cadddr e))) entries)))))
      (list #t #t (list (cons "e5" 3) (cons "e6" 3))))

(printf "\n== the written frontier is raised under the store's real name ==\n")
;; THE ONE READ NO FAULT CAN REACH. Reconciliation keys its registry
;; update on the store's own metadata and instance files, and both go
;; through `read-whole`, which opens a Chez port directly rather than the
;; injected `fd-open` -- so `open-fail` arms, announces itself, and
;; injects nothing. The state the guard exists for needs those files to
;; read at open and fail HERE, which is why there is a rendezvous at that
;; exact point and this section rather than a fault.
;;
;; WHAT IT STOPS: keyed by "unknown" or #f the update matches no entry,
;; the merge returns the registry untouched, the write is acknowledged,
;; and the frontier never moves -- so a store later restored to an
;; earlier sequence walks past the rollback check that frontier exists to
;; fail, and the acknowledged request runs a second time.
(define id-base (test-dir "log15identity"))
(define id-fifo (string-append id-base "/fifo"))
(define id-trace (string-append id-base "/trace"))
(define id-pid (string-append id-base "/pid"))
(define id-store (string-append id-base "/s"))
(define id-child (string-append id-base "/child.sc"))
(system (string-append "rm -rf " id-base "; mkdir -p " id-store " " id-base "/home"))
(put! id-child
      (string->utf8
        (string-append
          "#!r6rs\n(import (chezscheme) (theourgia log) (theourgia ffi)\n"
          "        (theourgia store) (theourgia reduce))\n"
          "(putenv \"THEOURGIA_HOME\" \"" id-base "/home\")\n"
          "(store-init! \"" id-store "\")\n"
          "(define a (guard (e (#t (list 'raised))) \n"
          "  (with-store-write \"" id-store "\" (lambda (st v)\n"
          "    '((insert root #f ((kind . section) (title . \"One\"))))))))\n"
          "(call-with-port (open-file-output-port \"" id-base "/answer\" (file-options no-fail))\n"
          "  (lambda (p) (put-bytevector p (string->utf8 (format \"~s\\n\" (car a))))))\n")))
(system (string-append "mkfifo " id-fifo))
(system (string-append "THEOURGIA_INJECT=on THEOURGIA_TRACE=1 "
                       "THEOURGIA_BARRIER=written-identity-read:" id-fifo " "
                       "scheme --script " id-child " > /dev/null 2> " id-trace
                       " & echo $! > " id-pid))
(define id-parked (wait-for-barrier id-trace "written-identity-read" 400))
(want "CONTROL: the child parks at the read this guard is about"
      id-parked
      #t)
;; The metadata goes away while the child is parked: opening, the
;; reservation and the segment barrier have all already succeeded.
(when id-parked
  (system (string-append "mv " id-store "/meta.sexp " id-base "/meta.aside"))
  (system (string-append "printf x > " id-fifo)))
(define id-answered (and id-parked (wait-for-file (string-append id-base "/answer") 400)))
;; THE ANSWER IS `unknown`, AND THAT IS RIGHT. The record is on the disk
;; by now -- opening, the reservation and the segment barrier have all
;; succeeded -- and what failed is the step that records how far records
;; reached. So the caller is told to ask again, which is the one answer
;; that is true about a store whose frontier could not be moved. What
;; matters is that it is not `ok`: an acknowledgement here is the
;; acknowledgement with nothing behind it.
(want "a write whose identity read fails is not acknowledged"
      (and id-answered
           (let ((t (text-of (string-append id-base "/answer"))))
             (list (and (crash-has-substring? t "error unknown") #t)
                   (and (crash-has-substring? t "(ok ") #t))))
      (list #t #f))
;; AND IT STOPPED BEFORE THE REGISTRY WAS READ. Without this the row
;; above is also passed by the other guard in the same procedure -- the
;; one that refuses when no entry matches -- which runs after
;; `read-registry` and emits `registry-check`. Two guards, one answer;
;; only the trace tells them apart.
(want "and it stopped before reading the registry, which is the other guard's place"
      (let* ((t (text-of id-trace))
             (after (let loop ((i 0))
                      (cond ((> (+ i 24) (string-length t)) "")
                            ((string=? (substring t i (+ i 24)) "barrier written-identity")
                             (substring t i (string-length t)))
                            (else (loop (+ i 1)))))))
        (crash-has-substring? after "registry-check"))
      #f)
(when id-parked
  (system (string-append "kill -9 $(cat " id-pid ") 2>/dev/null")))

;; THE OTHER ARM OF THE SAME GUARD. It refuses on two conditions -- the
;; store id unreadable, and the instance nonce absent -- and the rows
;; above move `meta.sexp`, which only ever exercises the first. A guard
;; with two arms and a case for one is a guard half of which nothing
;; would notice the loss of. The file moved is chosen by name rather than
;; by which read happens first: R6RS does not fix the order in which
;; those two initialisers are evaluated.
(define n-base (test-dir "log15instance"))
(define n-fifo (string-append n-base "/fifo"))
(define n-trace (string-append n-base "/trace"))
(define n-pid (string-append n-base "/pid"))
(define n-store (string-append n-base "/s"))
(define n-child (string-append n-base "/child.sc"))
(system (string-append "rm -rf " n-base "; mkdir -p " n-store " " n-base "/home"))
(put! n-child
      (string->utf8
        (string-append
          "#!r6rs\n(import (chezscheme) (theourgia log) (theourgia ffi)\n"
          "        (theourgia store) (theourgia reduce))\n"
          "(putenv \"THEOURGIA_HOME\" \"" n-base "/home\")\n"
          "(store-init! \"" n-store "\")\n"
          "(define a (guard (e (#t (list 'raised))) \n"
          "  (with-store-write \"" n-store "\" (lambda (st v)\n"
          "    '((insert root #f ((kind . section) (title . \"One\"))))))))\n"
          "(call-with-port (open-file-output-port \"" n-base "/answer\" (file-options no-fail))\n"
          "  (lambda (p) (put-bytevector p (string->utf8 (format \"~s\\n\" (car a))))))\n")))
(system (string-append "mkfifo " n-fifo))
(system (string-append "THEOURGIA_INJECT=on THEOURGIA_TRACE=1 "
                       "THEOURGIA_BARRIER=written-identity-read:" n-fifo " "
                       "scheme --script " n-child " > /dev/null 2> " n-trace
                       " & echo $! > " n-pid))
(define n-parked (wait-for-barrier n-trace "written-identity-read" 400))
(want "CONTROL: the child parks with the metadata left alone"
      n-parked
      #t)
(when n-parked
  (system (string-append "mv " n-store "/instance.sexp " n-base "/instance.aside"))
  (system (string-append "printf x > " n-fifo)))
(define n-answered (and n-parked (wait-for-file (string-append n-base "/answer") 400)))
(want "an absent instance nonce refuses the same way an unreadable id does"
      (and n-answered
           (let ((t (text-of (string-append n-base "/answer"))))
             (list (and (crash-has-substring? t "error unknown") #t)
                   (and (crash-has-substring? t "(ok ") #t))))
      (list #t #f))
(want "and it too stopped before reading the registry"
      (let* ((t (text-of n-trace))
             (after (let loop ((i 0))
                      (cond ((> (+ i 24) (string-length t)) "")
                            ((string=? (substring t i (+ i 24)) "barrier written-identity")
                             (substring t i (string-length t)))
                            (else (loop (+ i 1)))))))
        (crash-has-substring? after "registry-check"))
      #f)
(when n-parked
  (system (string-append "kill -9 $(cat " n-pid ") 2>/dev/null")))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "log15 complete\n")
