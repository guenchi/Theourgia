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

;; The machine registry and instance authority (design 4.1 / 5.2 step 7,
;; plan L19, L22, L24(f), L12's registry branch).
;;
;; WHY ANY OF THIS EXISTS: restore a backup of a store in place and
;; nothing inside the directory can tell. The identity triple is
;; unchanged, the owner is unchanged, the log is simply shorter -- every
;; check that reads only the store agrees that all is well. The water
;; mark is kept somewhere the backup did not cover, so that the one fact
;; that gives it away survives.
;;
;; AND WHY IT IS A PRECONDITION, NOT A RECORD: the mark is raised and
;; made durable BEFORE the log write, so the only disagreement a crash
;; can leave is registry-ahead-of-log. That is refused and repaired by
;; adopt. The other order would leave a log entry with no mark, and a
;; restore to just before it would look legitimate.
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia ffi)
        (theourgia trace))

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

(define bad 0)
(define (want label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(define d (test-dir "log13work"))
(define W "wwwc3q2a")
(define home (string-append d "-home"))
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
(define (len-of path)
  (let ((b (slurp path))) (if (bytevector? b) (bytevector-length b) b)))
(define (wpath n) (string-append d "/writers/" W "/" (segment-file-name n)))
(define (build!)
  (system (string-append "rm -rf " d " " d "-home; mkdir -p " d "/writers/" W " " d "/snap " home))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"c3\"))\n"))
  (file-ensure! (string-append d "/lock"))
  (put! (string-append d "/writers/" W "/owner.sexp") (string->utf8 "((machine \"m\"))\n"))
  (putenv "THEOURGIA_HOME" home)
  ;; ONLY THE LOCAL WRITER GETS AN owner.sexp -- that file is what makes
  ;; a writer local, so stamping every directory turned the mirrored
  ;; writer into a second local one.
  (let ((n (instance-install! d))) (owner-install! d W n))
  (put! (wpath 1) (cat (r 1) (r 2))))
(define (traced thunk)
  (let ((p (open-output-string)))
    (parameterize ((current-error-port p))
      (trace-enable! #t)
      (guard (e (#t (trace-enable! #f) (raise e))) (thunk))
      (trace-enable! #f))
    (get-output-string p)))
(define (lines-of text)
  (let ((n (string-length text)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((>= i n) (reverse (if (> i start) (cons (substring text start i) acc) acc)))
            ((char=? (string-ref text i) #\newline)
             (loop (+ i 1) (+ i 1) (cons (substring text start i) acc)))
            (else (loop (+ i 1) start acc))))))
(define (has-substring? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) sub) #t)
            (else (loop (+ i 1)))))))
(define (events text) (filter (lambda (l) (has-substring? l "(trace ")) (lines-of text)))
(define (op-of line)
  (let* ((i (+ 7 (let loop ((k 0)) (if (string=? (substring line k (+ k 7)) "(trace ") k (loop (+ k 1))))))
         (j (let scan ((j i)) (if (or (>= j (string-length line))
                                      (char=? (string-ref line j) #\space)) j (scan (+ j 1))))))
    (substring line i j)))
(define (append-one! s payload)
  (let ((v (session-view s)))
    (session-append! s (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                   (view-expect-seq v) "agent:claude" '() payload))))
(define (with-session proc)
  (parameterize ((log-clock (lambda () fixed-ts)))
    (let ((s (log-begin d (lambda args 'applied))))
      (let ((out (proc s))) (log-end! s) out))))
(define (registry-text) (let ((b (slurp (registry-path)))) (if (bytevector? b) (utf8->string b) b)))

;; ONE FIELD AT A TIME. The first version rewrote the whole file, so the
;; machine check fired and the other three were never the reason for
;; anything -- removing the inode comparison left every row green.
(define (instance-fields)
  (let* ((t (utf8->string (slurp (string-append d "/instance.sexp"))))
         (p (open-string-input-port t)))
    (read p)))
(define (rewrite-instance! machine device inode nonce)
  (put! (string-append d "/instance.sexp")
        (string->utf8 (string-append "((machine \"" machine "\")"
                                     " (device " (number->string device) ")"
                                     " (inode " (number->string inode) ")"
                                     " (nonce \"" nonce "\"))\n"))))
(define (field-of name)
  (let loop ((xs (instance-fields)))
    (cond ((null? xs) #f)
          ((eq? (caar xs) name) (cadr (car xs)))
          (else (loop (cdr xs))))))
(define (tamper-one field)
  (lambda ()
    (let ((m (field-of 'machine)) (dev (field-of 'device))
          (ino (field-of 'inode)) (n (field-of 'nonce)))
      (case field
        ((machine) (rewrite-instance! "other-machine" dev ino n))
        ((device) (rewrite-instance! m (+ dev 1) ino n))
        ((inode) (rewrite-instance! m dev (+ ino 1) n))
        ((nonce) (put! (string-append d "/instance.sexp")
                       (string->utf8 (string-append "((machine \"" m "\") (device "
                                                    (number->string dev) ") (inode "
                                                    (number->string ino) "))\n"))))))))
(printf "== L19(a): the commit sequence, named by path ==\n")
(build!)
(define a-trace (traced (lambda () (with-session (lambda (s) (append-one! s '(put "w.3" ())))))))
;; The whole point of the ordering is WHICH file each step touches, so
;; every step is identified by its path rather than by its op name.
(define (step-of l)
  (let ((op (op-of l)))
    (cond
      ((and (string=? op "flock") (has-substring? l (string-append d "/lock"))) "store-lock")
      ((and (string=? op "flock") (has-substring? l (string-append home "/lock"))) "machine-lock")
      ((and (string=? op "unlock") (has-substring? l (string-append d "/lock"))) "store-unlock")
      ((and (string=? op "unlock") (has-substring? l (string-append home "/lock"))) "machine-unlock")
      ((string=? op "catch-up") "catch-up")
      ((string=? op "frame") "frame")
      ((string=? op "registry-check") "registry-check")
      ((string=? op "registry-write") "registry-write")
      ((and (string=? op "fsync") (has-substring? l "instances.sexp")) "registry-fsync")
      ((and (string=? op "write") (has-substring? l "000001.sexp")) "write-log")
      ((and (string=? op "fsync") (has-substring? l "000001.sexp")) "fsync-log")
      ((string=? op "apply") "apply")
      (else #f))))
;; FROM THE APPEND'S OWN START. The session's takeover barrier flushes
;; and its delivery applies before any of this; folding them in would
;; make the row assert three mechanisms at once.
;; ⚠️ THE STORE LOCK IS OBSERVED, NOT ASSUMED. The first version consed
;; the string "store-lock" onto the front of the observed list, so the
;; row asserted an event it had written itself: removing the acquisition
;; entirely would have left it green. The sequence is taken from the
;; real flock event onward, with the barrier and delivery in between
;; dropped by the projection.
(want "store-lock, catch-up, frame, machine-lock, registry, then the log, then unlock"
      (let* ((es (events a-trace))
             (from-lock (let loop ((ls es))
                          (cond ((null? ls) '())
                                ((equal? (step-of (car ls)) "store-lock") ls)
                                (else (loop (cdr ls))))))
             (before-append
               (let loop ((ls from-lock) (acc '()))
                 (cond ((null? ls) (reverse acc))
                       ((string=? (op-of (car ls)) "catch-up")
                        (append (reverse acc) ls))
                       (else (loop (cdr ls) (if (equal? (step-of (car ls)) "store-lock")
                                                (cons (car ls) acc)
                                                acc)))))))
        (filter (lambda (x) x) (map step-of before-append)))
      '("store-lock" "catch-up" "frame" "machine-lock" "registry-check" "registry-write"
        "registry-fsync" "machine-unlock" "write-log" "fsync-log" "apply" "store-unlock"))
;; THE EXACT ENTRY. Three unrelated substrings are satisfied by a file
;; that happens to contain them in any arrangement -- including one that
;; recorded the wrong sequence for the wrong writer.
(want "the registry carries exactly this store, instance, writer and mark"
      (let* ((t (registry-text))
             (d* (guard (e (#t 'unreadable)) (read (open-string-input-port t))))
             (nonce (field-of 'nonce)))
        (and (list? d*) (= 1 (length d*))
             (equal? (list-head (car d*) 4) (list "c3" nonce W 3))))
      #t)

(printf "== L19(b): registry ahead of the log is a rollback ==\n")
;; The in-place restore, which is the case nothing inside the store can
;; see: commit two records, then put the log back the way it was.
(build!)
(define before-restore (slurp (wpath 1)))
(with-session (lambda (s) (append-one! s '(put "w.3" ()))))
(with-session (lambda (s) (session-applied! s 0 (list (cons W 3)))
                         (append-one! s '(put "w.4" ()))))
(put! (wpath 1) before-restore)
(want "the store looks untouched from inside, and the append is still refused"
      (let ((res (with-session (lambda (s) (append-one! s '(put "w.3" ()))))))
        (list (car res) (cadr res)))
      (list 'refused-before-reserve 'registry-ahead))
(want "and nothing was appended"
      (equal? (slurp (wpath 1)) before-restore) #t)
;; CONTROL: the SAME machine identity, an EMPTY registry. Pointing at a
;; fresh home would also mint a fresh machine nonce, and the append would
;; then be refused for identity rather than for the mark -- a control
;; that varies two things at once cannot show which one refused.

;; AND THE PREMISE IS MEASURED, NOT ASSUMED. This home is a sibling that
;; nothing else clears, and the scratch name carries a process id that
;; the system reuses, so "empty registry" could quietly be someone
;; else's registry. It is emptied here and the emptiness is part of the
;; answer: a control whose premise is checked by nothing is not a
;; control.
(want "CONTROL: same machine, empty registry, the restored store writes fine"
      (let ((alt (string-append d "-home2")))
        (system (string-append "rm -rf " alt "; mkdir -p " alt
                               "; cp " home "/machine.sexp " alt "/"))
        (let ((empty (not (file-exists? (string-append alt "/instances.sexp")))))
          (putenv "THEOURGIA_HOME" alt)
          (let ((res (with-session (lambda (s) (append-one! s '(put "w.3" ()))))))
            (putenv "THEOURGIA_HOME" home)
            (list empty (car res)))))
      (list #t 'committed))

(printf "== L24(f): every append re-verifies the authority ==\n")
;; Not just the number. P opens W, Q adopts W into U, P then takes the
;; lock: W's last sequence still equals the mark, so a number check
;; passes -- and W has been retired.
(define (tamper-and-append! change)
  (build!)
  (with-session (lambda (s) (append-one! s '(put "w.3" ()))))
  (change)
  (let ((res (with-session
               (lambda (s)
                 (session-applied! s 0 (list (cons W 3)))
                 (let ((v (session-view s)))
                   (if v
                       (append-one! s '(put "w.4" ()))
                       ;; No usable view is itself an outcome: the frame
                       ;; is built by hand so the product's refusal is
                       ;; what gets reported, not a crash in the fixture.
                       (session-append! s (make-frame 0 0 W 4 "agent:claude" '()
                                                      '(put "w.4" ())))))))))
    (list (car res) (cadr res))))
(want "a retired writer is refused even though its sequence still matches"
      (tamper-and-append!
        (lambda ()
          (put! (string-append d "/writers/" W "/retired.sexp")
                (string->utf8 (string-append "((prefix 1 " (number->string (* 3 R)) " 3) (tx \"t\"))\n")))))
      (list 'refused-before-reserve 'retired))
(want "a changed machine is refused, and named"
      (tamper-and-append! (tamper-one 'machine))
      (list 'refused-before-reserve (list 'instance 'machine)))
(want "a changed device is refused, and named"
      (tamper-and-append! (tamper-one 'device))
      (list 'refused-before-reserve (list 'instance 'device)))
(want "a changed inode is refused, and named"
      (tamper-and-append! (tamper-one 'inode))
      (list 'refused-before-reserve (list 'instance 'inode)))
(want "a missing nonce is refused, and named"
      (tamper-and-append! (tamper-one 'nonce))
      (list 'refused-before-reserve (list 'instance 'nonce)))
(want "a malformed instance file is refused, not ignored"
      (tamper-and-append!
        (lambda () (put! (string-append d "/instance.sexp") (string->utf8 "((oops\n"))))
      (list 'refused-before-reserve 'instance-malformed))
(want "a missing instance file is refused, not treated as permission"
      (tamper-and-append!
        (lambda () (system (string-append "rm -f " d "/instance.sexp"))))
      (list 'refused-before-reserve 'no-instance))
(want "CONTROL: untouched, the same second append commits"
      (tamper-and-append! (lambda () (if #f #f)))
      (list 'committed 4))

(printf "== the two outcomes only a fault can reach ==\n")
;; Both need a child process: THEOURGIA_INJECT is an expansion-time gate,
;; and a fault armed here would apply to every append in the file.
(define child-path (string-append d "/child.ss"))
(define child-out (string-append d "/child.out"))
(define (write-child!)
  (put! child-path
        (string->utf8
          (string-append
            "#!chezscheme\n(import (chezscheme) (theourgia log) (theourgia ffi))\n"
            "(putenv \"THEOURGIA_HOME\" \"" d "-home\")\n"
            "(define res\n"
            "  (guard (e (#t (list 'raised)))\n"
            "    (parameterize ((log-clock (lambda () " (number->string fixed-ts) ")))\n"
            "      (let* ((s (log-begin \"" d "\" (lambda args 'applied)))\n"
            "             (v (session-view s))\n"
            "             (r (session-append! s (make-frame (view-revision v) (view-epoch v)\n"
            "                                               (view-writer v) (view-expect-seq v)\n"
            "                                               \"agent:claude\" '() '(put \"w.3\" ())))))\n"
            "        (log-end! s) r))))\n"
            "(printf \"~s ~s ~s\\n\" (car res) (file-size \"" (wpath 1) "\")\n"
            "        (if (file-exists? \"" d "-home/instances.sexp\") 'registry 'no-registry))\n"))))
(define (child-says fault)
  (build!)
  (write-child!)
  (system (string-append (if fault (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=" fault " ") "")
                         "scheme --script " child-path " > " child-out " 2>/dev/null"))
  (let ((text (let ((b (slurp child-out))) (if (bytevector? b) (utf8->string b) ""))))
    (guard (e (#t (list 'unreadable text)))
      (let ((p (open-string-input-port text))) (list (read p) (read p) (read p))))))

(want "CONTROL: nothing armed, the child commits and the registry records it"
      (child-says #f) (list 'committed (* 3 R) 'registry))
;; RESERVED AND NOTHING WRITTEN. write-eio-after-partial cannot produce
;; this -- it fails only after its partial lands -- so the outcome that
;; means "the mark is up and the log is untouched, a retry is safe" had
;; no way to be reached at all.
(want "a write that fails on its first byte is reserved-not-written"
      (child-says "write-eio-first@commit:file=000001.sexp")
      (list 'reserved-not-written (* 2 R) 'registry))
;; A PROBE THAT CANNOT ANSWER IS NOT AN ANSWER OF NO. Before this fault
;; existed, restoring the swallowed fallback behaved identically on every
;; input that could be built, so the rule had no witness.
(want "a failed size probe stops the append instead of skipping rotation"
      (child-says "stat-fail@commit:file=000001.sexp")
      (list 'raised (* 2 R) 'no-registry))

(printf "== L12: the registry fsync branch ==\n")
;; The registry is the commit's precondition, so a registry that cannot
;; be made durable must leave the log untouched -- not merely unflushed.
;; NO REGISTRY FILE AT ALL is the right reading: the mark is installed by
;; atomic-write!, whose rename happens only after the flush, so a flush
;; that fails leaves the old contents in place -- here, none. The point
;; of the row is the log: a mark that could not be made durable must not
;; be followed by a record.
(want "a registry fsync failure fails the append with the log untouched"
      (child-says "fsync-fail@registry:file=instances.sexp")
      (list 'raised (* 2 R) 'no-registry))

(printf "== L19(b'): a reservation that outlives its write ==\n")
;; The mark is raised before the write, so a crash between them leaves
;; the registry ahead. The next attempt must refuse rather than write a
;; record whose sequence is already spoken for.
(build!)
(write-child!)
(system (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=write-eio-first@commit:file=000001.sexp "
                       "scheme --script " child-path " > " child-out " 2>/dev/null"))
(want "after a reserved-but-unwritten record, the next append is refused"
      (let ((res (with-session (lambda (s) (append-one! s '(put "w.3" ()))))))
        (list (car res) (cadr res)))
      (list 'refused-before-reserve 'registry-ahead))
(want "and the log is still what it was"
      (len-of (wpath 1)) (* 2 R))

(printf "== the nonce is the registry key, so it is verified ==\n")
;; Changing the nonce alone -- machine, device and inode all still right
;; -- moves the registry lookup to an entry that does not exist, so the
;; mark that would have refused a restored log is simply not found. The
;; row restores the log too, which is what makes the bypass visible: a
;; nonce check that is only "is it present" leaves this green.
(build!)
(define nonce-before (slurp (wpath 1)))
(with-session (lambda (s) (append-one! s '(put "w.3" ()))))
(put! (wpath 1) nonce-before)
(rewrite-instance! (field-of 'machine) (field-of 'device) (field-of 'inode) "a-different-nonce")
(want "a store whose nonce was swapped cannot write past the old mark"
      (let ((res (with-session (lambda (s) (append-one! s '(put "w.3" ()))))))
        (list (car res) (cadr res)))
      (list 'refused-before-reserve (list 'instance 'nonce)))
(want "and the log is untouched"
      (equal? (slurp (wpath 1)) nonce-before) #t)

(printf "== the machine home is not allowed to be the store ==\n")
;; The store lock is already held when the reservation runs, so a home
;; inside the store would make it wait for a lock this very call stack
;; holds -- a hang, which is the failure a suite reports worst.
(build!)
(want "a machine home whose lock IS the store lock is refused, not deadlocked"
      (begin (putenv "THEOURGIA_HOME" d)
             (let ((out (guard (e (#t 'refused))
                          (with-session (lambda (s) (append-one! s '(put "w.3" ()))))
                          'accepted)))
               (putenv "THEOURGIA_HOME" home)
               out))
      'refused)

(printf "\n~a failures\n" bad)
(printf "log13 complete\n")
