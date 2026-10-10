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
    ;; A SESSION HERE ENDS THE WAY A REQUEST ENDS: append, then the
    ;; commit barrier, then release the lock. Durability is per REQUEST
    ;; now -- an append no longer fsyncs -- so a session that stopped
    ;; before the barrier would leave nothing for the rows below to see.
    (let ((s (log-begin d (lambda args 'applied))))
      (let ((out (proc s)))
        (guard (e (#t (if #f #f))) (session-commit! s))
        (log-end! s)
        out))))
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
;; NOTE: THE STORE LOCK IS OBSERVED, NOT ASSUMED. The first version consed
;; the string "store-lock" onto the front of the observed list, so the
;; row asserted an event it had written itself: removing the acquisition
;; entirely would have left it green. The sequence is taken from the
;; real flock event onward, with the barrier and delivery in between
;; dropped by the projection.
;; THE LOG'S FLUSH MOVED TO THE END OF THE REQUEST. It used to sit
;; between the write and the apply, one per record; it is now the
;; request's barrier, after every record and before the lock is
;; released. The registry's own flush is unmoved: it belongs to the
;; reservation, which still happens before the write.
(want "reserve, write, apply, flush, then record how far it reached"
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
        ;; THREE REGISTRY FLUSHES AND TWO MACHINE-LOCK SECTIONS, and
        ;; each is a different sentence.
        ;;
        ;; The FIRST section reserves: it raises `authorised`, which is
        ;; leave to write at these positions, and it happens before the
        ;; write because a record written without leave is a record
        ;; nothing vouches for.
        ;;
        ;; The middle `registry-fsync` is the BARRIER'S: the registry is
        ;; a row of the recovery closure -- it is what says this machine
        ;; holds the store at all -- so the barrier flushes it along
        ;; with the segment.
        ;;
        ;; The LAST section records `written`: how far records actually
        ;; reached the disk. It comes AFTER the flush that made that
        ;; true, and it is the one number the rollback gate trusts --
        ;; raising it any earlier would be promising on the barrier's
        ;; behalf.
        "registry-fsync" "machine-unlock" "write-log" "apply"
        "fsync-log" "registry-fsync"
        "machine-lock" "registry-check" "registry-write" "registry-fsync"
        "machine-unlock" "store-unlock"))
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

;; THE FRONTIER IS RAISED ON A REAL ENTRY UNDER THE STORE'S REAL NAME, OR
;; THE ANSWER SAYS SO. Two guards sit in that one procedure and they are
;; exercised elsewhere, because neither is reachable from here: the
;; identity half needs the metadata to read at open and fail at that one
;; call, which `log15` reaches by parking the process at a rendezvous and
;; moving the file aside; the entry half needs a path that reconciles
;; without reserving, which is a resend, and `q7` drives one.
;;
;; Named here because this is the section about that number, and a reader
;; looking for its guards should be told where they are rather than
;; conclude there are none.

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
(define child-path (string-append d "/child.sc"))
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
;; A RESERVATION THAT WENT UNUSED IS ORDINARY NOW, AND IT COST A FORMAT
;; CHANGE TO MAKE IT SO. The registry entry says two things: `authorised`
;; is leave to write at these positions, `written` is how far records
;; actually reached the disk. A crash between the two leaves `authorised`
;; above the log's end -- and so does every refusal, and so does every
;; request that reserves its whole range and uses part of it.
;;
;; So the next append PROCEEDS. It used to be refused, and that refusal
;; was never the intent: it was a consequence of the registry holding one
;; number that had to mean both things at once. A store could be stopped
;; by a crash that lost nothing.
;;
;; WHAT STILL REFUSES IS THE ROW BELOW: `written` above the log's end,
;; which is history the store was told it had and no longer has. That is
;; the question this file exists to answer, and it is unchanged.
(build!)
(write-child!)
(system (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=write-eio-first@commit:file=000001.sexp "
                       "scheme --script " child-path " > " child-out " 2>/dev/null"))
(want "a reservation nobody used does not stop the next append"
      (let ((res (with-session (lambda (s) (append-one! s '(put "w.3" ()))))))
        (car res))
      'committed)
(want "and the record it wrote is there"
      (len-of (wpath 1)) (* 3 R))
;; TWIN: and the registry says so -- `authorised` had already been raised
;; by the attempt that crashed, and `written` caught up only when the
;; barrier made the record durable. Two facts, and only the second one
;; moved.
(want "TWIN: written caught up to the record, and only at the barrier"
      (let* ((t (registry-text))
             (d* (guard (e (#t 'unreadable)) (read (open-string-input-port t))))
             (e (and (list? d*) (pair? d*) (car d*))))
        (and (list? e) (= 6 (length e)) (list (list-ref e 3) (list-ref e 5))))
      (list 3 3))

(printf "== one shape, and the old one is converted rather than tolerated ==\n")
;; A WATER MARK USED TO BE A SINGLE NUMBER meaning both "authorised to
;; write here" and "written this far". They are two facts now, and a
;; five-element entry is the moment before anyone noticed. Its `written`
;; is its mark, because that is exactly what the mark meant while the two
;; were the same.
;;
;; IT IS CONVERTED ONCE, ON THE NEXT LOAD UNDER THE MACHINE LOCK -- not
;; read leniently for ever. A format that can be read two ways is a
;; format two readers will eventually disagree about, and the
;; disagreement would be about whether a store rolled back.
(build!)
(with-session (lambda (s) (append-one! s '(put "w.3" ()))))
(define (put-registry! text)
  (call-with-port (open-file-output-port (registry-path) (file-options no-fail))
    (lambda (o) (put-bytevector o (string->utf8 text)))))
(define five-element
  (let* ((t (registry-text))
         (d* (read (open-string-input-port t)))
         (e (car d*)))
    (list-head e 5)))
(want "CONTROL: the entry this fixture just made has six elements"
      (length (car (read (open-string-input-port (registry-text)))))
      6)
;; THE GUARD IS PART OF THE ROW. Without the upgrade every reader of the
;; sixth field raises on the five-element entry this row plants, and an
;; unguarded row would end the file there with no failure count -- which
;; reads exactly like a run nobody made.
(want "a five element entry is upgraded on the next load, written = its mark"
      (guard (e (#t (list 'raised)))
        (put-registry! (format "~s\n" (list five-element)))
        ;; any load under the machine lock does it; an append is one
        (with-session (lambda (s) (session-applied! s 0 (list (cons W 3)))
                                  (append-one! s '(put "w.4" ()))))
        (let ((e (car (read (open-string-input-port (registry-text))))))
          (list (length e) (list-ref e 5))))
      (list 6 4))
;; TWIN: and the upgrade is what the ROLLBACK GATE then reads. Before it,
;; `written` would have been missing entirely.
(want "TWIN: and the upgraded entry still names this store, instance and writer"
      (let ((e (car (read (open-string-input-port (registry-text))))))
        (list (list-ref e 0) (list-ref e 2) (list-ref e 4)))
      (list (list-ref five-element 0) (list-ref five-element 2) 'active))

;; A STORE ID THAT IS NOT A STRING IS NOT A STORE ID. `format-1?` checks
;; the format field and nothing else, so metadata saying `(store-id 7)`
;; reaches the registry -- and an entry keyed by 7 is one the water-mark
;; reader does not recognise, so `written` is never raised for it and
;; every reservation appends a duplicate. The store would then
;; acknowledge writes with no working rollback witness.
;;
;; "unknown" IS THE SAME ANSWER AS AN ABSENT ID, deliberately: both are
;; "this metadata does not say", and neither may become a key of a shape
;; the rest of the file cannot read.
(build!)
(want "a store id that is not a string is not used as one"
      (begin
        (put! (string-append d "/meta.sexp")
              (string->utf8 "((format 1) (store-id 7))\n"))
        (store-id-of d))
      "unknown")
(want "TWIN: a string one is used as it stands"
      (begin
        (put! (string-append d "/meta.sexp")
              (string->utf8 "((format 1) (store-id \"real\"))\n"))
        (store-id-of d))
      "real")
;; TWIN: and an absent id answers the same way, which is what makes the
;; row above a statement about the shape rather than about that one value.
(want "TWIN: and an absent id answers the same"
      (begin
        (put! (string-append d "/meta.sexp") (string->utf8 "((format 1))\n"))
        (store-id-of d))
      "unknown")
;; AND THE ROW IS ABOUT THE SHAPE, SO IT HAS TO ASK ABOUT MORE THAN ONE
;; WRONG SHAPE. A check written as "not a number" rather than "is a
;; string" passes the row above and still lets a symbol, a boolean or a
;; list become a registry key. The empty list is the one that would
;; survive longest unnoticed: in Scheme it is TRUE, so a guard written
;; as `(if id id "unknown")` hands it straight back.
(for-each
  (lambda (pair)
    (want (string-append "a store id written as " (car pair) " is not used as one")
          (begin
            (put! (string-append d "/meta.sexp")
                  (string->utf8 (string-append "((format 1) (store-id " (cdr pair) "))\n")))
            (store-id-of d))
          "unknown"))
  (list (cons "a symbol" "sym")
        (cons "true" "#t")
        (cons "false" "#f")
        (cons "the empty list" "()")
        (cons "a list" "(a b)")
        (cons "a vector" "#(1 2)")
        ;; AN IMPROPER PAIR IS NOT A LIST, so a check written as a
        ;; blacklist of the shapes above -- number, symbol, boolean,
        ;; list, vector -- lets it through. It is the shape a blacklist
        ;; reaches last, which is what makes it worth a row.
        (cons "an improper pair" "(a . b)")
        (cons "a string beside something else" "(\"s\" . 1)")
        ;; A CHARACTER AND A BYTEVECTOR are the two remaining datum
        ;; classes the extended reader can produce, and a blacklist long
        ;; enough to cover everything above still admits them. The list
        ;; is here to stop being the thing under test: what is being
        ;; asked is "is it a string", and the answer must not depend on
        ;; how many other shapes anyone thought of.
        ;; THE SYNTAX THIS CODEC DOES NOT HAVE. `#\a` and `#vu8(1 2)`
        ;; are Scheme, but metadata is read with the extended codec, and
        ;; that codec RAISES on both -- the guard turns the raise into
        ;; #f and the answer is "unknown" without the check ever seeing
        ;; a character or a bytevector. Kept, and labelled for what it
        ;; actually is: unreadable metadata answers like absent
        ;; metadata. It is not evidence about either type.
        (cons "syntax the codec cannot read" "#\\a")))
;; A BYTEVECTOR, SPELLED THE WAY THIS CODEC SPELLS ONE. `#vu8"AQI="` is
;; the form that parses, and it is the shape a check written as a
;; blacklist of the types above reaches last.
;;
;; THE STIMULUS IS ASSERTED BEFORE THE ANSWER IS. A row whose input
;; silently failed to parse would read exactly like a row whose input was
;; correctly refused -- both say "unknown" -- so this one first requires
;; that the text really did become a bytevector.
(want "CONTROL: this text really does parse to a bytevector"
      (let ((d (guard (e (#t 'raised))
                 (string->sexpr-extended "((format 1) (store-id #vu8\"AQI=\"))"))))
        (and (pair? d) (bytevector? (cadr (assq 'store-id d)))))
      #t)
(want "a store id written as a bytevector is not used as one"
      (begin
        (put! (string-append d "/meta.sexp")
              (string->utf8 "((format 1) (store-id #vu8\"AQI=\"))\n"))
        (store-id-of d))
      "unknown")
(build!)

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
(printf "rows: ~a\n" rows-run)
(printf "log13 complete\n")
