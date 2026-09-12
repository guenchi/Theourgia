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

;; Write-side refusals and the readiness gate (plan L4', L19(d), L24 c d k).
;;
;; INTEGRITY IS PER WRITER, NOT A GLOBAL BOOLEAN. Another writer's
;; damaged segment stops that writer and nothing else; damage in this
;; writer's own history stops writing here and the only way out is
;; adopt. The two are tested against the same store so that "everything
;; stopped" cannot pass for either.
;;
;; AND THE GATE HAS TWO SIDES. A record of this writer that the reducer
;; answered `pending` means the append's own predecessor is not applied,
;; so the facts the next record would carry would be computed against a
;; state without it. Another writer's pending records are unrelated
;; history: the log is not a total order and refusing there would be a
;; different, wrong rule.
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia ffi)
        (theourgia trace)
        (only (igropyr crypto) sha256 bytevector->hex))

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

(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (want-1 label (caught got) (caught expect))))))

(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))

(define base (test-dir "log14"))
(define d (string-append base "/store"))
;; THE REGISTRY LIVES OUTSIDE THE STORE. That is the whole reason it can
;; see a rollback: a backup of the store directory does not contain it.
;; The earlier fixture kept it under the store and so could not have
;; shown the difference at all.
(define home (string-append base "/machine-home"))
(define W "wwwd4q2a")
(define M "mmmd4q2a")
(define fixed-ts 1757300000003)
(define (rec seq ts deps payload)
  (encode-record seq ts "agent:claude" deps (storable-encode payload)))
(define (r tag n) (rec n (+ 1757300000000 n) '()
                       (list 'put (string-append tag "." (number->string n)) '())))
(define R (bytevector-length (r "w" 1)))
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
(define (wpath w n) (string-append d "/writers/" w "/" (segment-file-name n)))
;; EVERYTHING A REFUSED APPEND COULD HAVE TOUCHED, not just the segment
;; files: a refusal that raised the water mark or rewrote a manifest
;; stayed green while only segments were compared.
(define (tree-bytes)
  (list (map (lambda (w)
               (cons w (list (map (lambda (n) (cons n (slurp (wpath w n))))
                                  (enumerate-segment-files d w))
                             (slurp (string-append d "/writers/" w "/owner.sexp"))
                             (slurp (string-append d "/writers/" w "/published.sexp"))
                             (slurp (string-append d "/writers/" w "/retired.sexp")))))
             (store-writers d))
        (slurp (string-append d "/instance.sexp"))
        (slurp (string-append d "/meta.sexp"))
        (slurp (registry-path))))

;; THE RANGE A SEGMENT HOLDS, READ OUT OF THE SEGMENT. A manifest entry
;; declares first and last sequence beside the hash. A fixture that
;; declared them from memory would be asserting its own arithmetic
;; rather than what it actually wrote, and the product's own check for a
;; manifest that contradicts its bytes would then be measuring the
;; fixture.
(define (segment-seqs bytes)
  (let ((text (utf8->string bytes)))
    (let loop ((i 0) (start 0) (seqs '()))
      (cond
        ((>= i (string-length text)) (reverse seqs))
        ((char=? (string-ref text i) #\newline)
         (let ((r (decode-line (string->utf8 (substring text start (+ i 1))))))
           (loop (+ i 1) (+ i 1)
                 (if (and (pair? r) (eq? (car r) 'ok)) (cons (cadr r) seqs) seqs))))
        (else (loop (+ i 1) start seqs))))))

(define (manifest-entry n hash bytes)
  (let ((seqs (segment-seqs bytes)))
    (if (null? seqs)
        (list n hash 1 1)
        (list n hash (apply min seqs) (apply max seqs)))))

(define (manifest-entry-text n hash bytes)
  (let ((e (manifest-entry n hash bytes)))
    (string-append "(" (number->string n) " \"" hash "\" "
                   (number->string (caddr e)) " " (number->string (cadddr e)) ")")))

(define (publish! . ns)
  (put! (string-append d "/writers/" M "/published.sexp")
        (string->utf8
          (string-append "("
            (apply string-append
              (map (lambda (n)
                     (manifest-entry-text n (bytevector->hex (sha256 (slurp (wpath M n))))
                                          (slurp (wpath M n))))
                   ns))
            ")\n"))))
(define (build!)
  (system (string-append "rm -rf " base "; mkdir -p " d "/writers/" W " " d "/writers/" M
                         " " d "/snap " home))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"d4\"))\n"))
  (file-ensure! (string-append d "/lock"))
  (putenv "THEOURGIA_HOME" home)
  (put! (wpath W 1) (cat (r "w" 1) (r "w" 2)))
  (put! (wpath M 1) (r "m" 1))
  (publish! 1)
  (let ((n (instance-install! d))) (owner-install! d W n)))
;; A MISSING VIEW IS AN OUTCOME, NOT A CRASH. Under a mutation that
;; changes when the view is withheld, the steps AFTER the one being
;; tested reach this with #f -- and an exception there ends the file with
;; zero FAIL lines, which reads exactly like a clean run.
(define (append-one! s payload)
  (let ((v (session-view s)))
    (if v
        (session-append! s (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                       (view-expect-seq v) "agent:claude" '() payload))
        (list 'no-view #f))))
;; A SESSION HERE ENDS THE WAY A REQUEST ENDS: append, then the commit
;; barrier, then release the lock. The barrier is what tells the registry
;; how far records reached the disk, and `written` is the number the
;; rollback gate reads -- so a session that stopped before it would leave
;; the registry saying nothing was ever written, and the restore these
;; rows are about would go undetected.
(define (with-session proc)
  (parameterize ((log-clock (lambda () fixed-ts)))
    (let ((s (log-begin d (lambda args 'applied))))
      (let ((out (proc s)))
        (guard (e (#t (if #f #f))) (session-commit! s))
        (log-end! s)
        out))))
(define (try-append)
  (parameterize ((log-clock (lambda () fixed-ts)))
    (let ((s (log-begin d (lambda args 'applied))))
      (let* ((v (session-view s))
             (res (if v
                      (append-one! s '(put "w.3" ()))
                      ;; No usable view is itself an outcome; the frame is
                      ;; built by hand so the product's refusal is what is
                      ;; reported rather than a crash in the fixture.
                      (session-append! s (make-frame 0 0 W 3 "agent:claude" '()
                                                     '(put "w.3" ()))))))
        (log-end! s)
        (list (car res) (cadr res))))))

(printf "== L4'(a): another writer's unlisted segment is ignored ==\n")
(build!)
(put! (wpath M 2) (cat (r "m" 2) (let ((h (r "m" 3)))
                                   (let ((o (make-bytevector 20)))
                                     (bytevector-copy! h 0 o 0 20) o))))
(want "the unlisted file is not an integrity error, and the local writer still writes"
      (list (try-append)
            (let ((s (log-begin d (lambda args 'applied))))
              (let ((k (map (lambda (e) (log-error-kind (cdr e)))
                            (load-integrity (session-load s)))))
                (log-end! s) k)))
      (list (list 'committed 3) '()))

(printf "== L4'(a'): appending to another writer's published segment ==\n")
(build!)
(let ((existing (slurp (wpath M 1))))
  (put! (wpath M 1) (cat existing (r "m" 2))))
(want "that writer stops with a hash error, and this one still writes"
      (list (try-append)
            (let ((s (log-begin d (lambda args 'applied))))
              (let ((k (map (lambda (e) (log-error-kind (cdr e)))
                            (load-integrity (session-load s)))))
                (log-end! s) k)))
      (list (list 'committed 3) '(manifest-hash)))

(printf "== L4'(b)(c)(e): damage in this writer's own history ==\n")
;; Three shapes that are NOT the repairable one. The repairable shape is
;; a residue with no terminating newline; each of these has a newline, so
;; each is indistinguishable from bytes that were flushed and then rotted.
(define (own-damage! kind)
  (build!)
  (case kind
    ((skip) (put! (wpath W 1) (cat (r "w" 1) (r "w" 3))))
    ((bad-then-good)
     (put! (wpath W 1) (cat (r "w" 1)
                            (let ((b (r "w" 2)))
                              (bytevector-u8-set! b 3 (if (= 48 (bytevector-u8-ref b 3)) 49 48))
                              b)
                            (r "w" 3))))
    ((crc-with-newline)
     (put! (wpath W 1) (cat (r "w" 1)
                            (let ((b (r "w" 2)))
                              (bytevector-u8-set! b 3 (if (= 48 (bytevector-u8-ref b 3)) 49 48))
                              b))))))
(define (refuses-and-leaves-everything kind)
  (own-damage! kind)
  (let* ((before (tree-bytes))
         (res (try-append))
         (after (tree-bytes)))
    (list (car res) (cadr res) (equal? before after))))
(want "a sequence gap refuses the write and changes no byte"
      (refuses-and-leaves-everything 'skip)
      (list 'refused-before-reserve 'integrity #t))
(want "a bad line followed by a good one refuses the write and changes no byte"
      (refuses-and-leaves-everything 'bad-then-good)
      (list 'refused-before-reserve 'integrity #t))
(want "a last line with a bad CRC but a terminating newline refuses too"
      (refuses-and-leaves-everything 'crc-with-newline)
      (list 'refused-before-reserve 'integrity #t))
;; THE GREEN TWIN, and it is the whole point of the fault model: the same
;; damage WITHOUT the terminating newline is the one shape that is
;; repairable, and it must still commit.
(build!)
(put! (wpath W 1) (cat (r "w" 1) (r "w" 2)
                       (let ((h (r "w" 3)))
                         (let ((o (make-bytevector 20)))
                           (bytevector-copy! h 0 o 0 20) o))))
;; AND THE BYTES IT LEAVES. Asserting only "committed" leaves green an
;; implementation that never truncated and appended after the residue.
(want "CONTROL: the same tail without a newline is repairable and commits"
      (list (try-append)
            (equal? (slurp (wpath W 1))
                    (cat (r "w" 1) (r "w" 2) (rec 3 fixed-ts '() '(put "w.3" ())))))
      (list (list 'committed 3) #t))
;; And a second attempt after a refusal still refuses, naming the remedy.
(own-damage! 'skip)
(want "a stopped writer stays stopped, and says what to do about it"
      ;; THE FIRST APPEND HAS TO REACH THE CATCH-UP to poison the
      ;; session, so its frame is built from the session's own view. A
      ;; hand-made frame was refused earlier, on the binding, and the
      ;; writer was never marked stopped at all.
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda args 'applied))))
          (let ((v (session-view s)))
            (session-append! s (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                           (view-expect-seq v) "agent:claude" '()
                                           '(put "w.9" ()))))
          (let ((second (guard (e ((log-error? e)
                                   (list (log-error-kind e)
                                         (cdr (assq 'remedy (log-error-detail e)))))
                                  (#t 'other))
                          (session-append! s (make-frame 0 0 W 3 "agent:claude" '()
                                                         '(put "w.3" ())))
                          'accepted)))
            (log-end! s)
            second)))
      (list 'writer-stopped 'adopt))

(printf "== L24(c): the readiness gate has two sides ==\n")
(build!)
;; THIS writer's own record left pending: the append's predecessor is not
;; applied, so its facts would be computed against a state without it.
(want "a pending record of this writer refuses the append until it is confirmed"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda (w seg off seq ts actor deps payload)
                                (if (string=? w W) 'pending 'applied)))))
          (let* ((before (session-view s))
                 (applied-while-pending
                   (let ((e (assoc W (map (lambda (f)
                                            (cons (car f) (cdr (assq 'applied (cdr f)))))
                                          (session-frontiers s)))))
                     (if e (cdr e) 'missing)))
                 (refused (session-append! s (make-frame 0 0 W 3 "agent:claude" '()
                                                         '(put "w.3" ()))))
                 (_ (session-applied! s 0 (list (cons W 2))))
                 (after (session-view s))
                 (ok (append-one! s '(put "w.3" ()))))
            (log-end! s)
            ;; THE FRONTIER ITSELF, so that a separate readiness flag
            ;; moving independently of the cursor cannot pass.
            (list before (car refused) (cadr refused) (and after #t) (car ok)
                  applied-while-pending))))
      (list #f 'refused-before-reserve 'predecessor-not-applied #t 'committed 0))
;; ANOTHER writer's pending record is unrelated history. Refusing here
;; would be a different rule, and a stricter one than the design states.
(build!)
(want "CONTROL: another writer's pending record does not block this one"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda (w seg off seq ts actor deps payload)
                                (if (string=? w M) 'pending 'applied)))))
          (let ((res (append-one! s '(put "w.3" ()))))
            (log-end! s)
            (car res))))
      'committed)

(printf "== a confirmation cannot exceed what exists ==\n")
;; An impossible confirmation used to be accepted, and the gate then
;; compared "applied 99" against "reaches 2" and handed out a view for
;; history that was never there.
(build!)
(want "confirming past the end of that writer's history is refused"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda args 'pending))))
          (let ((out (list (guard (e (#t 'refused))
                             (session-applied! s 0 (list (cons W 99)))
                             'accepted)
                           (and (session-view s) #t))))
            (log-end! s) out)))
      (list 'refused #f))
(want "CONTROL: confirming exactly what exists is accepted and opens the gate"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda args 'pending))))
          (let ((out (list (guard (e (#t 'refused))
                             (session-applied! s 0 (list (cons W 2)))
                             'accepted)
                           (and (session-view s) #t))))
            (log-end! s) out)))
      (list 'accepted #t))
;; And a record this session just wrote may be confirmed, even though the
;; discovery it opened with ends earlier.
(build!)
(want "CONTROL: this session's own commit may be confirmed"
      (with-session
        (lambda (s)
          (append-one! s '(put "w.3" ()))
          (guard (e (#t 'refused)) (session-applied! s 0 (list (cons W 3))) 'accepted)))
      'accepted)

(printf "== a stopped writer advertises no readiness ==\n")
;; The preparation interface must not offer a view the write interface
;; will permanently refuse.
(own-damage! 'skip)
(want "after integrity stops the writer, there is no view and the reason says so"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda args 'applied))))
          (let ((v (session-view s)))
            (session-append! s (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                           (view-expect-seq v) "agent:claude" '()
                                           '(put "w.9" ())))
            ;; The second append RAISES rather than returning a refusal
            ;; -- a stopped writer is not a per-call outcome, it is a
            ;; state the session stays in. What this row adds to that is
            ;; the view: the preparation interface must stop advertising
            ;; readiness the moment the write interface stops honouring
            ;; it, and it did not.
            (let ((after (session-view s))
                  (again (guard (e ((log-error? e) (log-error-kind e)) (#t 'other))
                           (session-append! s (make-frame 0 0 W 3 "agent:claude" '()
                                                          '(put "w.3" ())))
                           'accepted)))
              (log-end! s)
              (list (and after #t) again)))))
      (list #f 'writer-stopped))

(printf "== L24(d): between a commit and its confirmation ==\n")
(build!)
(want "no usable view, the second append refused, then both released by the confirmation"
      (with-session
        (lambda (s)
          (let* ((a (append-one! s '(put "w.3" ())))
                 (v (session-view s))
                 (b (session-append! s (make-frame 0 0 W 4 "agent:claude" '()
                                                   '(put "w.4" ()))))
                 (_ (session-applied! s 0 (list (cons W 3))))
                 (c (append-one! s '(put "w.4" ()))))
            (list (car a) v (car b) (cadr b) (car c) (cadr c)))))
      (list 'committed #f 'refused-before-reserve 'not-ready 'committed 4))

(printf "== L19(d): a real backup, restored in place ==\n")
;; The registry is outside the store, so a backup of the store directory
;; cannot carry it. Everything inside the directory agrees that all is
;; well -- same owner, same identity, a shorter log -- and the mark is
;; the only thing that disagrees.
;;
;; TWO SEPARATE EXPERIMENTS, because they interfere: replacing the
;; directory changes its inode, and the identity recorded in the backup
;; then names the old one. Running them in one store made the in-place
;; case fail on identity and prove nothing about the mark.
(build!)
(define backup (string-append base "/backup"))
(system (string-append "rm -rf " backup "; cp -R " d " " backup))
(define ino-before (call-with-values (lambda () (path-device-inode d)) list))
(system (string-append "rm -rf " d "; cp -R " backup " " d))
(want "a restore that REPLACES the directory changes its inode"
      (equal? (call-with-values (lambda () (path-device-inode d)) list) ino-before)
      #f)
(want "and is therefore refused on identity, before the mark is consulted"
      (try-append) (list 'refused-before-reserve (list 'instance 'inode)))

;; The case the registry is FOR: the contents restored into the same
;; directory, so every check inside the store passes.
(build!)
(system (string-append "rm -rf " backup "; cp -R " d " " backup))
(with-session (lambda (s) (append-one! s '(put "w.3" ()))))
(with-session (lambda (s) (session-applied! s 0 (list (cons W 3)))
                         (append-one! s '(put "w.4" ()))))
(system (string-append "find " d " -mindepth 1 -delete; cp -R " backup "/. " d "/"))
(want "the identity still matches, so the local writer is recognised"
      (let ((s (log-begin d (lambda args 'applied))))
        (let ((w (session-writer s))) (log-end! s) w))
      W)
(want "and the mark is what refuses the write"
      (try-append) (list 'refused-before-reserve 'registry-ahead))

(printf "== L24(k): the vocabulary, now that the write path exists ==\n")
(build!)
(define k-trace
  (let ((p (open-output-string)))
    (parameterize ((current-error-port p))
      (trace-enable! #t)
      (with-session (lambda (s) (append-one! s '(put "w.3" ()))))
      (trace-enable! #f))
    (get-output-string p)))
(define (has-substring? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) sub) #t)
            (else (loop (+ i 1)))))))
(define (lines-of text)
  (let ((n (string-length text)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((>= i n) (reverse (if (> i start) (cons (substring text start i) acc) acc)))
            ((char=? (string-ref text i) #\newline)
             (loop (+ i 1) (+ i 1) (cons (substring text start i) acc)))
            (else (loop (+ i 1) start acc))))))
(define k-events (filter (lambda (l) (has-substring? l "(trace ")) (lines-of k-trace)))
(define (op-of line)
  (let* ((i (+ 7 (let loop ((k 0)) (if (string=? (substring line k (+ k 7)) "(trace ") k (loop (+ k 1))))))
         (j (let scan ((j i)) (if (or (>= j (string-length line))
                                      (char=? (string-ref line j) #\space)) j (scan (+ j 1))))))
    (substring line i j)))
(define k-ops (map op-of k-events))
;; DELIBERATELY ABSENT, and named so that the row says whether each was
;; missing or never expected: `link` is emitted only by publication;
;; `unlink` only when a temporary has to be removed after a failed
;; atomic write; `snapshot-read` only when a snapshot is present, and
;; this store has none; `lock-wait` only when another process is
;; actually holding the lock. `ftruncate` is absent because this
;; fixture's tail is sound -- log12 has the row where it appears.
(want "one ordinary commit emits exactly this vocabulary"
      (list-sort string<?
                 (let dedupe ((l (list-sort string<? k-ops)) (acc '()))
                   (cond ((null? l) acc)
                         ((member (car l) acc) (dedupe (cdr l) acc))
                         (else (dedupe (cdr l) (cons (car l) acc))))))
      (list-sort string<?
                 '("apply" "catch-up" "copy" "create" "enter-critical" "flock" "frame"
                   "fsync" "parse" "registry-check" "registry-write" "rename"
                   "unlock" "write")))
;; DEDUPLICATION THROWS AWAY THE THING THAT MATTERS. `apply` appearing
;; somewhere in the set was satisfied by delivery emitting it, which is
;; how a commit path that never applied anything stayed green. There is
;; exactly one apply, it names this writer and the sequence just
;; written, and it sits between the log flush and the unlock.
(want "there is exactly one apply, and it names the record just committed"
      (let ((applies (filter (lambda (l) (string=? (op-of l) "apply")) k-events)))
        (list (length applies)
              (and (pair? applies)
                   (has-substring? (car applies) (string-append "(" W " . 3)")))))
      (list 1 #t))
;; PARSED, NOT MATCHED. A substring test says nothing about how many
;; fields an event has, and " #f)" is satisfied by a subject that ends
;; in those characters as readily as by a bytes field.
(define (event-fields line)
  (guard (e (#t 'unparsable))
    (let ((d* (read (open-string-input-port line))))
      (and (list? d*) (= 4 (length d*)) (eq? (car d*) 'trace) (cdr d*)))))
(want "every event is exactly (trace op subject bytes)"
      (length (filter (lambda (l) (not (list? (event-fields l)))) k-events))
      0)
(want "the ops that move no bytes report #f, and the ones that do report a number"
      (let* ((fields (map event-fields k-events))
             (movers (filter (lambda (f) (memq (car f) '(write))) fields))
             (still (filter (lambda (f) (memq (car f) '(flock unlock enter-critical
                                                       catch-up frame apply
                                                       registry-check registry-write
                                                       create rename))) fields)))
        (list (> (length movers) 0)
              (for-all (lambda (f) (and (integer? (caddr f)) (> (caddr f) 0))) movers)
              (for-all (lambda (f) (eq? (caddr f) #f)) still)))
      (list #t #t #t))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "log14 complete\n")
