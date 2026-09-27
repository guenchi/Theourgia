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

;; Epochs, quarantine reload and the reset handshake
;; (plan L25 a b c e h; design 5.2 step 2 and 5.2').
;;
;; A QUARANTINE MOVES A BOUNDARY BACKWARDS, which is the one thing an
;; incremental catch-up cannot express: records the session already
;; delivered stop being history. So the session reloads whole, inside the
;; lock it already holds, and the epoch changes so that anything computed
;; against the old boundary can be recognised and refused.
;;
;; AND IF THE REDUCER HAD APPLIED WHAT IS NOW GONE, nothing short of
;; discarding its state is correct. The log layer stops delivering until
;; the reducer says it has done so -- a handshake, not a poll -- and only
;; then replays under the new epoch.
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia ffi)
        (theourgia trace) (theourgia crc32)
        (only (theourgia digest) sha256 bytevector->hex))

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

(define d (test-dir "log16work"))
(define home (string-append d "-home"))
(define W "wwwf6q2a")
(define M "mmmf6q2a")
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
(define (qpath w) (string-append d "/writers/" w "/quarantine.sexp"))

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
;; The mirrored writer holds three records so that a fork can land in the
;; middle of its history rather than at either end.
(define (build!)
  (system (string-append "rm -rf " d " " d "-home; mkdir -p " d "/writers/" W " " d "/writers/" M
                         " " d "/snap " home))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"f6\"))\n"))
  (file-ensure! (string-append d "/lock"))
  (putenv "THEOURGIA_HOME" home)
  (put! (wpath W 1) (cat (r "w" 1) (r "w" 2)))
  (put! (wpath M 1) (cat (r "m" 1) (r "m" 2) (r "m" 3)))
  (publish! 1)
  (let ((n (instance-install! d))) (owner-install! d W n)))
(define (quarantine! w fork)
  (put! (qpath w)
        (string->utf8 (string-append "((format 1) (fork " (number->string fork) "))\n"))))
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
(define (count-op text op)
  (length (filter (lambda (l) (string=? (op-of l) op)) (events text))))
(define (append-one! s payload)
  (let ((v (session-view s)))
    (if v
        (session-append! s (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                       (view-expect-seq v) "agent:claude" '() payload))
        (list 'no-view #f))))
(define (have-of s w)
  (let ((e (assoc w (map (lambda (f) (cons (car f) (cdr (assq 'contiguous (cdr f)))))
                         (session-frontiers s)))))
    (if e (cdr e) 'missing)))

(printf "== L25(a): a changed version reloads the whole session ==\n")
;; The expected boundaries are worked out from the record list by hand:
;; the mirror holds m.1 m.2 m.3, and a fork at 2 leaves only m.1.
(build!)
(define a-trace "")
(define a-second #f)
(define a-result
  (parameterize ((log-clock (lambda () fixed-ts)))
    ;; THE REDUCER HAS NOT APPLIED WHAT THE FORK WILL REMOVE. Delivery
    ;; answering `applied` to everything would put its cursor past the
    ;; new boundary, and then ANY effective fork forces a reset -- which
    ;; is the next row's subject, not this one's. Here the mirror's later
    ;; records are still pending, so the reload is a reload and nothing
    ;; more.
    (let ((s (log-begin d (lambda (w seg off seq . rest)
                            (if (and (string=? w M) (>= seq 2)) 'pending 'applied)))))
      (let* ((epoch0 (session-epoch s))
             (have0 (have-of s M))
             (first (append-one! s '(put "w.3" ()))))
        ;; ONLY THIS WRITER IS CONFIRMED. If the reducer had also
        ;; confirmed the mirror's third record, the fork would put its
        ;; applied cursor past the new boundary and the session would go
        ;; to reset-pending -- which is the NEXT row's subject. Keeping
        ;; them apart is what lets each say which mechanism it tested.
        (session-applied! s epoch0 (list (cons W 3)))
        ;; Another process installs a quarantine between the two appends.
        (quarantine! M 2)
        (set! a-second #f)
        (set! a-trace (traced (lambda () (set! a-second (append-one! s '(put "w.4" ()))))))
        ;; A SECOND ATTEMPT, with a view taken AFTER the reload.
        (let ((a-third (append-one! s '(put "w.4" ()))))
          (let ((out (list (car first) epoch0 have0 (session-epoch s) (have-of s M)
                           (car a-second) (cadr a-second) (car a-third))))
            (log-end! s)
            out))))))
(want "the first append commits, the mirror reaches 3, the epoch starts at 0"
      (list (car a-result) (cadr a-result) (caddr a-result))
      (list 'committed 0 3))
(want "after the version changes the epoch is one greater and the fork has moved have"
      (list (cadddr a-result) (list-ref a-result 4))
      (list 1 1))
;; TWO WRITERS REDISCOVERED, and no lock event of any kind in between:
;; the exclusive lock is already held, and reaching for a shared one here
;; would deadlock against it. The append's own catch-up is absent because
;; that append never gets that far -- see the row below.
(want "the reload rediscovers every writer, and takes no lock while doing it"
      (list (count-op a-trace "catch-up") (count-op a-trace "unlock") (count-op a-trace "flock"))
      (list 2 0 0))
;; THE RELOAD HAPPENS INSIDE session-append!, so a view taken before it
;; is already superseded when the frame arrives. Both the epoch and the
;; revision moved; the epoch is what the refusal names, because it is
;; checked first and it is the coarser fact -- a new epoch invalidates
;; every view, while a new revision invalidates only the older ones.
;; The caller re-takes the view and the same append succeeds, which is
;; the design's "a relevant change invalidates the view", observed.
(want "a frame built before the reload is refused, and a fresh one succeeds"
      (list (list-ref a-result 5) (list-ref a-result 6) (list-ref a-result 7))
      (list 'refused-before-reserve 'epoch 'committed))
;; CONTROL: nothing changed, nothing reloads.
(build!)
(define b-trace "")
(define b-result
  (parameterize ((log-clock (lambda () fixed-ts)))
    (let ((s (log-begin d (lambda args 'applied))))
      (append-one! s '(put "w.3" ()))
      (session-applied! s 0 (list (cons W 3) (cons M 3)))
      (set! b-trace (traced (lambda () (append-one! s '(put "w.4" ())))))
      (let ((out (list (session-epoch s) (have-of s M)))) (log-end! s) out))))
(want "CONTROL: with no version change the epoch stands and nothing is rediscovered"
      (list b-result (count-op b-trace "catch-up"))
      (list (list 0 3) 1))

(printf "== L25(b): the reset handshake ==\n")
;; The reducer had applied m.3; the fork removes it, so its state was
;; built on history that no longer exists and only a discard is correct.
(build!)
(define c-out
  (parameterize ((log-clock (lambda () fixed-ts)))
    (let ((s (log-begin d (lambda args 'applied))))
      (append-one! s '(put "w.3" ()))
      (session-applied! s 0 (list (cons W 3) (cons M 3)))
      (quarantine! M 2)
      (let* ((refused (session-append! s (make-frame 0 0 W 4 "agent:claude" '()
                                                     '(put "w.4" ()))))
             (view-while-pending (session-view s))
             (pending? (session-reset-pending s))
             (wrong-epoch (guard (e (#t 'refused))
                            (session-reset-done! s 99) 'accepted))
             (still-pending? (session-reset-pending s))
             ;; GUARDED, so that an implementation which never entered
             ;; reset-pending -- or which let the wrong epoch clear it --
             ;; reports a red row instead of ending the file. A run that
             ;; stops with no FAIL lines reads exactly like a clean one.
             (done (guard (e (#t 'refused)) (session-reset-done! s (session-epoch s))))
             (after (session-view s)))
        (let ((out (list (car refused) (cadr refused) view-while-pending pending?
                         wrong-epoch still-pending? done (and after #t))))
          (log-end! s)
          out)))))
(want "reset-pending: no view, a structured refusal naming it, and a wrong epoch is refused"
      c-out
      (list 'refused-before-reserve 'reset-pending #f #t 'refused #t 1 #t))

(printf "== the reset guard on the view, witnessed on its own ==\n")
;; THE GUARD IS PRIMARY, NOT REDUNDANT, and the row above could not show
;; it: reload clears the applied cursor, so the readiness gate answers
;; "no" anyway and deleting the reset guard changes nothing. Here the
;; local writer is EMPTY -- its readiness is 0 >= 0, which passes -- so
;; the only thing that can withhold the view is the reset itself.
(define (build-empty-local!)
  (system (string-append "rm -rf " d " " d "-home; mkdir -p " d "/writers/" W " " d "/writers/" M
                         " " d "/snap " home))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"f6\"))\n"))
  (file-ensure! (string-append d "/lock"))
  (putenv "THEOURGIA_HOME" home)
  (put! (wpath M 1) (cat (r "m" 1) (r "m" 2) (r "m" 3)))
  (publish! 1)
  (let ((n (instance-install! d))) (owner-install! d W n)))
(build-empty-local!)
(want "with an empty local writer, only the reset withholds the view"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda args 'applied))))
          (let* ((ready-before (and (session-view s) #t))
                 (_ (session-applied! s 0 (list (cons M 3))))
                 (__ (quarantine! M 2))
                 (refused (session-append! s (make-frame 0 0 W 1 "agent:claude" '()
                                                         '(put "w.1" ()))))
                 (pending? (session-reset-pending s))
                 (view-now (session-view s)))
            (log-end! s)
            (list ready-before (car refused) (cadr refused) pending? view-now))))
      (list #t 'refused-before-reserve 'reset-pending #t #f))

(printf "== L25(b): an old-epoch report AFTER the replay ==\n")
;; Tested here rather than during reset-pending: an implementation that
;; refuses every report while a reset is outstanding passes that test
;; without ever comparing an epoch. After the replay, reports are being
;; accepted again, so refusing THIS one can only be the epoch check.
(build!)
(want "after the replay, a report carrying the old epoch is refused and the cursor holds"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda (w seg off seq . rest)
                                (if (string=? w M) 'pending 'applied)))))
          (session-applied! s 0 (list (cons M 3)))
          (quarantine! M 2)
          (session-append! s (make-frame 0 0 W 3 "agent:claude" '() '(put "w.3" ())))
          (session-reset-done! s (session-epoch s))
          (let* ((old (guard (e (#t 'refused))
                        (session-applied! s 0 (list (cons M 1))) 'accepted))
                 (after-old (have-of s M))
                 (new (guard (e (#t 'refused))
                        (session-applied! s (session-epoch s) (list (cons M 1)))
                        'accepted)))
            (log-end! s)
            (list old after-old new))))
      (list 'refused 1 'accepted))

(printf "== a checksum is not a version ==\n")
;; TWO QUARANTINE MARKERS WITH DIFFERENT FORKS AND THE SAME CRC-32. One
;; excludes m.2 and one does not, and padded with these exact runs of
;; whitespace both check to 8ac8e0bf. A session that decides "did this
;; file change" by CRC therefore misses the second marker entirely and
;; keeps writing against a boundary that has moved.
;;
;; The collision was constructed by the reviewer and verified here before
;; being written down: crc32-string-hex returns 8ac8e0bf for both, and
;; sha256 does not.
(define (mask->whitespace hex)
  (let ((n (string->number hex 16)) (out (make-string 48)))
    (let loop ((i 0))
      (if (= i 48)
          out
          (begin (string-set! out i (if (bitwise-bit-set? n (- 47 i)) #\tab #\space))
                 (loop (+ i 1)))))))
(define fork-3-padded
  (string-append "((format 1) (fork 3))" (mask->whitespace "10a9b5942ffa") "\n"))
(define fork-2-padded
  (string-append "((format 1) (fork 2))" (mask->whitespace "2684240d0db7") "\n"))
(want "the two markers really do share a CRC and really do differ"
      (list (string=? (crc32-string-hex fork-3-padded) (crc32-string-hex fork-2-padded))
            (string=? fork-3-padded fork-2-padded))
      (list #t #f))
(build!)
(put! (qpath M) (string->utf8 fork-3-padded))
(want "swapping one colliding marker for the other still reloads and moves the boundary"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda (w seg off seq . rest)
                                (if (and (string=? w M) (>= seq 2)) 'pending 'applied)))))
          (let ((have-before (have-of s M)))
            (put! (qpath M) (string->utf8 fork-2-padded))
            (session-append! s (make-frame 0 0 W 3 "agent:claude" '() '(put "w.3" ())))
            (let ((out (list have-before (session-epoch s) (have-of s M))))
              (log-end! s)
              out))))
      (list 2 1 1))

(printf "== a reload delivers what it newly found ==\n")
;; A reload that needs no reset still discovered records the reducer has
;; not seen. Without delivering them they are never delivered at all:
;; not here, not on a later append (the versions match again), and not
;; through reset-done (no reset is pending).
(build!)
(define delivered-after-reload '())
(want "records published mid-session reach the reducer (section-13 L5')"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda (w seg off seq . rest)
                                (set! delivered-after-reload
                                      (cons (list w seq) delivered-after-reload))
                                'applied))))
          (session-applied! s 0 (list (cons W 2) (cons M 3)))
          (set! delivered-after-reload '())
          ;; The mirror publishes a second segment while the session is open.
          (put! (wpath M 2) (cat (r "m" 4) (r "m" 5)))
          (publish! 1 2)
          (session-append! s (make-frame 0 0 W 3 "agent:claude" '() '(put "w.3" ())))
          (let ((out (reverse delivered-after-reload)))
            (log-end! s)
            out)))
      (list (list M 4) (list M 5)))

(printf "== a report cannot arrive between the reset and its acknowledgement ==\n")
(build!)
(want "session-applied! is refused while a reset is outstanding"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda args 'applied))))
          (session-applied! s 0 (list (cons W 2) (cons M 3)))
          (quarantine! M 2)
          (session-append! s (make-frame 0 0 W 3 "agent:claude" '() '(put "w.3" ())))
          (let* ((during (guard (e (#t 'refused))
                           (session-applied! s (session-epoch s) (list (cons M 1)))
                           'accepted))
                 (_ (session-reset-done! s (session-epoch s)))
                 (after (guard (e (#t 'refused))
                          (session-applied! s (session-epoch s) (list (cons M 1)))
                          'accepted)))
            (log-end! s)
            (list during after))))
      (list 'refused 'accepted))

(printf "== a rejection is only for what was delivered ==\n")
;; Inside the validated extent is not the same as handed to the reducer.
(build!)
(want "rejecting a record that was never delivered is refused"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda (w seg off seq . rest)
                                (if (and (string=? w M) (>= seq 2)) 'pending 'applied)))))
          ;; m.2 and m.3 were delivered and are pending; a record the
          ;; reload has not produced at all is a different matter.
          (let* ((pending-ok (session-reject! s (session-epoch s) (cons M 2) 'no-such-verb))
                 (never (session-reject! s (session-epoch s) (cons M 99) 'no-such-verb))
                 (applied-already (session-reject! s (session-epoch s) (cons M 1) 'too-late)))
            (log-end! s)
            (list (car pending-ok) (car never) (cadr never)
                  (car applied-already) (cadr applied-already)))))
      (list 'rejected 'refused 'not-delivered 'refused 'already-applied))

(printf "== a rejected writer stays stopped across a reload ==\n")
;; Recording a rejection and then delivering that writer again on the
;; next pass would make the rejection advice rather than a decision. The
;; reload is what makes this observable: it is the only thing that
;; delivers again within one session.
(build!)
(define after-reject '())
(want "once rejected, that writer is not delivered again; the other still is"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda (w seg off seq . rest)
                                (set! after-reject (cons (list w seq) after-reject))
                                (if (and (string=? w M) (>= seq 2)) 'pending 'applied)))))
          (session-reject! s (session-epoch s) (cons M 2) 'no-such-verb)
          (session-applied! s 0 (list (cons W 2)))
          (set! after-reject '())
          ;; Both writers gain history; only the unrejected one may arrive.
          (put! (wpath M 2) (cat (r "m" 4) (r "m" 5)))
          (publish! 1 2)
          (session-append! s (make-frame 0 0 W 3 "agent:claude" '() '(put "w.3" ())))
          (let ((out (reverse after-reject)))
            (log-end! s)
            (list out (length (filter (lambda (e) (string=? (car e) M)) out))))))
      (list '() 0))

(printf "== L25(e): a frame does not survive its epoch ==\n")
;; The sequence and the writer are still right; only the epoch is not.
;; Checking those two is not enough, which is the whole point.
(build!)
(want "a frame prepared before the reset is refused afterwards, on the epoch"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda args 'applied))))
          (append-one! s '(put "w.3" ()))
          (session-applied! s 0 (list (cons W 3) (cons M 3)))
          (let ((stale (let ((v (session-view s)))
                         (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                     (view-expect-seq v) "agent:claude" '()
                                     '(put "w.4" ())))))
            (quarantine! M 2)
            (session-append! s (make-frame 0 0 W 4 "agent:claude" '() '(put "w.4" ())))
            (session-reset-done! s (session-epoch s))
            (let ((res (session-append! s stale)))
              (log-end! s)
              (list (car res) (cadr res)
                    (frame-writer stale) (frame-expect-seq stale))))))
      (list 'refused-before-reserve 'epoch W 4))

(printf "== L25(c): a fork inside this writer's own history refuses the write ==\n")
(build!)
(define c-before (slurp (wpath W 1)))
(quarantine! W 2)
(want "the append is refused, naming quarantine, and nothing is written"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda args 'applied))))
          (let ((res (session-append! s (make-frame 0 0 W 2 "agent:claude" '()
                                                    '(put "w.2" ())))))
            (log-end! s)
            (list (car res) (cadr res) (equal? (slurp (wpath W 1)) c-before)
                  ;; THE REGISTRY TOO. Refusing after reserving would
                  ;; leave the mark raised for a record that will never
                  ;; exist, and the log bytes alone cannot show it.
                  (slurp (string-append home "/instances.sexp"))))))
      (list 'refused-before-reserve 'quarantined #t 'no-such-file))

(printf "== L25(h): an authority change also changes the epoch ==\n")
;; Nothing about quarantine here: the writer simply retires between two
;; appends. The session must notice for the same reason -- what it is
;; allowed to do has changed underneath it.
(build!)
(want "a retirement appearing mid-session bumps the epoch and refuses the write"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (let ((s (log-begin d (lambda args 'applied))))
          (append-one! s '(put "w.3" ()))
          (session-applied! s 0 (list (cons W 3) (cons M 3)))
          (put! (string-append d "/writers/" W "/retired.sexp")
                (string->utf8 (string-append "((prefix 1 " (number->string (* 3 R))
                                             " 3) (tx \"t\"))\n")))
          (let* ((res (session-append! s (make-frame 0 1 W 4 "agent:claude" '()
                                                     '(put "w.4" ()))))
                 (out (list (session-epoch s) (car res) (cadr res))))
            (log-end! s)
            out)))
      (list 1 'refused-before-reserve 'retired))
(want "an owner rewritten in place also bumps the epoch"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (begin
          (build!)
          (let ((s (log-begin d (lambda args 'applied))))
            (append-one! s '(put "w.3" ()))
            (session-applied! s 0 (list (cons W 3) (cons M 3)))
            ;; The path is unchanged and so is every other file; only the
            ;; contents differ. Comparing existence rather than content
            ;; let this pass unnoticed.
            (put! (string-append d "/writers/" W "/owner.sexp")
                  (string->utf8 "((machine \"m\") (instance \"someone-else\"))\n"))
            (let* ((res (session-append! s (make-frame 0 1 W 4 "agent:claude" '()
                                                       '(put "w.4" ()))))
                   (out (list (session-epoch s) (car res))))
              (log-end! s)
              out))))
      (list 1 'refused-before-reserve))
(want "CONTROL: with neither the quarantine nor the authority touched, the epoch stands"
      (parameterize ((log-clock (lambda () fixed-ts)))
        (begin
          (build!)
          (let ((s (log-begin d (lambda args 'applied))))
            (append-one! s '(put "w.3" ()))
            (session-applied! s 0 (list (cons W 3) (cons M 3)))
            (let ((res (append-one! s '(put "w.4" ()))))
              (let ((out (list (session-epoch s) (car res)))) (log-end! s) out)))))
      (list 0 'committed))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "log16 complete\n")
