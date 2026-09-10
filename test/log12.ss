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

;; The append path (design 5.2 steps 1-9, plan L1 L4' L9 L13 L24 e h i).
;;
;; THE ORDER OF THE STEPS IS THE CONTRACT. Validation and framing happen
;; before any maintenance, so input the store will refuse cannot leave a
;; truncation or a rotation behind -- a caller who sends an unstorable
;; value and then sends a legal one must not need adopt in between. A
;; clean fixture that is not due to rotate cannot see this: the ordering
;; is only visible when there IS maintenance pending, so the refusal rows
;; below are run against a torn tail and against a rotation-due segment.
;;
;; The registry is batch C: the reservation step is a placeholder here,
;; so reserved-not-written is reachable only through a write that moves
;; zero bytes, and the water-mark assertions are not in this file.
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia ffi)
        (theourgia trace)
        (only (igropyr crypto) sha256 bytevector->hex))
(define bad 0)
(define (want label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(define d "/private/tmp/claude-501/-Users-guenchi-Workshop/ff8debcd-6740-4e42-80ca-8d637b6249df/scratchpad/tg/log12work")
(define W "wwwx7q2a")
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
;; AN ABSENT FILE IS A READING, NOT AN EXCEPTION. A mutation that stops
;; rotation happening leaves segment 2 missing, and an exception there
;; ends the file with zero FAIL lines -- which reads exactly like a
;; clean run to anything counting failures.
(define (slurp path)
  (guard (e (#t 'no-such-file))
    (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
      (if (eof-object? b) (make-bytevector 0) b))))
(define (len-of path)
  (let ((b (slurp path))) (if (bytevector? b) (bytevector-length b) b)))
(define (wpath n) (string-append d "/writers/" W "/" (segment-file-name n)))
(define (build! . segs)
  (system (string-append "rm -rf " d "; mkdir -p " d "/writers/" W " " d "/snap"))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"t\"))\n"))
  (file-ensure! (string-append d "/lock"))
  (put! (string-append d "/writers/" W "/owner.sexp") (string->utf8 "((machine \"m\"))\n"))
  (if (null? segs)
      (put! (wpath 1) (cat (r 1) (r 2)))
      (for-each (lambda (i bytes) (put! (wpath i) bytes))
                (let loop ((i 1) (l segs)) (if (null? l) '() (cons i (loop (+ i 1) (cdr l)))))
                segs)))
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
(define (ops text) (map op-of (events text)))
;; A session, an append built from the session's own view, and the answer.
(define (append-one! s payload)
  (let ((v (session-view s)))
    (session-append! s (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                   (view-expect-seq v) "agent:claude" '() payload))))
;; A FIXED CLOCK, so that every record this fixture writes is the same
;; width as the ones it builds by hand. With the real clock the appended
;; record's timestamp has a different number of digits and every
;; byte-length assertion below would be measuring the calendar.
(define fixed-ts 1757300000003)
;; The clock is an argument so a row that needs a LATER now -- the
;; rotation age trigger -- can set it. An earlier version parameterized
;; it here unconditionally and silently overrode the rotation row's
;; clock, so that row measured a two-millisecond-old segment and
;; concluded rotation does not happen.
(define (with-session/clock ms proc)
  (parameterize ((log-clock (lambda () ms)))
    (let ((s (log-begin d (lambda args 'applied))))
      (let ((out (proc s)))
        (log-end! s)
        out))))
(define (with-session proc) (with-session/clock fixed-ts proc))

(printf "== L24(h): the five outcomes are distinguishable ==\n")
(build!)
(want "a legal append commits and names its sequence"
      (with-session (lambda (s) (let ((res (append-one! s '(put "w.3" ()))))
                                  (list (car res) (cadr res)))))
      (list 'committed 3))
;; THE EXPECTED BYTES, not the expected length. A length assertion is
;; satisfied by any equal-sized record, including one written to the
;; wrong sequence or with a damaged prefix.
(want "and the record is on disk exactly once, byte for byte what it should be"
      (equal? (slurp (wpath 1)) (cat (r 1) (r 2) (rec 3 fixed-ts '() '(put "w.3" ()))))
      #t)

;; THE OTHER THREE OUTCOMES, each reached by a fault aimed at the commit
;; stage in a CHILD process. They cannot be reached from inside this file:
;; THEOURGIA_INJECT is an expansion-time gate, and the persistent faults
;; would apply to every append here. The child reports the outcome symbol
;; and the resulting file length; the parent checks both.
(define child-path (string-append d "/append-child.ss"))
(define child-out (string-append d "/append-child.out"))
(define (write-child!)
  (put! child-path
        (string->utf8
          (string-append
            "#!chezscheme\n(import (chezscheme) (theourgia log) (theourgia ffi))\n"
            "(define res\n"
            "  (guard (e (#t (list 'raised)))\n"
            "    (parameterize ((log-clock (lambda () " (number->string fixed-ts) ")))\n"
            "      (let* ((s (log-begin \"" d "\" (lambda args 'applied)))\n"
            "             (v (session-view s))\n"
            "             (r (session-append! s (make-frame (view-revision v) (view-epoch v)\n"
            "                                               (view-writer v) (view-expect-seq v)\n"
            "                                               \"agent:claude\" '() '(put \"w.3\" ())))))\n"
            "        (log-end! s)\n"
            "        r))))\n"
            "(printf \"~s ~s\\n\" (car res) (file-size \"" (wpath 1) "\"))\n"))))
(define (child-says fault)
  (build!)
  (write-child!)
  (system (string-append (if fault (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=" fault " ") "")
                         "scheme --script " child-path " > " child-out " 2>/dev/null"))
  (let ((text (utf8->string (slurp child-out))))
    (guard (e (#t (list 'unreadable text)))
      (let ((p (open-string-input-port text))) (list (read p) (read p))))))

(printf "== L24(h): the outcomes that only a fault can reach ==\n")
(want "CONTROL: with nothing armed the child commits and the file grows"
      (child-says #f) (list 'committed (* 3 R)))
;; A PARTIAL WRITE IS NOT A FAILED ONE. The bytes that reached the file
;; are still there, so a caller that retries would append the record a
;; second time -- which is why this outcome is named separately from
;; reserved-not-written rather than folded into "it failed".
;;
;; reserved-not-written HAS NO ROW: it needs a write that moves ZERO
;; bytes and then fails, and no fault in section 13's list produces one
;; (write-eio-after-partial fails only AFTER its partial). Reported as a
;; gap rather than approximated with this one.
(want "a partial write is reported as partial, with the bytes it managed on disk"
      (child-says "write-eio-after-partial@commit:file=000001.sexp")
      (list 'partial-write 127))
(want "a log fsync failure is written-fsync-failed, with the bytes on disk"
      (child-says "fsync-fail@commit:file=000001.sexp")
      (list 'written-fsync-failed (* 3 R)))
(want "a short write is retried and still commits"
      (child-says "short-write@commit:file=000001.sexp")
      (list 'committed (* 3 R)))

(printf "== a writer with integrity errors refuses to write ==\n")
;; Found by a mutation that survived every other row: ignoring the
;; integrity list let a poisoned writer keep appending, which is the one
;; thing adopt exists to force.
(build!)
(put! (wpath 1) (cat (r 1) (let ((b (r 2)))
                             (bytevector-u8-set! b 3 (if (= 48 (bytevector-u8-ref b 3)) 49 48))
                             b)))
(define poisoned-before (slurp (wpath 1)))
(want "a CRC error in this writer's own history refuses the append"
      (let ((res (with-session (lambda (s) (append-one! s '(put "w.9" ()))))))
        (list (car res) (cadr res)))
      (list 'refused-before-reserve 'integrity))
(want "and nothing was written"
      (equal? (slurp (wpath 1)) poisoned-before) #t)

(printf "== L24(e): each frame binding is checked, and named ==\n")
;; Run against a store that has BOTH a torn tail and pending work, so
;; that "nothing happened" means something: on a clean store a refusal
;; and a no-op are indistinguishable.
(define (torn-store!)
  (build!)
  (put! (wpath 1) (cat (r 1) (r 2) (let ((h (r 3)))
                                     (let ((o (make-bytevector 20)))
                                       (bytevector-copy! h 0 o 0 20) o)))))
(define (refuse-with mangle)
  (torn-store!)
  (let* ((before (slurp (wpath 1)))
         (answer (with-session
                   (lambda (s)
                     (let ((v (session-view s)))
                       (session-append! s (mangle v))))))
         (after (slurp (wpath 1))))
    (list (car answer) (cadr answer) (equal? before after))))
(define (frame-from v vid epoch writer seq)
  (make-frame vid epoch writer seq "agent:claude" '() '(put "w.9" ())))
(want "a stale view revision is refused before anything is reserved"
      (refuse-with (lambda (v) (frame-from v (+ 1 (view-revision v)) (view-epoch v)
                                           (view-writer v) (view-expect-seq v))))
      (list 'refused-before-reserve 'view #t))
(want "a wrong epoch is refused, and says so"
      (refuse-with (lambda (v) (frame-from v (view-revision v) (+ 1 (view-epoch v))
                                           (view-writer v) (view-expect-seq v))))
      (list 'refused-before-reserve 'epoch #t))
(want "a frame naming another writer is refused, and says so"
      (refuse-with (lambda (v) (frame-from v (view-revision v) (view-epoch v)
                                           "someone-else" (view-expect-seq v))))
      (list 'refused-before-reserve 'writer #t))
(want "a wrong expected sequence is refused, and says so"
      (refuse-with (lambda (v) (frame-from v (view-revision v) (view-epoch v)
                                           (view-writer v) (+ 5 (view-expect-seq v)))))
      (list 'refused-before-reserve 'expect-seq #t))
(want "CONTROL: the same store, an intact frame, succeeds and repairs the tail"
      (let ()
        (torn-store!)
        (let ((res (with-session (lambda (s) (append-one! s '(put "w.3" ()))))))
          (list (car res) (bytevector-length (slurp (wpath 1))))))
      (list 'committed (* 3 R)))

(printf "== L13: refused input leaves the store byte for byte unchanged ==\n")
;; A record instance is not storable. The point of the row is not that it
;; is refused -- it is that the refusal happens before the maintenance,
;; so the torn tail is STILL torn afterwards and no rotation occurred.
(define-record-type opaque (fields x))
(torn-store!)
(define l13-before (slurp (wpath 1)))
(define l13-answer
  (with-session (lambda (s) (guard (e (#t (list 'raised 'raised)))
                              (append-one! s (make-opaque 1))))))
(want "an unstorable payload is refused before reserve"
      (car l13-answer) 'refused-before-reserve)
(want "and the segment is byte for byte what it was, tail still torn"
      (equal? (slurp (wpath 1)) l13-before) #t)
(want "and no new segment appeared"
      (enumerate-segment-files d W) '(1))
(want "the same writer can then append legally, with no adopt in between"
      (let ((res (with-session (lambda (s) (append-one! s '(put "w.3" ()))))))
        (car res))
      'committed)

(printf "== L1 / L4'(d): a torn tail is repaired, and only at its own end ==\n")
(torn-store!)
(define l1-trace (traced (lambda () (with-session (lambda (s) (append-one! s '(put "w.3" ())))))))
;; FROM THE APPEND'S OWN START. The session's takeover barrier flushes
;; before any of this, and folding those fsyncs into the sequence would
;; make the row assert the barrier and the repair at once -- two
;; mechanisms in one expectation, so a change to either rewrites it.
(define (ops-after text mark)
  (let loop ((os (ops text)))
    (cond ((null? os) '())
          ((string=? (car os) mark) (cdr os))
          (else (loop (cdr os))))))
(want "the repair truncates, then writes, then flushes, then applies"
      (filter (lambda (o) (member o '("ftruncate" "write" "fsync" "apply" "unlock")))
              (ops-after l1-trace "catch-up"))
      '("ftruncate" "write" "fsync" "apply" "unlock"))
(want "and the file is the valid prefix plus the new record, byte for byte"
      (equal? (slurp (wpath 1)) (cat (r 1) (r 2) (rec 3 fixed-ts '() '(put "w.3" ()))))
      #t)
(want "CONTROL: an untorn store is not truncated at all"
      (begin (build!)
             (let ((t (traced (lambda () (with-session (lambda (s) (append-one! s '(put "w.3" ()))))))))
               (length (filter (lambda (o) (string=? o "ftruncate")) (ops t)))))
      0)

(printf "== L24(i): the apply is inside the lock ==\n")
(build!)
(define i-trace (traced (lambda () (with-session (lambda (s) (append-one! s '(put "w.3" ())))))))
;; THE APPEND'S OWN FLUSH, IDENTIFIED BY ITS TARGET. Looking at "the
;; last fsync anywhere" leaves the row green when the append's fsync is
;; skipped entirely -- the session's takeover flush still precedes apply
;; and unlock. The events are taken from the append onward and the fsync
;; must name the segment that was written.
(want "write the segment, flush THAT segment, apply, unlock -- in that order"
      (let ((es (let loop ((l (events i-trace)))
                  (cond ((null? l) '())
                        ((string=? (op-of (car l)) "catch-up") (cdr l))
                        (else (loop (cdr l)))))))
        (map op-of
             (filter (lambda (l)
                       (or (member (op-of l) '("apply" "unlock"))
                           (and (member (op-of l) '("write" "fsync"))
                                (has-substring? l "000001.sexp"))))
                     es)))
      '("write" "fsync" "apply" "unlock"))

(printf "== L9 / 4.4: rotation, and sealed segments stay sealed ==\n")
;; The age trigger, reached through the clock seam. Without it the row
;; would have to wait an hour or assert nothing.
(build!)
(define rot-clock (+ 1757300000000 3600000 5000))
(define rot-trace
  (traced (lambda () (with-session/clock rot-clock
                       (lambda (s) (append-one! s '(put "w.3" ())))))))
(want "an old segment rotates, and the new record goes to the new segment"
      (list (enumerate-segment-files d W) (len-of (wpath 1)) (len-of (wpath 2)))
      (list '(1 2) (* 2 R) R))
(want "rotation is fsync current, create next, fsync next, fsync directory"
      (let loop ((os (ops-after rot-trace "catch-up")) (acc '()))
        (cond ((null? os) (reverse acc))
              ((string=? (car os) "write") (reverse acc))
              ((member (car os) '("create" "fsync")) (loop (cdr os) (cons (car os) acc)))
              (else (loop (cdr os) acc))))
      '("fsync" "create" "fsync" "fsync"))
(want "and nothing writes to or truncates the sealed segment"
      (filter (lambda (l) (and (has-substring? l "000001.sexp")
                               (member (op-of l) '("write" "ftruncate"))))
              (events rot-trace))
      '())
(want "CONTROL: a young segment does not rotate"
      (begin (build!)
             (with-session/clock (+ 1757300000000 5000)
               (lambda (s) (append-one! s '(put "w.3" ()))))
             (enumerate-segment-files d W))
      '(1))

(printf "== the size trigger, and a probe that cannot answer ==\n")
;; The size trigger has its own row because the age trigger cannot stand
;; in for it: an implementation that swallows a failed size probe and
;; calls it "not due" keeps every age row green.
;; REAL RECORDS, not padding: a segment padded with bytes that are not
;; records has a torn tail, maintenance truncates it, and the size the
;; trigger sees is the truncated one. The first version did that and
;; concluded the size trigger does not work.
(define bulk-count (+ 1 (quotient 1048576 R)))
(build! (apply cat (let loop ((i 1)) (if (> i bulk-count) '() (cons (r i) (loop (+ i 1)))))))
(want "a segment at the size limit rotates before the next record"
      (begin (with-session (lambda (s) (append-one! s (list 'put "w.next" '()))))
             (list (enumerate-segment-files d W)
                   (equal? (slurp (wpath 2))
                           (rec (+ bulk-count 1) fixed-ts '() (list 'put "w.next" '())))))
      (list '(1 2) #t))

(printf "== the clock is wall time, not process time ==\n")
;; Chez's real-time is milliseconds since THIS PROCESS started. A log
;; written with it carries timestamps that are not comparable across
;; restarts, and the age trigger computes a negative age and never fires.
;; The row runs with NO clock parameterized, so it sees the default.
(build!)
(define default-ts
  (let ()
    (let ((s (log-begin d (lambda args 'applied))))
      (let ((v (session-view s)))
        (session-append! s (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                       (view-expect-seq v) "agent:claude" '() '(put "w.3" ()))))
      (log-end! s))
    ;; Read it back through the codec rather than by slicing the text:
    ;; the field offsets are the format's business, not this row's.
    (let* ((bytes (slurp (wpath 1)))
           (n (bytevector-length bytes))
           (start (let loop ((i (- n 2)))
                    (cond ((< i 0) 0)
                          ((= 10 (bytevector-u8-ref bytes i)) (+ i 1))
                          (else (loop (- i 1))))))
           (last (let ((o (make-bytevector (- n start))))
                   (bytevector-copy! bytes start o 0 (- n start))
                   o))
           (r (decode-line last)))
      (and (eq? (car r) 'ok) (caddr r)))))
(want "the default clock stamps records in epoch milliseconds"
      (and (integer? default-ts) (> default-ts 1500000000000)) #t)

(printf "== L24(d): between an append and its confirmation ==\n")
;; The log layer knows the record is on disk. It does not know whether
;; the reduction the next record's facts would be computed from includes
;; it -- so there is no usable preparation view until the reducer says so.
(build!)
(want "the second append is refused until the reducer confirms, then succeeds"
      (with-session
        (lambda (s)
          (let* ((a (append-one! s '(put "w.3" ())))
                 (view-while-pending (session-view s))
                 (b (session-append! s (make-frame 0 0 W 4 "agent:claude" '()
                                                   '(put "w.4" ()))))
                 (_ (session-applied! s 0 (list (cons W 3))))
                 (c (append-one! s '(put "w.4" ()))))
            (list (car a) view-while-pending (car b) (cadr b) (car c) (cadr c)))))
      (list 'committed #f 'refused-before-reserve 'not-ready 'committed 4))
(want "and both records are on disk, byte for byte"
      (equal? (slurp (wpath 1))
              (cat (r 1) (r 2) (rec 3 fixed-ts '() '(put "w.3" ()))
                   (rec 4 fixed-ts '() '(put "w.4" ()))))
      #t)
(want "a frame built from a superseded view is refused"
      (with-session
        (lambda (s)
          (let ((v (session-view s)))
            (append-one! s '(put "w.5" ()))
            (session-applied! s 0 (list (cons W 5)))
            (let ((res (session-append!
                         s (make-frame (view-revision v) (view-epoch v)
                                       (view-writer v) (view-expect-seq v)
                                       "agent:claude" '() '(put "w.6" ())))))
              (list (car res) (cadr res))))))
      (list 'refused-before-reserve 'view))

(printf "\n~a failures\n" bad)
(printf "log12 complete\n")
