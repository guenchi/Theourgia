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

;; The write session: skeleton (design 5.2 / 5.2', plan L24 a c d g j k).
;;
;; Three obligations, none of which a successful load makes visible:
;;
;;   DELIVERY IMPLIES DURABILITY -- every record the reducer receives was
;;     flushed before the first callback, so the deps and snapshot cuts
;;     computed from it cannot point at history a crash removes.
;;   THE APPLIED CURSOR IS THE REDUCER'S -- reading a record, checking it
;;     and flushing it establish that the bytes are there, not that
;;     anything was applied to the state those facts are computed from.
;;   ONE ACTIVE OPERATION PER STORE -- because against yourself a lock
;;     does not fail, it hangs, and a hang is the failure a suite reports
;;     worst.
;;
;; Everything here is new code, so there is no "before" reading to quote:
;; the red evidence is the mutation list in the delivery letter, and each
;; mechanism has a mutation only its own rows kill.
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia ffi)
        (theourgia trace)
        (only (igropyr crypto) sha256 bytevector->hex))

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
    (system (string-append "mkdir -p " path))
    path))

(define bad 0)
(define (want label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(define d (test-dir "log11work"))
;; Ordered so that lexical order and "which one is local" disagree: if
;; delivery ever ordered by anything but the writer id, the b-first
;; expectation below would still pass with the ids the other way round.
;; The LOCAL writer is lexically LAST and the mirrored one has TWO
;; segments, so "writer-id order, then segment order" is a different
;; answer from "the local writer first" or "whatever order the
;; directory listing gave".
(define A "zzz1x2qa")
(define B "aaa9x2qa")
(define (rec seq deps payload)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" deps (storable-encode payload)))
(define (r tag n) (rec n '() (list 'put (string-append tag "." (number->string n)) '())))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (slurp path)
  (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
    (if (eof-object? b) (make-bytevector 0) b)))
(define (wpath w n) (string-append d "/writers/" w "/" (segment-file-name n)))
(define (build!)
  (system (string-append "rm -rf " d "; mkdir -p " d "/writers/" A " " d "/writers/" B " " d "/snap"))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"t\"))\n"))
  (file-ensure! (string-append d "/lock"))
  (put! (string-append d "/writers/" A "/owner.sexp") (string->utf8 "((machine \"m\"))\n"))
  (putenv "THEOURGIA_HOME" (string-append d "/home"))
  ;; ONLY THE LOCAL WRITER GETS AN owner.sexp -- that file is what makes
  ;; a writer local, so stamping every directory turned the mirrored
  ;; writer into a second local one.
  (let ((n (instance-install! d))) (owner-install! d A n))
  (put! (wpath A 1) (cat (r "a" 1) (r "a" 2)))
  (put! (wpath B 1) (r "b" 1))
  (put! (wpath B 2) (r "b" 2))
  ;; A mirrored writer without a valid manifest has its segments ignored,
  ;; and then every ordering row below would be measuring a store with
  ;; one writer in it.
  (put! (string-append d "/writers/" B "/published.sexp")
        (string->utf8 (string-append "((1 . \"" (bytevector->hex (sha256 (slurp (wpath B 1))))
                                     "\") (2 . \"" (bytevector->hex (sha256 (slurp (wpath B 2))))
                                     "\"))\n"))))
(define (traced thunk)
  (let ((p (open-output-string)))
    (parameterize ((current-error-port p))
      (trace-enable! #t)
      (guard (e (#t (trace-enable! #f) (raise e)))
        (thunk))
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
;; The store directory's own name, which the work directory carries a pid
;; suffix on -- so it is read from d rather than spelled out.
(define (store-basename)
  (let loop ((i (- (string-length d) 1)))
    (cond ((< i 0) d)
          ((char=? (string-ref d i) #\/) (substring d (+ i 1) (string-length d)))
          (else (loop (- i 1))))))
;; The subject is the second field, so "ends with" means "ends with,
;; just before the space that starts the bytes field".
(define (ends-with-before-space? line suffix)
  (let* ((n (string-length line))
         (cut (let loop ((i (- n 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref line i) #\space) i)
                      (else (loop (- i 1))))))
         (subject-end (or cut n))
         (m (string-length suffix)))
    (and (>= subject-end m)
         (string=? (substring line (- subject-end m) subject-end) suffix))))
(define (op-of line)
  (let* ((i (+ 7 (let loop ((k 0)) (if (string=? (substring line k (+ k 7)) "(trace ") k (loop (+ k 1))))))
         (j (let scan ((j i)) (if (or (>= j (string-length line))
                                      (char=? (string-ref line j) #\space)) j (scan (+ j 1))))))
    (substring line i j)))
(define (ops text) (map op-of (events text)))
(define (count-of text op) (length (filter (lambda (o) (string=? o op)) (ops text))))
(define (index-of lst pred)
  (let loop ((l lst) (i 0))
    (cond ((null? l) #f) ((pred (car l)) i) (else (loop (cdr l) (+ i 1))))))

(printf "== L24(g): delivery implies durability ==\n")
(build!)
(define seen '())
;; NOT get-output-string INSIDE THE CALLBACK: it DRAINS the port, so
;; reading the trace mid-run destroys the events the rows below assert
;; on -- and the rows would then be measuring an empty string rather than
;; a missing barrier. The ordering is established structurally instead,
;; by where the fsyncs sit among the events.
(define g-trace
  (traced
    (lambda ()
      (let ((s (log-begin d (lambda (w seg off seq ts actor deps payload)
                              ;; A MARKER IN THE TRACE ITSELF. Without one
                              ;; the row could only count fsyncs, and
                              ;; moving every flush to AFTER the callbacks
                              ;; would leave the count unchanged -- which
                              ;; is the whole thing the barrier is for.
                              (trace-event! 'probe-first-callback (list w seq) #f)
                              (set! seen (cons (list w seq) seen))
                              'applied))))
        (log-end! s)))))
(want "every writer's records arrive, in writer-id order then segment order"
      (reverse seen)
      (list (list B 1) (list B 2) (list A 1) (list A 2)))
;; The barrier is the only evidence: the records read back correctly
;; whether or not anything was flushed.
(want "every flush happens before the first record is delivered"
      (let* ((es (events g-trace))
             (first-callback (index-of es (lambda (l) (string=? (op-of l) "probe-first-callback"))))
             (last-fsync (let loop ((l es) (i 0) (last #f))
                           (cond ((null? l) last)
                                 ((string=? (op-of (car l)) "fsync") (loop (cdr l) (+ i 1) i))
                                 (else (loop (cdr l) (+ i 1) last))))))
        (and first-callback last-fsync (< last-fsync first-callback)))
      #t)
;; WHICH PATHS, not how many. A count of four is satisfied by flushing
;; one writer's file and directory twice and never touching the other.
(want "each writer's segment, its metadata and its directory are flushed"
      (let ((flushed (map (lambda (l)
                            (let* ((i (+ 7 6 1))
                                   (j (- (string-length l) 4)))
                              (let scan ((k (- (string-length l) 1)))
                                (cond ((< k 0) l)
                                      ((char=? (string-ref l k) #\/) (substring l (+ k 1) j))
                                      (else (scan (- k 1)))))))
                          (filter (lambda (l) (string=? (op-of l) "fsync")) (events g-trace)))))
        (list-sort string<? flushed))
      ;; EVERY NAME A CRASH COULD TAKE AWAY while leaving a record that
      ;; depended on it. The mirror's directory appears twice from two
      ;; obligations that neither may assume the other ran: the takeover
      ;; barrier flushes the segments about to be delivered, and the
      ;; version barrier flushes the manifest those segments are history
      ;; BY -- and after a mid-session reload the second runs again and
      ;; the first does not. Above those sit writers/ (a writer directory
      ;; that appeared has its own entry), and the store's own identity
      ;; files with the store directory that holds them.
      (list-sort string<? (list "000001.sexp" A
                                "000001.sexp" "000002.sexp" "published.sexp" B B
                                "writers"
                                "meta.sexp" (store-basename)
                                "instance.sexp" (store-basename))))

;; INJECTED FAILURE AT THE BARRIER MUST DELIVER NOTHING. Half a barrier
;; is worse than none: the reducer would have applied records whose
;; durability was never established, which is exactly the state deps and
;; cuts must never be computed from.
;;
;; RUN IN A CHILD PROCESS, and not by arming the fault for this one.
;; THEOURGIA_INJECT is an expansion-time gate and fsync-fail is
;; persistent within its stage, so arming it here would fail the barrier
;; of every session in the file -- the rows above included. Skipping the
;; row when it is unarmed and breaking the file when it is armed means it
;; tests nothing in either configuration, which is worse than not having
;; it. The child arms it for itself and reports two numbers.
(build!)
(define child-path (string-append d "/barrier-child.ss"))
(define child-out (string-append d "/barrier-child.out"))
(define (write-child!)
  (put! child-path
      (string->utf8
        (string-append
          "#!chezscheme\n(import (chezscheme) (theourgia log))\n"
          "(define n 0)\n"
          "(define outcome\n"
          "  (guard (e (#t 'raised))\n"
          "    (let ((s (log-begin \"" d "\" (lambda args (set! n (+ n 1)) 'applied))))\n"
          "      (log-end! s)\n"
          "      'no-failure)))\n"
          "(printf \"~s ~s\\n\" outcome n)\n"))))
(write-child!)
(define child-status
  (system (string-append
            "THEOURGIA_INJECT=on "
            "THEOURGIA_FAULT=fsync-fail@deliver-barrier:file=writers "
            "scheme --script " child-path " > " child-out " 2>/dev/null")))
(define child-said
  (let ((text (utf8->string (slurp child-out))))
    (guard (e (#t (list 'unreadable text)))
      (let ((p (open-string-input-port text)))
        (list (read p) (read p))))))
(want "a barrier fsync failure delivers zero records"
      child-said (list 'raised 0))
;; CONTROL: the same child with nothing armed must deliver all three, or
;; the row above would also pass if the child were simply broken.
(build!)
(write-child!)
(system (string-append "scheme --script " child-path " > " child-out " 2>/dev/null"))
(want "CONTROL: the same child with no fault armed delivers every record"
      (let ((text (utf8->string (slurp child-out))))
        (guard (e (#t (list 'unreadable text)))
          (let ((p (open-string-input-port text)))
            (list (read p) (read p)))))
      (list 'no-failure 4))

;; The child takes the switch from THEOURGIA_TRACE rather than calling
;; trace-enable! itself: Chez invokes a library on first REFERENCE, so
;; the platform layer's load-time read of the environment runs at the
;; first log-begin -- after any call the child script made -- and would
;; overwrite it with the environment's value. The first version did call
;; trace-enable! and produced an empty trace, which reads exactly like a
;; lock that was never contended.
;;
;; THE LOCK IS HELD ACROSS THE WHOLE SESSION, and the only way to see
;; that from outside is to have something outside want it. A second
;; PROCESS is the honest observer here: within this process the guard
;; refuses before flock is ever reached, so an in-process attempt would
;; prove nothing about the lock. The child is killed while it waits --
;; its trace showing lock-wait and never flock is the reading.
(build!)
(define waiter-path (string-append d "/waiter.ss"))
(define waiter-out (string-append d "/waiter.trace"))
(put! waiter-path
      (string->utf8
        (string-append
          "#!chezscheme\n(import (chezscheme) (theourgia log))\n"
          "(let ((s (log-begin \"" d "\" (lambda args 'applied)))) (log-end! s))\n")))
(define waiter-trace "")
(define waiter-ran #f)
(let ((s (log-begin d (lambda args
                        (unless waiter-ran
                          (set! waiter-ran #t)
                          (system (string-append
                                    "THEOURGIA_TRACE=1 "
                                    "perl -e 'alarm 5; exec @ARGV' scheme --script "
                                    waiter-path " > /dev/null 2> " waiter-out))
                          (set! waiter-trace (utf8->string (slurp waiter-out))))
                        'applied))))
  (log-end! s))
(want "another process waits on the store lock for the whole callback"
      (list (has-substring? waiter-trace "lock-wait")
            (has-substring? waiter-trace "enter-critical"))
      (list #t #f))
;; CONTROL: with no session open the same child takes the lock at once
;; and reaches the critical section, so the reading above is about the
;; lock being held and not about the child being broken.
(build!)
(put! waiter-path
      (string->utf8
        (string-append
          "#!chezscheme\n(import (chezscheme) (theourgia log))\n"
          "(let ((s (log-begin \"" d "\" (lambda args 'applied)))) (log-end! s))\n")))
(system (string-append "THEOURGIA_TRACE=1 perl -e 'alarm 5; exec @ARGV' scheme --script "
                       waiter-path " > /dev/null 2> " waiter-out))
(want "CONTROL: with no session open it does not wait, and does enter"
      (let ((t (utf8->string (slurp waiter-out))))
        (list (has-substring? t "lock-wait") (has-substring? t "enter-critical")))
      (list #f #t))

(printf "== L24(j): one active operation per store ==\n")
(build!)
(define inner-result 'never-ran)
(let ((s (log-begin d (lambda (w seg off seq ts actor deps payload)
                        (when (eq? inner-result 'never-ran)
                          (set! inner-result
                                (guard (e ((log-error? e) (log-error-kind e)) (#t 'other))
                                  (log-begin d (lambda args 'applied))
                                  'accepted)))
                        'applied))))
  (log-end! s))
(want "a second log-begin from inside a callback is refused, not deadlocked"
      inner-result 'active-operation)
;; The guard publication and adopt will take is the same one, so it is
;; exercised here rather than waiting for them to exist.
(build!)
(define guard-during 'never-ran)
(let ((s (log-begin d (lambda args
                        (when (eq? guard-during 'never-ran)
                          (set! guard-during
                                (guard (e ((log-error? e) (log-error-kind e)) (#t 'other))
                                  (with-store-operation d 'publish (lambda () 'ran)))))
                        'applied))))
  (log-end! s))
(want "and so is any other store operation while a session is open"
      guard-during 'active-operation)
(want "after the session ends the store is free again"
      (list (store-operation-active? d)
            (with-store-operation d 'publish (lambda () 'ran)))
      (list #f 'ran))
;; An escape must not wedge the store for the rest of the process: the
;; next operation would be refused by a flag nobody is left to clear.
(build!)
;; THE REOPEN IS GUARDED so that a leak reports as a red row rather than
;; as an exception that ends the file. An unguarded call here made the
;; mutation that leaks the guard produce NO failing row at all -- the run
;; simply stopped, and a stopped run with zero FAIL lines reads exactly
;; like a clean one to anything counting failures.
(want "a callback that raises releases both the guard and the lock"
      (list (guard (e (#t 'raised))
              (log-begin d (lambda args (raise 'the-reducer-gave-up))))
            (store-operation-active? d)
            (guard (e ((log-error? e) (list 'refused (log-error-kind e))) (#t 'other))
              (let ((s (log-begin d (lambda args 'applied)))) (log-end! s) 'reopened)))
      (list 'raised #f 'reopened))

(printf "== a rejected record stops that writer, and only that writer ==\n")
;; rejected means "no premise for this record can ever arrive", so the
;; records after it in the same writer have nothing to be applied
;; against. Delivering them anyway asks the reducer to decide the same
;; unanswerable question once per record.
(build!)
(define rej-seen '())
(let ((s (log-begin d (lambda (w seg off seq ts actor deps payload)
                        (set! rej-seen (cons (list w seq) rej-seen))
                        (if (and (string=? w B) (= seq 1))
                            (list 'rejected 'unknown-verb)
                            'applied)))))
  (log-end! s))
(want "the rejected writer delivers nothing further, the other is untouched"
      (reverse rej-seen)
      (list (list B 1) (list A 1) (list A 2)))
(want "and the rejected writer's applied cursor never moved"
      (let ((s (log-begin d (lambda args 'applied))))
        (let ((out (map (lambda (f) (cons (car f) (cdr (assq 'applied (cdr f)))))
                        (session-frontiers s))))
          (log-end! s) out))
      (list (cons B 2) (cons A 2)))

(printf "== the store guard is keyed by identity, not by spelling ==\n")
;; "/s" and "/s/." are two strings and one directory. With a string key
;; the nested call slipped past the guard and then blocked on the flock
;; the outer call was holding -- the deadlock the guard exists to
;; prevent, reachable by writing the path differently.
(build!)
(define alias-seen 'never-ran)
(let ((s (log-begin d (lambda args
                        (when (eq? alias-seen 'never-ran)
                          (set! alias-seen
                                (list (store-operation-active? (string-append d "/."))
                                      (store-operation-active? (string-append d "/../"
                                        (let loop ((i (- (string-length d) 1)))
                                          (if (char=? (string-ref d i) #\/)
                                              (substring d (+ i 1) (string-length d))
                                              (loop (- i 1)))))))))
                        'applied))))
  (log-end! s))
(want "an aliased spelling of the same store is seen as the same store"
      alias-seen (list #t #t))
(want "CONTROL: a different store is not"
      (store-operation-active? "/tmp") #f)

(printf "== L24(a): the lock file is never replaced ==\n")
(build!)
(define lock-path (string-append d "/lock"))
(define before-ino (call-with-values (lambda () (path-device-inode lock-path)) list))
(define a-trace
  (traced (lambda ()
            (let ((s (log-begin d (lambda args 'applied)))) (log-end! s))
            (let ((s (log-begin d (lambda args 'applied)))) (log-end! s)))))
(want "the inode is the same across sessions"
      (call-with-values (lambda () (path-device-inode lock-path)) list) before-ino)
(want "and nothing renames, unlinks or relinks it"
      (filter (lambda (l) (and (has-substring? l "/lock")
                               (member (op-of l) '("rename" "unlink" "link" "create"))))
              (events a-trace))
      '())

(printf "== L24(c): the applied cursor belongs to the reducer ==\n")
;; Reading, checking and flushing a record all leave it where it was.
(build!)
(define c-session #f)
(define frontiers-while-pending #f)
(set! c-session
      (log-begin d (lambda (w seg off seq ts actor deps payload) 'pending)))
(set! frontiers-while-pending (session-frontiers c-session))
(want "answering pending everywhere leaves every applied cursor at zero"
      (map (lambda (f) (cons (car f) (cdr (assq 'applied (cdr f))))) frontiers-while-pending)
      (list (cons B 0) (cons A 0)))
(want "CONTROL: the contiguous frontier did advance, so the records were really read"
      (map (lambda (f) (cons (car f) (cdr (assq 'contiguous (cdr f))))) frontiers-while-pending)
      (list (cons B 2) (cons A 2)))
(want "the reducer's out-of-band confirmation is what moves it"
      (begin (session-applied! c-session 0 (list (cons A 2)))
             (map (lambda (f) (cons (car f) (cdr (assq 'applied (cdr f)))))
                  (session-frontiers c-session)))
      (list (cons B 0) (cons A 2)))
;; A REPORT WHOSE EPOCH DOES NOT MATCH. Named for what it actually
;; exercises: this batch has no reset, so no epoch is ever superseded --
;; the row reports a future epoch to an epoch-0 session. The
;; after-a-reset case belongs with reset and is not covered here.
(want "a report whose epoch does not match the session is refused"
      (guard (e ((log-error? e) (log-error-kind e)) (#t 'other))
        (session-applied! c-session 1 (list (cons A 2)))
        'accepted)
      'stale-epoch)
(want "a cut naming the same writer twice is malformed, not first-entry-wins"
      (guard (e (#t 'refused))
        (session-applied! c-session 0 (list (cons A 3) (cons A 1)))
        'accepted)
      'refused)
;; THE RETURNED CUT IS A COPY. Handing back the session's own alist made
;; set-cdr! a third way to move the applied cursor -- no epoch check, no
;; confirmation, no revision bump.
(want "mutating a returned cut does not move the session's cursor"
      (let ((reported (session-applied! c-session 0 (list (cons A 2)))))
        (set-cdr! (car reported) 99)
        (map (lambda (f) (cons (car f) (cdr (assq 'applied (cdr f)))))
             (session-frontiers c-session)))
      (list (cons B 0) (cons A 2)))
(log-end! c-session)

(printf "== L24(d): the preparation view moves only with the cursor ==\n")
(build!)
(define d-session (log-begin d (lambda args 'applied)))
(define v1 (session-view d-session))
(want "the view names the epoch, the writer and the next sequence"
      (list (view-epoch v1) (view-writer v1) (view-expect-seq v1))
      (list 0 A 3))
(define v2 (begin (session-applied! d-session 0 (list (cons A 2))) (session-view d-session)))
(want "confirming advances the revision, so a frame built on the old view is identifiable"
      (list (= (view-revision v1) (view-revision v2)) (view-revision v2))
      (list #f 1))
(want "and the old view is still immutable, field for field -- the cut included"
      (list (view-revision v1) (view-epoch v1) (view-writer v1) (view-expect-seq v1)
            (view-applied-cut v1))
      (list 0 0 A 3 (list (cons B 2) (cons A 2))))
(want "and mutating a view's cut does not reach the session either"
      (let ((v (session-view d-session)))
        (if (pair? (view-applied-cut v))
            (begin (set-cdr! (car (view-applied-cut v)) 77)
                   (map (lambda (f) (cons (car f) (cdr (assq 'applied (cdr f)))))
                        (session-frontiers d-session)))
            'no-cut-to-mutate))
      (list (cons B 2) (cons A 2)))
(log-end! d-session)

(printf "== L24(k): the seam vocabulary this batch emits ==\n")
;; The full enumeration needs the write path (registry-check,
;; registry-write, catch-up, frame, apply, ftruncate, write). What is
;; asserted here is the shape of every op this batch does emit; the rest
;; is named in the delivery letter as not yet reachable.
(build!)
(define k-trace (traced (lambda () (let ((s (log-begin d (lambda args 'applied)))) (log-end! s)))))
(want "the lock ops carry (path . mode) and no byte count"
      (let ((ls (filter (lambda (l) (string=? (op-of l) "flock")) (events k-trace))))
        (list (and (pair? ls) (has-substring? (car ls) "/lock . exclusive"))
              (and (pair? ls) (has-substring? (car ls) " #f)"))))
      (list #t #t))
(want "enter-critical is emitted inside the critical section, with the same subject"
      (let ((es (ops k-trace)))
        (and (member "enter-critical" es)
             (< (index-of es (lambda (o) (string=? o "flock")))
                (index-of es (lambda (o) (string=? o "enter-critical"))))
             (< (index-of es (lambda (o) (string=? o "enter-critical")))
                (index-of es (lambda (o) (string=? o "unlock"))))))
      #t)
(want "fsync carries a bare path and no byte count"
      (let ((ls (filter (lambda (l) (string=? (op-of l) "fsync")) (events k-trace))))
        ;; ENDS WITH, NOT CONTAINS. A root that happens to contain
        ;; ".sexp" made a directory flush satisfy a row about file paths.
        (list (and (pair? ls) (ends-with-before-space? (car ls) ".sexp"))
              (and (pair? ls) (has-substring? (car ls) " #f)")))) 
      (list #t #t))
(want "every session takes and releases exactly one store lock"
      (list (count-of k-trace "flock") (count-of k-trace "unlock"))
      (list 1 1))
;; create / rename / link / unlink are emitted by ffi.ss for the crash
;; model. atomic-write! is the one path in this batch that makes a
;; directory entry appear, so it is what shows their shape.
(define entry-trace
  (traced (lambda () (atomic-write! (string-append d "/probe.sexp") (string->utf8 "x\n") 'snapshot))))
;; Reported field by field rather than by taking the car of a list that
;; may be empty: an absent event is the very thing this row exists to
;; catch, and it must come out as a reading, not as an exception.
(want "create names the temporary, rename names (old . new), and both carry no byte count"
      (let ((cs (filter (lambda (l) (string=? (op-of l) "create")) (events entry-trace)))
            (rs (filter (lambda (l) (string=? (op-of l) "rename")) (events entry-trace))))
        (list (length cs) (length rs)
              (and (pair? rs) (has-substring? (car rs) " . "))
              (and (pair? rs) (has-substring? (car rs) " #f)"))))
      (list 1 1 #t #t))

(printf "\n~a failures\n" bad)
(printf "log11 complete\n")
