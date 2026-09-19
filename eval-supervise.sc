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

;;; (theourgia eval-supervise) -- watching one evaluation, in the caller's
;;; own process.
;;;
;;; NEVER: NO HELPER PROCESS BETWEEN US AND THE CHILD. The adapter owns the
;;; child, so the child's end is the adapter's DOWN and nothing has to be
;;; reaped by hand.
;;;
;;; NEVER: THE HANDSHAKE IS A SAFETY ORDERING, NOT A GREETING. Until the
;;; worker's `(ready <pgid>)` has arrived there may be no process group,
;;; so a timeout before it may kill only the direct child; after it, the
;;; whole group. A worker that never says ready is killed by pid and
;;; reported unavailable -- which is also the only safe thing to do,
;;; since at that point it has run no untrusted code and can have no
;;; grandchildren.
;;;
;;; NOTE: THE DEADLINE IS ABSOLUTE AND IT COVERS THE MAILBOX. It is computed
;;; once, from the moment `spawn-worker!` returns, and every wait below is
;;; `(max 0 (- deadline (now)))` -- a budget recomputed per message is a
;;; budget a chatty child can refill.

(library (theourgia eval-supervise)
  (export supervise-eval)
  (import (rnrs base) (rnrs control) (rnrs lists) (rnrs bytevectors)
          ;; NOTE: `utf8->string` COMES FROM `(rnrs bytevectors)`, and this file
          ;; must not define its own. An earlier draft did -- decoding byte
          ;; by byte through `integer->char`, which is Latin-1 -- and it
          ;; would have quietly mangled every non-ASCII character the
          ;; output quota is measured in.
          (only (chezscheme) real-time
                open-string-input-port read guard condition? message-condition?
                condition-message)
          (only (theourgia sched) spawn receive send self sleep-ms monitor)
          (only (theourgia proc) spawn-worker! worker-write! worker-close-stdin!
                worker-kill! worker-close! worker-rss worker-ref-pid)
          (only (theourgia ffi) signal-pid!)
          (only (theourgia render) render-wire))

  ;; NOTE: THE TWO CLOCKS ARE DIFFERENT AND BOTH ARE NEEDED. `ready-ms` is
  ;; how long a worker may take to exist; the caller's timeout is how long
  ;; an evaluation may take once it does. Folding them into one would let
  ;; a slow start eat the evaluation's budget.
  (define ready-ms 500)
  (define rss-interval-ms 50)

  ;; NEVER: SIGKILL, AND TO THE WHOLE GROUP ONCE THERE IS ONE. `worker-kill!`
  ;; takes a SIGNAL NUMBER and signals the child itself; a grandchild the
  ;; evaluation started would survive it. After `ready` the worker has a
  ;; session of its own, so the negative pid reaches every process in it.
  ;;
  ;; NOTE: AND THE ADAPTER IS CLOSED TOO. Killing the child ends the child;
  ;; the adapter is what owns the pipes, and leaving it alive leaves this
  ;; process waiting for streams that will never say anything again.
  (define sigkill 9)

  (define (stop-group! ref pgid)
    (when pgid (signal-pid! (- pgid) sigkill))
    (worker-kill! ref sigkill)
    (worker-close! ref))

  ;; ---- the snapshot the worker evaluates against -----------------------------
  ;;
  ;; NEVER: TAKEN BEFORE THE CHILD EXISTS. The committed side is pinned by the
  ;; cut, which loads deterministically; the drafts are pinned by being
  ;; copied here. A commit landing while the evaluation runs changes
  ;; neither, which is what "the view is fixed at spawn" means.
  (define (view-datum writer cut drafts)
    (list 'working writer cut drafts))

  ;; ---- one evaluation --------------------------------------------------------

  (define (supervise-eval spec)
    (let ((store (spec-of spec 'store))
          (cut (spec-of spec 'cut))
          (under (spec-of spec 'under))
          (source (spec-of spec 'source))
          (timeout-ms (spec-of spec 'timeout-ms))
          (memory-bytes (spec-of spec 'memory-bytes))
          (output-bytes (spec-of spec 'output-bytes))
          (view (spec-of spec 'view)))
      ;; NOTE: THE CPU CEILING GOES IN argv, because the worker installs it
      ;; before it says ready and ready comes before anything we send.
      ;; It is the wall budget rounded up plus one second: a limit below
      ;; the wall clock would turn every slow-but-legal evaluation into a
      ;; CPU kill, and one far above it would never fire before the
      ;; supervisor's own deadline did.
      (let* ((cpu-seconds (+ 1 (div (+ timeout-ms 999) 1000)))
             (me self)
             (pid (spawn-worker! (spec-of spec 'scheme)
                                 (list "scheme" "--script" (spec-of spec 'worker)
                                       store cut under
                                       (number->string cpu-seconds)
                                       (number->string output-bytes))
                                 '()
                                 me))
             (deadline (+ (real-time) timeout-ms)))
        (wait-for-ready pid me deadline
                        (lambda (ref pgid)
                          (run ref pgid me deadline source view
                               timeout-ms memory-bytes output-bytes))))))

  (define (spec-of spec key)
    (let ((e (assq key spec))) (and e (cdr e))))

  ;; NOTE: THE WORKER'S PATH COMES FROM THE CALLER. Resolving it here against
  ;; the working directory would work from the repository root and fail
  ;; everywhere else -- the caller is the one that knows where it was run
  ;; from.

  ;; NEVER: BEFORE READY WE MAY KILL ONLY THE pid. There is no group yet, and
  ;; there is also nothing to escape: the worker has read no source.
  (define (wait-for-ready pid me deadline continue)
    (let ((ready-deadline (+ (real-time) ready-ms)))
      (let wait ((ref #f))
        (receive
          (after (max 0 (- ready-deadline (real-time)))
                 ;; NEVER: BY pid ONLY: there is no group yet, and there is
                 ;; nothing to escape either -- no source has been read.
                 (when ref (worker-kill! ref sigkill) (worker-close! ref))
                 '(error eval-worker-unavailable (reason no-ready)))
          (`(spawned ,r) (wait r))
          (`(worker-out ,r ,stream ,bv)
           (let ((datum (first-datum bv)))
             (if (and (pair? datum) (eq? 'ready (car datum)))
                 (continue r (cadr datum))
                 (wait r))))
          ;; NEVER: A REFUSED SPAWN ARRIVES AS THE ADAPTER'S DOWN, and it is
          ;; the only thing that arrives: no ref, no I/O, nothing to kill.
          (`#(DOWN ,who ,reason)
           (list 'error 'spawn-refused (list 'reason (reason-text reason))))))))

  (define (first-datum bv)
    (guard (e (#t #f)) (read (open-string-input-port (utf8->string bv)))))

  (define (reason-text reason)
    (cond ((and (condition? reason) (message-condition? reason)) (condition-message reason))
          ((pair? reason) reason)
          (else reason)))

  (define (run ref pgid me deadline source view timeout-ms memory-bytes output-bytes)
    (worker-write! ref (string->utf8 (render-wire 'go)) 'go)
    (worker-write! ref (string->utf8 (render-wire view)) 'view)
    (worker-write! ref (string->utf8 (render-wire (list 'source source))) 'source)
    (worker-close-stdin! ref)
    (collect ref pgid deadline "" "" "" 0 (+ (real-time) rss-interval-ms)
             memory-bytes output-bytes timeout-ms #f))

  ;; NOTE: THE QUOTA COUNTS DECODED USER BYTES, ACROSS BOTH STREAMS. What
  ;; travels is the framed text, where a backslash is two characters; the
  ;; promise is about what the user printed.
  (define (collect ref pgid deadline protocol out err used next-rss
                   memory-bytes output-bytes timeout-ms exited)
    (cond
      ((>= (real-time) deadline)
       (stop-group! ref pgid)
       (limit-answer 'time timeout-ms out))
      ;; NEVER: THE SAMPLE DEADLINE ADVANCES WHEN A SAMPLE IS TAKEN, NOT WHEN
      ;; THE POLL TICKS. Pushing it forward from the `after` branch --
      ;; which is what this did -- moves it 50ms into the future every
      ;; 25ms, so on a worker that says nothing it is never due and the
      ;; memory budget is never read at all. Such a worker ran to the
      ;; TIME limit instead, an answer that names the wrong resource and
      ;; arrives after the whole clock rather than at 50ms.
      ((>= (real-time) next-rss)
       (let ((rss (worker-rss ref)))
         (if (and rss (> rss memory-bytes))
             (begin (stop-group! ref pgid)
                    (limit-answer 'memory memory-bytes out))
             (collect ref pgid deadline protocol out err used
                      (+ (real-time) rss-interval-ms)
                      memory-bytes output-bytes timeout-ms exited))))
      (else
       (receive
         (after (min 25 (max 0 (- deadline (real-time))))
                (collect ref pgid deadline protocol out err used next-rss
                         memory-bytes output-bytes timeout-ms exited))
         (`(worker-out ,r ,stream ,bv)
          (if (eq? stream 'stdout)
              (collect ref pgid deadline (string-append protocol (utf8->string bv))
                       out err used next-rss memory-bytes output-bytes timeout-ms exited)
              (let-values (((o e n) (absorb-frames (utf8->string bv))))
                (let ((used (+ used n)))
                  (if (> used output-bytes)
                      (begin (stop-group! ref pgid)
                             (limit-answer 'output output-bytes (string-append out o)))
                      (collect ref pgid deadline protocol
                               (string-append out o) (string-append err e) used
                               next-rss memory-bytes output-bytes timeout-ms exited))))))
         (`(worker-eof ,r ,stream)
          (collect ref pgid deadline protocol out err used next-rss
                   memory-bytes output-bytes timeout-ms exited))
         (`(worker-error ,r ,stream ,n)
          (collect ref pgid deadline protocol out err used next-rss
                   memory-bytes output-bytes timeout-ms exited))
         (`(worker-exit ,r ,status ,signal)
          (collect ref pgid deadline protocol out err used next-rss
                   memory-bytes output-bytes timeout-ms (list status signal)))
         ;; NEVER: THE ADAPTER'S DOWN IS THE END, and only then is the whole
         ;; story in: the exit AND both streams have been seen.
         (`#(DOWN ,who ,reason)
          (finish protocol out err exited))))))

  (define (limit-answer resource limit out)
    (list 'error 'eval-limit (list 'resource resource) (list 'limit limit)
          (list 'stdout out)))

  ;; NEVER: A PROTOCOL LINE THAT NEVER FINISHED IS NOT AN ANSWER. The worker
  ;; writes its result with a terminating newline; anything else means the
  ;; child died with the answer half-written, and reporting the fragment
  ;; as a result would hand back something nobody produced.
  (define (finish protocol out err exited)
    (let ((datum (and (ends-with-newline? protocol)
                      (guard (e (#t #f)) (read (open-string-input-port protocol))))))
      (cond
        ((not datum)
         (list 'error 'eval-worker-exit
               (list 'status (if (pair? exited) (car exited) 'unknown))))
        ((and (pair? datum) (eq? 'ok (car datum)))
         (append datum (list (list 'stdout out) (list 'stderr err))))
        (else datum))))

  (define (ends-with-newline? text)
    (let ((n (string-length text)))
      (and (> n 0) (char=? (string-ref text (- n 1)) #\newline))))

  ;; NOTE: FRAMES ARRIVE IN PIECES AND IN ANY MIXTURE. Each complete line is
  ;; one datum; a partial tail is kept for the next chunk. NEVER: The count
  ;; returned is the DECODED length, which is what the quota is about.
  (define (absorb-frames text)
    (let loop ((rest text) (out "") (err "") (n 0))
      (let ((cut (newline-at rest)))
        (if (not cut)
            (values out err n)
            (let* ((line (substring rest 0 cut))
                   (tail (substring rest (+ cut 1) (string-length rest)))
                   (datum (guard (e (#t #f)) (read (open-string-input-port line)))))
              (cond
                ((and (pair? datum) (eq? 'out (car datum)) (string? (cadr datum)))
                 (loop tail (string-append out (cadr datum)) err
                       (+ n (string-length (cadr datum)))))
                ((and (pair? datum) (eq? 'err (car datum)) (string? (cadr datum)))
                 (loop tail out (string-append err (cadr datum))
                       (+ n (string-length (cadr datum)))))
                (else (loop tail out err n))))))))

  (define (newline-at text)
    (let loop ((i 0))
      (cond ((>= i (string-length text)) #f)
            ((char=? (string-ref text i) #\newline) i)
            (else (loop (+ i 1))))))
)
