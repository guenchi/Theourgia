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

;;; (theourgia proc) -- child processes, one adapter process per child.
;;;
;;; ⭐ THE SAME SHAPE AS `net`, AND THE SAME REASON. The adapter owns the
;;; child, so its death is what ends the child: igropyr signals a dying
;;; owner's children and delivers `#(DOWN pid reason)` to the monitors.
;;; Nothing here retires, guards, or keeps a registry.
;;;
;;; ⛔ AND NO VERB GOES THROUGH THE ADAPTER AT ALL. `net` needs `start`
;;; and `stop` because they move the target and the read state; a child's
;;; reads begin at birth and its target never moves, so writing, closing
;;; stdin, signalling and closing are all direct calls.
;;;
;;; ⚠️ CLOSING IS KILLING THE ADAPTER, here as in `net`. It cannot be a
;;; direct `tcp-close!`: a child's pipes have no free hook slot -- the
;;; library owns it -- so killing the owner is the one spelling that
;;; works on both sides.

(library (theourgia proc)
  (export spawn-worker! worker-write! worker-close-stdin!
          worker-kill! worker-close! worker-alive? worker-rss
          worker-ref? worker-ref-proc worker-ref-pid)
  (import (rnrs base) (rnrs control) (rnrs records syntactic)
          (rnrs lists)
          (only (igropyr tcp)
                proc-spawn! proc-write! proc-stdin-close! proc-kill! proc-pid)
          (only (igropyr actor)
                spawn send self receive monitor kill process-alive?)
          (only (theourgia ffi) process-alive-signal0? process-rss-bytes))

  (define-record-type (worker-ref make-worker-ref worker-ref?)
    (fields (immutable proc worker-ref-proc)
            (immutable pid worker-ref-pid)))

  ;; ---- birth -------------------------------------------------------------
  ;;
  ;; The adapter spawns the child itself, so igropyr makes it the owner
  ;; and there is no state in which a child exists with no adapter, or an
  ;; adapter waits for a child that was refused.
  ;;
  ;; ⚠️ THE CALLER GETS A pid, NOT A ref. The ref arrives as `(spawned
  ;; ref)` once the child exists; a refusal arrives as the adapter's DOWN
  ;; carrying `(spawn-refused reason)`. The caller monitors that pid
  ;; before it can miss either.
  (define (spawn-worker! file argv opts target)
    (spawn (lambda () (worker-adapter file argv opts target))))

  (define (worker-adapter file argv opts target)
    (let ((me self))
      (monitor target)
      ;; ⛔ NO GUARD: igropyr refuses an empty argv, a bad cwd or a bad
      ;; env by RAISING, and that raise is this process's death reason.
      ;; A returned `(failed . reason)` is the same ending by another
      ;; road, so it is spelled the same way.
      (let ((p (proc-spawn! file argv (cons (cons 'owner me) opts))))
        (when (and (pair? p) (eq? 'failed (car p)))
          (kill me (list 'spawn-refused (cdr p))))
        (let ((ref (make-worker-ref p me)))
          (send target (list 'spawned ref))
          (serving ref p target #f #f #f)))))

  ;; ⛔ A STREAM'S EOF IS NOT THE CHILD'S END, and the child's exit is not
  ;; the streams' end. igropyr's own P13 sequence closes stdout while the
  ;; child still runs, writes to stderr, and only then exits; a
  ;; grandchild can hold a pipe open long after its parent is reaped.
  ;; The adapter ends only when the exit AND both streams have been seen
  ;; -- and a consumer that does not want to wait for a held pipe closes
  ;; the worker, which kills this process.
  (define (serving ref p target exited out-done err-done)
    (if (and exited out-done err-done)
        (kill self (cons 'exited exited))
        (receive
          (`#(proc-data ,pp ,stream ,bv)
           (send target (list 'worker-out ref stream bv))
           (serving ref p target exited out-done err-done))
          (`#(proc-eof ,pp ,stream)
           (send target (list 'worker-eof ref stream))
           (serving ref p target exited
                    (or out-done (eq? stream 'stdout))
                    (or err-done (eq? stream 'stderr))))
          ;; An error on a stream is that stream's ending too: nothing
          ;; further will arrive on it, so waiting for an EOF that cannot
          ;; come would hold the adapter for ever.
          (`#(proc-error ,pp ,stream ,n)
           (send target (list 'worker-error ref stream n))
           (serving ref p target exited
                    (or out-done (eq? stream 'stdout))
                    (or err-done (eq? stream 'stderr))))
          (`#(proc-exit ,pp ,status ,signal)
           (send target (list 'worker-exit ref status signal))
           (serving ref p target (list status signal) out-done err-done))
          (`#(DOWN ,who ,r) (kill self (list 'target-down r))))))

  ;; ---- the verbs, all direct --------------------------------------------
  ;;
  ;; ⚠️ EXACTLY ONE COMPLETION PER RETURNED CALL. igropyr answers a write
  ;; it cannot even attempt by returning #f without ever running the
  ;; completion, so the facade supplies that one itself -- otherwise a
  ;; caller holding a token would wait for something nobody will send.
  (define (worker-write! ref bv tok)
    (let* ((me self)
           (ok (proc-write! (worker-ref-proc ref) bv
                            (lambda (status)
                              (send me (list 'written ref tok status))))))
      (unless ok (send me (list 'written ref tok 'refused)))
      ok))

  (define (worker-close-stdin! ref) (proc-stdin-close! (worker-ref-proc ref)))

  (define (worker-kill! ref signum) (proc-kill! (worker-ref-proc ref) signum))

  ;; ⚠️ SIGTERM, NOT A SIGNAL OF OUR CHOOSING: closing kills the adapter,
  ;; and igropyr signals a dead owner's children with SIGTERM. A child
  ;; that ignores it is the eval guardian's business, by process group.
  (define (worker-close! ref)
    (let ((pid (worker-ref-pid ref)))
      (when (process-alive? pid) (kill pid 'closed))))

  ;; ⛔ LIVENESS IS ASKED OF THE OPERATING SYSTEM, not of igropyr's
  ;; record: between a child dying and the exit callback running, that
  ;; record still reads as running, and a sampler trusting it would
  ;; report memory for a process that no longer exists.
  (define (worker-alive? ref)
    (let ((pid (proc-pid (worker-ref-proc ref))))
      (and pid (process-alive-signal0? pid))))

  (define (worker-rss ref)
    (let ((pid (proc-pid (worker-ref-proc ref))))
      (and pid (process-rss-bytes pid))))
)
