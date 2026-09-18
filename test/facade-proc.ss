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

;; (theourgia proc) UNDER §7.6.32/§7.6.33.
;;
;; ⭐ THE SEQUENCE IS THE TEST, NOT THE MAPPINGS. Every translation can be
;; right on its own while the adapter ends too early: igropyr's own P13
;; closes a child's stdout while it still runs, writes to stderr, then
;; exits. An adapter that treated the first EOF as the end would pass
;; every per-message row and lose everything after it.
;;
;; ⛔ AND EVERY ENDING IS READ FROM `#(DOWN pid reason)`. There is no
;; table of adapters to ask, which is the point: a count this library
;; kept about itself could be told that a live process was gone, and
;; when that happened two rows written for the leak went green together.

(import (chezscheme) (theourgia sched) (theourgia proc)
        (only (igropyr tcp) proc-count))

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

(define (settles-to base read)
  (let loop ((k 0))
    (cond ((<= (read) base) 'back)
          ((> k 200) (list 'still (- (read) base)))
          (else (sleep-ms 20) (loop (+ k 1))))))

;; What `ps` says about a pid, in kilobytes: a reading from outside this
;; tree, so a row about a memory measurement is not checking the tree
;; against itself.
(define (ps-rss-kb pid)
  (let ((out (string-append "/tmp/p32-ps-" (number->string (get-process-id)))))
    (system (string-append "ps -o rss= -p " (number->string pid) " > " out " 2>/dev/null"))
    (guard (e (#t #f))
      (let ((n (call-with-input-file out read)))
        (and (number? n) (> n 0) n)))))

(define (digits-of t)
  (let* ((n (string-length t))
         (d (let trim ((i 0) (out '()))
              (cond ((>= i n) (list->string (reverse out)))
                    ((char-numeric? (string-ref t i))
                     (trim (+ i 1) (cons (string-ref t i) out)))
                    (else (list->string (reverse out)))))))
    (and (> (string-length d) 0) (string->number d))))

;; ⚠️ ps ANSWERS IN KILOBYTES, this library in bytes. Written without the
;; conversion an earlier row called two readings that agreed exactly --
;; 2129920 and 2080 -- a disagreement by a factor of 1024.
(define (agrees? pid ours)
  (let ((outside (and pid (ps-rss-kb pid))))
    (cond
      ((not (and pid ours outside)) (list 'incomplete pid ours outside))
      ((and (< ours (* 2 (* outside 1024))) (> (* 2 ours) (* outside 1024))) 'agrees)
      (else (list 'disagrees ours (* outside 1024))))))

(start-scheduler
  (lambda ()
    (let ((main self))

      ;; ---- P-01 an ordinary child, start to finish --------------------
      ;;
      ;; ⚠️ THE DOWN COMES LAST. Messages the adapter sent before it died
      ;; are ahead of the runtime's DOWN in this mailbox, so a consumer
      ;; that reads in order sees the child's whole story and then its
      ;; ending.
      (let* ((pid (spawn-worker! "/bin/sh" '("sh" "-c" "echo hi; exit 3") '() main))
             (m (monitor pid)))
        (want "P-01 a child's output, exit and DOWN all arrive, DOWN last"
              (let wait ((seen '()))
                (receive
                  (after 6000 (list 'incomplete (reverse seen)))
                  (`(spawned ,r) (wait (cons 'spawned seen)))
                  (`(worker-out ,r ,s ,bv) (wait (cons (list 'out (utf8->string bv)) seen)))
                  (`(worker-eof ,r ,s) (wait (cons (list 'eof s) seen)))
                  (`(worker-exit ,r ,st ,sg) (wait (cons (list 'exit st sg) seen)))
                  (`#(DOWN ,w ,rr)
                   (list (if (memq 'spawned seen) 'had-ref 'no-ref)
                         (if (member '(exit 3 0) seen) 'had-exit 'no-exit)
                         (if (and (pair? rr) (eq? 'exited (car rr))) 'down-exited rr)))))
              '(had-ref had-exit down-exited)))

      ;; ---- P-02 a refusal igropyr RAISES ------------------------------
      ;;
      ;; ⛔ AN EMPTY ARGV IS REJECTED BY RAISING, not by a return value,
      ;; and there is no guard in the adapter -- so the condition itself
      ;; becomes the death reason. §L⑥: a consumer must treat a reason it
      ;; does not recognise as an unexpected ending and log it, ⛔ never
      ;; match the set exhaustively.
      (let ((base-proc (proc-count))
            (base-procs (process-count)))
        (let* ((pid (spawn-worker! "/bin/sh" '() '() main))
               (m (monitor pid)))
          (want "P-02 a refusal that arrives by raising is still just a DOWN"
                (receive (after 5000 'no-down)
                         (`(spawned ,r) 'unexpectedly-spawned)
                         (`#(DOWN ,w ,r) (if (condition? r) 'condition (list 'other r))))
                'condition))
        (want "P-02 TWIN: and it leaves no child and no process behind"
              (list (settles-to base-proc proc-count)
                    (settles-to base-procs process-count))
              '(back back)))

      ;; ---- P-03 a refusal igropyr RETURNS -----------------------------
      (let ((base-proc (proc-count)))
        (let* ((pid (spawn-worker! "/nonexistent-for-this-row" '("x") '() main))
               (m (monitor pid)))
          (want "P-03 a missing executable ends as DOWN (spawn-refused …)"
                (receive (after 5000 'no-down)
                         (`(spawned ,r) 'unexpectedly-spawned)
                         (`#(DOWN ,w ,r)
                          (if (and (pair? r) (eq? 'spawn-refused (car r)))
                              'spawn-refused
                              (list 'other r))))
                'spawn-refused))
        (want "P-03 TWIN: and proc-count never moved"
              (settles-to base-proc proc-count) 'back))

      ;; ---- P-04 closing a worker ends the child -----------------------
      ;;
      ;; ⚠️ §L⑦: CLOSING IS KILLING THE ADAPTER, so the child is signalled
      ;; by the runtime with SIGTERM -- not with a signal we chose. A
      ;; child that ignores SIGTERM is the guardian's problem, by group.
      ;;
      ;; ⚠️ §L⑧: AFTERWARDS NOTHING MORE COMES FROM THAT REF. The adapter
      ;; is gone, so there is nobody left to translate the child's exit;
      ;; already-queued messages remain observable and the DOWN is the
      ;; boundary in the mailbox.
      (let ((base-proc (proc-count)))
        (let* ((pid (spawn-worker! "/bin/sh" '("sh" "-c" "sleep 30") '() main))
               (m (monitor pid)))
          (let ((ref (receive (after 5000 'none) (`(spawned ,r) r))))
            (want "P-04 the worker is running before we close it"
                  (and (not (eq? ref 'none)) (worker-alive? ref)) #t)
            (worker-close! ref)
            (want "P-04 TWIN: closing it ends the child and the adapter"
                  (begin
                    (receive (after 4000 'no-down) (`#(DOWN ,w ,r) 'down))
                    (list (settles-to base-proc proc-count)
                          (let settle ((k 0))
                            (cond ((not (worker-alive? ref)) 'child-gone)
                                  ((> k 200) 'child-still-running)
                                  (else (sleep-ms 20) (settle (+ k 1)))))))
                  '(back child-gone))
            (want "P-04 and nothing further arrives from that ref"
                  (let listen ((k 0))
                    (receive
                      (after 60 (if (< k 20) (listen (+ k 1)) 'nothing-more))
                      (`(worker-exit ,r ,st ,sg) (if (eq? r ref) 'still-reporting (listen (+ k 1))))
                      (`(worker-out ,r ,s ,bv) (if (eq? r ref) 'still-reporting (listen (+ k 1))))
                      (`(worker-eof ,r ,s) (if (eq? r ref) 'still-reporting (listen (+ k 1))))))
                  'nothing-more))))

      ;; ---- P-05 igropyr's own P13 ordering ----------------------------
      ;;
      ;; stdout ends while the child runs, stderr speaks afterwards, the
      ;; exit comes last. An adapter that ended at the first EOF would
      ;; lose the stderr line and the exit.
      (let* ((pid (spawn-worker! "/bin/sh"
                    '("sh" "-c" "exec 1>&-; sleep 0.2; printf late >&2; exit 7") '() main))
             (m (monitor pid)))
        (want "P-05 output after the first stream ends still arrives, then the exit"
              (let wait ((late #f) (ex #f))
                (receive
                  (after 8000 (list 'incomplete late ex))
                  (`(spawned ,r) (wait late ex))
                  (`(worker-out ,r ,s ,bv)
                   (wait (if (eq? s 'stderr) (utf8->string bv) late) ex))
                  (`(worker-eof ,r ,s) (wait late ex))
                  (`(worker-error ,r ,s ,n) (wait late ex))
                  (`(worker-exit ,r ,st ,sg) (wait late (list st sg)))
                  (`#(DOWN ,w ,rr) (list late ex))))
              '("late" (7 0))))

      ;; ---- P-06 a signal, and what the exit says about it -------------
      (let* ((pid (spawn-worker! "/bin/sh" '("sh" "-c" "sleep 30") '() main))
             (m (monitor pid)))
        (let ((ref (receive (after 5000 'none) (`(spawned ,r) r))))
          (want "P-06 a worker killed by signal reports that signal"
                (begin
                  (worker-kill! ref 9)
                  (let wait ()
                    (receive
                      (after 6000 'no-exit)
                      (`(worker-exit ,r ,st ,sg) (if (eq? r ref) (list st sg) (wait)))
                      (`(worker-out ,r ,s ,bv) (wait))
                      (`(worker-eof ,r ,s) (wait))
                      (`(worker-error ,r ,s ,n) (wait))
                      (`#(DOWN ,w ,rr) 'down-before-exit))))
                '(0 9))
          (receive (after 3000 'no-down) (`#(DOWN ,w ,r) 'down))))

      ;; ---- P-07 one completion per returned write ---------------------
      ;;
      ;; ⚠️ igropyr ANSWERS #f FOR A WRITE IT CANNOT EVEN ATTEMPT, without
      ;; ever running the completion -- so the facade supplies that one
      ;; itself. Otherwise a caller holding a token waits for something
      ;; nobody will send.
      (let* ((pid (spawn-worker! "/bin/sh" '("sh" "-c" "cat") '() main))
             (m (monitor pid)))
        (let ((ref (receive (after 5000 'none) (`(spawned ,r) r))))
          (want "P-07 a write that is accepted completes exactly once"
                (begin
                  (worker-write! ref (string->utf8 "ping\n") 'w1)
                  (let wait ((n 0) (st 'none))
                    (receive
                      (after 800 (list n st))
                      (`(written ,r ,tok ,s) (if (eq? tok 'w1) (wait (+ n 1) s) (wait n st)))
                      (`(worker-out ,r ,s ,bv) (wait n st))
                      (`(worker-eof ,r ,s) (wait n st)))))
                '(1 0))
          ;; ⛔ AND A WRITE igropyr REFUSES OUTRIGHT STILL COMPLETES.
          ;; It answers #f without ever running the completion closure,
          ;; so the facade supplies that one itself -- otherwise a caller
          ;; holding a token waits for something nobody will ever send.
          ;; Measured: the seed that removes it survived every other row
          ;; in this file, because nothing else writes to a worker whose
          ;; child is already gone.
          (worker-close! ref)
          (receive (after 3000 'no-down) (`#(DOWN ,w ,r) 'down))
          (want "P-07 TWIN: a write that is refused outright completes too"
                (let ((answered (worker-write! ref (string->utf8 "too late\n") 'w2)))
                  (list answered
                        (let wait ((k 0))
                          (receive
                            (after 60 (if (< k 20) (wait (+ k 1)) 'no-completion))
                            (`(written ,r ,tok ,st) (if (eq? tok 'w2) st (wait (+ k 1))))
                            (`(worker-out ,r ,s ,bv) (wait (+ k 1)))
                            (`(worker-eof ,r ,s) (wait (+ k 1)))
                            (`(worker-exit ,r ,st2 ,sg) (wait (+ k 1)))))))
                '(#f refused))))

      ;; ---- P-08 the memory reading, against an outside one ------------
      ;;
      ;; ⛔ A NUMBER IN A WIDE BAND IS NOT A READING: `(and (worker-alive?
      ;; ref) 1)` is a number in that band. Each child prints its own pid
      ;; and the row compares this library's reading with `ps` for that
      ;; same pid, and requires the one holding forty megabytes to read
      ;; far larger than the one holding none.
      (let* ((big (spawn-worker! "/bin/sh"
                    '("sh" "-c" "x=$(dd if=/dev/zero bs=1000000 count=40 2>/dev/null | tr '\\0' 'a'); echo $$; sleep 8")
                    '() main))
             (small (spawn-worker! "/bin/sh" '("sh" "-c" "echo $$; sleep 8") '() main))
             (mb (monitor big)) (ms (monitor small)))
        (let gather ((refs '()) (pids '()) (k 0))
          (if (or (and (= 2 (length refs)) (= 2 (length pids))) (> k 200))
              (let* ((big-ref (cond ((assq big refs) => cdr) (else #f)))
                     (small-ref (cond ((assq small refs) => cdr) (else #f)))
                     (big-pid (cond ((assq big pids) => cdr) (else #f)))
                     (small-pid (cond ((assq small pids) => cdr) (else #f)))
                     (big-rss (and big-ref (worker-rss big-ref)))
                     (small-rss (and small-ref (worker-rss small-ref))))
                (want "P-08 each reading agrees with ps about that same pid"
                      (list (agrees? big-pid big-rss) (agrees? small-pid small-rss))
                      '(agrees agrees))
                (want "P-08 TWIN: the child holding forty megabytes reads far larger"
                      (and (number? big-rss) (number? small-rss) (> big-rss (* 10 small-rss)))
                      #t)
                (when big-ref (worker-close! big-ref))
                (when small-ref (worker-close! small-ref)))
              (receive
                (after 60 (gather refs pids (+ k 1)))
                (`(spawned ,r) (gather (cons (cons (worker-ref-pid r) r) refs) pids (+ k 1)))
                (`(worker-out ,r ,s ,bv)
                 (let ((d (digits-of (utf8->string bv))))
                   (gather refs (if d (cons (cons (worker-ref-pid r) d) pids) pids) (+ k 1))))
                (`(worker-eof ,r ,s) (gather refs pids (+ k 1)))
                (`(worker-exit ,r ,st ,sg) (gather refs pids (+ k 1)))
                (`#(DOWN ,w ,rr) (gather refs pids (+ k 1)))))))

      ;; ---- P-09 many children, and the counters come home -------------
      (let ((base-proc (proc-count))
            (base-procs (process-count)))
        (let loop ((n 0))
          (when (< n 8)
            (let* ((pid (spawn-worker! "/bin/sh" '("sh" "-c" "exit 0") '() main))
                   (m (monitor pid)))
              (let wait ()
                (receive
                  (after 5000 'gave-up)
                  (`#(DOWN ,w ,r) 'done)
                  (`(spawned ,r) (wait))
                  (`(worker-out ,r ,s ,bv) (wait))
                  (`(worker-eof ,r ,s) (wait))
                  (`(worker-exit ,r ,st ,sg) (wait)))))
            (loop (+ n 1))))
        (want "P-09 eight children come and go, and proc-count comes home"
              (settles-to base-proc proc-count) 'back)
        (want "P-09 TWIN: and so does process-count"
              (settles-to base-procs process-count) 'back))

      (printf "rows: ~a\n~a failures\nfacade-proc complete\n" rows bad)
      (exit (if (zero? bad) 0 1)))))
