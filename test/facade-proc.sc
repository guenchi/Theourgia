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
;; KEY: THE SEQUENCE IS THE TEST, NOT THE MAPPINGS. Every translation can be
;; right on its own while the adapter ends too early: igropyr's own P13
;; closes a child's stdout while it still runs, writes to stderr, then
;; exits. An adapter that treated the first EOF as the end would pass
;; every per-message row and lose everything after it.
;;
;; NEVER: AND EVERY ENDING IS READ FROM `#(DOWN pid reason)`. There is no
;; table of adapters to ask, which is the point: a count this library
;; kept about itself could be told that a live process was gone, and
;; when that happened two rows written for the leak went green together.

(import (chezscheme) (theourgia sched) (theourgia proc)
        (only (igropyr tcp) proc-count proc-pid))

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
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define (settles-to base read)
  (let loop ((k 0))
    (cond ((<= (read) base) 'back)
          ((> k 200) (list 'still (- (read) base)))
          (else (sleep-ms 20) (loop (+ k 1))))))

;; SCRATCH PATHS LIVE UNDER THE RUNNER'S TWO ROOTS (F71): files and
;; directories under THEOURGIA_TEST_ROOT, socket paths under
;; THEOURGIA_TEST_SOCK, which is short enough for one. Run alone, without
;; them, a path falls back to /tmp as it always did.
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

;; What `ps` says about a pid, in kilobytes: a reading from outside this
;; tree, so a row about a memory measurement is not checking the tree
;; against itself.
(define (ps-rss-kb pid)
  (let ((out (string-append scratch-base "/p32-ps-" (number->string (get-process-id)))))
    (system (string-append "ps -o rss= -p " (number->string pid) " > " out " 2>/dev/null"))
    (guard (e (#t #f))
      (let ((n (call-with-input-file out read)))
        (and (number? n) (> n 0) n)))))

;; END A PROCESS, AND WHAT IT STARTED, AND SEE THEM GONE, with a bound. A
;; process that has ended but is not yet reaped (a zombie) counts as gone.
;; NEVER: A ROW THAT STARTS A CHILD ENDS IT. P-08's workers used to be
;; closed and left to run out their `sleep 8` in this fixture's process
;; group after the fixture had ended; the runner now reports such a member
;; as LEFT IN GROUP (launcher design L5). It is called BEFORE the workers
;; are closed: closing a worker ends its shell, and the shell's `sleep`,
;; now without a parent, can no longer be found as its child (measured:
;; called after the close, both sleeps were still in the group).
(include "pid-state.ss")
;; THE CHILDREN OF A PID, read from pgrep's own output through a process
;; port, not through a file under the scratch root: a row that damaged its
;; own scratch root would otherwise list no children and wait for none
;; (codex r8 C1). #f means the list could not be read.
(define (children-of pid)
  (guard (e (#t #f))
    (let* ((p (process (string-append "pgrep -P " (number->string pid) " 2>/dev/null; echo rc $?")))
           (in (car p)))
      (close-port (cadr p))
      ;; pgrep's own status comes last, as `rc N`: 0 is a list and 1 is an
      ;; empty one; anything else is a pgrep that could not list, and the
      ;; answer is #f, not "no children" (codex r9 C1).
      (let loop ((acc '()))
        (let ((n (read in)))
          (cond
            ((eof-object? n) (close-port in) #f)
            ((eq? n 'rc)
             (let ((rc (read in)))
               (close-port in)
               (and (memv rc '(0 1)) (reverse acc))))
            (else (loop (if (integer? n) (cons n acc) acc)))))))))
;; NEVER: A SHELL IS STOPPED BEFORE ITS CHILDREN ARE LISTED, so it cannot
;; start one between the listing and its own end (codex r7 C1). Every pid
;; listed is then ended and waited for, by pid, with a bound. A list that
;; could not be read is said, and the whole bound is waited out.
;;
;; A #f AMONG THE PIDS IS A WORKER WHOSE OS PID IS NOT KNOWN (never
;; started, or already exited and reaped): its children cannot be listed,
;; so that too is said and the whole bound is waited out (codex r10 C1).
(define (end-and-wait! maybe-pids)
  (define pids (filter number? maybe-pids))
  (define unknown (not (for-all number? maybe-pids)))
  (when unknown
    (printf "P-08: a worker has no OS pid in the library; waiting out the bound\n"))
  (for-each (lambda (pid) (system (string-append "kill -STOP " (number->string pid) " 2>/dev/null"))) pids)
  (let* ((lists (map children-of pids))
         (unread (not (for-all list? lists)))
         (all (append pids (apply append (filter list? lists)))))
    (when unread
      (printf "P-08: the children of a worker could not be listed; waiting out the bound\n"))
    (for-each (lambda (pid) (system (string-append "kill -TERM " (number->string pid) " 2>/dev/null"))) all)
    (for-each (lambda (pid) (system (string-append "kill -CONT " (number->string pid) " 2>/dev/null"))) pids)
    ;; NEVER: ROUNDS THAT RUN OUT ARE SAID (F90). The loop used to end in
    ;; silence with a process still there, and only the runner's LEFT IN
    ;; GROUP, after the fixture, said anything; that stays the guard, and
    ;; this line names the pid here, at the step that failed to end it.
    ;; NEVER: let*, NOT let. `left` must be read after the rounds; under
    ;; `let` the two are evaluated in no promised order, and a pid read
    ;; before the wait is reported although it ended during it.
    (let* ((rounds (let wait ((k 0))
                    (if (or (> k 100) (and (not unread) (not unknown) (for-all pid-gone? all)))
                        k
                        (begin (system "sleep 0.05") (wait (+ k 1))))))
          (left (filter (lambda (pid) (not (pid-gone? pid))) all)))
      (when (pair? left)
        (printf "P-08: still running after TERM and ~a rounds of 50 ms: ~a\n"
                rounds
                (apply string-append
                       (map (lambda (pid) (string-append " " (number->string pid))) left)))))))

;; F90-2: THE LINE APPEARS WHEN A CHILD OUTLASTS THE ROUNDS. A shell that
;; ignores TERM and execs `sleep` keeps the disposition across the exec, so
;; the pid stays through every round; the row reads what end-and-wait!
;; printed, then ends the child with KILL itself.
(let* ((f (string-append scratch-base "/f90-ignorer-" (number->string (get-process-id)) ".pid"))
       (in? (lambda (text needle)
              (let ((n (string-length needle)) (m (string-length text)))
                (let loop ((i 0))
                  (cond ((> (+ i n) m) #f)
                        ((string=? (substring text i (+ i n)) needle) #t)
                        (else (loop (+ i 1)))))))))
  ;; NEVER: THE CHILD SAYS IT IS READY ONLY AFTER ITS TRAP. A child stopped
  ;; and sent TERM before its trap has run dies on CONT with the default
  ;; disposition, and the row is red for a race of its own. The first
  ;; version, with no ready file, read (#t #f #f) once under the launcher
  ;; and green three times after; this race is the reading of that red, not
  ;; a reproduction of it.
  (system (string-append "sh -c 'trap \"\" TERM; echo 1 > " f ".ready; exec sleep 30' > /dev/null 2>&1 & echo $! > " f))
  (let wait ((k 0))
    (unless (or (file-exists? (string-append f ".ready")) (> k 200))
      (system "sleep 0.05")
      (wait (+ k 1))))
  (let* ((ready (file-exists? (string-append f ".ready")))
         (pid (guard (e (#t #f)) (call-with-input-file f read)))
         (said (if (integer? pid)
                   (with-output-to-string (lambda () (end-and-wait! (list pid))))
                   "")))
    (when (integer? pid) (system (string-append "kill -KILL " (number->string pid) " 2>/dev/null")))
    (system (string-append "rm -f " f " " f ".ready"))
    (unless (in? said "still running after TERM")
      (printf "   F90-2: end-and-wait! said ~s for pid ~s\n" said pid))
    (want "F90-2 the child said it was ready, and end-and-wait! names it still running after TERM when its rounds run out"
          (list ready
                (integer? pid)
                (in? said "still running after TERM")
                (and (integer? pid) (in? said (string-append " " (number->string pid)))))
          '(#t #t #t #t))))

(define (digits-of t)
  (let* ((n (string-length t))
         (d (let trim ((i 0) (out '()))
              (cond ((>= i n) (list->string (reverse out)))
                    ((char-numeric? (string-ref t i))
                     (trim (+ i 1) (cons (string-ref t i) out)))
                    (else (list->string (reverse out)))))))
    (and (> (string-length d) 0) (string->number d))))

;; NOTE: ps ANSWERS IN KILOBYTES, this library in bytes. Written without the
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
      ;; NOTE: THE DOWN COMES LAST. Messages the adapter sent before it died
      ;; are ahead of the runtime's DOWN in this mailbox, so a consumer
      ;; that reads in order sees the child's whole story and then its
      ;; ending.
      (let ((pid (spawn-worker! "/bin/sh" '("sh" "-c" "echo hi; exit 3") '() main)))
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
      ;; NEVER: AN EMPTY ARGV IS REJECTED BY RAISING, not by a return value,
      ;; and there is no guard in the adapter -- so the condition itself
      ;; becomes the death reason. §L⑥: a consumer must treat a reason it
      ;; does not recognise as an unexpected ending and log it, NEVER: never
      ;; match the set exhaustively.
      (let ((base-proc (proc-count))
            (base-procs (process-count)))
        (let ((pid (spawn-worker! "/bin/sh" '() '() main)))
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
        (let ((pid (spawn-worker! "/nonexistent-for-this-row" '("x") '() main)))
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

      ;; ---- P-03 TWIN: a caller that watches as well hears twice -------
      ;;
      ;; NEVER: THE FACADE TAKES THE WATCH, AND A CALLER MAY TAKE ONE TOO --
      ;; so that caller gets TWO DOWNs for the same pid, with the same
      ;; reason, and cannot demonitor the one whose object it never
      ;; received. That is not a defect to be fixed; it is the shape this
      ;; arrangement has, and consumers have to tolerate the second one.
      ;;
      ;; KEY: IT IS ASSERTED HERE, ONCE, ON PURPOSE. It used to be true
      ;; accidentally at a dozen call sites, where the second DOWN was
      ;; nobody's business and simply sat in the mailbox until the NEXT
      ;; row read it -- eight rows across two fixtures reported reasons
      ;; belonging to their predecessors. Every one of those watches is
      ;; gone; this row is where the fact lives.
      (let ((base-proc (proc-count)))
        (let ((pid (spawn-worker! "/nonexistent-for-the-twin" '("x") '() main)))
          (monitor pid)
          (want "P-03 TWIN: a caller that also monitors gets exactly two DOWNs, alike"
                (let gather ((seen '()) (k 0))
                  (if (>= k 2)
                      (let ((a (car seen)) (b (cadr seen)))
                        (list (length seen)
                              (if (eq? (car a) (car b)) 'same-pid 'DIFFERENT-PIDS)
                              (if (equal? (cdr a) (cdr b)) 'same-reason (list (cdr a) (cdr b)))))
                      (receive (after 6000 (list 'only (length seen)))
                               (`(spawned ,r) (gather seen k))
                               (`#(DOWN ,w ,r) (gather (append seen (list (cons w r))) (+ k 1))))))
                '(2 same-pid same-reason)))
        ;; NEVER: AND A THIRD NEVER COMES. Two watches, two DOWNs -- a row
        ;; that only counted "at least two" would pass against a facade
        ;; that watched twice itself.
        (want "P-03 TWIN: and no third DOWN follows"
              (receive (after 1000 'exactly-two) (`#(DOWN ,w ,r) 'A-THIRD-ARRIVED))
              'exactly-two)
        (want "P-03 TWIN: and the refused spawn left no child behind"
              (settles-to base-proc proc-count) 'back))

      ;; ---- P-04 closing a worker ends the child -----------------------
      ;;
      ;; NOTE: §L⑦: CLOSING IS KILLING THE ADAPTER, so the child is signalled
      ;; by the runtime with SIGTERM -- not with a signal we chose. A
      ;; child that ignores SIGTERM is the guardian's problem, by group.
      ;;
      ;; NOTE: §L⑧: AFTERWARDS NOTHING MORE COMES FROM THAT REF. The adapter
      ;; is gone, so there is nobody left to translate the child's exit;
      ;; already-queued messages remain observable and the DOWN is the
      ;; boundary in the mailbox.
      (let ((base-proc (proc-count)))
        (let ((pid (spawn-worker! "/bin/sh" '("sh" "-c" "sleep 30") '() main))
              )
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
      (let ((pid (spawn-worker! "/bin/sh"
                    '("sh" "-c" "exec 1>&-; sleep 0.2; printf late >&2; exit 7") '() main)))
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
      (let ((pid (spawn-worker! "/bin/sh" '("sh" "-c" "sleep 30") '() main))
            )
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
      ;; NOTE: igropyr ANSWERS #f FOR A WRITE IT CANNOT EVEN ATTEMPT, without
      ;; ever running the completion -- so the facade supplies that one
      ;; itself. Otherwise a caller holding a token waits for something
      ;; nobody will send.
      (let ((pid (spawn-worker! "/bin/sh" '("sh" "-c" "cat") '() main))
            )
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
          ;; NEVER: AND A WRITE igropyr REFUSES OUTRIGHT STILL COMPLETES.
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
      ;; NEVER: A NUMBER IN A WIDE BAND IS NOT A READING: `(and (worker-alive?
      ;; ref) 1)` is a number in that band. Each child prints its own pid
      ;; and the row compares this library's reading with `ps` for that
      ;; same pid, and requires the one holding forty megabytes to read
      ;; far larger than the one holding none.
      (let ((big (spawn-worker! "/bin/sh"
                    '("sh" "-c" "x=$(dd if=/dev/zero bs=1000000 count=40 2>/dev/null | tr '\\0' 'a'); echo $$; sleep 8")
                    '() main))
             (small (spawn-worker! "/bin/sh" '("sh" "-c" "echo $$; sleep 8") '() main))
             )
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
                ;; THE CLEANUP LIST IS THE LIBRARY'S OWN OS PID, not the pid
                ;; the child printed: a child slow to print it would take
                ;; itself off the list (codex r10 C1). A ref that never
                ;; arrived, or has no pid, is #f, and end-and-wait! says so.
                (end-and-wait! (list (and big-ref (proc-pid (worker-ref-proc big-ref)))
                                     (and small-ref (proc-pid (worker-ref-proc small-ref)))))
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
            (let ((pid (spawn-worker! "/bin/sh" '("sh" "-c" "exit 0") '() main))
                  )
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
