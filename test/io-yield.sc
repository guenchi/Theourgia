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

;; THE YIELD AFTER BLOCKING I/O (ffi.sc, io-yield!).
;;
;; igropyr preempts on Chez's tick timer, and a blocking foreign call spends
;; wall time and almost no ticks: an export writing a thousand files kept one
;; time slice for its whole write loop, and a read on a new connection to the
;; same daemon waited for the loop to end (9.8 s measured, 0.06 s outside it).
;; atomic-write!, fsync! and fsync-dir! now end with io-yield!, which parks
;; the caller for a millisecond once it has run 50 ms of wall time since its
;; own last yield.
;;
;; YA-1 is the reading through a real daemon. The others read the guards in
;; this process, through io-yield-tally: (attempts yields), where an attempt
;; is a call that passed every guard and reached the clock. Counting attempts
;; rather than yields keeps the rows off the 50 ms clock.
;;
;; THE TICKER IS THE DAEMON'S SIGNAL WATCHER (daemon.sc, watch-for-signals!):
;; it sleeps 25 ms (drain-poll-ms) and writes its store-check trace line every
;; 40 wakes (store-check-polls), about once a second when nothing is busy.
;; Each wake needs a turn, and during the write loop a turn comes at the
;; worker's yields, once per Y = 50 ms (ffi.sc io-yield-ms) and a 1 ms
;; sleep; the watcher's own 25 ms sleep runs out inside that. So the line
;; comes about every store-check-polls x (max(drain-poll-ms, Y) + 1 ms) =
;; 40 x 51 ms, and the bound is that plus half a second of scheduling slack:
;; 2540 ms. The ticks are timed by stamps on the daemon's trace lines. The two constants are internal to daemon.sc, so
;; the bound is a literal here; a change to store-check-polls,
;; drain-poll-ms or io-yield-ms must change it too. Without the yield the
;; watcher got one turn in the whole loop (a 29 s gap in the d4 reading).
;;
;; YA-1 DEPENDS ON THE DISK. Its write loop is long only where a flush is slow
;; (two F_FULLFSYNC per file on macOS, about 10 ms); where flushes are fast
;; the loop is short, nothing starves, and the row reads green with or
;; without the yield. It prints how long the loop ran, so a reading says
;; which case it was.

(import (chezscheme) (theourgia sched) (theourgia net)
        (only (theourgia ffi) io-yield! io-yield-tally with-no-yield lock-try-acquire! lock-release! file-ensure!
              symlink! fsync! theourgia-stage fd-open fd-close)
        (only (theourgia log) atomic-write!)
        (only (theourgia code-project) capture-import))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0) (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e)))) e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define this-lib (getenv "CHEZSCHEMELIBDIRS"))
(define (lib-env) (string-append "CHEZSCHEMELIBDIRS=" this-lib " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"))
(define pid-text (number->string (get-process-id)))
(define scratch (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/iy-" pid-text))
(define sockets (string-append (or (getenv "THEOURGIA_TEST_SOCK") "/tmp") "/iy-" pid-text))
(system (string-append "rm -rf " scratch " " sockets "; mkdir -p " scratch " " sockets))
(define home (string-append scratch "/home"))
(putenv "THEOURGIA_HOME" home)

(define (file-text p) (if (file-exists? p) (let ((t (call-with-input-file p get-string-all))) (if (string? t) t "")) ""))
(define (spin-ms ms) (let ((t (real-time))) (let loop () (when (< (- (real-time) t) ms) (loop)))))
(define (epoch-ms) (let ((t (current-time 'time-utc))) (+ (* 1000 (time-second t)) (quotient (time-nanosecond t) 1000000))))
(define (tally-delta thunk)
  (let ((a (io-yield-tally)))
    (thunk)
    (let ((b (io-yield-tally))) (map - b a))))

;; ---- YA-2 outside a scheduler -------------------------------------------------
;; This program has loaded (theourgia sched) and not started it: io-yield!
;; returns, twice and past the clock, and nothing is attempted.
(want "YA-2 outside a started scheduler io-yield! is a no-op: it returns, past the clock, and attempts nothing"
      (tally-delta (lambda () (io-yield!) (spin-ms 60) (io-yield!)))
      '(0 0))

;; ---- YA-8 the tables are touched only with interrupts disabled ----------------
;; The tables and counters io-yield! keeps are shared by every actor, and a
;; lookup a preemption splits while another actor grows the table can miss a
;; key that is there (igropyr actor.sc says so of its own name table). No
;; probe here can make that window open, so the rule is read from the source:
;; ffi.sc's forms, read as data, and in io-yield!, with-no-yield and
;; io-yield-tally every hashtable operation and every set! of a counter sits
;; inside a with-interrupts-disabled form, and the sleeper's call does not.
(define (read-forms path)
  (call-with-port (open-input-file path)
    (lambda (p) (let loop ((out '())) (let ((d (read p))) (if (eof-object? d) (reverse out) (loop (cons d out))))))))
(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f) ((char=? (string-ref self i) #\/) i) (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define (find-define forms name)
  (let walk ((x forms))
    (cond ((not (pair? x)) #f)
          ((and (eq? (car x) 'define) (pair? (cdr x)) (pair? (cadr x)) (eq? (caadr x) name)) x)
          (else (or (walk (car x)) (walk (cdr x)))))))
(define table-ops '(hashtable-update! hashtable-ref hashtable-set! hashtable-contains? hashtable-delete!))
(define counters '(io-attempts io-yields))
;; -> (where . what) for each table operation, counter set!, counter read
;;    and sleeper call in FORM, where is inside or outside
(define (sites form)
  (let walk ((x form) (inside #f))
    (define (here what) (list (cons (if inside 'inside 'outside) what)))
    (cond ((memq x counters) (here (list 'read x)))
          ((not (pair? x)) '())
          ((eq? (car x) 'quote) '())
          ((eq? (car x) 'with-interrupts-disabled) (apply append (map (lambda (y) (walk y #t)) (cdr x))))
          ((and (eq? (car x) 'set!) (pair? (cdr x)) (memq (cadr x) counters) (pair? (cddr x)))
           (append (here (list 'set! (cadr x))) (walk (caddr x) inside)))
          (else
           (append (cond ((memq (car x) table-ops) (here (car x)))
                         ((equal? x '(io-sleeper)) (here 'sleeper))
                         (else '()))
                   (if (list? x) (apply append (map (lambda (y) (walk y inside)) x)) '()))))))
(define ffi-forms (read-forms (string-append script-dir "/../ffi.sc")))
(define io-sites
  (apply append (map (lambda (name) (let ((f (find-define ffi-forms name))) (if f (sites f) (list (cons 'missing name)))))
                     '(io-yield! with-no-yield io-yield-tally))))
(want "YA-8 in io-yield!, with-no-yield and io-yield-tally no table operation, counter set! or counter read is outside an interrupt region"
      (filter (lambda (s) (and (eq? (car s) 'outside) (not (eq? (cdr s) 'sleeper)))) io-sites) '())
(want "YA-8 TWIN: the walk finds them, inside a region: eight table operations, both counters set, four counter reads (the tally's two, one in each increment); the sleeper's call outside one"
      (list (length (filter (lambda (s) (and (eq? (car s) 'inside) (symbol? (cdr s)) (memq (cdr s) table-ops))) io-sites))
            (length (filter (lambda (s) (and (eq? (car s) 'inside) (pair? (cdr s)) (eq? (cadr s) 'set!))) io-sites))
            (length (filter (lambda (s) (and (eq? (car s) 'inside) (pair? (cdr s)) (eq? (cadr s) 'read))) io-sites))
            (filter (lambda (s) (eq? (cdr s) 'sleeper)) io-sites))
      '(8 2 4 ((outside . sleeper))))
;; THE SAME RULE FOR THE DAEMON'S PROGRESS TABLES (daemon.sc): the counts by
;; ticket, the connection that owns each, and the executor's association,
;; every operation in a region.
(define daemon-forms (read-forms (string-append script-dir "/../daemon.sc")))
(define progress-sites
  (apply append (map (lambda (name) (let ((f (find-define daemon-forms name))) (if f (sites f) (list (cons 'missing name)))))
                     '(progress-register! progress-drop! progress-count progress-bump! with-ticket progress-reclaim!))))
(want "YA-8 the daemon's progress tables: no table operation outside an interrupt region, in the six that touch them"
      (filter (lambda (s) (not (eq? (car s) 'inside))) progress-sites) '())
(want "YA-8 TWIN: the walk finds them: fifteen table operations, all inside (two in register, three in drop, one in count, three in the bump, three in the association, three in the reclaim)"
      (length (filter (lambda (s) (and (eq? (car s) 'inside) (memq (cdr s) table-ops))) progress-sites))
      15)

;; ---- the tree YA-1 exports ----------------------------------------------------
;; SIZED BY THIS DISK. Forty atomic writes are timed first, and the tree gets
;; enough files for a write loop of about 8 s (at least 1000, at most 8000);
;; a loop that is still under 4 s is said to be one and not read (below).
(define per-file-ms
  (let ((dir (string-append scratch "/calibrate")) (t0 (real-time)))
    (system (string-append "mkdir -p " dir))
    (do ((i 0 (+ i 1))) ((= i 40))
      (atomic-write! (string-append dir "/c" (number->string i)) (make-bytevector 1024 65) 'working))
    (/ (- (real-time) t0) 40.0)))
(define files (max 1000 (min 8000 (exact (ceiling (/ 8000 (max per-file-ms 0.01)))))))
(define tree (string-append scratch "/tree"))
(define store (string-append scratch "/store"))
(do ((d 0 (+ d 1))) ((= d 10))
  (system (string-append "mkdir -p " tree "/d" (number->string d))))
(do ((i 0 (+ i 1))) ((= i files))
  (call-with-output-file (string-append tree "/d" (number->string (mod i 10)) "/f" (number->string i) ".sc")
    (lambda (p)
      (do ((k 0 (+ k 1))) ((= k 16))
        (fprintf p "(define (f~a-~a x) (+ x ~a)) ;; a line of text to make the file a size\n" i k k)))))
(define (cli . words)
  (string-append (lib-env) " THEOURGIA_LOCAL=1 scheme --script ../theourgia.sc"
                 (apply string-append (map (lambda (w) (string-append " " w)) words))
                 " 2>/dev/null < /dev/null"))
(system (cli "init" "--store" store "> /dev/null"))
(system (cli "import-code" tree "--store" store "> /dev/null"))

;; the local route: no scheduler, so no yield, and the export is whole
(define local-out (string-append scratch "/local-export"))
(system (cli "export-code" local-out "--store" store ">" (string-append scratch "/local-export.out")))
(want "YA-2 the local route's export (no scheduler) answers ok with every file"
      (file-text (string-append scratch "/local-export.out"))
      (format "(ok (files ~a))\n" files))
(printf "YA-1 sizing: ~a ms per atomic write here, so ~a files~%" per-file-ms files)

;; the first file's holder, for the reads YA-1 sends
(system (cli "outline" "--depth" "1" "--store" store ">" (string-append scratch "/outline.out")))
(define first-id
  (let* ((t (file-text (string-append scratch "/outline.out")))
         (s (let loop ((i 0)) (cond ((>= (+ i 2) (string-length t)) #f) ((string=? (substring t i (+ i 2)) "- ") (+ i 2)) (else (loop (+ i 1)))))))
    (and s (let loop ((j s)) (if (or (>= j (string-length t)) (char=? (string-ref t j) #\space)) (substring t s j) (loop (+ j 1)))))))
(unless (string? first-id)
  (printf "NOT A READING: no holder id in the outline~%~a~%" (file-text (string-append scratch "/outline.out")))
  (printf "io-yield complete~%")
  (exit 2))

(define (line-complete? bv)
  (let loop ((i 0)) (cond ((>= i (bytevector-length bv)) #f) ((= (bytevector-u8-ref bv i) 10) #t) (else (loop (+ i 1))))))
(define (request verb . args)
  (string->utf8 (string-append "(request 1 " (format "~s" store) " \"tester\" #f wire #f #f " (symbol->string verb)
                               (apply string-append (map (lambda (a) (string-append " " (format "~s" a))) args)) ")\n")))
(define (answer-text r) (if (and (pair? r) (eq? 'answer (car r))) (utf8->string (cadr r)) (format "~s" r)))
;; the stdout an answer envelope carries, or the text itself when it is not one
(define (stdout-of text)
  (guard (e (#t text))
    (let* ((d (read (open-string-input-port text)))
           (s (and (pair? d) (eq? (car d) 'answer) (assq 'stdout (cdr d)))))
      (if (and s (pair? (cdr s)) (string? (cadr s))) (cadr s) text))))
(define (starts-with? t p) (and (>= (string-length t) (string-length p)) (string=? (substring t 0 (string-length p)) p)))

;; the stamps of the log's store-check lines, in order
(define (tick-times log)
  (let ((text (file-text log)) (needle " (trace store-check "))
    (let loop ((start 0) (out '()))
      (let ((nl (let find ((i start)) (cond ((>= i (string-length text)) #f) ((char=? (string-ref text i) #\newline) i) (else (find (+ i 1)))))))
        (if (not nl)
            (reverse out)
            (let* ((line (substring text start nl))
                   (sp (let find ((i 0)) (cond ((>= i (string-length line)) #f) ((char=? (string-ref line i) #\space) i) (else (find (+ i 1))))))
                   (stamp (and sp (string->number (substring line 0 sp)))))
              (loop (+ nl 1)
                    (if (and stamp (>= (string-length line) (+ sp (string-length needle)))
                             (string=? (substring line sp (+ sp (string-length needle))) needle))
                        (cons stamp out) out))))))))
(define (files-written dir)
  (let loop ((d 0) (n 0))
    (if (= d 10)
        n
        (let ((sub (string-append dir "/d" (number->string d))))
          (loop (+ d 1) (+ n (if (file-directory? sub) (length (directory-list sub)) 0)))))))

(define (ya-1)
  (let* ((sock (string-append sockets "/d.sock"))
         (log (string-append scratch "/daemon.log"))
         (out (string-append scratch "/export")))
    ;; EACH TRACE LINE IS STAMPED AS IT ARRIVES (epoch milliseconds, by perl
    ;; on the daemon's stderr), so a watcher tick is timed when it happened,
    ;; not when this program's polling noticed it.
    (system (string-append "THEOURGIA_TRACE=1 " (lib-env) " sh -c 'scheme --script ../theourgiad.sc serve " store
                           " --socket " sock " 2>&1 | perl -MTime::HiRes=time -ne \"\\$|=1; printf qq(%d %s), time()*1000, \\$_\" > " log "' &"))
    (let up ((k 0))
      (cond ((file-exists? sock) #t)
            ((> k 400) (printf "NOT A READING: the daemon never created its socket~%~a~%" (file-text log)) (exit 2))
            (else (sleep-ms 50) (up (+ k 1)))))
    (let ((me self))
      (spawn (lambda () (send me (list 'exported (answer-text (exchange sock (request 'export-code out) line-complete? 180000)))))))
    ;; every 200 ms: how many files the export has written, and one read
    ;; through a new connection (its time, and whether it answered ok); the
    ;; watcher's ticks are read from the stamped log at the end. A read
    ;; counts when the export had written some files and not all at the
    ;; moment it was sent, however long it then took. The window runs from
    ;; the first written file to the export's answer, so a stall at its end
    ;; is inside it.
    (let loop ((reads '()) (first-write #f))
      (let ((exported (receive (after 200 #f) (`(exported ,a) a))))
        (if exported
            (let ((end (epoch-ms)))
              (system (string-append "pkill -f " sock " 2>/dev/null"))
              (let* ((window (if first-write (- end first-write) 0))
                     (short? (< window 4000))
                     (ticks (tick-times log))
                     (loop-ticks (filter (lambda (t) (and first-write (<= first-write t) (<= t end))) ticks))
                     (points (and first-write (append (list first-write) (sort < loop-ticks) (list end))))
                     (gap (if points (apply max 0 (map - (cdr points) (reverse (cdr (reverse points))))) 0))
                     (slowest (apply max 0 (map car reads))))
                (printf "YA-1 reading: write loop ~a ms, ~a reads in it (slowest ~a ms), ~a ticks (longest gap ~a ms)~%"
                        window (length reads) slowest (length loop-ticks) gap)
                (when short?
                  (printf "SKIP YA-1 liveness counts: the write loop lasted ~a ms, under 4000 ms, even at ~a files (flushes are fast here); the read and watcher counts below are not read, the latency of the reads that were sent still is~%"
                          window files))
                (want "YA-1 the export through the daemon answers ok with every file"
                      (stdout-of exported) (format "(ok (files ~a))\n" files))
                ;; ONE READ, NOT A NUMBER OF THEM: the reads are serial, so how many
                ;; fit in the window depends on how long each takes, and the
                ;; latency row below judges every read sent while the export was
                ;; partial. This row only says the latency row read something.
                (want "YA-1 at least one read was sent during the write loop, so the latency row below reads something"
                      (or short? (>= (length reads) 1)) #t)
                (want "YA-1 every read on a new connection during the write loop answers ok within 1 s"
                      (list (for-all cadr reads) (< slowest 1000)) '(#t #t))
                ;; THE GAP CARRIES THE COUNT: with no gap past the bound, the
                ;; window edges included, a window of W ms holds at least
                ;; ceil(W / 2540) - 1 lines, so the row asks for one line
                ;; (the walk saw the log at all) and the gap.
                (want "YA-1 the daemon's watcher ticks during the write loop: at least one line, and no gap past 40 x 51 ms + 500 ms from the first written file to the export's answer"
                      (list (or short? (>= (length loop-ticks) 1)) (or short? (< gap 2540))) '(#t #t))))
            (let* ((n (files-written out))
                   (first-write (or first-write (and (> n 0) (epoch-ms))))
                   (partial? (and (> n 0) (< n files)))
                   (t0 (real-time))
                   (a (answer-text (exchange sock (request 'read first-id) line-complete? 30000)))
                   (reads (if partial? (cons (list (- (real-time) t0) (starts-with? (stdout-of a) "(ok")) reads) reads)))
              (loop reads first-write)))))))

;; YA-6 AN IMPORT'S CAPTURE LETS OTHER ACTORS RUN. The capture reads every
;; file of a tree (code-project.sc capture-import) and calls progress! per
;; file, which yields; a ticker actor that sleeps 10 ms between ticks keeps
;; its cadence while it runs, and the capture's yields show in the tally. The
;; window is the capture alone: an import through a daemon then commits what
;; it captured as a batch, and the batch path reports no progress and yields
;; only at its flushes (B5's stated limit; README, Waiting for an answer), so
;; a window over the whole import would time that.
;; THE TREE IS SIZED TO THE MACHINE: reads are fast (YA-1's tree of 1000
;; files is captured in under 100 ms here), so the faster of two timed
;; captures of it (the first can be cold) says how many copies of it make a
;; capture of about 1.5 s (at most 40).
;; The bound is the ticker's 10 ms + Y + 500 ms, the margin for a loaded
;; runner. A capture still under 800 ms reads red, not skipped.
;; THE YIELD COUNT IS WHAT TELLS; THE CADENCE ROW IS A GUARD. Measured with
;; both of the capture's progress! calls removed: 0 yields, and the ticker
;; still kept its cadence (longest gap 12 ms), because igropyr preempts an
;; actor that runs Scheme code; a loop of fast reads never holds the thread
;; long. So only the tally row goes red without the yield; the cadence row
;; says the capture leaves the others running. It stays green for that
;; mutant, and can still go red for what no yield prevents: the host
;; descheduling the process, a long region with interrupts disabled. Neither
;; row proves a bound of 50 ms between explicit yields.
;; THE TALLY IS THE PROCESS'S, the throttle the actor's: the count is taken
;; just before and just after the capture, and a control of the same length,
;; the ticker still running and this actor only sleeping, must count none,
;; so no other actor supplies the yields the row reads. The threshold is
;; rounded up.
(define (ya-6)
  (let ((st6 (string-append scratch "/store6")) (tree6 (string-append scratch "/tree6")) (me self))
    (system (cli "init" "--store" st6 "> /dev/null"))
    (let* ((timed (lambda () (let ((t0 (real-time))) (capture-import st6 tree #f #f) (max 1 (- (real-time) t0)))))
           (first (min (timed) (timed)))
           (copies (max 1 (min 40 (exact (ceiling (/ 1500 first)))))))
      (system (string-append "mkdir -p " tree6))
      (do ((c 0 (+ c 1))) ((= c copies))
        (system (string-append "cp -R " tree " " tree6 "/c" (number->string c))))
      (let* ((ticker (spawn (lambda ()
                              (let loop ()
                                (receive
                                  (after 10 (send me (list 'tick (real-time))) (loop))
                                  (`(stop) (void)))))))
             (y0 (io-yield-tally))
             (t0 (real-time))
             (r (guard (e (#t 'raised)) (capture-import st6 tree6 #f #f)))
             (t1 (real-time))
             (yields (- (cadr (io-yield-tally)) (cadr y0)))
             (control (let ((c0 (io-yield-tally)))
                        (sleep-ms (max 100 (- t1 t0)))
                        (- (cadr (io-yield-tally)) (cadr c0)))))
        (send ticker '(stop))
        (let* ((ticks (let collect ((out '())) (receive (after 0 (reverse out)) (`(tick ,t) (collect (cons t out))))))
               (in (filter (lambda (t) (and (<= t0 t) (<= t t1))) ticks))
               (points (append (list t0) in (list t1)))
               (gap (apply max 0 (map - (cdr points) (reverse (cdr (reverse points))))))
               (window (- t1 t0)))
          (printf "YA-6 reading: first capture ~a ms, ~a copies, capture ~a ms, ~a yields, ~a ticks in it, longest gap ~a ms, control ~a yields~%"
                  first copies window yields (length in) gap control)
          (want "YA-6 the capture runs to its end"
                (not (eq? r 'raised)) #t)
          (want "YA-6 the capture yields: at least one yield per 100 ms of it, rounded up"
                (and (>= window 800) (>= yields (quotient (+ window 99) 100))) #t)
          (want "YA-6 TWIN: a control as long as the capture, the ticker running and this actor sleeping, counts no yield"
                control 0)
          (want "YA-6 a 10 ms ticker keeps its cadence through an import's capture of at least 800 ms: a tick in every 10 ms + Y + 500 ms"
                (list (>= window 800) (>= (length in) 2) (< gap 560)) '(#t #t #t)))))))

(start-scheduler
  (lambda ()
    (ya-1)
    (ya-6)

    ;; ---- YA-3 interrupts disabled ------------------------------------------------
    ;; The clock is started, then run past: the same call yields with interrupts
    ;; enabled and attempts nothing inside a region that disabled them.
    (io-yield!)
    (spin-ms 60)
    (want "YA-3 with interrupts disabled, io-yield! attempts nothing and does not yield"
          (tally-delta (lambda () (with-interrupts-disabled (io-yield!)))) '(0 0))
    (want "YA-3 TWIN: the same call with interrupts enabled attempts once and yields"
          (tally-delta (lambda () (io-yield!))) '(1 1))

    ;; ---- YA-4 the stage --------------------------------------------------------
    ;; Unarmed, the worker yields under a stage; the actor that runs in the
    ;; yield sees that stage and sets its own; the worker resumes with its own.
    (let ((me self))
      (parameterize ((theourgia-stage 'commit))
        (io-yield!)
        (spin-ms 60)
        (spawn (lambda () (let ((seen (theourgia-stage))) (theourgia-stage 'publish) (send me (list 'other seen)))))
        (let* ((d (tally-delta (lambda () (io-yield!))))
               (mine (theourgia-stage))
               (seen (receive (after 2000 'never) (`(other ,s) s))))
          (want "YA-4 a yield under a stage: another actor ran in it (and saw the stage), the worker resumes with its own"
                (list d seen mine) '((1 1) commit commit)))))
    ;; Armed, nothing yields: a child process with a fault armed (injection on)
    ;; runs the same sequence and prints its tally.
    (let ((child (string-append scratch "/armed.sc")))
      (call-with-output-file child
        (lambda (p)
          (write '(import (chezscheme) (theourgia sched) (only (theourgia ffi) io-yield! io-yield-tally)) p)
          (write '(define (spin-ms ms) (let ((t (real-time))) (let loop () (when (< (- (real-time) t) ms) (loop))))) p)
          (write '(start-scheduler (lambda () (io-yield!) (spin-ms 60) (io-yield!) (write (io-yield-tally)) (newline) (exit 0))) p)))
      (system (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=conn-raise@conn " (lib-env)
                             " scheme --script " child " > " scratch "/armed.out 2>/dev/null < /dev/null"))
      (want "YA-4 with a fault armed, io-yield! attempts nothing and does not yield"
            (file-text (string-append scratch "/armed.out")) "(0 0)\n"))

    ;; ---- YA-5 the lock service -------------------------------------------------
    ;; Main's handlers run to the end without yielding (daemon.sc): the calls
    ;; they make -- take a lock, release it, make sure a file exists, publish a
    ;; link -- reach no io-yield!, past the clock as well.
    (let ((lock (string-append scratch "/ya5.lock")) (link (string-append scratch "/ya5.link")))
      (io-yield!)
      (spin-ms 60)
      (want "YA-5 a lock taken and released, a file ensured and a link made attempt no yield"
            (tally-delta (lambda ()
                           (file-ensure! lock)
                           (let ((l (lock-try-acquire! lock 'exclusive))) (lock-release! l))
                           (symlink! "ya5.lock" link)))
            '(0 0))
      (want "YA-5 TWIN: an fsync! past the clock, in the same actor, attempts and yields"
            (tally-delta (lambda () (let ((fd (fd-open lock '(write)))) (fsync! fd lock 'working) (fd-close fd))))
            '(1 1)))

    ;; ---- YA-7 one yield per atomic write ----------------------------------------
    ;; The write's own fsync! and fsync-dir! run with yields suppressed: one
    ;; attempt, at its end. Without the suppression it would be three.
    (want "YA-7 an atomic-write! attempts one yield (its inner fsync! and fsync-dir! are suppressed)"
          (car (tally-delta (lambda () (atomic-write! (string-append scratch "/ya7.txt") (string->utf8 "seven\n") 'working))))
          1)

    ;; ---- YA-9 the suppression's extent and the clock ------------------------
    ;; Suppression is this actor's: another actor inside with-no-yield does not
    ;; stop this one from yielding.
    (let ((me self))
      (io-yield!)
      (spin-ms 60)
      (let ((other (spawn (lambda () (with-no-yield (lambda () (send me '(inside)) (receive (`(leave) #t))))))))
        (receive (after 2000 #f) (`(inside) #t))
        (want "YA-9 another actor inside with-no-yield does not suppress this actor's yield"
              (tally-delta (lambda () (io-yield!))) '(1 1))
        (send other '(leave))))
    ;; The extent ends where it ends: an inner extent leaves the outer one in
    ;; force, and an extent left by a raise is gone.
    (io-yield!)
    (spin-ms 60)
    (want "YA-9 nested: after an inner extent ends, the outer one still suppresses; after a raise out of one, nothing does"
          (list (tally-delta (lambda () (with-no-yield (lambda () (with-no-yield (lambda () #t)) (io-yield!)))))
                (begin (guard (e (#t #f)) (with-no-yield (lambda () (raise 'out))))
                       (tally-delta (lambda () (io-yield!)))))
          '((0 0) (1 1)))
    ;; A second call inside the 50 ms after a yield attempts and does not yield.
    (want "YA-9 a call right after a yield attempts and does not yield"
          (tally-delta (lambda () (io-yield!))) '(1 0))

    (system (string-append "rm -rf " scratch " " sockets))
    (printf "rows: ~a~%~a failures~%io-yield complete~%" rows bad)
    (exit (if (= bad 0) 0 1))))
