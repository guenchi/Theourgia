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

;; WHAT A LOAD COSTS, AS A RATIO. Each cost is read against a reference arm
;; run in this same process, interleaved with it, as a ratio of minima (the
;; shape startup-ratio.sc gives the command line's start). The judged arms:
;;
;;   LB-1  a cold read of one block, in this process, of the mirrored twin
;;         (every segment manifest-listed: the read path a peer's records
;;         take)
;;   LB-2  one write through with-store-write, in this process, of a local
;;         store (`insert`)
;;   LB-3  one write through a daemon serving the other local store
;;         (`insert`)
;;   LB-4  a read at an old cut through the same daemon (`read --cut`, the
;;         cut at half of its records)
;;   LB-D  one decode-line of every line of the log, in this process: the
;;         product's decoder, read on its own row
;;
;; and, printed but not judged: a cold read of the local store (LB-1l), and
;; a write through a second daemon, serving the twin (LB-3t).
;;
;; THE LOCAL LAYOUT IS THE CASE THAT IS JUDGED for writes and for the
;; daemon: one machine, several sessions. The twin guards the manifest
;; path, through the cold read.
;;
;; THE REFERENCE ARM IS NOT THE PRODUCT'S. It is load-budget-reference.ss,
;; checked in beside this file, a library that imports (chezscheme) only:
;; per line, the checksum field compared with a CRC-32 from its own table,
;; the text decoded, and the datum read by its own reader. That is the kind
;; of work a load repeats, so a ratio is a count of "decodes of the whole
;; log", and none of it moves when the product's decoder does: a slower
;; decoder shows on LB-D and on the others, against an unchanged yardstick.
;; Its import form is read as data (R-1, with a negative), its size and
;; digest are recorded (R-2: a changed yardstick invalidates the
;; thresholds), and it must read every line of the store whole (R-3), or it
;; would be a cheap arm and every ratio would read high.
;;
;; EACH JUDGED ARM HAS A RED CONTROL: the same arm with its work done twice
;; (the answer unchanged, the cost of a second identical operation added,
;; which no candidate needs), over CONTROL-ROUNDS rounds, judged against the
;; same threshold. Its row wants `over`; a threshold that a doubled arm
;; passes is not a threshold.
;;
;; THE THRESHOLDS are recorded constants (`load-budget-record`), read on the
;; named base, never computed from the run being judged. While one is #f
;; its row and its control's answer `uncalibrated`, a FAIL, and the reading
;; is printed for whoever sets it.
;;
;; THE STORES are made by the product on every run (load-budget-store.ss):
;; a given number of records (THEOURGIA_LOAD_BUDGET_RECORDS, 10000 unless
;; set) imported twice, into two local stores whose writers' 1 MiB segments
;; the product rotates, and the twin that holds the first store's segments
;; as a mirrored writer's. Each is its own store (its own identity), so the
;; writes of LB-2, LB-3 and LB-3t do not meet in the machine's registry.
;; ROUNDS (THEOURGIA_LOAD_BUDGET_ROUNDS, 5 unless set) is how many times
;; each arm runs; each run of an arm is preceded by one of the reference.
;;
;; EACH TIMED ANSWER IS CHECKED. An arm that answered an error, or the
;; wrong thing, fast would read as a fast arm, so every answer is kept and
;; LB-0 judges each after the rounds, outside the timed intervals: a read's
;; block and version, a read at the old cut's cut, an insert's one new block
;; holding the title it was given. Its NEGATIVE shows the judge flags a read
;; at the present cut and an insert whose block lacks its title.
;;
;; AND IT LEAVES NOTHING. The daemons are owned from their spawn and stopped
;; by pid with a bounded wait on every way out, and H-1 asserts that they,
;; the scratch and the socket directory are gone.
;;
;; ENVIRONMENT READ: THEOURGIA_TEST_ROOT (where the stores go),
;; THEOURGIA_TEST_SOCK (where the daemons' sockets go),
;; THEOURGIA_LOAD_BUDGET_RECORDS, THEOURGIA_LOAD_BUDGET_ROUNDS,
;; CHEZSCHEMELIBDIRS and CHEZSCHEMELIBEXTS (passed to the imports' children
;; and to the daemons). THEOURGIA_HOME is SET to a directory under the root.

(import (chezscheme) (theourgia sched) (theourgia net)
        (theourgia rpc)
        (only (theourgia log) log-publish! segment-sha writer-directory segment-file-name
              read-manifest)
        (only (theourgia wire) decode-line)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (theourgia degeneracy) degeneracy-checks-on))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" label got expected))))
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

;; ---- the record (recorded, never computed) --------------------------------------
(define load-budget-record
  '((base . "ecd5596")
    (taken . "2026-11-13 14:54-14:58Z: three runs on the base's product files, 10000 records, 5 rounds, alone of the session's lines")
    ;; Each threshold is the largest of the three readings times 1.15, the
    ;; readings beside it. A later step that lowers an arm records a new
    ;; threshold from its own reading; an arm that did not move keeps its own.
    ;;
    ;; THE STORE PROCESS KEEPS ITS MEMORY (ecd5596): a write through a daemon
    ;; (LB-3) and a read at a cut through one (LB-4) decode only what no
    ;; earlier load of the store process saw, and fell from 2.15 and 1.96 to
    ;; 0.36 and 0.18; LB-1, LB-2 and LB-D did not move (4.15 to 4.20, 2.06,
    ;; 1.63) and keep their thresholds. These two thresholds catch the memory
    ;; lost (back to about 2) by a factor of five and more, and the doubled
    ;; work of the controls (0.77, 0.34) reads over them. THE WINDOW WAS NOT
    ;; QUIET: a 1-minute load of 7.6 falling to 4.5, mediaanalysisd at 50 to
    ;; 179% and an editor's renderer at 20 to 155% of a CPU beside it; each
    ;; arm's range over its smallest reading: lb-1 1.35%, lb-2 0.58%, lb-3
    ;; 2.51%, lb-4 6.11%, lb-d 0.86% -- lb-1's, lb-3's and lb-4's wider than
    ;; the window before. A quiet window may tighten lb-3 and lb-4 later.
    (lb-1 . #e4.85) (lb-2 . #e2.39) (lb-3 . #e0.422) (lb-4 . #e0.220) (lb-d . #e1.87)
    (readings (lb-1 4.201 4.145 4.182) (lb-2 2.067 2.059 2.055) (lb-3 0.367 0.360 0.358)
              (lb-4 0.183 0.191 0.180) (lb-d 1.629 1.639 1.625))
    ;; THE READING BEFORE, its lb-3 and lb-4 replaced above: base 78803a7,
    ;; 2026-11-12 12:08-12:13Z (the load remembers what it decoded; every
    ;; judged arm fell by about one decode pass from 9af33ae's first reading).
    (before (lb-1 . #e4.85) (lb-2 . #e2.39) (lb-3 . #e2.51) (lb-4 . #e2.26) (lb-d . #e1.87)
            (readings (lb-1 4.217 4.190 4.176) (lb-2 2.052 2.076 2.049) (lb-3 2.150 2.186 2.142)
                      (lb-4 1.955 1.957 1.967) (lb-d 1.620 1.605 1.633)))
    (reference-sha256 . "a77e68214614c2389ceee422bdb2aa49c9f17323ce579fb3c61d5fcbe8be3d73")
    (reference-lines . 164)))
(define (record-ref key) (cdr (assq key load-budget-record)))
(define judged '(lb-1 lb-2 lb-3 lb-4 lb-d))
(define control-rounds 2)

;; ---- places --------------------------------------------------------------------
(define script-dir
  (let* ((argv0 (car (command-line)))
         (cut (let loop ((i (- (string-length argv0) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref argv0 i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring argv0 0 cut) ".")))
(define lb-core (string-append script-dir "/../core.sc"))
(define reference-source (string-append script-dir "/load-budget-reference.ss"))
(define (env-number name default)
  (let* ((v (getenv name)) (n (and v (string->number v))))
    (if (and n (exact? n) (integer? n) (> n 0)) n default)))
(define records (env-number "THEOURGIA_LOAD_BUDGET_RECORDS" 10000))
(define rounds (env-number "THEOURGIA_LOAD_BUDGET_ROUNDS" 5))
(define pid-text (number->string (get-process-id)))
(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                   (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/load-budget-" pid-text))
;; THE SOCKETS IN A DIRECTORY OF THEIR OWN, under THEOURGIA_TEST_SOCK
;; (which the runner keeps short enough for a socket's path; /tmp only
;; when the file runs alone). A daemon keeps a lock file beside its socket
;; for as long as the path exists, so the directory, not the socket, is
;; what this file owns and removes.
(define socket-base
  (let ((v (getenv "THEOURGIA_TEST_SOCK")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define socket-dir (string-append socket-base "/lb-" pid-text))
(define (socket-of name) (string-append socket-dir "/" name ".sock"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))

(include "load-budget-store.ss")

(define (file-text path)
  (if (file-exists? path) (let ((t (call-with-input-file path get-string-all))) (if (string? t) t "")) ""))
(define (trimmed s)
  (let loop ((e (string-length s)))
    (if (and (> e 0) (char-whitespace? (string-ref s (- e 1)))) (loop (- e 1)) (substring s 0 e))))

;; ---- the reference, loaded and read ---------------------------------------------
;;
;; LOADED AS A LIBRARY, its procedure taken from the library's own
;; environment: what it can reach is what its import form names, and R-1
;; reads that form. Loaded inside the run's guard (below), so a reference
;; that cannot be read or loaded ends the run through cleanup! too.
(define reference-decode-all #f)
(define (load-reference!)
  (load reference-source)
  (set! reference-decode-all
        (eval 'reference-decode-all (environment '(load-budget-reference)))))

(define (forms-of path)
  (call-with-input-file path
    (lambda (p) (let loop ((acc '())) (let ((x (read p))) (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))
(define (spec->name spec)
  (if (and (pair? spec) (memq (car spec) '(only prefix rename except for)))
      (spec->name (cadr spec))
      spec))
(define (library-imports forms)
  (apply append
    (map (lambda (f)
           (cond ((and (pair? f) (eq? (car f) 'import)) (map spec->name (cdr f)))
                 ((and (pair? f) (eq? (car f) 'library))
                  (let ((clause (find (lambda (x) (and (pair? x) (eq? (car x) 'import))) (cddr f))))
                    (if clause (map spec->name (cdr clause)) '())))
                 (else (list (list 'not-a-library-form (if (pair? f) (car f) f))))))
         forms)))
(define (lines-of path)
  (let ((t (file-text path)))
    (let loop ((i 0) (n 0))
      (cond ((= i (string-length t)) n)
            ((char=? (string-ref t i) #\newline) (loop (+ i 1) (+ n 1)))
            (else (loop (+ i 1) n))))))

;; ---- the daemons -----------------------------------------------------------------
;;
;; Product daemons: no injection build, no trace. THEOURGIA_PROBE=off only
;; keeps the once-a-second probe from refolding between the timed requests.
;;
;; OWNED FROM THE SPAWN. A daemon is started in the background by a shell
;; that Chez's `process` runs, and that shell prints `$!` -- the pid of
;; the background command, which execs scheme in place -- on the pipe this
;; file reads before it does anything else. So the pid comes from the
;; spawning operation, not from a file the child writes, and the daemon is
;; registered before it has done anything: one that never becomes ready is
;; still stopped. Stopping is a SIGTERM, a wait for the process to be gone
;; (a daemon drains on SIGTERM and exits later), then SIGKILL and a second
;; wait. A daemon that outlives both STAYS OWNED, and `cleanup!` then keeps
;; the scratch and the socket directory and says so: removing a store from
;; under a live daemon is not a cleanup. Every way out of this file -- the
;; end, a row that raised, a daemon that never started -- goes through
;; `cleanup!`.
;; -> (name store socket pid)
(define daemons '())
(define prepared? #f)
;; The command name of each owned pid, read once the daemon is ready (H-1).
(define owned-commands '())
(define (command-of pid)
  (let* ((p (process (string-append "ps -o comm= -p " (number->string pid))))
         (line (get-line (car p))))
    (close-port (car p))
    (close-port (cadr p))
    (if (string? line)
        (let* ((s (trimmed line))
               (slash (let loop ((i (- (string-length s) 1)))
                        (cond ((< i 0) #f) ((char=? (string-ref s i) #\/) i) (else (loop (- i 1)))))))
          (if slash (substring s (+ slash 1) (string-length s)) s))
        'none)))
(define (prepare!)
  (system (string-append "rm -rf " (lb-quote root) " " (lb-quote socket-dir)
                         "; mkdir -p " (lb-quote (string-append root "/home")) " " (lb-quote socket-dir)))
  (set! prepared? #t))
(define (alive? pid) (zero? (system (string-append "kill -0 " (number->string pid) " 2>/dev/null"))))
(define (await-exit pid ms)
  (let wait ((left ms))
    (cond ((not (alive? pid)) #t)
          ((<= left 0) #f)
          (else (sleep-ms 100) (wait (- left 100))))))
;; -> the daemons still alive, which stay in `daemons`
(define stopped-pids '())
(define (stop-daemons!)
  (set! daemons
        (filter (lambda (d)
                  (let ((pid (cadddr d)))
                    (system (string-append "kill " (number->string pid) " 2>/dev/null"))
                    (let ((gone (or (await-exit pid 10000)
                                    (begin
                                      (system (string-append "kill -KILL " (number->string pid) " 2>/dev/null"))
                                      (await-exit pid 5000)))))
                      (when gone (set! stopped-pids (cons pid stopped-pids)))
                      (not gone))))
                daemons))
  daemons)
;; -> #t when everything is gone; #f, after saying what is kept, when a
;; daemon is still alive
(define (cleanup!)
  (let ((left (stop-daemons!)))
    (if (null? left)
        (begin
          (when prepared?
            (system (string-append "rm -rf " (lb-quote root) " " (lb-quote socket-dir))))
          #t)
        (begin
          (printf "NOT CLEANED: daemon(s) ~a still alive after SIGKILL; ~a and ~a are kept~%"
                  (map cadddr left) root socket-dir)
          #f))))
(define (start-daemon! name store)
  (let ((runner (string-append root "/daemon-" name ".ss"))
        (pidfile (string-append root "/daemon-" name ".pid"))
        (log (string-append root "/daemon-" name ".log"))
        (socket-path (socket-of name)))
    (call-with-output-file runner
      (lambda (port)
        (for-each (lambda (l) (display l port) (newline port))
          (list "(import (chezscheme) (theourgia daemon))"
                (format "(call-with-output-file ~s (lambda (p) (write (get-process-id) p)) 'truncate)" pidfile)
                (format "(serve ~s ~s)" store socket-path))))
      'replace)
    ;; THE BACKGROUND COMMAND STAYS A SIMPLE COMMAND: its redirections on it,
    ;; nothing wrapped around it. Only then is `$!` the pid of the process
    ;; that execs scheme; a subshell, a group or a pipeline here would make
    ;; `$!` a shell's pid, and the daemon would be unowned again without any
    ;; row noticing -- except H-1, which asks each owned pid's command.
    (let ((pid (let* ((p (process (string-append
                                   "THEOURGIA_PROBE=off CHEZSCHEMELIBDIRS=" (lb-quote (getenv "CHEZSCHEMELIBDIRS"))
                                   " CHEZSCHEMELIBEXTS=" (lb-quote (getenv "CHEZSCHEMELIBEXTS"))
                                   " scheme --script " (lb-quote runner) " > " (lb-quote log)
                                   " 2>&1 < /dev/null & echo $!")))
                      (line (get-line (car p))))
                 (close-port (car p))
                 (close-port (cadr p))
                 (and (string? line) (string->number (trimmed line))))))
      (unless pid
        (printf "NOT A READING: the shell that starts the daemon ~a printed no pid~%" name)
        (cleanup!)
        (exit 2))
      (let ((d (list name store socket-path pid)))
        (set! daemons (cons d daemons))
        (let up ((k 0))
          (cond ((and (file-exists? socket-path) (file-exists? pidfile))
                 (set! owned-commands (cons (command-of pid) owned-commands))
                 d)
                ((> k 1200)
                 (printf "NOT A READING: the daemon ~a never created its socket in 60 s~%~a~%" name (file-text log))
                 (cleanup!)
                 (exit 2))
                (else (sleep-ms 50) (up (+ k 1)))))))))

(define (line-complete? bv)
  (let loop ((i 0))
    (cond ((>= i (bytevector-length bv)) #f)
          ((= (bytevector-u8-ref bv i) 10) #t)
          (else (loop (+ i 1))))))
(define (ask d verb . args)
  (let* ((request (string-append "(request 1 " (format "~s" (cadr d)) " \"tester\" #f wire #f #f "
                                 (symbol->string verb)
                                 (apply string-append (map (lambda (a) (string-append " " (format "~s" a))) args))
                                 ")\n"))
         (r (exchange (caddr d) (string->utf8 request) line-complete? 120000)))
    (if (and (pair? r) (eq? 'answer (car r)))
        (let* ((datum (guard (e (#t #f)) (read (open-string-input-port (utf8->string (cadr r))))))
               (hit (and (pair? datum) (eq? 'answer (car datum)) (assq 'stdout (cdr datum)))))
          (if (and (pair? hit) (string? (cadr hit)))
              (or (guard (e (#t #f)) (read (open-string-input-port (cadr hit)))) (cadr hit))
              datum))
        (list 'transport r))))

;; ---- the product's decoder over the log (LB-D, and G-1's count) ------------------
(define (lines-decoded segments)
  (let each ((bvs segments) (count 0))
    (if (null? bvs)
        count
        (let* ((bv (car bvs)) (n (bytevector-length bv)))
          (let line ((start 0) (count count))
            (if (>= start n)
                (each (cdr bvs) count)
                (let find ((i start))
                  (cond ((= i n) (each (cdr bvs) count))
                        ((= 10 (bytevector-u8-ref bv i))
                         (let ((one (make-bytevector (+ 1 (- i start)))))
                           (bytevector-copy! bv start one 0 (+ 1 (- i start)))
                           (line (+ i 1) (if (eq? 'ok (car (decode-line one))) (+ count 1) count))))
                        (else (find (+ i 1)))))))))))

;; ---- timing ----------------------------------------------------------------------
(define (timed thunk)
  (let* ((t0 (current-time 'time-monotonic))
         (v (thunk))
         (t1 (current-time 'time-monotonic))
         (d (time-difference t1 t0)))
    (cons (+ (* 1000.0 (time-second d)) (/ (time-nanosecond d) 1e6)) v)))
(define (minimum xs) (apply min xs))
(define (median xs)
  (let ((s (list-sort < xs)) (n (length xs)))
    (if (odd? n) (list-ref s (div n 2)) (/ (+ (list-ref s (- (div n 2) 1)) (list-ref s (div n 2))) 2))))
(define (ms x) (/ (round (* 10 x)) 10))
(define (r3 x) (/ (round (* 1000 x)) 1000))

(define (answer-ok? a) (and (pair? a) (eq? (car a) 'ok)))
(define (answer-clause key a)
  (and (answer-ok? a) (let ((hit (assq key (filter pair? (cdr a))))) (and hit (cadr hit)))))

;; Each arm run ROUNDS times, each run preceded by one of the reference.
;; -> (values ref-samples ((arm . samples) ...)), samples in run order.
(define (run-rounds arms rounds bytes)
  (let ((ref '()) (samples (map (lambda (a) (cons (car a) '())) arms)))
    (do ((r 0 (+ r 1))) ((= r rounds))
      (for-each
        (lambda (arm)
          (set! ref (cons (car (timed (lambda () (reference-decode-all bytes)))) ref))
          (let ((t (car (timed (cdr arm)))))
            (set-cdr! (assq (car arm) samples) (cons t (cdr (assq (car arm) samples))))))
        arms))
    (values (reverse ref) (map (lambda (s) (cons (car s) (reverse (cdr s)))) samples))))

(define (judge arm ratio)
  (let ((k (record-ref arm)))
    (if k (if (<= ratio k) 'within 'over) 'uncalibrated)))

(start-scheduler
  (lambda ()
    (guard (c ((not (warning? c))
               (set! bad (+ bad 1))
               (printf "FAIL a row raised outside its check: ")
               (display-condition c)
               (newline)
               (cleanup!)
               (printf "rows: ~a~%~a failures~%" rows bad)
               (exit 1)))
      (prepare!)
      (load-reference!)
      (printf "== the reference arm imports nothing of the tree, and is the recorded one ==~%")
      (want "R-1 the reference is one library whose imports are (chezscheme) alone"
            (library-imports (forms-of reference-source))
            '((chezscheme)))
      (let ((neg (string-append root "/reference-negative.ss")))
        (call-with-output-file neg
          (lambda (p)
            (for-each (lambda (f)
                        (write (if (and (pair? f) (eq? (car f) 'library))
                                   (map (lambda (x)
                                          (if (and (pair? x) (eq? (car x) 'import))
                                              (append x '((only (theourgia wire) decode-line)))
                                              x))
                                        f)
                                   f)
                               p))
                      (forms-of reference-source)))
          'replace)
        (want "R-1 NEGATIVE: the same library importing (theourgia wire) is found"
              (library-imports (forms-of neg))
              '((chezscheme) (theourgia wire))))
      (want "R-2 the reference's size and digest are the recorded ones"
            (if (and (record-ref 'reference-sha256) (record-ref 'reference-lines))
                (list (lines-of reference-source)
                      (bytevector->hex (sha256 (lb-file-bytes reference-source))))
                (list 'uncalibrated (lines-of reference-source)
                      (bytevector->hex (sha256 (lb-file-bytes reference-source)))))
            (list (record-ref 'reference-lines) (record-ref 'reference-sha256)))

      (printf "== the stores: ~a records each, made by the product ==~%" records)
      (let* ((made (lb-generate! root records))
             (local (cdr (assq 'local made)))
             (served (cdr (assq 'served made)))
             (twin (cdr (assq 'twin made)))
             (writer (cdr (assq 'writer made)))
             (served-writer (cdr (assq 'served-writer made)))
             (segments (cdr (assq 'segments made)))
             (gen-ms (cdr (assq 'ms made)))
             (bytes (map lb-file-bytes (lb-segment-files local writer)))
             (served-bytes (map lb-file-bytes (lb-segment-files served served-writer)))
             (sizes (map bytevector-length bytes))
             (block (string-append writer ".1"))
             (served-block (string-append served-writer ".1"))
             (old-cut-list (list (cons served-writer (div records 2))))
             (old-cut (format "((~s . ~a))" served-writer (div records 2)))
             (local-read (rpc-dispatch local (list 'read block) "tester"))
             (twin-read (rpc-dispatch twin (list 'read block) "tester"))
             (served-read (rpc-dispatch served (list 'read served-block) "tester")))
        (printf "generator: files ~a ms, import ~a ms, second import ~a ms, twin ~a ms; ~a segments, ~a bytes~%"
                (cdr (assq 'files gen-ms)) (cdr (assq 'import gen-ms)) (cdr (assq 'served gen-ms))
                (cdr (assq 'twin gen-ms)) segments (apply + sizes))
        (want "G-1 the local writer holds the records asked for, in more than one segment of at most 1 MiB"
              (list (lines-decoded bytes) (> segments 1) (for-all (lambda (s) (<= s 1048576)) sizes))
              (list records #t #t))
        (want "G-2 the served store holds as many records, in as many segments"
              (list (lines-decoded served-bytes) (length served-bytes))
              (list records segments))
        (want "G-3 the twin lists every segment in the mirrored writer's manifest"
              (map car (or (read-manifest twin writer) '()))
              (map (lambda (i) (+ i 1)) (iota segments)))
        (want "G-4 a read of the first block answers ok, and the same, from the store and from its twin"
              (list (answer-ok? local-read) (equal? local-read twin-read))
              '(#t #t))
        (want "R-3 the reference reads every line of the store whole"
              (reference-decode-all bytes)
              records)
        (let* ((d-served (start-daemon! "served" served))
               (warm-served (ask d-served 'outline))
               (t-twin (real-time))
               (d-twin (start-daemon! "twin" twin))
               (warm-twin (ask d-twin 'outline))
               (twin-start-ms (- (real-time) t-twin)))
          (printf "the second daemon (the twin's) started and answered its first request in ~a ms~%" twin-start-ms)
          (want "G-5 both daemons answer" (list (answer-ok? warm-served) (answer-ok? warm-twin)) '(#t #t))
          ;; THE JUDGED ARMS KEEP THE SWITCH THE RUN GIVES THEM (only the
          ;; generator's imports run without it): this process's checks are on
          ;; exactly when the variable names some, and each daemon's own
          ;; environment, read from the process table, carries the variable as
          ;; this process has it. So a gate cannot judge these arms plain unseen.
          (let* ((v (getenv "THEOURGIA_DEGENERACY"))
                 (on? (and v (not (member v '("" "0"))) #t))
                 (env-of (lambda (pid)
                           (let* ((p (process (string-append "ps eww -o command= -p " (number->string pid))))
                                  (t (get-string-all (car p))))
                             (close-port (car p)) (close-port (cadr p))
                             (if (eof-object? t) "" t))))
                 (carries? (lambda (text)
                             (let ((want (string-append "THEOURGIA_DEGENERACY=" (or v ""))))
                               (let find ((i 0))
                                 (cond ((> (+ i (string-length want)) (string-length text)) #f)
                                       ((and (string=? (substring text i (+ i (string-length want))) want)
                                             (or (= (+ i (string-length want)) (string-length text))
                                                 (char-whitespace? (string-ref text (+ i (string-length want))))))
                                        #t)
                                       (else (find (+ i 1)))))))))
            (printf "the switch: ~s; this process's checks ~s~%" v (degeneracy-checks-on))
            (want "LB-S the judged arms keep the switch: this process's checks are on exactly when the variable names some, and both daemons' environments carry the variable as this process has it"
                  (list (eq? on? (pair? (degeneracy-checks-on)))
                        (map (lambda (d) (if v (carries? (env-of (cadddr d))) (not (carries? (env-of (cadddr d))))))
                             (list d-served d-twin)))
                  '(#t (#t #t))))
          ;; EVERY ANSWER IS KEPT with what it should be, and judged after the
          ;; rounds, outside every timed interval (LB-0): a read's block and
          ;; version are the ones read before the rounds, and a read at the old
          ;; cut answers that cut; an insert names one new block of its store,
          ;; and that block holds the title it was given when its store is read
          ;; after the daemons have stopped.
          (let* ((answers '())
                 (keep! (lambda (arm a . title)
                          (set! answers (cons (list arm a (and (pair? title) (car title))) answers))
                          a))
                 (k 0)
                 (title (lambda (arm) (set! k (+ k 1)) (format "~a ~a" arm k)))
                 (insert-into (lambda (arm send)
                                (let ((t (title arm))) (keep! arm (send t) t))))
                 (ops
                   (list
                     (cons 'lb-1 (lambda () (keep! 'lb-1 (rpc-dispatch twin (list 'read block) "tester"))))
                     (cons 'lb-1l (lambda () (keep! 'lb-1l (rpc-dispatch local (list 'read block) "tester"))))
                     (cons 'lb-2 (lambda () (insert-into 'lb-2 (lambda (t) (rpc-dispatch local (list 'insert "--title" t) "tester")))))
                     (cons 'lb-3 (lambda () (insert-into 'lb-3 (lambda (t) (ask d-served 'insert "--title" t)))))
                     (cons 'lb-3t (lambda () (insert-into 'lb-3t (lambda (t) (ask d-twin 'insert "--title" t)))))
                     (cons 'lb-4 (lambda () (keep! 'lb-4 (ask d-served 'read served-block "--cut" old-cut))))
                     (cons 'lb-d (lambda () (keep! 'lb-d (if (= records (lines-decoded bytes)) '(ok) '(short)))))))
                 (doubled
                   (map (lambda (arm)
                          (let ((op (cdr (assq arm ops))))
                            (cons arm (lambda () (op) (op)))))
                        judged)))
            (let-values (((ref samples) (run-rounds ops rounds bytes)))
              (let-values (((cref csamples) (run-rounds doubled control-rounds bytes)))
                (unless (null? (stop-daemons!))
                  (raise (condition (make-error)
                                    (make-message-condition "a daemon outlived SIGTERM and SIGKILL")
                                    (make-irritants-condition (map cadddr daemons)))))
                (let* ((titles (lambda (store)
                                 (let ((o (rpc-dispatch store '(outline) "tester")))
                                   (map (lambda (row) (cons (list-ref row 1) (list-ref row 5)))
                                        (let ((hit (and (answer-ok? o) (assq 'rows (filter pair? (cdr o))))))
                                          (if hit (cdr hit) '()))))))
                       (by-store (list (cons 'lb-2 (titles local)) (cons 'lb-3 (titles served))
                                       (cons 'lb-3t (titles twin))))
                       (core (lambda (a) (and (answer-ok? a) (list (cadr a) (answer-clause 'version a)))))
                       (wrong
                         (lambda (e)
                           (let ((arm (car e)) (a (cadr e)) (t (caddr e)))
                             (case arm
                               ((lb-1 lb-1l)
                                (and (not (equal? (core a) (core local-read))) 'not-the-block-read-before))
                               ((lb-4)
                                (cond ((not (equal? (core a) (core served-read))) 'not-the-block-read-before)
                                      ((not (equal? (answer-clause 'cut a) old-cut-list))
                                       (list 'cut (answer-clause 'cut a)))
                                      (else #f)))
                               ((lb-2 lb-3 lb-3t)
                                (let ((new (answer-clause 'state a)))
                                  (cond ((not (and (list? new) (= 1 (length new))))
                                         (list 'not-one-new-block new))
                                        ((not (equal? (cdr (or (assoc (caar new) (cdr (assq arm by-store)))
                                                               '(#f . #f)))
                                                      t))
                                         (list 'not-holding-its-title (caar new) t))
                                        (else #f))))
                               (else (and (not (answer-ok? a)) 'not-ok)))))))
                  (want "LB-0 every timed operation answered what it should, judged after the rounds"
                        (filter (lambda (x) x)
                                (map (lambda (e) (let ((w (wrong e))) (and w (list (car e) w)))) answers))
                        '())
                  (want "LB-0 CONTROL: the old cut is not the served store's present cut"
                        (equal? (answer-clause 'cut served-read) old-cut-list)
                        #f)
                  (want "LB-0 NEGATIVE: the judge flags a read at the present cut, and an insert whose block lacks its title"
                        (let ((an-insert (find (lambda (e) (eq? (car e) 'lb-2)) answers)))
                          (map (lambda (e) (and (wrong e) #t))
                               (list (list 'lb-4 served-read #f)
                                     (list 'lb-2 (cadr an-insert) "not the title it was given"))))
                        '(#t #t)))
                (let ((ref-min (minimum ref)) (cref-min (minimum cref)))
                  (printf "reference: min ~a ms, median ~a ms, max ~a ms over ~a~%"
                          (ms ref-min) (ms (median ref)) (ms (apply max ref)) (length ref))
                  (for-each
                    (lambda (s)
                      (let ((xs (cdr s)))
                        (printf "reading ~a ratio ~a min ~a ms median ~a ms max ~a ms samples ~a~%"
                                (car s) (r3 (/ (minimum xs) ref-min)) (ms (minimum xs)) (ms (median xs))
                                (ms (apply max xs)) (map ms xs))))
                    samples)
                  (for-each
                    (lambda (s)
                      (printf "control ~a ratio ~a (doubled; reference min ~a ms) samples ~a~%"
                              (car s) (r3 (/ (minimum (cdr s)) cref-min)) (ms cref-min) (map ms (cdr s))))
                    csamples)
                  (for-each
                    (lambda (arm)
                      (want (format "~a the ratio is within its recorded threshold"
                                    (string-upcase (symbol->string arm)))
                            (judge arm (/ (minimum (cdr (assq arm samples))) ref-min))
                            'within)
                      (want (format "~a CONTROL: with its work done twice, the row is not green"
                                    (string-upcase (symbol->string arm)))
                            (judge arm (/ (minimum (cdr (assq arm csamples))) cref-min))
                            'over))
                    judged)))))))
      (let ((clean (cleanup!)))
        (want "H-1 every owned pid was a scheme process, every daemon is gone, and so are the scratch and the socket directory"
              (list owned-commands clean daemons (filter alive? stopped-pids) (file-exists? root)
                    (file-exists? socket-dir) (length stopped-pids))
              (list '("scheme" "scheme") #t '() '() #f #f 2)))
      (printf "~%~a failures~%rows: ~a~%load-budget complete~%" bad rows)
      (exit (if (zero? bad) 0 1)))))
