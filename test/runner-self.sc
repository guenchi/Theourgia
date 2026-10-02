;; Copyright 2018 - 2026 guenchi
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

;; The runner, run on a directory of tiny fixtures, answers for what it
;; leaves behind and counts what it says it counts.
;;
;; Each case builds a small test directory -- copies of run-fixtures.sh,
;; env.sh and row-baseline-check.sh, an empty row baseline, and fixtures
;; written here, always of both kinds (.sc and .py), because the runner
;; runs an unmatched pattern as a file (design amendment 2, A2.8) -- and
;; runs the runner in it, in the background, on a base directory of its
;; own. Then it reads the runner's output and the file system. Design:
;; theourgos core/briefs/runner-hygiene-design.md with amendments 1 and 2.
;;
;; WHAT THE RUNNER SAYS IS CHECKED AGAINST WHAT A FIXTURE SAW. The roots
;; are read by an inner fixture from its own environment, and the runner's
;; lines are compared with those, so a runner that printed paths it did not
;; use would be red.
;;
;; Every process this file starts is stopped by its pid before the file
;; ends, and each one also stops by itself within 90 s.
;;
;; RS-5 kills its inner runner with SIGKILL, not SIGTERM: a handler can run
;; on TERM and tidy up, and the row asks what a run that could do nothing
;; leaves.
;;
;; GUARD ROWS ARE GREEN ON THE TREE BEFORE THIS CHANGE BY CONSTRUCTION: that
;; runner made no roots, so it could neither fail to remove them nor remove
;; someone else's. They go red for a runner that makes roots and gets that
;; wrong, which is the runner this change brings. Each is labelled GUARD.
;;
;; ON macOS THE ENVIRONMENT OF A SYSTEM BINARY (/bin, /usr/bin) IS NOT
;; VISIBLE, so a leaked /bin/sleep carries the token and cannot be counted
;; (design amendment 3). The row for a leaked process that is not scheme
;; therefore leaves a Homebrew python3, whose environment is visible.

(import (chezscheme))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (let ((ok (equal? got expected)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expected)))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e)
                                     e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expected)
     (begin (set! rows (+ rows 1))
            (want-1 label (caught got) (caught expected))))))

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                   (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/runner-self-" (number->string (get-process-id))))
(when (file-exists? root)
  (assertion-violation 'runner-self "scratch directory already exists" root))
;; PATHS GO INTO SHELL COMMANDS UNQUOTED, so a root that would need quoting
;; is refused rather than half-used.
(let loop ((i 0))
  (when (< i (string-length root))
    (let ((c (string-ref root i)))
      (unless (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\/ #\. #\- #\_)))
        (assertion-violation 'runner-self "the scratch root needs quoting" root)))
    (loop (+ i 1))))
(system (string-append "mkdir -p " root))

;; ---- small tools -----------------------------------------------------------

(define (sh . parts) (system (apply string-append parts)))
(define (spit! path text)
  (call-with-output-file path (lambda (o) (put-string o text)) 'replace))
(define (slurp path)
  (if (file-exists? path)
      (call-with-input-file path
        (lambda (i) (let loop ((acc '()))
                      (let ((c (read-char i)))
                        (if (eof-object? c) (list->string (reverse acc)) (loop (cons c acc)))))))
      ""))
(define (read-datum path)
  (guard (e (#t #f))
    (and (file-exists? path) (call-with-input-file path read))))
(define (lines-of text)
  (let loop ((i 0) (start 0) (acc '()))
    (cond ((= i (string-length text))
           (reverse (if (> i start) (cons (substring text start i) acc) acc)))
          ((char=? (string-ref text i) #\newline)
           (loop (+ i 1) (+ i 1) (cons (substring text start i) acc)))
          (else (loop (+ i 1) start acc)))))
(define (starts-with? s p)
  (and (string? s) (>= (string-length s) (string-length p))
       (string=? p (substring s 0 (string-length p)))))
(define (contains? s w)
  (let loop ((i 0))
    (and (<= (+ i (string-length w)) (string-length s))
         (or (string=? w (substring s i (+ i (string-length w)))) (loop (+ i 1))))))
(define (basename p)
  (let loop ((i (- (string-length p) 1)))
    (cond ((< i 0) p)
          ((char=? (string-ref p i) #\/) (substring p (+ i 1) (string-length p)))
          (else (loop (- i 1))))))
(define (pause-tenths n)
  (sleep (make-time 'time-duration (* (remainder n 10) 100000000) (quotient n 10))))
;; Waits up to `secs` for `path` to exist; answers whether it does.
(define (await path secs)
  (let loop ((k 0))
    (cond ((file-exists? path) #t)
          ((> k (* 10 secs)) #f)
          (else (pause-tenths 1) (loop (+ k 1))))))
(define (stop-pid! file)
  (let ((pid (read-datum file)))
    (when (and (integer? pid) (> pid 1))
      (sh "kill -9 " (number->string pid) " 2>/dev/null"))))
(define (alive? pid)
  (and (integer? pid) (> pid 1)
       (= 0 (system (string-append "kill -0 " (number->string pid) " 2>/dev/null")))))
(define (entries dir)
  (if (file-directory? dir) (list-sort string<? (directory-list dir)) '()))
(define (ths-now)
  (filter (lambda (n) (starts-with? n "ths.")) (entries "/tmp")))
;; THE RUNNER'S PRIVATE SNAPSHOT DIRECTORY is made with mktemp -t under
;; $TMPDIR (or /tmp), named ths-snap.<token>.<random> (design amendment 4 and
;; the code session's reading: a bare mktemp name left an unnamed directory
;; behind a SIGKILLed run). Full paths of those carrying `token`.
(define tmp-base
  (let ((t (or (getenv "TMPDIR") "/tmp")))
    (if (and (> (string-length t) 1) (char=? #\/ (string-ref t (- (string-length t) 1))))
        (substring t 0 (- (string-length t) 1))
        t)))
(define (snap-dirs token)
  (if (and (string? token) (> (string-length token) 0))
      (map (lambda (n) (string-append tmp-base "/" n))
           (filter (lambda (n) (starts-with? n (string-append "ths-snap." token ".")))
                   (entries tmp-base)))
      '()))
(define (mode-of path)
  (sh "stat -f %Lp " path " > " root "/mode.txt 2>/dev/null")
  (read-datum (string-append root "/mode.txt")))

;; ---- the inner fixtures ------------------------------------------------------

(define (scheme-fixture name body)
  (cons (string-append name ".sc")
        (string-append "(import (chezscheme))\n" body
                       "(printf \"0 failures~%" name " complete~%\")\n")))
(define wait-for-go
  (string-append
    "(let loop ((k 0))\n"
    "  (unless (or (file-exists? \"go\") (> k 900))\n"
    "    (sleep (make-time 'time-duration 100000000 0))\n"
    "    (loop (+ k 1))))\n"))
(define (await-file name)
  (string-append
    "(let loop ((k 0))\n"
    "  (unless (or (file-exists? \"" name "\") (> k 300))\n"
    "    (sleep (make-time 'time-duration 100000000 0))\n"
    "    (loop (+ k 1))))\n"))
(define fixture-texts
  (list
    (cons "ok.py" "print(\"0 failures\")\nprint(\"ok complete\")\n")
    (cons "redpy.py" "print(\"1 failures\")\nprint(\"redpy complete\")\n")
    ;; A PROBE: it prints a usage line and stops, which is how the runner
    ;; tells a probe from a fixture. It is not a python fixture that ran.
    (cons "probepy.py" "import sys\nprint(\"usage: probepy <input>\")\nsys.exit(2)\n")
    (scheme-fixture "green" "")
    ;; A ROW THAT COULD NOT BE ESTABLISHED: one VOID line, no failure.
    (scheme-fixture "void" "(printf \"VOID somerow: the instrument did not see its own control~%\")\n")
    ;; A FIXTURE THAT COUNTS ROWS, which the empty table does not list.
    (scheme-fixture "rowsy" "(printf \"ok one~%rows: 1~%\")\n")
    (cons "red.sc" "(import (chezscheme))\n(printf \"1 failures~%red complete~%\")\n")
    ;; What the runner handed its fixtures, read from the fixture's own
    ;; environment; and a marker file written into each root, so "removed"
    ;; is about roots that held something.
    (scheme-fixture "probe-roots"
      (string-append
        "(define (v n) (or (getenv n) \"\"))\n"
        "(define troot (v \"THEOURGIA_TEST_ROOT\"))\n"
        "(define tsock (v \"THEOURGIA_TEST_SOCK\"))\n"
        "(define (mark! d) (and (> (string-length d) 0) (file-directory? d)\n"
        "  (begin (call-with-output-file (string-append d \"/probe-mark\") (lambda (o) (write 1 o)) 'replace) #t)))\n"
        "(define marked (list (mark! troot) (mark! tsock)))\n"
        "(call-with-output-file \"roots.txt\"\n"
        "  (lambda (o) (write (list troot tsock marked (v \"THEOURGIA_SUITE_TOKEN\")) o))\n"
        "  'replace)\n"))
    ;; Holds the run open until the outer file says go (at most 90 s).
    (scheme-fixture "wait"
      (string-append
        "(call-with-output-file \"wait.pid\" (lambda (o) (write (get-process-id) o)) 'replace)\n"
        "(call-with-output-file \"started\" (lambda (o) (write 1 o)) 'replace)\n"
        wait-for-go))
    ;; Leaves a scheme process behind, started from a shell.
    (scheme-fixture "leaker"
      (string-append
        "(system \"scheme --script sleeper.ss > /dev/null 2>&1 < /dev/null &\")\n"
        (await-file "sleeper.pid")))
    ;; Leaves a scheme process behind the way the product starts a daemon:
    ;; posix_spawnp, and the child calls setsid.
    (scheme-fixture "leakspawn"
      (string-append
        "(define spawn! (eval 'spawn-detached! (environment '(theourgia ffi))))\n"
        "(spawn! (list \"scheme\" \"--script\" \"sleeper2.ss\"))\n"
        (await-file "sleeper2.pid")))
    ;; Leaves a process behind that is not scheme.
    (scheme-fixture "leaksh"
      "(system \"python3 -c 'import os, time; os.setsid(); time.sleep(90)' > /dev/null 2>&1 < /dev/null & echo $! > sleepsh.pid\")\n")
    ;; Makes the run's base unsearchable, as the last .sc fixture, so the
    ;; runner cannot remove its scratch root and cannot see that it failed to.
    (scheme-fixture "zz-lockbase"
      (string-append
        "(define r (or (getenv \"THEOURGIA_TEST_ROOT\") \"\"))\n"
        "(define base (let loop ((i (- (string-length r) 1)))\n"
        "  (cond ((< i 1) #f) ((char=? (string-ref r i) #\\/) (substring r 0 i)) (else (loop (- i 1))))))\n"
        "(call-with-output-file \"locked.txt\" (lambda (o) (write (list r base) o)) 'replace)\n"
        ";; ONLY A RUN ROOT'S PARENT: a runner that hands over no run-<token>\n"
        ";; directory gets nothing locked, because its parent is someone else's.\n"
        "(define run-root? (let loop ((i (- (string-length r) 1)))\n"
        "  (cond ((< i 0) #f) ((char=? (string-ref r i) #\\/)\n"
        "         (let ((n (substring r (+ i 1) (string-length r))))\n"
        "           (and (> (string-length n) 4) (string=? (substring n 0 4) \"run-\"))))\n"
        "        (else (loop (- i 1))))))\n"
        "(when (and base run-root?) (system (string-append \"chmod 000 \" base)))\n"))
    ;; Makes the socket root unsearchable, as the last .sc fixture.
    (scheme-fixture "zz-locksock"
      (string-append
        "(define d (or (getenv \"THEOURGIA_TEST_SOCK\") \"\"))\n"
        "(call-with-output-file \"lockedsock.txt\" (lambda (o) (write d o)) 'replace)\n"
        "(when (and (> (string-length d) 9) (string=? (substring d 0 9) \"/tmp/ths.\"))\n"
        "  (system (string-append \"chmod 000 \" d)))\n"))
    ;; Holds the run open until go, then -- a fixture still running after its
    ;; runner is gone -- writes into the scratch root it was handed.
    (scheme-fixture "wait-late"
      (string-append
        "(call-with-output-file \"wait.pid\" (lambda (o) (write (get-process-id) o)) 'replace)\n"
        "(call-with-output-file \"started\" (lambda (o) (write 1 o)) 'replace)\n"
        wait-for-go
        "(let ((r (getenv \"THEOURGIA_TEST_ROOT\")))\n"
        "  (when (and r (> (string-length r) 0))\n"
        "    (system (string-append \"mkdir -p \" r \"/late && echo 1 > \" r \"/late/mark\"))))\n"))
    ;; A fixture that makes itself a session leader, as a daemon does.
    (cons "setsid.py"
          (string-append
            "import os\n"
            "try:\n"
            "    os.setsid()\n"
            "    r = 'ok'\n"
            "except OSError as e:\n"
            "    r = 'refused %d' % e.errno\n"
            "open('setsid.txt', 'w').write(r)\n"
            "print('0 failures')\n"
            "print('setsid complete')\n"))
    ;; A fixture that leaves a member in its own group (no setsid).
    (scheme-fixture "leavegroup"
      "(system \"sleep 60 > /dev/null 2>&1 < /dev/null & echo $! > lg.pid\")\n")
    ;; A fixture that reports whether the runner's marker reached it.
    (scheme-fixture "marker"
      "(call-with-output-file \"marker.txt\" (lambda (o) (write (or (getenv \"THEOURGIA_RUNNER_NORMALISED\") \"\") o)) 'replace)\n")
    ;; A fixture that reports what its standard input is.
    (cons "stdin.py"
          (string-append
            "import os, stat\n"
            "st = os.fstat(0)\n"
            "dn = os.stat('/dev/null')\n"
            "r = 'null' if (stat.S_ISCHR(st.st_mode) and st.st_rdev == dn.st_rdev) else 'other'\n"
            "open('stdin.txt', 'w').write(r)\n"
            "print('0 failures')\n"
            "print('stdin complete')\n"))
    ;; A fixture that reports how it found SIGINT when it started.
    (cons "sigint.py"
          (string-append
            "import signal\n"
            "r = 'ignored' if signal.getsignal(signal.SIGINT) == signal.SIG_IGN else 'default'\n"
            "open('sigint.txt', 'w').write(r)\n"
            "print('0 failures')\n"
            "print('sigint complete')\n"))
    ;; A fixture whose child ignores TERM, then holds the run until go.
    (scheme-fixture "termchild"
      (string-append
        "(system \"sh -c 'trap \\\"\\\" TERM; echo $$ > tchild.pid; exec sleep 60' > /dev/null 2>&1 < /dev/null &\")\n"
        (await-file "tchild.pid")
        "(call-with-output-file \"started\" (lambda (o) (write 1 o)) 'replace)\n"
        wait-for-go))
    ;; Arms the failing awk of RS-11d, as the last .sc fixture.
    (scheme-fixture "zz-armawk"
      "(call-with-output-file \"awk-armed\" (lambda (o) (write 1 o)) 'replace)\n")
    ;; Binds a real unix socket with a 60-byte name under the socket root.
    (cons "bind.py"
          (string-append
            "import os, socket\n"
            "d = os.environ.get('THEOURGIA_TEST_SOCK', '')\n"
            "p = os.path.join(d, 'b' * 55 + '.sock')\n"
            "ok = False\n"
            "try:\n"
            "    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)\n"
            "    s.bind(p)\n"
            "    s.close()\n"
            "    os.unlink(p)\n"
            "    ok = len(d) > 0\n"
            "except OSError:\n"
            "    ok = False\n"
            "open('bind.txt', 'w').write('bound' if ok else 'not-bound')\n"
            "print('0 failures')\n"
            "print('bind complete')\n"))))
;; NOT FIXTURES: `.ss` and `.sh`, which the runner does not run.
(define helper-texts
  (list
    ;; A PREFLIGHT, which the runner runs before any fixture when the file is
    ;; there: it starts a writer that, six seconds on, writes into the scratch
    ;; root, and then holds the run for thirty seconds.
    (cons "slow-preflight.ss"
          (string-append
            "(import (chezscheme))\n"
            "(call-with-output-file \"pre.pid\" (lambda (o) (write (get-process-id) o)) 'replace)\n"
            "(call-with-output-file \"pre-root.txt\" (lambda (o) (write (or (getenv \"THEOURGIA_TEST_ROOT\") \"\") o)) 'replace)\n"
            "(system \"sh -c 'echo $$ > writer.pid; sleep 6; mkdir -p \\\"$THEOURGIA_TEST_ROOT/late\\\"' > /dev/null 2>&1 < /dev/null &\")\n"
            "(call-with-output-file \"pre-started\" (lambda (o) (write 1 o)) 'replace)\n"
            "(sleep (make-time 'time-duration 0 30))\n"
            "(printf \"0 failures~%expansion-branches complete~%\")\n"))
    ;; A LEAVER: it leaves the fixture's group with setsid, so the launcher
    ;; does not stop it and only the token count at the end can find it.
    (cons "sleeper.ss"
          (string-append
            "(import (chezscheme))\n"
            "((eval 'setsid! (environment '(theourgia ffi))))\n"
            "(call-with-output-file \"sleeper.pid\" (lambda (o) (write (get-process-id) o)) 'replace)\n"
            "(sleep (make-time 'time-duration 0 90))\n"))
    (cons "sleeper2.ss"
          (string-append
            "(import (chezscheme))\n"
            "((eval 'setsid! (environment '(theourgia ffi))))\n"
            "(call-with-output-file \"sleeper2.pid\" (lambda (o) (write (get-process-id) o)) 'replace)\n"
            "(sleep (make-time 'time-duration 0 90))\n"))
    (cons "foreign.ss"
          (string-append
            "(import (chezscheme))\n"
            "(call-with-output-file \"foreign.pid\" (lambda (o) (write (get-process-id) o)) 'replace)\n"
            "(sleep (make-time 'time-duration 0 90))\n"))
    ;; Runs the runner in <dir> with THEOURGIA_TEST_ROOT = <base>, or with it
    ;; unset when <base> is "-"; records the runner's own pid, its output and
    ;; its exit status. A third argument replaces THEOURGIA_LIBDIR.
    (cons "drive.sh"
          (string-append
            "cd \"$1\" || exit 9\n"
            "if [ \"$2\" = - ]; then unset THEOURGIA_TEST_ROOT; else export THEOURGIA_TEST_ROOT=\"$2\"; fi\n"
            "if [ -n \"$3\" ] && [ \"$3\" != - ]; then export THEOURGIA_LIBDIR=\"$3\"; fi\n"
            "if [ -n \"$4\" ] && [ \"$4\" != - ]; then export PATH=\"$4:$PATH\"; fi\n"
            "if [ -n \"$5\" ] && [ \"$5\" != - ]; then export TMPDIR=\"$5\"; fi\n"
            ". ./env.sh\n"
            "if [ -n \"$DRIVE_IGNORE_TERM\" ]; then\n"
            "  perl -e '$SIG{TERM} = q(IGNORE); exec @ARGV' sh -c 'echo $$ > runner.pid; exec sh run-fixtures.sh out' > run.log 2>&1\n"
            "else\n"
            "  sh -c 'echo $$ > runner.pid; exec sh run-fixtures.sh out' > run.log 2>&1\n"
            "fi\n"
            "echo $? > run.rc\n"))))

(define case-n 0)
;; `(dir base)`: a fresh inner test directory holding the named fixtures.
(define (inner! names)
  (set! case-n (+ case-n 1))
  (let* ((dir (string-append root "/case" (number->string case-n)))
         (base (string-append root "/base" (number->string case-n))))
    (sh "mkdir -p " dir " " base)
    (for-each (lambda (f) (sh "cp " f " " dir "/" f))
              '("run-fixtures.sh" "env.sh" "row-baseline-check.sh"))
    ;; THE LAUNCHER GOES WITH THE RUNNER when this tree has one: the runner
    ;; calls ./launch.pl beside itself and refuses to start without it.
    (when (file-exists? "launch.pl") (sh "cp launch.pl " dir "/launch.pl"))
    (spit! (string-append dir "/rows-baseline.txt") "")
    (for-each (lambda (h) (spit! (string-append dir "/" (car h)) (cdr h))) helper-texts)
    (for-each (lambda (name)
                (let ((t (assoc name fixture-texts)))
                  (unless t (assertion-violation 'inner! "no such inner fixture" name))
                  (spit! (string-append dir "/" name) (cdr t))))
              names)
    (list dir base)))
;; Options: a base ("-" for unset), a THEOURGIA_LIBDIR ("-" to keep), a
;; directory to put first on PATH ("-" for none), a TMPDIR ("-" for the
;; ambient one), a file to give the runner as standard input (else
;; /dev/null), and #t to start the runner with TERM ignored.
(define (start! c . opts)
  (let ((opt (lambda (k) (and (> (length opts) k) (list-ref opts k)))))
    (sh (if (opt 5) "DRIVE_IGNORE_TERM=1 " "")
        "sh " (car c) "/drive.sh " (car c) " "
        (or (opt 0) (cadr c))
        " " (or (opt 1) "-")
        " " (or (opt 2) "-")
        " " (or (opt 3) "")
        " < " (or (opt 4) "/dev/null") " > /dev/null 2>&1 &")))
(define (finished? c secs) (await (string-append (car c) "/run.rc") secs))
(define (rc-of c) (read-datum (string-append (car c) "/run.rc")))
(define (run-log c) (slurp (string-append (car c) "/run.log")))
(define (roots-of c) (read-datum (string-append (car c) "/roots.txt")))
(define (go! c) (spit! (string-append (car c) "/go") "1"))
(define (leaked? c) (contains? (run-log c) "LEAKED-PROCESSES"))
;; The lines from the LEAKED-PROCESSES line up to the verdict.
(define (leak-lines c)
  (let loop ((ls (lines-of (run-log c))) (in? #f) (acc '()))
    (cond ((null? ls) (reverse acc))
          ((starts-with? (car ls) "LEAKED-PROCESSES") (loop (cdr ls) #t (cons (car ls) acc)))
          ((and in? (starts-with? (car ls) "REFUSING")) (reverse acc))
          (in? (loop (cdr ls) #t (cons (car ls) acc)))
          (else (loop (cdr ls) #f acc)))))
(define (word-in? line w)
  (let loop ((i 0))
    (and (<= (+ i (string-length w)) (string-length line))
         (or (and (string=? w (substring line i (+ i (string-length w))))
                  (or (= i 0) (not (char-numeric? (string-ref line (- i 1)))))
                  (or (= (+ i (string-length w)) (string-length line))
                      (not (char-numeric? (string-ref line (+ i (string-length w)))))))
             (loop (+ i 1))))))
;; Some leak line holds the pid, and that same line holds `text`.
(define (listed? c pid text)
  (and (integer? pid)
       (exists (lambda (l) (and (word-in? l (number->string pid)) (contains? l text)))
               (leak-lines c))))
(define (line-with c . words)
  (exists (lambda (l) (for-all (lambda (w) (contains? l w)) words)) (lines-of (run-log c))))
;; THE INNER RUN'S OWN ACCOUNT, for a row that went red (F95). The inner run
;; lives in this fixture's scratch, which is removed; a red reading of its
;; exit code alone -- (#f 6 #f) -- said that it stopped and not why. These
;; are the lines that say why: the verdicts, the abort and the output it
;; quoted, the summary. Bounded, so a runaway log cannot swamp the line.
(define (inner-account c)
  ;; NOTE: `NOTE <fixture>:` lines are the launcher's own notes, copied from its
  ;; result file -- a watcher that could not block a signal, an anchor that
  ;; died. When the launcher wrote one, it is the nearest thing to a cause.
  (let ((marks '("REFUSING" "NOT ALL LAUNCHED" "ABORTED AT" "  | " "RESULT NOT READ"
                 "GROUP NOT READ" "OUTLIVED KILL" "LEFT IN GROUP" "NOTE " "fixtures run:")))
    (let loop ((ls (lines-of (run-log c))) (acc '()) (k 0))
      (cond ((or (null? ls) (>= k 40)) (cons 'inner (reverse acc)))
            ((exists (lambda (m) (starts-with? (car ls) m)) marks)
             (loop (cdr ls) (cons (car ls) acc) (+ k 1)))
            (else (loop (cdr ls) acc k))))))
;; The reading, and on red the inner account beside it.
(define (with-account c got expected)
  (if (equal? got expected) got (append got (list (inner-account c)))))

;; ============================================================================

(printf "== RS-1, RS-4, RS-6: an asker alive during the run; the two roots ==\n")
(let ((c (inner! '("ok.py" "bind.py" "probe-roots.sc" "wait.sc"))))
  ;; OTHER PEOPLE'S DIRECTORIES IN THE SAME BASE: one ordinary, one named
  ;; the way a run root is named. Neither is this run's to remove.
  (sh "mkdir -p " (cadr c) "/keep-me " (cadr c) "/run-zzzzzz")
  (spit! (string-append (cadr c) "/keep-me/file") "1")
  (spit! (string-append (cadr c) "/run-zzzzzz/file") "1")
  (start! c)
  (let ((started (await (string-append (car c) "/started") 60)))
    ;; THE ASKER: a shell loop whose own command line holds the text a
    ;; command-line match looks for, started after the run's first count.
    (sh "cd " (car c) " && { sh -c 'i=0; while [ ! -f stop-asker ] && [ $i -lt 90 ]; do sleep 1; i=$((i+1)); done # scheme --script' "
        "> /dev/null 2>&1 < /dev/null & echo $! > asker.pid; }")
    (pause-tenths 10)
    (go! c)
    (let ((done (finished? c 120)))
      (spit! (string-append (car c) "/stop-asker") "1")
      (stop-pid! (string-append (car c) "/asker.pid"))
      (let* ((r (roots-of c))
             (troot (and r (car r)))
             (tsock (and r (cadr r))))
        (want "CONTROL RS-1 the inner run started and finished, and its probe reported"
              (list started done (and r #t))
              '(#t #t #t))
        (want "RS-1 a shell loop naming \"scheme --script\", started mid-run, is not counted as a leak; status 0"
              (list (leaked? c) (rc-of c))
              '(#f 0))
        (want "RS-6 the scratch root is <base>/run-<token> and the socket root /tmp/ths.<token>, at most 20 bytes, each marked by the probe"
              (and r
                   (list (starts-with? troot (string-append (cadr c) "/run-"))
                         (starts-with? tsock "/tmp/ths.")
                         (<= (string-length tsock) 20)
                         (caddr r)))
              '(#t #t #t (#t #t)))
        (want "RS-6 one token: run-<token>, ths.<token> and THEOURGIA_SUITE_TOKEN agree"
              (and r (> (string-length tsock) 4) (> (string-length troot) 4)
                   (let ((a (basename troot)) (b (basename tsock)))
                     (list (string=? (substring a 4 (string-length a)) (substring b 4 (string-length b)))
                           (string=? (list-ref r 3) (substring b 4 (string-length b))))))
              '(#t #t))
        (want "RS-6 a 60-byte socket name binds under the socket root"
              (slurp (string-append (car c) "/bind.txt"))
              "bound")
        (want "GUARD RS-6 this machine shows environments: the count is not in its machine-wide fallback"
              (contains? (run-log c) "machine-wide")
              #f)
        (want "RS-4 the start line names both roots the fixtures saw, and the removal line names both"
              (and r (> (string-length tsock) 0)
                   (list (and (line-with c "this run" troot tsock) #t)
                         (and (line-with c "removed" troot tsock) #t)))
              '(#t #t))
        (want "RS-4 after the run neither root exists"
              (and r (> (string-length tsock) 0)
                   (list (file-exists? troot) (file-exists? tsock)))
              '(#f #f))
        (want "RS-4 and no ths-snap.<token>.* snapshot directory of this run remains"
              (and r (> (string-length (list-ref r 3)) 0)
                   (snap-dirs (list-ref r 3)))
              '())
        (want "GUARD RS-4 the base's other directories are untouched, the run-shaped one included"
              (list (file-exists? (string-append (cadr c) "/keep-me/file"))
                    (file-exists? (string-append (cadr c) "/run-zzzzzz/file")))
              '(#t #t))))))

(printf "== RS-6: no base given ==\n")
(let ((c (inner! '("ok.py" "probe-roots.sc"))))
  (start! c "-")
  (let* ((done (finished? c 120))
         (r (roots-of c)))
    (want "RS-6 with THEOURGIA_TEST_ROOT unset the scratch root is /tmp/run-<token>, and it is gone afterwards"
          (and r (list done (starts-with? (car r) "/tmp/run-") (file-exists? (car r))))
          '(#t #t #f))))

(printf "== RS-2: a process from outside the run ==\n")
(let ((c (inner! '("ok.py" "wait.sc"))))
  (start! c)
  (let ((started (await (string-append (car c) "/started") 60)))
    ;; STARTED BY THIS FILE after the run's first count: it carries this
    ;; file's environment, which holds an outer run's token if any, and not
    ;; the inner run's.
    (sh "cd " (car c) " && { scheme --script foreign.ss > /dev/null 2>&1 < /dev/null & }")
    (let ((foreign-up (await (string-append (car c) "/foreign.pid") 30)))
      (go! c)
      (let ((done (finished? c 120)))
        (stop-pid! (string-append (car c) "/foreign.pid"))
        (want "CONTROL RS-2 the inner run started and finished, the foreign process alive across its end"
              (list started foreign-up done)
              '(#t #t #t))
        (want "RS-2 a scheme process without the run's token is not counted, and the run's status is 0"
              (list (leaked? c) (rc-of c))
              '(#f 0))))))

(printf "== RS-3: fixtures that leave processes running ==\n")
(let ((c (inner! '("ok.py" "leaker.sc" "leakspawn.sc" "leaksh.sc"))))
  (start! c)
  (let* ((done (finished? c 120))
         (p1 (read-datum (string-append (car c) "/sleeper.pid")))
         (p2 (read-datum (string-append (car c) "/sleeper2.pid")))
         (p3 (read-datum (string-append (car c) "/sleepsh.pid"))))
    (for-each (lambda (f) (stop-pid! (string-append (car c) "/" f)))
              '("sleeper.pid" "sleeper2.pid" "sleepsh.pid"))
    (want "CONTROL RS-3 the inner run finished and each left process wrote its pid"
          (list done (integer? p1) (integer? p2) (integer? p3))
          '(#t #t #t #t))
    (want "RS-3 a scheme left from a shell is listed with its command line, and the run refuses with status 3"
          (list (listed? c p1 "sleeper.ss") (rc-of c))
          '(#t 3))
    (want "RS-3 a scheme left through posix_spawnp and setsid, as the product starts a daemon, is listed"
          (listed? c p2 "sleeper2.ss")
          #t)
    (want "RS-3 a process that is not scheme but carries the run's token is listed"
          ;; MATCHED ON ITS ARGUMENTS: Homebrew's python3 re-executes as Python.app
          ;; in the same pid, so the command line the system reports never says
          ;; "python3" (measured 09-23).
          (listed? c p3 "time.sleep(90)")
          #t)))
;; THE TWO COUNTS CAN CANCEL. A foreign process alive at the first count
;; ends during the run while a fixture leaves one behind: a before/after
;; total is unchanged, and the leak is still a leak.
(let ((c (inner! '("ok.py" "leaker.sc" "wait.sc"))))
  (sh "cd " (car c) " && { scheme --script foreign.ss > /dev/null 2>&1 < /dev/null & }")
  (let ((foreign-up (await (string-append (car c) "/foreign.pid") 30)))
    (start! c)
    (let ((started (await (string-append (car c) "/started") 60)))
      (stop-pid! (string-append (car c) "/foreign.pid"))
      (pause-tenths 10)
      (go! c)
      (let* ((done (finished? c 120))
             (p (read-datum (string-append (car c) "/sleeper.pid"))))
        (stop-pid! (string-append (car c) "/sleeper.pid"))
        (want "CONTROL RS-3 cancel: the foreign process was up before the run, the run finished"
              (list foreign-up started done (integer? p))
              '(#t #t #t #t))
        (want "RS-3 a leak is reported even when a foreign process ended during the run"
              (listed? c p "sleeper.ss")
              #t)))))

(printf "== RS-4: removal on every exit ==\n")
;; A RED VERDICT.
(let ((c (inner! '("ok.py" "probe-roots.sc" "red.sc"))))
  (start! c)
  (let* ((done (finished? c 120))
         (r (roots-of c)))
    (want "RS-4 after a run that refuses for a red fixture, both roots are gone"
          (and r (list done (rc-of c) (file-exists? (car r)) (file-exists? (cadr r))))
          '(#t 2 #f #f)))
  ;; AN EARLY REFUSAL, before any fixture: a library path with no igropyr.
  (let* ((c2 (inner! '("ok.py" "green.sc")))
         (before (ths-now)))
    (start! c2 #f (string-append root "/no-such-libdir"))
    (let* ((done (finished? c2 120))
           (after (ths-now)))
      (want "GUARD RS-4 after a run refused on its library path, the base is empty and no new /tmp/ths.* remains"
            (list done (rc-of c2) (entries (cadr c2))
                  (filter (lambda (n) (not (member n before))) after))
            '(#t 1 () ())))))

(printf "== RS-11: a root that cannot be removed is not reported removed (codex r2 A2) ==\n")
(let ((c (inner! '("ok.py" "green.sc" "zz-lockbase.sc"))))
  (start! c)
  (let* ((done (finished? c 120))
         (l (read-datum (string-append (car c) "/locked.txt")))
         (troot (and l (car l))))
    ;; Restore the base before reading anything under it, then tidy up.
    (sh "chmod 700 " (cadr c))
    (want "CONTROL RS-11 the run finished, and the fixture locked this run's base"
          (list done (and l (equal? (cadr l) (cadr c))))
          '(#t #t))
    (want "RS-11 the runner says NOT REMOVED for its scratch root and exits 5, not \"removed\" and 0"
          (and troot
               (list (and (line-with c "NOT REMOVED" troot) #t)
                     (rc-of c)
                     (file-exists? troot)))
          '(#t 5 #t))
    (when (and troot (starts-with? troot (string-append (cadr c) "/run-")))
      (sh "rm -rf " troot))))

;; A LEAK AND A LOCKED BASE TOGETHER: the count must not depend on the
;; scratch root the fixtures were handed, and the leak still refuses first.
(let ((c (inner! '("ok.py" "leaker.sc" "zz-lockbase.sc"))))
  (start! c)
  (let* ((done (finished? c 120))
         (l (read-datum (string-append (car c) "/locked.txt")))
         (troot (and l (car l)))
         (p (read-datum (string-append (car c) "/sleeper.pid"))))
    (sh "chmod 700 " (cadr c))
    (stop-pid! (string-append (car c) "/sleeper.pid"))
    (want "CONTROL RS-11b the run finished, the base was locked, and the leak wrote its pid"
          (list done (and l (equal? (cadr l) (cadr c))) (integer? p))
          '(#t #t #t))
    (want "RS-11b with its base locked the run still lists the leak, says NOT REMOVED, and refuses with 3"
          (and troot
               (list (listed? c p "sleeper.ss")
                     (and (line-with c "NOT REMOVED" troot) #t)
                     (rc-of c)))
          '(#t #t 3))
    (when (and troot (starts-with? troot (string-append (cadr c) "/run-")))
      (sh "rm -rf " troot))))

;; A LOCKED SOCKET ROOT DOES NOT BLIND THE COUNT. The process snapshots live
;; in a directory of the runner's own, not exported (design amendment 4), so
;; a fixture that locks what it was handed changes nothing about the count;
;; and the runner still removes the root it made.
(let ((c (inner! '("ok.py" "green.sc" "zz-locksock.sc"))))
  (start! c)
  (let* ((done (finished? c 120))
         (d (read-datum (string-append (car c) "/lockedsock.txt"))))
    (when (and (string? d) (starts-with? d "/tmp/ths.") (file-exists? d)) (sh "chmod 700 " d))
    (want "CONTROL RS-11c the run finished and the fixture was handed a socket root to lock"
          (list done (and (string? d) (starts-with? d "/tmp/ths.")))
          '(#t #t))
    (want "RS-11c with the socket root locked the count is still taken, the run exits 0, and the socket root is gone"
          (and (string? d)
               (list (and (line-with c "LEAK COUNT NOT TAKEN") #t)
                     (and (line-with c "none carrying") #t)
                     (rc-of c)
                     (file-exists? d)))
          '(#f #t 0 #f))
    (when (and (string? d) (starts-with? d "/tmp/ths.") (<= (string-length d) 20))
      (sh "rm -rf " d))))

;; WHEN THE COUNT CANNOT BE TAKEN, IT SAYS SO AND REFUSES. An `awk` on PATH
;; that fails exactly when it is asked to match this run's token -- the
;; matcher's own call -- and passes every other use through: a failure a
;; pipe after it must not mask as "none carrying". It fails only once the
;; last .sc fixture has armed it (a file in the runner's directory), so the
;; runner's opening probe, which uses the same matcher, is left alone.
(let* ((c (inner! '("ok.py" "green.sc" "zz-armawk.sc")))
       (shim (string-append root "/shim-awk"))
       (real (begin (sh "command -v awk > " root "/real-awk")
                    (let ((t (slurp (string-append root "/real-awk"))))
                      (if (and (> (string-length t) 0)
                               (char=? #\newline (string-ref t (- (string-length t) 1))))
                          (substring t 0 (- (string-length t) 1))
                          t)))))
  (sh "mkdir -p " shim)
  (spit! (string-append shim "/awk")
         (string-append "#!/bin/sh\n"
                        "if [ -f awk-armed ]; then for a in \"$@\"; do case \"$a\" in *THEOURGIA_SUITE_TOKEN=*) exit 2;; esac; done; fi\n"
                        "exec " real " \"$@\"\n"))
  (sh "chmod +x " shim "/awk")
  (start! c #f #f shim)
  (let ((done (finished? c 120)))
    (want "CONTROL RS-11d the real awk was found, the run finished, it was armed, and the opening probe was not failed"
          (list (> (string-length real) 0) done
                (file-exists? (string-append (car c) "/awk-armed"))
                (contains? (run-log c) "machine-wide"))
          '(#t #t #t #f))
    (want "RS-11d a matcher that fails is LEAK COUNT NOT TAKEN, never \"none carrying\", and the run refuses with 3"
          (list (and (line-with c "LEAK COUNT NOT TAKEN") #t)
                (and (line-with c "none carrying") #t)
                (rc-of c))
          '(#t #f 3))))

;; THE THIRD DIRECTORY BEHIND AN UNSEARCHABLE PARENT (codex r4 A1). With
;; TMPDIR set to the run's base, the private snapshot directory sits beside
;; the scratch root, and locking the base hides both. The snapshots cannot
;; be taken either, so the count is not taken; what matters here is that the
;; snapshot directory is not reported removed.
(let ((c (inner! '("ok.py" "green.sc" "zz-lockbase.sc"))))
  (start! c #f #f #f (cadr c))
  (let* ((done (finished? c 120))
         (l (read-datum (string-append (car c) "/locked.txt"))))
    (sh "chmod 700 " (cadr c))
    (want "CONTROL RS-11e the run finished and the fixture locked this run's base, which was TMPDIR"
          (list done (and l (equal? (cadr l) (cadr c))))
          '(#t #t))
    (want "RS-11e the snapshot directory behind the locked base is NOT REMOVED too, and the run refuses"
          (list (and (line-with c "NOT REMOVED" "ths-snap.") #t)
                (and (memv (rc-of c) '(3 5)) #t))
          '(#t #t))
    (want "RS-11e a result that cannot be read is RESULT NOT READ, and nothing is launched after it"
          (list (and (line-with c "RESULT NOT READ") #t)
                (file-exists? (string-append (car c) "/out/ok.out")))
          '(#t #f))
    ;; F95: THE ABORT SAYS WHERE AND WHY, and quotes the fixture's output,
    ;; in the run's own log -- the one place that survives the run's removal
    ;; of its scratch.
    (want "F95 an aborted run names the fixture it stopped at and the reason, and quotes that fixture's last lines of output"
          (list (and (line-with c "ABORTED AT" "could not be read") #t)
                (and (line-with c "the last 20 lines of") #t))
          '(#t #t))
    (for-each (lambda (n)
                (when (or (starts-with? n "run-") (starts-with? n "ths-snap."))
                  (sh "rm -rf " (cadr c) "/" n)))
              (entries (cadr c)))))

;; A SNAPSHOT THAT NEVER SUCCEEDS IS A COUNT NOT TAKEN, from the first probe
;; on (codex r4 A2): a `ps` on PATH that fails every time must not send the
;; run to the fallback, whose line says the platform hides environments.
(let* ((c (inner! '("ok.py" "green.sc")))
       (shim (string-append root "/shim-ps")))
  (sh "mkdir -p " shim)
  (spit! (string-append shim "/ps") "#!/bin/sh\nexit 1\n")
  (sh "chmod +x " shim "/ps")
  (start! c #f #f shim)
  (let ((done (finished? c 120)))
    (want "CONTROL RS-11f the run with a failing ps finished"
          done
          #t)
    (want "RS-11f with every snapshot failing the run says LEAK COUNT NOT TAKEN, not the fallback, and refuses with 3"
          (list (and (line-with c "LEAK COUNT NOT TAKEN") #t)
                (contains? (run-log c) "machine-wide")
                (rc-of c))
          '(#t #f 3))))

(printf "== RS-7: the summary's python numbers ==\n")
(let ((c (inner! '("ok.py" "red.sc"))))
  (start! c)
  (let ((done (finished? c 120)))
    (want "RS-7 one red .sc, one green .py: the exact summary, and the old wording is gone"
          (list done
                (contains? (run-log c) "not-green: 1   (python among them: 0)")
                (and (line-with c "python fixtures run: 1") #t)
                (contains? (run-log c) "(of those,"))
          '(#t #t #t #f))))
(let ((c (inner! '("ok.py" "redpy.py" "green.sc"))))
  (start! c)
  (let ((done (finished? c 120)))
    (want "RS-7 one red .py, one green .py, one green .sc: python among them 1, python fixtures run 2"
          (list done
                (contains? (run-log c) "not-green: 1   (python among them: 1)")
                (and (line-with c "python fixtures run: 2") #t))
          '(#t #t #t))))

(let ((c (inner! '("ok.py" "probepy.py" "green.sc"))))
  (start! c)
  (let ((done (finished? c 120)))
    (want "CONTROL RS-7 the runner classified the python probe as a probe"
          (list done (and (line-with c "probes" "probepy") #t))
          '(#t #t))
    (want "RS-7 a python probe is not counted in python fixtures run (codex r1 A6)"
          (list (and (line-with c "python fixtures run: 1") #t)
                (and (line-with c "python fixtures run: 2") #t))
          '(#t #f))))


(printf "== RS-V: void rows are counted by fixture in the summary ==\n")
(let ((c (inner! '("ok.py" "green.sc" "void.sc"))))
  (start! c)
  (let ((done (finished? c 120)))
    (want "RS-V a fixture that printed one VOID row is named in the summary's void line, and a green one is not"
          (list done (and (line-with c "void rows: 1 in: void(1)") #t) (and (line-with c "void rows:" "green") #t))
          '(#t #t #f))))
(let ((c (inner! '("ok.py" "green.sc"))))
  (start! c)
  (let ((done (finished? c 120)))
    (want "RS-V TWIN: with no VOID row the summary says so, with the number"
          (list done (and (line-with c "void rows: 0") #t))
          '(#t #t))))

(printf "== RS-L: under THEOURGIA_FIXTURE_LIMIT the row baseline is not judged ==\n")
;; The variable is set and cleared around each start here, and the outer
;; value restored: this file may itself run under a limit.
(define outer-limit (or (getenv "THEOURGIA_FIXTURE_LIMIT") ""))
(let ((c (inner! '("ok.py" "rowsy.sc"))))
  (putenv "THEOURGIA_FIXTURE_LIMIT" "")
  (start! c)
  (putenv "THEOURGIA_FIXTURE_LIMIT" outer-limit)
  (let ((done (finished? c 120)))
    (want "RS-L CONTROL: without the limit, a fixture whose rows the table does not list refuses the run"
          (list done (and (line-with c "NO ROW BASELINE" "rowsy") #t) (and (line-with c "REFUSING: the row baseline") #t))
          '(#t #t #t))))
(let ((c (inner! '("ok.py" "rowsy.sc"))))
  (putenv "THEOURGIA_FIXTURE_LIMIT" "60")
  (start! c)
  (putenv "THEOURGIA_FIXTURE_LIMIT" outer-limit)
  (let ((done (finished? c 120)))
    (want "RS-L under the limit the step prints one line saying it did not run, and the baseline does not refuse"
          (list done (and (line-with c "row baseline: not checked: THEOURGIA_FIXTURE_LIMIT=60") #t)
                (and (line-with c "NO ROW BASELINE") #t) (and (line-with c "REFUSING: the row baseline") #t))
          '(#t #t #f #f))))

(printf "== RS-8: the token matcher, on synthetic snapshots (codex r1 A2, A3) ==\n")
;; THE MATCHER IS TAKEN OUT OF THE RUNNER AS IT IS, not copied: the lines
;; from `carrying_token() {` to the closing `}` are sourced by a shell that
;; sets the run root and the token, over three snapshot files written here.
;; `.ps-argv` is the command lines taken first, `.ps-env` the environments,
;; `.ps-argv2` the command lines taken after them (design amendment on r1's
;; findings: a process can be born, exec, or change its argv between two
;; snapshots). SELF stands for the matching shell's own pid. The shell sets
;; both run_root and snap_dir to the case directory: the snapshots moved from
;; the scratch root to the socket root (a locked scratch base blinded the
;; count, codex r2 follow-up), and the rows hold whichever the matcher reads.
;; The "is not listed" rows read green on a runner with no matcher at all;
;; the CONTROL row above them is what goes red there.
(define (matcher-case name argv env argv2)
  (let* ((d (string-append root "/matcher-" name)))
    (sh "mkdir -p " d)
    (spit! (string-append d "/.ps-argv") argv)
    (spit! (string-append d "/.ps-env") env)
    (spit! (string-append d "/.ps-argv2") argv2)
    (spit! (string-append d "/m.sh")
           (string-append
             "run_root=\"$1\"\n"
             "snap_dir=\"$1\"\n"
             "THEOURGIA_SUITE_TOKEN=tokA1\n"
             "export THEOURGIA_SUITE_TOKEN\n"
             "for f in .ps-argv .ps-env .ps-argv2; do sed \"s/SELF/$$/g\" \"$run_root/$f\" > \"$run_root/$f.t\" && mv \"$run_root/$f.t\" \"$run_root/$f\"; done\n"
             "eval \"$(sed -n '/^carrying_token() {/,/^}/p' run-fixtures.sh)\"\n"
             "carrying_token | tr '\\n' ' ' > \"$run_root/found\"\n"))
    (sh "sh " d "/m.sh " d " > " d "/m.out 2>&1")
    (let ((found (slurp (string-append d "/found"))))
      (let loop ((ws (lines-of (list->string (map (lambda (c) (if (char=? c #\space) #\newline c))
                                                  (string->list found)))))
                 (acc '()))
        (cond ((null? ws) (list-sort < acc))
              ((string->number (car ws)) => (lambda (n) (loop (cdr ws) (cons n acc))))
              (else (loop (cdr ws) acc)))))))
(define S "/opt/x/scheme --script a.ss")
(define T "THEOURGIA_SUITE_TOKEN=tokA1")
(define (ln . parts) (string-append (apply string-append parts) "\n"))
(want "CONTROL RS-8 the matcher was found in run-fixtures.sh and lists a steady process carrying the token"
      (matcher-case "steady" (ln "123 1 " S) (ln "123 1 " S " " T " HOME=/h") (ln "123 1 " S))
      '(123))
(want "RS-8 a process born between the first command-line snapshot and the environment snapshot is listed"
      (matcher-case "born" "" (ln "123 1 " S " " T " HOME=/h") (ln "123 1 " S))
      '(123))
(want "RS-8 a process that exec'd between the first command-line snapshot and the environment snapshot is listed"
      (matcher-case "exec" (ln "123 1 /bin/sh -c x") (ln "123 1 " S " " T " HOME=/h") (ln "123 1 " S))
      '(123))
(want "RS-8 a token in the command line only, written there before the environment snapshot, is not listed"
      (matcher-case "argv" (ln "123 1 " S) (ln "123 1 " S " " T " HOME=/h") (ln "123 1 " S " " T))
      '())
(want "RS-8 a longer token that begins with this run's is not listed"
      (matcher-case "prefix" (ln "123 1 " S) (ln "123 1 " S " " T "X HOME=/h") (ln "123 1 " S))
      '())
(want "RS-8 the matching shell itself and its direct child are not listed; a grandchild is"
      ;; All three are in all three snapshots, so this row is about the
      ;; exclusion alone and not about a process born between them.
      (matcher-case "self"
                    (string-append (ln "SELF 1 sh m.sh") (ln "124 SELF " S) (ln "125 124 " S))
                    (string-append (ln "SELF 1 sh m.sh " T) (ln "124 SELF " S " " T) (ln "125 124 " S " " T))
                    (string-append (ln "SELF 1 sh m.sh") (ln "124 SELF " S) (ln "125 124 " S)))
      '(125))

(printf "== RS-9: the fallback lists what it counts (codex r1 A7) ==\n")
;; THE FALLBACK COUNTS scheme PROCESSES BY NAME, MACHINE-WIDE, and reports a
;; leak only when the count rose. So this row can read red when another
;; session's scheme processes end during the inner run and cancel the leak
;; (09-23: red once in a whole-file run while the code session was running
;; its own scheme probes; green three times out of three run alone). That is
;; the fallback's named limit, not the listing's; a red here is read, not
;; waived, and rerun alone.
;; A `uname` on PATH that answers Linux leaves the runner without an
;; environment flag, which is the fallback by construction.
(let* ((c (inner! '("ok.py" "leaker.sc")))
       (shim (string-append root "/shim-uname")))
  (sh "mkdir -p " shim)
  (spit! (string-append shim "/uname") "#!/bin/sh\necho Linux\n")
  (sh "chmod +x " shim "/uname")
  (start! c #f #f shim)
  (let* ((done (finished? c 120))
         (p (read-datum (string-append (car c) "/sleeper.pid"))))
    (stop-pid! (string-append (car c) "/sleeper.pid"))
    (want "CONTROL RS-9 with uname answering Linux the run finished and said it was in its machine-wide fallback"
          (list done (contains? (run-log c) "machine-wide") (integer? p))
          '(#t #t #t))
    (want "RS-9 in the fallback the leak is still listed per pid, with its command line"
          (listed? c p "sleeper.ss")
          #t)))

(printf "== RS-10: a slow start is waited for, not read once (codex r1 A5) ==\n")
;; A `scheme` on PATH that waits 2 s before it becomes the real scheme: the
;; runner's environment probe is not visible at 1 s, and is at 2 s.
(let* ((c (inner! '("ok.py" "green.sc")))
       (shim (string-append root "/shim-scheme"))
       (real (begin (sh "command -v scheme > " root "/real-scheme")
                    (let ((t (slurp (string-append root "/real-scheme"))))
                      (if (and (> (string-length t) 0)
                               (char=? #\newline (string-ref t (- (string-length t) 1))))
                          (substring t 0 (- (string-length t) 1))
                          t)))))
  (sh "mkdir -p " shim)
  (spit! (string-append shim "/scheme") (string-append "#!/bin/sh\nsleep 2\nexec " real " \"$@\"\n"))
  (sh "chmod +x " shim "/scheme")
  (start! c #f #f shim)
  (let ((done (finished? c 180)))
    (want "CONTROL RS-10 the real scheme was found and the run with the slow shim finished"
          (list (> (string-length real) 0) done)
          '(#t #t))
    (want "RS-10 a probe that takes 2 s to become scheme is waited for: the count is by the run's token"
          (list (contains? (run-log c) "machine-wide")
                (contains? (run-log c) "by this run's token"))
          '(#f #t))))

(printf "== RS-5b: a runner stopped with TERM stops its running fixture ==\n")
;; TERM TO THE RUNNER'S PID, the way a gate is stopped: the fixture it was
;; running must not outlive it, or it writes into a root the runner has just
;; reported removed (measured 09-23 16:14-16:16 on the r4 gate: cli1 rebuilt
;; the scratch root with 21 files two minutes after "removed").
(let ((c (inner! '("ok.py" "probe-roots.sc" "wait-late.sc"))))
  (start! c)
  (let* ((started (await (string-append (car c) "/started") 60))
         (runner (read-datum (string-append (car c) "/runner.pid")))
         (holder (read-datum (string-append (car c) "/wait.pid"))))
    (when (and (integer? runner) (> runner 1))
      (sh "kill -TERM " (number->string runner)))
    (pause-tenths 30)
    (let ((alive (and (integer? holder)
                      (= 0 (system (string-append "kill -0 " (number->string holder) " 2>/dev/null"))))))
      (go! c)
      (pause-tenths 40)
      (stop-pid! (string-append (car c) "/wait.pid"))
      (let* ((r (roots-of c))
             (troot (and r (car r))))
        (want "CONTROL RS-5b the run started and its runner and holder wrote their pids"
              (list started (integer? runner) (integer? holder))
              '(#t #t #t))
        (want "RS-5b three seconds after TERM the fixture it was running is gone, and the scratch root stays removed"
              (and r (list alive (file-exists? troot)))
              '(#f #f))
        (when (and troot (starts-with? troot (string-append (cadr c) "/run-")))
          (sh "rm -rf " troot))
        (when (and r (starts-with? (cadr r) "/tmp/ths.") (<= (string-length (cadr r)) 20))
          (sh "rm -rf " (cadr r)))
        (when r
          (for-each (lambda (d)
                      (when (starts-with? d (string-append tmp-base "/ths-snap." (list-ref r 3) "."))
                        (sh "rm -rf " d)))
                    (snap-dirs (list-ref r 3))))))))

(printf "== RS-12: how a fixture is started, and how it is stopped (codex r5) ==\n")
(let ((c (inner! '("ok.py" "green.sc" "setsid.py" "sigint.py"))))
  (start! c)
  (let ((done (finished? c 120)))
    (want "CONTROL RS-12 the run finished and both python fixtures reported"
          (list done
                (file-exists? (string-append (car c) "/setsid.txt"))
                (file-exists? (string-append (car c) "/sigint.txt")))
          '(#t #t #t))
    (want "RS-12a a fixture may make itself a session leader: os.setsid() is ok"
          (slurp (string-append (car c) "/setsid.txt"))
          "ok")
    ;; WHATEVER THE CALLER HANDED DOWN: the launcher resets SIGINT. On the
    ;; tree before this change the answer depends on who started the suite
    ;; (from this session's tools it arrives ignored even there).
    (want "RS-12b a fixture starts with SIGINT at its default, not ignored"
          (slurp (string-append (car c) "/sigint.txt"))
          "default")))
;; A PREFLIGHT IS STOPPED LIKE A FIXTURE (codex r5 A2).
(let ((c (inner! '("ok.py" "green.sc"))))
  (sh "cp " (car c) "/slow-preflight.ss " (car c) "/expansion-branches.sc")
  (start! c)
  (let* ((pre (await (string-append (car c) "/pre-started") 60))
         (runner (read-datum (string-append (car c) "/runner.pid"))))
    (when (and (integer? runner) (> runner 1)) (sh "kill -TERM " (number->string runner)))
    (pause-tenths 110)
    (let* ((writer (read-datum (string-append (car c) "/writer.pid")))
           (troot (read-datum (string-append (car c) "/pre-root.txt")))
           (writer-alive (and (integer? writer)
                              (= 0 (system (string-append "kill -0 " (number->string writer) " 2>/dev/null")))))
           (root-there (and (string? troot) (> (string-length troot) 0) (file-exists? troot))))
      (finished? c 60)
      (stop-pid! (string-append (car c) "/writer.pid"))
      (stop-pid! (string-append (car c) "/pre.pid"))
      (want "CONTROL RS-12c the preflight started, handed a scratch root, and the runner was stopped by TERM"
            (list pre (integer? runner) (and (string? troot) (starts-with? troot (string-append (cadr c) "/run-"))))
            '(#t #t #t))
      (want "RS-12c eleven seconds after TERM during a preflight, its writer is gone and the scratch root is not there"
            (list writer-alive root-there)
            '(#f #f))
      (when (and (string? troot) (starts-with? troot (string-append (cadr c) "/run-")))
        (sh "rm -rf " troot)))))
;; A CHILD THAT IGNORES TERM IS STOPPED WITH ITS GROUP (codex r5 A1). The
;; window codex read is narrow, so on a runner that sends KILL to the group
;; this row may be green without polling the group; it is a GUARD for the
;; property, labelled so.
(let ((c (inner! '("ok.py" "termchild.sc"))))
  (start! c)
  (let* ((started (await (string-append (car c) "/started") 60))
         (runner (read-datum (string-append (car c) "/runner.pid")))
         (child (read-datum (string-append (car c) "/tchild.pid"))))
    (when (and (integer? runner) (> runner 1)) (sh "kill -TERM " (number->string runner)))
    (let ((done (finished? c 60)))
      (pause-tenths 10)
      (let ((alive (and (integer? child)
                        (= 0 (system (string-append "kill -0 " (number->string child) " 2>/dev/null"))))))
        (go! c)
        (stop-pid! (string-append (car c) "/tchild.pid"))
        (want "CONTROL RS-12d the fixture's TERM-ignoring child started, and the runner exited after TERM"
              (list started (integer? child) done)
              '(#t #t #t))
        (want "GUARD RS-12d a second after the runner exits, a child that ignores TERM is gone with its group"
              alive
              #f)))))

(printf "== RS-13: every fixture reads /dev/null, whatever the runner was given ==\n")
;; THE RUNNER ITSELF IS GIVEN A REGULAR FILE AS STANDARD INPUT here, so the
;; fixture sees /dev/null only if the runner hands it that; a row whose
;; runner already read /dev/null could not tell. (Replaces client-program
;; P-17, which matched the runner's text and went red when the redirection
;; moved into the launcher with the behaviour unchanged.)
;;
;; WHAT MAKES IT RED, measured 09-23: the runner of 89124d4 with its
;; `< /dev/null` removed from the foreground fixture line gives "other".
;; A launcher that starts the fixture as a background job does not, with or
;; without the explicit redirection: POSIX gives an asynchronous list
;; /dev/null as standard input when job control is off. The row holds the
;; property, not the line that provides it today.
(let ((c (inner! '("ok.py" "green.sc" "stdin.py"))))
  (spit! (string-append (car c) "/given-stdin") "not the null device\n")
  (start! c #f #f #f #f (string-append (car c) "/given-stdin"))
  (let ((done (finished? c 120)))
    (want "CONTROL RS-13 the run finished and the fixture reported its standard input"
          (list done (file-exists? (string-append (car c) "/stdin.txt")))
          '(#t #t))
    (want "RS-13 a fixture's standard input is /dev/null although the runner's was a regular file"
          (slurp (string-append (car c) "/stdin.txt"))
          "null")))

(printf "== RS-14: a member left in the group is reported at its fixture ==\n")
(let ((c (inner! '("ok.py" "green.sc" "leavegroup.sc"))))
  (start! c)
  (let* ((done (finished? c 120))
         (p (read-datum (string-append (car c) "/lg.pid"))))
    (want "CONTROL RS-14 the run finished and the fixture recorded its group member"
          (list done (integer? p))
          '(#t #t))
    (want "RS-14 LEFT IN GROUP names the fixture and the pid, the run refuses with 3, and the member is gone"
          (with-account c
                        (list (and (integer? p) (line-with c "LEFT IN GROUP" "leavegroup" (number->string p)) #t)
                              (rc-of c)
                              (alive? p))
                        '(#t 3 #f))
          '(#t 3 #f))
    (stop-pid! (string-append (car c) "/lg.pid"))))

(printf "== RS-15: a runner started with TERM ignored still stops on TERM ==\n")
;; MEASURED: a non-interactive sh started with TERM ignored cannot trap TERM.
;; The runner re-execs itself through perl to put TERM back to default first
;; (launcher design L7'').
(let ((c (inner! '("ok.py" "probe-roots.sc" "wait.sc" "marker.sc"))))
  (start! c #f #f #f #f #f #t)
  (let* ((started (await (string-append (car c) "/started") 60))
         (runner (read-datum (string-append (car c) "/runner.pid"))))
    (when (and (integer? runner) (> runner 1)) (sh "kill -TERM " (number->string runner)))
    (let ((done (finished? c 20)))
      (go! c)
      (stop-pid! (string-append (car c) "/wait.pid"))
      (let ((r (roots-of c)))
        (want "CONTROL RS-15 the run started under an ignored TERM, and TERM was sent to its pid"
              (list started (integer? runner))
              '(#t #t))
        (want "RS-15 within twenty seconds of TERM the runner has stopped with 143 and said SIGNALLED"
              (list done (rc-of c) (and (line-with c "SIGNALLED") #t))
              '(#t 143 #t))
        ;; F104 (I3): AND IT NAMES THE LAUNCH THE SIGNAL CUT. TERM reaches the
        ;; runner while `wait` holds the run open (it wrote `started`), so the
        ;; line names wait and the signal's number. Without it SIGNALLED alone
        ;; did not say which launch was cut.
        (want "F104 RS-15 the signalled run names the launch the signal cut: ABORTED AT wait: signal 15"
              (and (line-with c "ABORTED AT wait: signal 15") #t)
              #t)
        (when (and r (starts-with? (car r) (string-append (cadr c) "/run-"))) (sh "rm -rf " (car r)))
        (when (and r (starts-with? (cadr r) "/tmp/ths.") (<= (string-length (cadr r)) 20)) (sh "rm -rf " (cadr r)))))))

(printf "== F90-1: a clock that cannot be read while stopping a launch is named ==\n")
;; A `perl` on PATH that answers the monotonic clock's reading with a word
;; that is not a number, once armed, and passes every other use through --
;; the runner's re-exec, launch.pl, the probe. It is armed by this row after
;; the fixture has started and before TERM, so the only clock reads it
;; fails are the ones the signal path makes: the wait for the watcher.
(let* ((c (inner! '("ok.py" "wait.sc")))
       (shim (string-append root "/shim-perl"))
       (real (begin (sh "command -v perl > " root "/real-perl")
                    (let ((t (slurp (string-append root "/real-perl"))))
                      (if (and (> (string-length t) 0)
                               (char=? #\newline (string-ref t (- (string-length t) 1))))
                          (substring t 0 (- (string-length t) 1))
                          t)))))
  (sh "mkdir -p " shim)
  (spit! (string-append shim "/perl")
         (string-append "#!/bin/sh\n"
                        "if [ -f clock-armed ]; then case \"$1\" in *CLOCK_MONOTONIC*) echo not-a-number; exit 0;; esac; fi\n"
                        "exec " real " \"$@\"\n"))
  (sh "chmod +x " shim "/perl")
  (start! c #f #f shim)
  (let* ((started (await (string-append (car c) "/started") 60))
         (runner (read-datum (string-append (car c) "/runner.pid"))))
    (spit! (string-append (car c) "/clock-armed") "1")
    (when (and (integer? runner) (> runner 1)) (sh "kill -TERM " (number->string runner)))
    (let ((done (finished? c 30)))
      (go! c)
      (stop-pid! (string-append (car c) "/wait.pid"))
      (let ((r (roots-of c)))
        (want "CONTROL F90-1 the real perl was found, the run started, the clock was armed, and TERM was sent"
              (list (> (string-length real) 0) started
                    (file-exists? (string-append (car c) "/clock-armed")) (integer? runner))
              '(#t #t #t #t))
        (want "F90-1 a clock that cannot be read while stopping the watcher is named as the clock, not as a watcher that did not stop"
              (list done (rc-of c)
                    (and (line-with c "GROUP NOT READ" "wait" "CLOCK NOT READ") #t)
                    (and (line-with c "did not stop within") #t))
              '(#t 143 #t #f))
        (when (and r (starts-with? (car r) (string-append (cadr c) "/run-"))) (sh "rm -rf " (car r)))
        (when (and r (starts-with? (cadr r) "/tmp/ths.") (<= (string-length (cadr r)) 20)) (sh "rm -rf " (cadr r)))))))

(printf "== RS-16: the runner's marker does not reach its fixtures ==\n")
(let ((c (inner! '("ok.py" "marker.sc"))))
  (start! c)
  (let ((done (finished? c 120)))
    (want "GUARD RS-16 a fixture does not see THEOURGIA_RUNNER_NORMALISED, so a runner it starts normalises itself"
          (list done (read-datum (string-append (car c) "/marker.txt")))
          '(#t ""))))

(printf "== RS-5: a run killed mid-way ==\n")
(let ((c (inner! '("ok.py" "probe-roots.sc" "wait.sc"))))
  (start! c)
  (let* ((started (await (string-append (car c) "/started") 60))
         (runner (read-datum (string-append (car c) "/runner.pid"))))
    (when (and (integer? runner) (> runner 1))
      (sh "kill -9 " (number->string runner)))
    (pause-tenths 10)
    (go! c)
    (stop-pid! (string-append (car c) "/wait.pid"))
    (let* ((r (roots-of c))
           (troot (and r (car r)))
           (tsock (and r (cadr r)))
           (left (entries (cadr c))))
      ;; drive.sh outlives the runner it started, so it does write a status:
      ;; 137, killed by signal 9.
      (want "CONTROL RS-5 the run started and its runner was killed by signal 9"
            (list started (integer? runner) (and (finished? c 30) (rc-of c)))
            '(#t #t 137))
      (want "RS-5 a killed run leaves exactly its two roots: one run-* entry in the base, and its socket root, marked"
            (and r (> (string-length tsock) 4)
                 (list (length left)
                       (and (pair? left) (starts-with? (car left) "run-"))
                       (equal? left (list (basename troot)))
                       (file-exists? (string-append troot "/probe-mark"))
                       (file-exists? (string-append tsock "/probe-mark"))))
            '(1 #t #t #t #t))
      (want "RS-5 and exactly one private snapshot directory, named ths-snap.<token>.*, mode 700"
            (and r (> (string-length (list-ref r 3)) 0)
                 (let ((ds (snap-dirs (list-ref r 3))))
                   (list (length ds)
                         (and (pair? ds) (file-directory? (car ds)))
                         (and (pair? ds) (mode-of (car ds))))))
            '(1 #t 700))
      ;; A SECOND RUN ON THE SAME BASE neither removes the killed run's roots
      ;; nor uses them.
      (let ((c2 (list (string-append root "/case" (number->string case-n) "b") (cadr c))))
        (sh "mkdir -p " (car c2))
        (for-each (lambda (f) (sh "cp " (car c) "/" f " " (car c2) "/" f))
                  '("run-fixtures.sh" "env.sh" "row-baseline-check.sh" "rows-baseline.txt"
                    "drive.sh" "ok.py" "probe-roots.sc"))
        (when (file-exists? (string-append (car c) "/launch.pl"))
          (sh "cp " (car c) "/launch.pl " (car c2) "/launch.pl"))
        (start! c2)
        (let* ((done (finished? c2 120))
               (r2 (roots-of c2)))
          (want "RS-5 a second run on the same base used other roots and left the killed run's three, marks intact"
                (and r (> (string-length tsock) 0)
                     (list done
                           (and r2 (not (equal? (car r2) troot)) (not (equal? (cadr r2) tsock)))
                           (file-exists? (string-append troot "/probe-mark"))
                           (file-exists? (string-append tsock "/probe-mark"))
                           (length (snap-dirs (list-ref r 3)))))
                '(#t #t #t #t 1))))
      ;; Tidy up what the killed run left, by the exact paths it reported.
      (when r
        (for-each (lambda (d)
                    (when (starts-with? d (string-append tmp-base "/ths-snap." (list-ref r 3) "."))
                      (sh "rm -rf " d)))
                  (snap-dirs (list-ref r 3))))
      (when (and r (starts-with? troot (string-append (cadr c) "/run-")))
        (sh "rm -rf " troot))
      (when (and r (starts-with? tsock "/tmp/ths.") (<= (string-length tsock) 20))
        (sh "rm -rf " tsock)))))

(printf "== the inner runs leave nothing behind ==\n")
;; A RUN THAT KEEPS ITS ROOTS SAYS WHERE THEY ARE: "NOT REMOVED: ...; the roots
;; are left for it: <run root> <socket root> <snapshot dir>". Rows whose inner
;; run keeps them by design (a census or a result that could not be read:
;; RS-11e, RS-11f) are tidied here from those exact paths. Each path is
;; checked against the three shapes a runner makes before anything is
;; removed. Found 09-23 by the code session: two sets left per runner-self run.
;; THE GUARD BELOW SEES ONLY TOKENS AN INNER RUN PRINTED on its "this run:"
;; line; a run that died before printing it is outside it. A before/after
;; listing of /tmp would not do better: another suite running at the same time
;; adds its own live roots there (measured 09-23 19:28, a concurrent suite13).
(define (inner-dirs)
  (filter (lambda (n) (starts-with? n "case")) (entries root)))
(define (words-of line)
  (let loop ((cs (string->list line)) (cur '()) (acc '()))
    (cond ((null? cs) (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
          ((char=? (car cs) #\space) (loop (cdr cs) '() (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
          (else (loop (cdr cs) (cons (car cs) cur) acc)))))
(define (strip-colon w)
  (if (and (> (string-length w) 0) (memv (string-ref w (- (string-length w) 1)) '(#\: #\; #\,)))
      (substring w 0 (- (string-length w) 1))
      w))
(define (runner-made? path)
  (or (and (starts-with? path (string-append root "/base")) (contains? path "/run-"))
      (and (starts-with? path "/tmp/ths.") (<= (string-length path) 20))
      (contains? path "/ths-snap.")))
(define (tokens-seen)
  (let loop ((ds (inner-dirs)) (acc '()))
    (if (null? ds) acc
        (loop (cdr ds)
              (append (filter (lambda (t) (> (string-length t) 0))
                              (map (lambda (l)
                                     (let ((ws (words-of l)))
                                       (if (and (>= (length ws) 4) (equal? (car ws) "this") (equal? (cadr ws) "run:")
                                                (equal? (caddr ws) "token"))
                                           (strip-colon (cadddr ws))
                                           "")))
                                   (lines-of (slurp (string-append root "/" (car ds) "/run.log")))))
                      acc)))))
(for-each
  (lambda (dname)
    (for-each
      (lambda (l)
        (when (starts-with? l "NOT REMOVED")
          (for-each (lambda (w)
                      (let ((pth (strip-colon w)))
                        (when (and (starts-with? pth "/") (runner-made? pth) (file-exists? pth))
                          (sh "chmod -R u+rwX " pth " 2>/dev/null; rm -rf " pth))))
                    (words-of l))))
      (lines-of (slurp (string-append root "/" dname "/run.log")))))
  (inner-dirs))
(let ((toks (tokens-seen)))
  (want "GUARD no inner run's token has a socket root or a snapshot directory left after the tidy"
        (filter (lambda (t)
                  (or (file-exists? (string-append "/tmp/ths." t))
                      (pair? (snap-dirs t))))
                toks)
        '()))

(printf "rows: ~a\n" rows)
(printf "~a failures\n" bad)
(sh "rm -rf " root)
(printf "runner-self complete\n")
