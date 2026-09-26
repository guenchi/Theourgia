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

;; The fixture launcher, test/launch.pl, given commands directly.
;;
;; The design (theourgos core/briefs/launcher-design.md, versions 2 to 4.1 and
;; its interface section) gives the launcher three processes: a watcher W that
;; is the process this file starts, an anchor A that leads the fixture's
;; group G, and the command C. W owns the time limit, the stop and the census,
;; and writes one result file:
;;   id ID / status N / cleanup clean|left|survivor|unknown / member ... /
;;   note ... / end
;; These rows give W commands a fixture could not easily be made to be: a
;; command that is not executable (126), one that does not exist (127), and
;; commands that signal their own group.
;;
;; Every process a row starts is ended by its pid before the row is judged,
;; and every one ends by itself within 90 s.

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
                 "/launch-self-" (number->string (get-process-id))))
(when (file-exists? root)
  (assertion-violation 'launch-self "scratch directory already exists" root))
(let loop ((i 0))
  (when (< i (string-length root))
    (let ((c (string-ref root i)))
      (unless (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\/ #\. #\- #\_)))
        (assertion-violation 'launch-self "the scratch root needs quoting" root)))
    (loop (+ i 1))))
(system (string-append "mkdir -p " root))

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
(define (pause-tenths n)
  (sleep (make-time 'time-duration (* (remainder n 10) 100000000) (quotient n 10))))
(define (await path secs)
  (let loop ((k 0))
    (cond ((file-exists? path) #t)
          ((> k (* 10 secs)) #f)
          (else (pause-tenths 1) (loop (+ k 1))))))
(define (alive? pid)
  (and (integer? pid) (> pid 1)
       (= 0 (system (string-append "kill -0 " (number->string pid) " 2>/dev/null")))))
(define (stop-pid! file)
  (let ((pid (read-datum file)))
    (when (and (integer? pid) (> pid 1))
      (sh "kill -9 " (number->string pid) " 2>/dev/null"))))
(define (now-ms) (let ((t (current-time))) (+ (* 1000 (time-second t)) (quotient (time-nanosecond t) 1000000))))

;; ---- the launcher --------------------------------------------------------------

(define launcher "launch.pl")
(define n 0)
;; A case directory, and the argument text that starts W in it. The command is
;; given as shell words already quoted by the row.
(define (case!)
  (set! n (+ n 1))
  (let ((d (string-append root "/c" (number->string n))))
    (sh "mkdir -p " d)
    d))
(define (getenv-or v d) (let ((x (getenv v))) (if (and x (> (string-length x) 0)) x d)))
;; THE LAUNCHER IS FOUND BESIDE THIS FILE: the runner runs fixtures with
;; test/ as the working directory, and so does a run by hand.
(define here (current-directory))
(define launcher-path (string-append here "/" launcher))
;; Runs W to its end in the foreground: `(status elapsed-ms)`.
(define (run-w d limit command)
  (let ((t0 (now-ms)))
    (sh "cd " d " && { perl " launcher-path
        " --limit " (number->string limit)
        " --out " d "/out --result " d "/result --id L" (number->string n)
        " -- " command " < /dev/null > " d "/w.log 2>&1; echo $? > " d "/w.rc; }")
    (list (read-datum (string-append d "/w.rc")) (- (now-ms) t0))))
;; Starts W in the background. W's own pid goes to w.pid from $!, never
;; from a pattern match: the shell around it carries the same words in its
;; command line. Its status goes to w.rc.
(define (start-w d limit command)
  (sh "cd " d " && { perl " launcher-path
      " --limit " (number->string limit)
      " --out " d "/out --result " d "/result --id L" (number->string n)
      " -- " command " < /dev/null > " d "/w.log 2>&1 & echo $! > " d "/w.pid; wait $!; echo $? > " d "/w.rc; }"
      " > /dev/null 2>&1 &"))
;; The result file as `(valid? id status cleanup members notes)`, or #f.
(define (result-of d)
  (let ((ls (lines-of (slurp (string-append d "/result")))))
    (and (pair? ls)
         (let ((field (lambda (k) (let ((l (find (lambda (x) (starts-with? x (string-append k " "))) ls)))
                                    (and l (substring l (+ 1 (string-length k)) (string-length l))))))
               (all (lambda (k) (map (lambda (x) (substring x (+ 1 (string-length k)) (string-length x)))
                                     (filter (lambda (x) (starts-with? x (string-append k " "))) ls)))))
           (list (and (equal? (car ls) (string-append "id L" (number->string n)))
                      (equal? (list-ref ls (- (length ls) 1)) "end"))
                 (field "id")
                 (let ((s (field "status"))) (and s (string->number s)))
                 (field "cleanup")
                 (all "member")
                 (all "note"))))))
(define (status-and-cleanup d)
  (let ((r (result-of d)))
    (and r (list (car r) (caddr r) (cadddr r)))))

(printf "== the launcher is there ==\n")
(want "CONTROL LS-0 test/launch.pl exists beside this file"
      (file-exists? launcher-path)
      #t)

(printf "== L3: status ==\n")
(let ((d (case!)))
  (let ((r (run-w d 30 "sh -c 'exit 7'")))
    (want "LS-1 an ordinary exit 7 is the status, and the group is clean"
          (list (car r) (status-and-cleanup d))
          '(7 (#t 7 "clean")))))
(let ((d (case!)))
  (let ((r (run-w d 30 "sh -c 'kill -TERM $$'")))
    (want "LS-2 death by TERM is 143"
          (list (car r) (status-and-cleanup d))
          '(143 (#t 143 "clean")))))
(let ((d (case!)))
  (let ((r (run-w d 30 (string-append d "/no-such-command"))))
    (want "LS-3 a command that does not exist is 127"
          (list (car r) (status-and-cleanup d))
          '(127 (#t 127 "clean")))))
(let ((d (case!)))
  (spit! (string-append d "/not-executable") "#!/bin/sh\nexit 0\n")
  (sh "chmod 644 " d "/not-executable")
  (let ((r (run-w d 30 (string-append d "/not-executable"))))
    (want "LS-4 a command that exists and cannot be executed is 126, not 127"
          (list (car r) (status-and-cleanup d))
          '(126 (#t 126 "clean")))))
(let ((d (case!)))
  (let ((r (run-w d 1 "sh -c 'sleep 30'")))
    (want "LS-5 over its limit the command is 142, the group is clean, and W returned within ten seconds"
          (list (car r) (status-and-cleanup d) (< (cadr r) 10000))
          '(142 (#t 142 "clean") #t))))

(printf "== L1: how the command starts ==\n")
;; W IS STARTED WITH TERM IGNORED AND BLOCKED, and with INT ignored, so the
;; command's clean state is W's doing and not the caller's.
;; CPYTHON INSTALLS ITS OWN default_int_handler FOR SIGINT whenever SIGINT is
;; at default when it starts (measured 09-23 by the code session and checked
;; here); that handler is therefore read as "default", and SIG_IGN is not.
(let ((d (case!)))
  (spit! (string-append d "/report.py")
         (string-append
           "import os, signal, stat\n"
           "names = ['SIGHUP','SIGINT','SIGQUIT','SIGTERM','SIGUSR1','SIGUSR2','SIGFPE','SIGALRM']\n"
           "def default(n):\n"
           "    h = signal.getsignal(getattr(signal, n))\n"
           "    return h == signal.SIG_DFL or (n == 'SIGINT' and h is signal.default_int_handler)\n"
           "odd = [n for n in names if not default(n)]\n"
           "mask = sorted(s.name for s in signal.pthread_sigmask(signal.SIG_BLOCK, []))\n"
           "leader = os.getpgrp() == os.getpid()\n"
           "try:\n"
           "    os.setsid(); ss = 'ok'\n"
           "except OSError as e:\n"
           "    ss = 'refused'\n"
           "st = os.fstat(0); dn = os.stat('/dev/null')\n"
           "null = stat.S_ISCHR(st.st_mode) and st.st_rdev == dn.st_rdev\n"
           "open('report.txt', 'w').write('(%s %s %s %s %s)' % ('(' + ' '.join(odd) + ')', '(' + ' '.join(mask) + ')',\n"
           "   '#t' if leader else '#f', ss, '#t' if null else '#f'))\n"))
  (sh "cd " d " && perl -e 'use POSIX; my $s = POSIX::SigSet->new(POSIX::SIGTERM()); "
      "POSIX::sigprocmask(POSIX::SIG_BLOCK(), $s); $SIG{TERM} = q(IGNORE); $SIG{INT} = q(IGNORE); exec @ARGV' "
      "perl " launcher-path " --limit 30 --out " d "/out --result " d "/result --id L" (number->string n)
      " -- python3 " d "/report.py < /dev/null > " d "/w.log 2>&1")
  (want "LS-6 the command has every checked signal at default, an empty mask, is not the group leader, may setsid, and reads /dev/null"
        (read-datum (string-append d "/report.txt"))
        '(() () #f ok #t)))

(printf "== L2': no signal to its own group ends the supervision ==\n")
(let ((d (case!)))
  (spit! (string-append d "/signaller.py")
         (string-append
           "import os, signal, time\n"
           "sigs = [signal.SIGTERM, signal.SIGINT, signal.SIGHUP, signal.SIGQUIT, signal.SIGUSR1]\n"
           "for s in sigs: signal.signal(s, signal.SIG_IGN)\n"
           "for s in sigs: os.killpg(os.getpgrp(), s)\n"
           "open('sent', 'w').write('1')\n"
           "k = 0\n"
           "while not os.path.exists('go') and k < 300:\n"
           "    time.sleep(0.1); k += 1\n"
           "raise SystemExit(5)\n"))
  (start-w d 60 (string-append "python3 " d "/signaller.py"))
  (let* ((sent (await (string-append d "/sent") 30))
         (w (begin (pause-tenths 5) (read-datum (string-append d "/w.pid"))))
         (w-alive (alive? w)))
    (spit! (string-append d "/go") "1")
    (await (string-append d "/w.rc") 30)
    (want "CONTROL LS-7 the command sent TERM, INT, HUP, QUIT and USR1 to its own group"
          sent
          #t)
    (want "LS-7 W was alive after all five, and the command's own status 5 came back"
          (list w-alive (read-datum (string-append d "/w.rc")) (status-and-cleanup d))
          '(#t 5 (#t 5 "clean")))))

(printf "== L4, L5: the group ends with its command ==\n")
(let ((d (case!)))
  (let ((r (run-w d 30 (string-append "sh -c 'sleep 60 & echo $! > " d "/d.pid; exit 0'"))))
    (let ((dp (read-datum (string-append d "/d.pid")))
          (res (result-of d)))
      (stop-pid! (string-append d "/d.pid"))
      ;; F95: ON RED THE LAUNCH'S OWN ACCOUNT GOES WITH THE READING -- its
      ;; result file and W's log, from this case's directory, which is
      ;; removed. (0 "unknown" #f #f) alone said the group was not read and
      ;; not why.
      (let ((got (list (car r)
                       (and res (cadddr res))
                       (and res (integer? dp)
                            (exists (lambda (m) (starts-with? m (string-append (number->string dp) " ")))
                                    (list-ref res 4)))
                       (alive? dp)))
            (expected '(0 "left" #t #f)))
        (want "LS-8 a descendant left in the group is reported as left, by pid, and is gone once W returns"
              (if (equal? got expected)
                  got
                  (append got (list (list 'inner
                                          (list 'result (slurp (string-append d "/result")))
                                          (list 'w-log (slurp (string-append d "/w.log")))))))
              expected)))))
(let ((d (case!)))
  (let ((r (run-w d 30 "sh -c '(sleep 1) & exit 0'")))
    (want "LS-9 a descendant that ends within the grace is not reported: clean"
          (list (car r) (status-and-cleanup d))
          '(0 (#t 0 "clean")))))
(let ((d (case!)))
  (let ((r (run-w d 1 (string-append "sh -c 'sleep 60 & echo $! > " d "/d.pid; sleep 60'"))))
    (let ((dp (read-datum (string-append d "/d.pid"))))
      (stop-pid! (string-append d "/d.pid"))
      (want "LS-10 over the limit, the command AND its descendant are gone once W returns"
            (list (car r) (alive? dp) (< (cadr r) 15000))
            '(142 #f #t)))))

(printf "== stop requests ==\n")
(let ((d (case!)))
  (start-w d 60 (string-append "sh -c 'sleep 60 & echo $! > " d "/d.pid; sleep 60'"))
  (let* ((up (await (string-append d "/d.pid") 30))
         (w (begin (pause-tenths 5) (read-datum (string-append d "/w.pid")))))
    (when (integer? w) (sh "kill -TERM " (number->string w)))
    (let* ((done (await (string-append d "/w.rc") 20))
           (dp (read-datum (string-append d "/d.pid"))))
      (stop-pid! (string-append d "/d.pid"))
      (want "LS-11 TERM to W stops its group: W exits 143, the group is clean, the descendant is gone"
            (list up (integer? w) done (read-datum (string-append d "/w.rc"))
                  (let ((r (result-of d))) (and r (cadddr r)))
                  (alive? dp))
            '(#t #t #t 143 "clean" #f)))))

(printf "== L6'': the census is validated ==\n")
;; A `ps` that prints nothing and exits 0, as Apple's ps can when it cannot
;; read the process table: the census is not valid, so cleanup is unknown.
(let* ((d (case!))
       (shim (string-append d "/shim")))
  (sh "mkdir -p " shim)
  (spit! (string-append shim "/ps") "#!/bin/sh\nexit 0\n")
  (sh "chmod +x " shim "/ps")
  ;; The shim must reach W, not only the command, so W is started with it.
  (sh "cd " d " && PATH=" shim ":$PATH perl " launcher-path " --limit 30 --out " d "/out --result "
      d "/result2 --id L" (number->string n) " -- sh -c 'exit 0' < /dev/null > " d "/w2.log 2>&1")
  (want "LS-12 a census that is empty (ps printing nothing, exit 0) is unknown, never clean"
        (let ((ls (lines-of (slurp (string-append d "/result2")))))
          (find (lambda (l) (starts-with? l "cleanup ")) ls))
        "cleanup unknown"))
(let* ((d (case!))
       (shim (string-append d "/shim")))
  (sh "mkdir -p " shim)
  (spit! (string-append shim "/ps") "#!/bin/sh\nsleep 100\n")
  (sh "chmod +x " shim "/ps")
  (let ((t0 (now-ms)))
    (sh "cd " d " && PATH=" shim ":$PATH perl " launcher-path " --limit 30 --out " d "/out --result "
        d "/result --id L" (number->string n) " -- sh -c 'exit 0' < /dev/null > " d "/w.log 2>&1")
    (let ((elapsed (- (now-ms) t0)))
      (sh "pkill -f '" shim "/ps' 2>/dev/null; true")
      (want "LS-13 a census that hangs is unknown, and W returns within sixty seconds"
            (list (let ((ls (lines-of (slurp (string-append d "/result")))))
                    (find (lambda (l) (starts-with? l "cleanup ")) ls))
                  (< elapsed 60000))
            '("cleanup unknown" #t)))))

(printf "rows: ~a\n" rows)
(printf "~a failures\n" bad)
(sh "rm -rf " root)
(printf "launch-self complete\n")
