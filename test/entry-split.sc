;; Copyright 2018 - 2026 The Theourgia Authors
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

;; Three entry programs, by role: theourgia.sc the thin client,
;; theourgiad.sc the daemon, core.sc the in-process route. No verb's
;; answer changes: the rows below compare bytes and exit codes with what
;; the old single program answered on a338bcd, recorded before any change
;; (the oracle strings here), with THEOURGIA_HOME and THEOURGIA_RUN fixed
;; per row so the per-run paths in the machine-home line are the same on
;; both sides. The old name is not spelled in this file: OLDNAME-1 greps for it.
(import (chezscheme) (only (theourgia extensions) extension-verbs))

(define bad 0)
(define rows 0)
(define (want-1 label got expect)
  (set! rows (+ rows 1))
  (if (equal? got expect)
      (printf "ok   ~a -> ~s\n" label got)
      (begin (set! bad (+ bad 1))
             (printf "FAIL ~a -> ~s   WANT ~s\n" label got expect))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define pid-text (number->string (get-process-id)))
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define here (string-append scratch-base "/entry-split-" pid-text))
(when (file-exists? here)
  (assertion-violation 'entry-split "scratch directory already exists" here))
(system (string-append "mkdir -p " here))
(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
;; A SOCKET PATH MUST BE SHORT, so the run root goes under the runner's
;; socket root when there is one.
(define sock-base
  (let ((v (getenv "THEOURGIA_TEST_SOCK")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

(define (file-text path)
  (if (not (file-exists? path))
      ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))
(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (program-exists? name) (file-exists? (string-append "../" name)))

;; ONE DIRECTORY PER ROW, and both programs of a comparison run against it in
;; turn: home, run and store are fixed so the machine-home line and the socket
;; path are the same bytes on both sides. cwd is the row's directory and stdin
;; is /dev/null, as the oracle was recorded.
(define row-n 0)
;; THE CLIENT LIBRARY'S START, for the rows about it: ensure-daemon! driven
;; as a program taking the thin client's command line (start-driver.ss).
(include "start-driver.ss")
(define start-driver (begin (system (string-append "mkdir -p " here)) (start-driver! here)))
(define (row-dir!)
  (set! row-n (+ row-n 1))
  (let ((d (string-append here "/r" (number->string row-n))))
    (system (string-append "mkdir -p " d "/home " d "/cwd"))
    d))
(define programs-root (string-append (current-directory) "/.."))
;; EXTRA ENVIRONMENT FOR ONE RUN, as "NAME=value " text; empty otherwise.
(define run-env (make-parameter ""))
(define (run-in d program argv . alarm)
  (let ((out (string-append d "/out")) (err (string-append d "/err")) (rc (string-append d "/rc")))
    (system (string-append
              "cd " d "/cwd && " (run-env) "HOME=" d "/home THEOURGIA_HOME=" d "/home"
              " THEOURGIA_RUN=" sock-base "/es-" pid-text "-" (number->string row-n)
              " CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' "
              (if (pair? alarm) (string-append "perl -e 'alarm " (number->string (car alarm)) "; exec @ARGV' ") "")
              "scheme --script " (if (char=? (string-ref program 0) #\/) program (string-append programs-root "/" program)) " " argv
              " < /dev/null > " out " 2> " err "; echo $? > " rc))
    (list (string->number (let ((t (file-text rc))) (substring t 0 (max 0 (- (string-length t) 1)))))
          (file-text out) (file-text err))))
(define (rc-of r) (car r))
(define (out-of r) (cadr r))
(define (err-of r) (caddr r))
(define (machine-home-line d) (string-append "(theourgia machine-home " d "/home)\n"))

;; ---- ORACLE: core.sc answers as the old program did on a338bcd (oracle bytes) -
(printf "== ORACLE: core.sc, the oracle rows ==\n")
;; The built-in verbs as the old program listed them, then the verbs registered
;; from outside the core table, taken from their registry's own data.
(define verbs-line
  (string-append
    "(verbs describe init insert set move del link unlink relation rule derive write restore commit drafts diagnostics discard batch split-suggest import-code export-code supply def import-md export-md adopt check snapshot publish outline read refs reach whereis search grep log tag diff locate position changed-files conflicts subscribe review-results collect-into"
    (apply string-append (map (lambda (e) (string-append " " (symbol->string (car e)))) extension-verbs))
    ")"))
(let ((d (row-dir!)))
  (want "ORACLE-1 core.sc with an empty argv prints the usage line on stdout, the machine-home line on stderr, exit 1"
        (let ((r (run-in d "core.sc" "")))
          (list (rc-of r) (out-of r) (string=? (err-of r) (machine-home-line d))))
        (list 1 "(usage (theourgia <verb> ...))\n" #t)))
(let ((d (row-dir!)))
  (want "ORACLE-2 core.sc with an unknown verb answers unknown-verb with the verb list, exit 1"
        (let ((r (run-in d "core.sc" "serv --store d")))
          (list (rc-of r) (out-of r) (string=? (err-of r) (machine-home-line d))))
        (list 1 (string-append "(error unknown-verb (spelling \"serv\") " verbs-line ")\n") #t)))

;; ---- NOSERVE: core.sc no longer serves --------------------------------------
(printf "== NOSERVE: core.sc has no serve role ==\n")
;; On the tree before the split this row serves in the foreground until the
;; alarm (exit 142); after it, serve is a verb core.sc does not know.
(let ((d (row-dir!)))
  (system (string-append "mkdir -p " d "/cwd/d"))
  (run-in d "core.sc" "init --store d")
  (want "NOSERVE-1 core.sc serve does not start a daemon: the unknown-verb answer, exit 1, and no socket under the run root"
        (let* ((r (run-in d "core.sc" "serve d" 8))
               (sockets (string-append d "/sockets")))
          (system (string-append "find " sock-base "/es-" pid-text "-" (number->string row-n) " -name socket 2>/dev/null | wc -l | tr -d ' ' > " sockets))
          (list (rc-of r)
                (cond ((contains? (out-of r) "(error unknown-verb (spelling \"serve\")") 'unknown-verb)
                      ((contains? (out-of r) "(serving") 'served)
                      (else (list 'said (out-of r))))
                (let ((t (file-text sockets))) (substring t 0 (max 0 (- (string-length t) 1))))))
        (list 1 'unknown-verb "0")))

;; ---- DVERB and DSERVE: theourgiad.sc -----------------------------------------
(printf "== DVERB / DSERVE: theourgiad.sc, the daemon program ==\n")
(define (theourgiad-row label argv expect-rc expect-out . alarm)
  (let ((d (row-dir!)))
    (want label
          (if (not (program-exists? "theourgiad.sc"))
              'no-such-program
              (let ((r (apply run-in d "theourgiad.sc" argv alarm)))
                (list (rc-of r) (out-of r) (string=? (err-of r) (machine-home-line d)))))
          (list expect-rc expect-out #t))))
(theourgiad-row "DVERB-1 theourgiad.sc with a verb that is not serve answers bad-request not-a-daemon-verb with the serve usage, exit 1"
                "init --store d" 1
                "(error bad-request (reason not-a-daemon-verb) (usage (serve (<store>) (\"--socket\" <path>) (\"--detach\" \"--log\" <path> (started-by-a-client-not-by-hand)) (\"--attempt\" <token> (started-by-a-client-not-by-hand)) (\"--session-size\" <entries>) (\"--idle-exit\" <seconds>) (\"--idle-grace-ms\" <milliseconds>))))\n")
(let ((d (string-append here "/r" (number->string row-n))))
  (want "DVERB-2 TWIN: and it created nothing under the store it was named"
        (if (program-exists? "theourgiad.sc") (file-exists? (string-append d "/cwd/d")) 'no-such-program)
        #f))
(theourgiad-row "DSERVE-1 theourgiad.sc serve with an empty --socket answers bad-socket-path, exit 2"
                "serve d --socket \"\"" 2 "(error bad-socket-path (reason empty))\n")
(theourgiad-row "DSERVE-2 theourgiad.sc serve --detach without --log answers detach-needs-a-log with the usage, exit 71"
                "serve d --detach" 71
                "(error detach-needs-a-log (usage (serve (<store>) (\"--socket\" <path>) (\"--detach\" \"--log\" <path> (started-by-a-client-not-by-hand)) (\"--attempt\" <token> (started-by-a-client-not-by-hand)) (\"--session-size\" <entries>) (\"--idle-exit\" <seconds>) (\"--idle-grace-ms\" <milliseconds>))))\n")
(let ((d (row-dir!)))
  (want "DSERVE-3 theourgiad.sc serve on an absent store answers store-not-found, exit 75, and makes nothing: no run directory, no lock, no exiting line"
        (if (not (program-exists? "theourgiad.sc"))
            'no-such-program
            ;; NOTHING IS MADE: serve asks whether the store is there before the
            ;; run directory and the lock, so the refusal carries no (written
            ;; ...) clause and no (exiting ...) line follows it. The report ends
            ;; with the attempt clause (#f: started by hand).
            ;; THE WHOLE STDOUT IS THE ONE REFUSAL, and the run root is not
            ;; there at all (not merely empty).
            (let* ((r (run-in d "theourgiad.sc" "serve d" 8))
                   (run (string-append sock-base "/es-" pid-text "-" (number->string row-n))))
              (list (rc-of r) (out-of r) (file-exists? run))))
        (list 75 "(error store-not-found (store \"d\") (attempt #f))\n" #f)))
;; AN OPTION serve DOES NOT KNOW IS REFUSED, as every other verb
;; refuses one; before it, `serve d --bogus` served in the foreground.
(let ((d (row-dir!)))
  (system (string-append "mkdir -p " d "/cwd/d"))
  (run-in d "core.sc" "init --store d")
  (want "DSERVE-4 TWIN: serve with an option it does not know answers bad-request unknown-option naming it, with the usage, exit 1, and leaves no socket"
        (if (not (program-exists? "theourgiad.sc"))
            'no-such-program
            (let* ((r (run-in d "theourgiad.sc" "serve d --bogus" 8))
                   (a (guard (e (#t #f)) (call-with-port (open-string-input-port (out-of r)) read)))
                   (sockets (string-append d "/sockets")))
              (system (string-append "find " sock-base "/es-" pid-text "-" (number->string row-n) " -name socket 2>/dev/null | wc -l | tr -d ' ' > " sockets))
              (list (rc-of r)
                    (and (pair? a) (list? a) (pair? (cdr a)) (list (car a) (cadr a)))
                    (let ((f (and (pair? a) (list? a) (find (lambda (x) (and (pair? x) (eq? (car x) 'reason))) (cdr a))))) (and f (cadr f)))
                    (let ((f (and (pair? a) (list? a) (find (lambda (x) (and (pair? x) (eq? (car x) 'option))) (cdr a))))) (and f (cadr f)))
                    (and (pair? a) (list? a) (and (find (lambda (x) (and (pair? x) (eq? (car x) 'usage))) (cdr a)) #t))
                    (let ((t (file-text sockets))) (substring t 0 (max 0 (- (string-length t) 1)))))))
        (list 1 '(error bad-request) 'unknown-option "--bogus" #t "0")))

;; A SOCKET UNDER A DIRECTORY THAT DOES NOT EXIST IS REFUSED, not created
;; (as an export's target is: refuse, do not create). Before it, serve made the whole
;; chain and served.
(let ((d (row-dir!)))
  (system (string-append "mkdir -p " d "/cwd/d"))
  (run-in d "core.sc" "init --store d")
  (want "DSERVE-5 TWIN: serve with a socket under a directory that does not exist answers socket-dir-missing naming the directory, exit 2, and creates nothing"
        (if (not (program-exists? "theourgiad.sc"))
            'no-such-program
            (let* ((absent (string-append sock-base "/es-absent-" pid-text "-" (number->string row-n)))
                   (r (run-in d "theourgiad.sc" (string-append "serve d --socket " absent "/deep/s.sock") 8))
                   (a (guard (e (#t #f)) (call-with-port (open-string-input-port (out-of r)) read))))
              (list (rc-of r)
                    (and (pair? a) (list? a) (pair? (cdr a)) (list (car a) (cadr a)))
                    (let ((f (and (pair? a) (list? a) (find (lambda (x) (and (pair? x) (eq? (car x) 'dir))) (cdr a))))) (and f (equal? (cadr f) (string-append absent "/deep"))))
                    (if (file-exists? absent) 'created 'nothing-created))))
        (list 2 '(error socket-dir-missing) #t 'nothing-created)))

;; A DIRECTORY THAT CANNOT BE SEARCHED IS NOT "MISSING" (ruling c). The
;; socket's directory sits under one at mode 000: its type cannot be asked,
;; and the refusal names it as unreadable, the way R1 names a path it could
;; not look at -- not socket-dir-missing, which would send the caller looking
;; for a directory that is there.
(let* ((d (row-dir!))
       (sealed (string-append sock-base "/es-sealed-" pid-text "-" (number->string row-n)))
       (inner (string-append sealed "/inner")))
  (system (string-append "mkdir -p " d "/cwd/d " inner))
  (run-in d "core.sc" "init --store d")
  (system (string-append "chmod 000 " sealed))
  (let* ((r (run-in d "theourgiad.sc" (string-append "serve d --socket " inner "/s.sock") 8))
         (a (guard (e (#t #f)) (call-with-port (open-string-input-port (out-of r)) read))))
    (system (string-append "chmod 700 " sealed))
    (want "SOCKDIR-1 TWIN: serve with a socket whose directory is under one at 000 answers unreadable naming that directory, exit 2, not socket-dir-missing"
          (if (not (program-exists? "theourgiad.sc"))
              'no-such-program
              (list (rc-of r)
                    (and (pair? a) (list? a) (pair? (cdr a)) (list (car a) (cadr a)))
                    (let ((f (and (pair? a) (list? a) (find (lambda (x) (and (pair? x) (eq? (car x) 'path))) (cdr a))))) (and f (string? (cadr f)) (contains? (cadr f) inner)))))
          (list 2 '(error unreadable) #t))
    (system (string-append "rm -rf " sealed))))

;; THE REFUSALS COME BEFORE THE DAEMON LIBRARY LOADS, as they did in the
;; base: igropyr reads IGROPYR_INJECT when a library that uses injection is
;; expanded, so a program that imported (theourgia daemon) statically would
;; print the injection banner, or raise on a bad value, before answering an
;; argument error the base answered first (review r1, measured on c2-r1).
(let ((d (row-dir!)))
  (want "DSERVE-6 TWIN: under IGROPYR_INJECT=bad the empty-socket refusal is the base's bytes and exit 2: the daemon library is not loaded before the checks"
        (if (not (program-exists? "theourgiad.sc"))
            'no-such-program
            (let ((r (parameterize ((run-env "IGROPYR_INJECT=bad ")) (run-in d "theourgiad.sc" "serve d --socket \"\""))))
              (list (rc-of r) (out-of r) (string=? (err-of r) (machine-home-line d)))))
        (list 2 "(error bad-socket-path (reason empty))\n" #t)))
(let ((d (row-dir!)))
  (want "DSERVE-7 TWIN: under IGROPYR_INJECT=on the same refusal carries no injection banner on stderr, as the base"
        (if (not (program-exists? "theourgiad.sc"))
            'no-such-program
            (let ((r (parameterize ((run-env "IGROPYR_INJECT=on ")) (run-in d "theourgiad.sc" "serve d --socket \"\""))))
              (list (rc-of r) (out-of r) (string=? (err-of r) (machine-home-line d)))))
        (list 2 "(error bad-socket-path (reason empty))\n" #t)))

;; ---- START: the daemon the thin client starts is theourgiad.sc ---------------
(printf "== START: the thin client starts theourgiad.sc ==\n")
(define (daemon-lines store)
  ;; ps into a file, then filter in here: the shell that runs ps carries no
  ;; store path, so the listing cannot match this fixture's own command.
  (let ((f (string-append here "/ps.txt")))
    (system (string-append "ps -axo pid,command > " f))
    (filter (lambda (l) (and (contains? l "serve") (contains? l store)))
            (let loop ((t (file-text f)) (acc '()))
              (let ((i (let find ((k 0)) (cond ((>= k (string-length t)) #f)
                                                 ((char=? (string-ref t k) #\newline) k)
                                                 (else (find (+ k 1)))))))
                (if i (loop (substring t (+ i 1) (string-length t)) (cons (substring t 0 i) acc))
                    (reverse acc)))))))
(define (pid-of line)
  (let loop ((i 0)) (if (char=? (string-ref line i) #\space) (loop (+ i 1))
                        (let end ((j i)) (if (and (< j (string-length line)) (char-numeric? (string-ref line j))) (end (+ j 1))
                                             (string->number (substring line i j)))))))
(let* ((d (row-dir!))
       (store (string-append d "/cwd/s")))
  (system (string-append "mkdir -p " store))
  (run-in d "theourgia.sc" (string-append "init --store " store))
  ;; THE TWIN FIRST: a plain command with no daemon answers in process and
  ;; starts nothing. The start below is the client library's (start-driver.ss).
  (let ((plain (run-in d "theourgia.sc" (string-append "outline --store " store " --wire") 30)))
    (want "START-0 the thin client with no daemon answers ok in process and starts no daemon"
          (list (rc-of plain) (and (> (string-length (out-of plain)) 4) (substring (out-of plain) 0 4)) (length (daemon-lines store)))
          '(0 "(ok " 0)))
  (let* ((forwarded (run-in d start-driver (string-append "outline --store " store " --wire") 30))
         (lines (daemon-lines store))
         (pids (map pid-of lines)))
    (want "CONTROL START the client library's start answered ok and one daemon is up for the store"
          (list (rc-of forwarded) (and (> (string-length (out-of forwarded)) 4) (substring (out-of forwarded) 0 4)) (length lines))
          '(0 "(ok " 1))
    ;; (the request goes --wire on both sides, so the bytes compared below are
    ;; the datum, not the rendering)
    (want "START-1 the daemon's command line names theourgiad.sc and not core.sc"
          (if (null? lines) 'no-daemon
              (list (and (contains? (car lines) "theourgiad.sc") #t) (and (contains? (car lines) "core.sc") #t)))
          '(#t #f))
    ;; ---- LOCAL while the daemon is up: the local route still answers in process
    (let* ((local (run-in d "core.sc" (string-append "outline --store " store) 30))
           (d2 d))
      (want "LOCAL-1 with a socket present, THEOURGIA_LOCAL=1 core.sc answers in process and byte for byte the daemon's answer"
            (let ((f (string-append d "/local.out")))
              (system (string-append "cd " d "/cwd && HOME=" d "/home THEOURGIA_HOME=" d "/home THEOURGIA_RUN=" sock-base "/es-" pid-text "-" (number->string row-n)
                                     " CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 scheme --script " programs-root "/core.sc outline --store " store " --wire < /dev/null > " f " 2>/dev/null"))
              (let ((l (file-text f)))
                (list (length (daemon-lines store)) (if (string=? l (out-of forwarded)) 'identical (list 'differ l (out-of forwarded))))))
            '(1 identical)))
    ;; stop the daemon by pid, and wait for it to go
    (for-each (lambda (p) (system (string-append "kill -TERM " (number->string p) " 2>/dev/null"))) pids)
    (let wait ((n 0))
      (when (and (< n 100) (not (null? (daemon-lines store))))
        (system "sleep 0.1") (wait (+ n 1))))
    (want "START-2 TWIN: the daemon stopped on TERM and left no process for the store"
          (length (daemon-lines store))
          0)))

;; THE CLIENT'S OWN SPAWN IS NEVER REFUSED BY THE DAEMON'S OPTION TABLE (found
;; in review, ruled). A store whose name starts with "--" reached the daemon as
;; a bare positional, which serve now refuses as an unknown option; the
;; spawn names it with --store, so the daemon gets the store the client
;; means. The store is relative, in the row's cwd, and carries this run's
;; pid so the ps filter finds only this row's daemon.
(let* ((d (row-dir!))
       (store (string-append "--dash-" pid-text)))
  (system (string-append "mkdir -p " d "/cwd/" store))
  (run-in d "theourgia.sc" (string-append "init --store " store))
  (let ((plain (run-in d "theourgia.sc" (string-append "outline --store " store " --wire") 30)))
    (want "DSERVE-8 the thin client with a store spelled --dash-<pid> and no daemon answers ok in process and starts none"
          (list (rc-of plain) (and (> (string-length (out-of plain)) 4) (substring (out-of plain) 0 4)) (length (daemon-lines store)))
          '(0 "(ok " 0)))
  (let* ((forwarded (run-in d start-driver (string-append "outline --store " store " --wire") 30))
         (lines (daemon-lines store))
         (pids (map pid-of lines)))
    (want "DSERVE-8 TWIN: the client library's start with a store spelled --dash-<pid> starts its daemon and is answered ok"
          (list (rc-of forwarded) (and (> (string-length (out-of forwarded)) 4) (substring (out-of forwarded) 0 4)) (length lines))
          '(0 "(ok " 1))
    (for-each (lambda (p) (system (string-append "kill -TERM " (number->string p) " 2>/dev/null"))) pids)
    (let wait ((n 0))
      (when (and (< n 100) (not (null? (daemon-lines store))))
        (system "sleep 0.1") (wait (+ n 1))))))

;; ---- EVAL: eval through core.sc ---------------------------------------------
(printf "== EVAL: eval through core.sc ==\n")
(let* ((d (row-dir!))
       (store (string-append d "/cwd/s")))
  (system (string-append "mkdir -p " store))
  (run-in d "core.sc" (string-append "init --store " store))
  (want "EVAL-1 core.sc eval answers ok with the values and the working-view clause"
        (let* ((r (run-in d "core.sc" (string-append "eval '(+ 1 2)' --store " store " --wire") 30))
               (a (guard (e (#t #f)) (call-with-port (open-string-input-port (out-of r)) read))))
          (list (rc-of r) (and (pair? a) (car a)) (and (pair? a) (list? a) (assq 'values (cdr a)))
                (and (pair? a) (list? a) (pair? (assq 'working-view (cdr a))))))
        '(0 ok (values (3)) #t)))

;; ---- OLDNAME: no source, script or README line spells the old name -----------
(printf "== OLDNAME: the old name is gone from the tree ==\n")
;; The needle is assembled so that this file does not match itself.
(let ((needle (string-append "cli" ".sc"))
      (f (string-append here "/hits.txt")))
  ;; THE REACH IS SOURCES, SCRIPTS AND DOCUMENTS: build caches (__pycache__,
  ;; compiled objects) are outputs of those files and are not read. A stale
  ;; .pyc of paths.py, compiled before the rename, was the reading that
  ;; drew this line.
  (system (string-append "cd " programs-root " && grep -rl --exclude-dir=.git --exclude-dir=build --exclude-dir=__pycache__ --exclude='*.so' -F '" needle "' . 2>/dev/null | sort > " f))
  (want "OLDNAME-1 no file in the tree spells the old program name"
        (let ((t (file-text f))) (if (string=? t "") 'none (list 'spelled-in t)))
        'none))

;; ---- BUILD (tripwire): build.ss copies the three programs ---------------------
(printf "== BUILD: build.ss's programs ==\n")
;; TODAY A TRIPWIRE, NOT A MEASUREMENT: it reads build.ss's list, not an
;; output directory. The copy itself is exercised by f0-ondemand's OUTDIR rows.
(want "BUILD-1 build.ss's programs list names theourgia.sc, core.sc, theourgiad.sc and eval-worker.sc"
      (let ((t (file-text (string-append programs-root "/build.ss"))))
        (map (lambda (n) (and (contains? t (string-append "\"" n "\"")) n))
             '("theourgia.sc" "core.sc" "theourgiad.sc" "eval-worker.sc")))
      '("theourgia.sc" "core.sc" "theourgiad.sc" "eval-worker.sc"))

;; ---- SPAWNS (tripwire): every daemon spawn in the fixtures names theourgiad.sc --
(printf "== SPAWNS: the daemon spawns in the fixtures ==\n")
;; TODAY A TRIPWIRE, NOT A MEASUREMENT: it reads the fixtures' source, not
;; the processes they start. It exists because the split's first gate went
;; red on two fixtures that spawned `core.sc serve` through a variable
;; (client-start.sc, envelope.sc: `(define cli ...)` and the word `serve`
;; beside it), which no grep for "core.sc serve" could see.
;;
;; WHAT IT CALLS A SPAWN: a list whose DIRECT elements include a string
;; holding `--script` and a string holding the word `serve` as a token
;; (split on blanks and double quotes). That list names the daemon program
;; if a direct element is a string holding "theourgiad.sc", a symbol whose
;; file-level `define` holds that string, or a subform holding it.
;;
;; NOTE: ITS REACH, STATED. It reads `.sc` files under test/ as data, so a
;; spawn assembled across several forms (a helper that adds `--script`
;; itself, as this file's `run-in` does) is not seen, and this file is left
;; out: its own spawns go through `run-in`. `.py` and `.sh` fixtures are not
;; read; none spawned `serve` when this row was written.
(define (serve-token? str)
  (let loop ((cs (string->list str)) (cur '()) (toks '()))
    (define (flush) (if (null? cur) toks (cons (list->string (reverse cur)) toks)))
    (cond ((null? cs) (and (member "serve" (flush)) #t))
          ((memv (car cs) '(#\space #\tab #\newline #\")) (loop (cdr cs) '() (flush)))
          (else (loop (cdr cs) (cons (car cs) cur) toks)))))
(define (strings-in x)
  (cond ((string? x) (list x))
        ((pair? x) (append (strings-in (car x)) (strings-in (cdr x))))
        ((vector? x) (strings-in (vector->list x)))
        (else '())))
;; A program is started by its object (`--program theourgiad.so`) when a
;; build left one, and by its source otherwise: both name the daemon.
(define (names-daemon-text? str) (or (contains? str "theourgiad.sc") (contains? str "theourgiad.so")))
(define (defines-of forms)
  (let loop ((xs forms) (acc '()))
    (cond ((null? xs) acc)
          ((and (pair? (car xs)) (eq? (caar xs) 'define) (pair? (cdar xs))
                (symbol? (cadar xs)) (pair? (cddar xs)))
           (loop (cdr xs) (cons (cons (cadar xs) (strings-in (caddar xs))) acc)))
          (else (loop (cdr xs) acc)))))
(define (spawn-form? x)
  (and (list? x)
       (let ((strs (filter string? x)))
         (and (exists (lambda (s) (or (contains? s "--script") (contains? s "--program"))) strs)
              (exists serve-token? strs)))))
(define (names-daemon? form defs)
  (exists (lambda (e)
            (cond ((string? e) (names-daemon-text? e))
                  ((symbol? e) (let ((d (assq e defs)))
                                 (and d (exists names-daemon-text? (cdr d)))))
                  ((pair? e) (exists names-daemon-text? (strings-in e)))
                  (else #f)))
          form))
(define (spawns-in x)
  (cond ((not (pair? x)) '())
        (else (append (if (spawn-form? x) (list x) '())
                      (if (list? x) (apply append (map spawns-in x)) '())))))
(define (read-all path)
  (guard (e (#t 'unreadable))
    (call-with-input-file path
      (lambda (p) (let loop ((acc '()))
                    (let ((x (read p))) (if (eof-object? x) (reverse acc) (loop (cons x acc)))))))))
(define (spawn-report file forms)
  (let ((defs (defines-of forms)))
    (map (lambda (f) (list file (names-daemon? f defs))) (apply append (map spawns-in forms)))))
(define fixture-files
  (list-sort string<?
    (filter (lambda (n) (and (> (string-length n) 3)
                             (string=? (substring n (- (string-length n) 3) (string-length n)) ".sc")
                             (not (string=? n "entry-split.sc"))))
            (directory-list (string-append programs-root "/test")))))
(define spawn-readings
  (apply append
    (map (lambda (n)
           (let ((forms (read-all (string-append programs-root "/test/" n))))
             (if (eq? forms 'unreadable) (list (list n 'unreadable)) (spawn-report n forms))))
         fixture-files)))
;; CONTROL: the checker, handed the shape that went red, says so. The forms
;; are built here rather than written out, so this file holds no spawn.
(want "CONTROL SPAWNS a spawn naming core.sc through a variable is reported; one naming theourgiad.sc is not"
      (let ((flag (string-append "--" "script")) (word (string-append "ser" "ve")))
        (list (spawn-report "made" (list (list 'define 'prog "../core.sc")
                                         (list 'string-append (string-append "scheme " flag " ") 'prog
                                               (string-append " " word " ") 'store)))
              (spawn-report "made" (list (list 'define 'prog "../theourgiad.sc")
                                         (list 'list "scheme" flag 'prog word 'store)))))
      '((("made" #f)) (("made" #t))))
(want "CONTROL OBJSPAWN a program-object spawn naming core.so through a variable is reported; one naming theourgiad.so is not"
      (let ((flag (string-append "--" "program")) (word (string-append "ser" "ve")))
        (list (spawn-report "made" (list (list 'define 'prog "../core.so")
                                         (list 'string-append (string-append "scheme " flag " ") 'prog
                                               (string-append " " word " ") 'store)))
              (spawn-report "made" (list (list 'define 'prog "../theourgiad.so")
                                         (list 'list "scheme" flag 'prog word 'store)))))
      '((("made" #f)) (("made" #t))))
(want "SPAWNS-1 the reading finds the spawns that went red, and the ones beside them"
      (map (lambda (n) (and (assoc n spawn-readings) n))
           '("client-start.sc" "detach.sc" "envelope.sc" "f0-ondemand.sc"))
      '("client-start.sc" "detach.sc" "envelope.sc" "f0-ondemand.sc"))
(want "SPAWNS-2 TRIPWIRE: every daemon spawn read in a fixture names theourgiad.sc, and every fixture could be read"
      (filter (lambda (r) (not (eq? (cadr r) #t))) spawn-readings)
      '())
(printf "SPAWNS reading: ~a spawn forms in ~a fixture files\n"
        (length spawn-readings)
        (length (fold-left (lambda (acc r) (if (member (car r) acc) acc (cons (car r) acc))) '() spawn-readings)))

(printf "== PROGNAMES: no program defines a name it imports ==\n")
;; A PROGRAM COMPILED BY build.ss MAY NOT DEFINE A NAME IT IMPORTS. Run as a
;; script, a top-level `define` of an imported name quietly replaces it; as
;; a compiled program it is "multiple definitions" and the build stops --
;; eval-worker.sc and eval-runner-exec.sc each defined `protocol`, which
;; (chezscheme) exports. Each program's imported names are read the way the
;; expander reads them, by building the environment of each import spec;
;; its own names are what its top-level forms define: define, define-syntax,
;; define-values, every name a define-record-type makes (the type, the
;; constructor, the predicate and each field's accessor and mutator), and
;; the same inside a top-level begin. A name a macro use introduces is not
;; read. The second check is the other half of the same class: the import
;; specs together must not bring one name with two bindings (eval-worker.sc
;; imported eval-context's display beside Chez's). It is read by compiling,
;; as a program, the program's import form alone, which is the refusal the
;; build meets. NEVER: NOT BY `environment`: given the same specs it takes
;; one of the two bindings and refuses nothing -- measured, the twin below
;; read #f that way.
(define six-programs '("theourgia.sc" "core.sc" "theourgiad.sc" "eval-worker.sc" "eval-runner-exec.sc" "mcp/server.sc"))
(define (program-forms rel) (read-all (string-append programs-root "/" rel)))
(define (imported-names forms)
  (let ((imp (find (lambda (f) (and (pair? f) (eq? (car f) 'import))) forms))
        (seen (make-eq-hashtable)))
    (when imp
      (for-each (lambda (spec)
                  (for-each (lambda (n) (hashtable-set! seen n #t)) (environment-symbols (environment spec))))
                (cdr imp)))
    seen))
;; the symbols of a formals list, proper or not
(define (formal-names x)
  (cond ((symbol? x) (list x)) ((pair? x) (append (formal-names (car x)) (formal-names (cdr x)))) (else '())))
(define (sym . parts) (string->symbol (apply string-append (map (lambda (p) (if (symbol? p) (symbol->string p) p)) parts))))
;; every name (define-record-type SPEC CLAUSE ...) binds
(define (record-names spec clauses)
  (let* ((type (if (pair? spec) (car spec) spec))
         (ctor (if (and (pair? spec) (pair? (cdr spec))) (cadr spec) (sym "make-" type)))
         (pred (if (and (pair? spec) (pair? (cdr spec)) (pair? (cddr spec))) (caddr spec) (sym type "?")))
         (fields (let ((c (find (lambda (c) (and (pair? c) (eq? (car c) 'fields))) clauses))) (if c (cdr c) '()))))
    (filter symbol?
      (append (list type ctor pred)
              (apply append
                (map (lambda (f)
                       (cond ((symbol? f) (list (sym type "-" f)))
                             ((and (pair? f) (eq? (car f) 'immutable))
                              (if (pair? (cddr f)) (list (caddr f)) (list (sym type "-" (cadr f)))))
                             ((and (pair? f) (eq? (car f) 'mutable))
                              (if (pair? (cddr f)) (cddr f) (list (sym type "-" (cadr f)) (sym type "-" (cadr f) "-set!"))))
                             (else '())))
                     fields))))))
(define (own-names forms)
  (apply append
    (map (lambda (f)
           (cond ((not (and (pair? f) (pair? (cdr f)))) '())
                 ((eq? (car f) 'begin) (own-names (cdr f)))
                 ((memq (car f) '(define define-syntax))
                  (let ((h (cadr f))) (cond ((symbol? h) (list h)) ((and (pair? h) (symbol? (car h))) (list (car h))) (else '()))))
                 ((eq? (car f) 'define-values) (formal-names (cadr f)))
                 ((eq? (car f) 'define-record-type) (record-names (cadr f) (cddr f)))
                 (else '())))
         forms)))
;; #f when the program's import form compiles as a program of its own,
;; else what the compiler said; the object goes to a scratch directory and
;; whatever the compiler prints to a string
(define (import-conflict forms)
  (let ((imp (find (lambda (f) (and (pair? f) (eq? (car f) 'import))) forms))
        (d (row-dir!)))
    (and imp
         (let ((src (string-append d "/imports.sc")) (obj (string-append d "/imports.so")))
           (call-with-output-file src (lambda (p) (write imp p) (newline p) (put-string p "(void)\n")))
           (guard (e (#t (if (message-condition? e) (condition-message e) 'refused)))
             (let ((sink (open-output-string)))
               (parameterize ((current-output-port sink) (console-output-port sink))
                 (compile-program src obj)))
             #f)))))
;; -> ((<file> <name> ...) ...) for every program defining an imported name
(define (name-collisions forms-of)
  (filter (lambda (r) (pair? (cdr r)))
          (map (lambda (rel)
                 (let* ((forms (forms-of rel)) (imported (imported-names forms)))
                   (cons rel (filter (lambda (n) (hashtable-ref imported n #f)) (own-names forms)))))
               six-programs)))
(want "PROGNAMES-1 no program's top-level definition takes a name its imports bring"
      (name-collisions program-forms)
      '())
(define (planted form)
  (name-collisions (lambda (rel) (if (string=? rel "eval-worker.sc")
                                     (append (program-forms rel) (list form))
                                     (program-forms rel)))))
(want "PROGNAMES-2 TWIN: a Chez name planted in eval-worker.sc by define, define-values, a top-level begin and a record's field accessor is reported each time; CONTROL: every program read has its own names, and Chez's protocol is among eval-worker's imports"
      (list (planted '(define protocol 1))
            (planted '(define-values (protocol) (values 1)))
            (planted '(begin (define protocol 1)))
            (planted '(define-record-type thing (fields (immutable x car))))
            (map (lambda (rel) (pair? (own-names (program-forms rel)))) six-programs)
            (hashtable-ref (imported-names (program-forms "eval-worker.sc")) 'protocol #f))
      (list '(("eval-worker.sc" protocol)) '(("eval-worker.sc" protocol)) '(("eval-worker.sc" protocol))
            '(("eval-worker.sc" car)) (map (lambda (rel) #t) six-programs) #t))
(want "PROGNAMES-3 every program's import form compiles as a program: no name is brought with two bindings"
      (filter cadr (map (lambda (rel) (list rel (import-conflict (program-forms rel)))) six-programs))
      '())
(want "PROGNAMES-4 TWIN: eval-worker.sc's import form with (theourgia eval-context) whole beside (chezscheme) does not compile"
      (let* ((forms (program-forms "eval-worker.sc"))
             (with (map (lambda (f) (if (and (pair? f) (eq? (car f) 'import))
                                        (append f (list '(theourgia eval-context)))
                                        f))
                        forms)))
        (and (import-conflict with) #t))
      #t)

(printf "== SOCKDIR: the thin client refuses before starting a daemon ==\n")
;; THE CLIENT LIBRARY'S START SAYS THE SAME THING BEFORE STARTING ANYTHING
;; (the main session's ruling b). Before it, a --socket under a directory that
;; does not exist started a daemon, which made the whole chain and served.
;; ensure-daemon! refuses, with the text the daemon uses, at once: well inside
;; the 10 s start budget, no daemon for the store, and nothing created. It is
;; read through start-driver.ss; the thin client starts no daemon at all, and
;; answers such a request in process (SOCKDIR-3).
(let* ((d (row-dir!))
       (store (string-append d "/cwd/s"))
       (absent (string-append sock-base "/es-cabsent-" pid-text "-" (number->string row-n))))
  (system (string-append "mkdir -p " store))
  (run-in d "core.sc" (string-append "init --store " store))
  (let* ((t0 (let ((t (current-time))) (+ (* 1000 (time-second t)) (quotient (time-nanosecond t) 1000000))))
         (r (run-in d start-driver (string-append "outline --store " store " --socket " absent "/deep/s.sock") 30))
         (ms (- (let ((t (current-time))) (+ (* 1000 (time-second t)) (quotient (time-nanosecond t) 1000000))) t0))
         (a (guard (e (#t #f)) (call-with-port (open-string-input-port (out-of r)) read)))
         (lines (daemon-lines store)))
    (for-each (lambda (l) (system (string-append "kill -TERM " (number->string (pid-of l)) " 2>/dev/null"))) lines)
    (want "SOCKDIR-2 the client library's start with a --socket under a directory that does not exist answers socket-dir-missing naming it at once, starts no daemon, and creates nothing"
          (list (rc-of r)
                (and (pair? a) (list? a) (pair? (cdr a)) (list (car a) (cadr a)))
                (let ((f (and (pair? a) (list? a) (find (lambda (x) (and (pair? x) (eq? (car x) 'dir))) (cdr a))))) (and f (equal? (cadr f) (string-append absent "/deep"))))
                (if (< ms 5000) 'under-half-the-budget (list 'took-ms ms))
                (length lines)
                (if (file-exists? absent) 'created 'nothing-created))
          (list 75 '(error socket-dir-missing) #t 'under-half-the-budget 0 'nothing-created))
    (system (string-append "rm -rf " absent))
    (let ((plain (run-in d "theourgia.sc" (string-append "outline --store " store " --socket " absent "/deep/s.sock --wire") 30)))
      (want "SOCKDIR-3 the thin client with that --socket and no daemon answers ok in process, starts no daemon, and creates nothing"
            (list (rc-of plain) (and (> (string-length (out-of plain)) 4) (substring (out-of plain) 0 4))
                  (length (daemon-lines store)) (if (file-exists? absent) 'created 'nothing-created))
            '(0 "(ok " 0 nothing-created)))))

(system (string-append "rm -rf " here " " sock-base "/es-" pid-text "-*"))
(printf "\n~a failures\nrows: ~a\nentry-split complete\n" bad rows)
(exit (if (zero? bad) 0 1))
