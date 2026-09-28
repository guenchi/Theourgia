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

;; eval --lang: a source in another language, run by its runner over the
;; store projected into a directory.
;;
;; Every product call is a subprocess of core.sc (eval runs in process)
;; with the source on standard input and --wire, one THEOURGIA_HOME and one
;; THEOURGIA_RUN for the file. Shell rows need /bin/sh only; the python and
;; node rows are red BY NAME when the interpreter is absent, not skipped.

(import (chezscheme))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a -> ~s\n" label got)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                   (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/eval-lang-" (number->string (get-process-id))))
(when (file-exists? root)
  (assertion-violation 'eval-lang "scratch directory already exists" root))
(system (string-append "mkdir -p " root))

(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(define (sh . xs) (system (apply string-append xs)))
(define (sh-out . xs)
  (let* ((p (process (apply string-append xs))) (s (get-string-all (car p))))
    (close-port (car p)) (close-port (cadr p))
    (if (eof-object? s) "" s)))
(define (text-of-file p)
  (if (file-exists? p) (let ((t (call-with-input-file p get-string-all))) (if (eof-object? t) "" t)) ""))
(define (put! p text) (call-with-output-file p (lambda (o) (put-string o text)) 'replace))
(define (has-substring? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0)) (cond ((> (+ i n) m) #f) ((string=? (substring text i (+ i n)) needle) #t) (else (loop (+ i 1)))))))
(define (find-headed x head)
  (cond ((and (pair? x) (eq? (car x) head)) x)
        ((pair? x) (or (find-headed (car x) head) (find-headed (cdr x) head)))
        (else #f)))
(define (clause-of a head) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) head))) (cdr a))))
(define (head-of a)
  (cond ((not (pair? a)) a) ((and (eq? (car a) 'error) (pair? (cdr a))) (list 'error (cadr a))) (else (car a))))
(define (count-incomplete a) (if (and (pair? a) (list? a)) (length (filter (lambda (c) (and (pair? c) (eq? (car c) 'incomplete))) (cdr a))) 0))

(define home (string-append root "/home"))
(define run (string-append root "/run"))
(define outside (string-append root "/outside"))
(sh "mkdir -p " (quoted home) " " (quoted run) " " (quoted outside))
(define scheme-path (let ((s (sh-out "command -v scheme"))) (substring s 0 (max 0 (- (string-length s) 1)))))

;; (ask env store source args...) -> the answer datum; the source on stdin.
(define n 0)
(define (ask env store source . args)
  (set! n (+ n 1))
  (let ((in (string-append root "/in-" (number->string n))) (out (string-append root "/out-" (number->string n))))
    (put! in source)
    (sh "THEOURGIA_LOCAL=1 THEOURGIA_HOME=" (quoted home) " THEOURGIA_RUN=" (quoted run) " " env " "
        (quoted scheme-path) " --script ../core.sc eval "
        (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
        "--store " (quoted store) " --wire < " (quoted in) " > " (quoted out) " 2> " (quoted (string-append out ".err")))
    (guard (e (#t 'UNREADABLE))
      (let ((d (call-with-input-file out read))) (if (eof-object? d) 'NO-ANSWER d)))))
(define (err-of) (text-of-file (string-append root "/out-" (number->string n) ".err")))
(define (cli store . args)
  (sh-out "THEOURGIA_LOCAL=1 THEOURGIA_HOME=" (quoted home) " " (quoted scheme-path) " --script ../core.sc "
          (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
          "--store " (quoted store) " --wire 2>/dev/null < /dev/null"))
(define (datum-of text) (guard (e (#t 'UNREADABLE)) (read (open-string-input-port text))))
(define (block-holding store needle)
  (let ((m (find-headed (datum-of (cli store "grep" needle)) 'match)))
    (and m (pair? (cdr m)) (cadr m))))
(define (eval-dirs-of dir) (filter (lambda (f) (and (> (string-length f) 5) (string=? (substring f 0 5) "eval-")))
                                   (directory-list dir)))
(define (eval-dirs) (eval-dirs-of run))
(define (lines-of text)
  (let loop ((cs (string->list text)) (cur '()) (acc '()))
    (cond ((null? cs) (reverse (cons (list->string (reverse cur)) acc)))
          ((char=? (car cs) #\newline) (loop (cdr cs) '() (cons (list->string (reverse cur)) acc)))
          (else (loop (cdr cs) (cons (car cs) cur) acc)))))
(define (ends-with? s e)
  (let ((n (string-length s)) (k (string-length e)))
    (and (>= n k) (string=? (substring s (- n k) n) e))))
(define ON "THEOURGIA_RUNNERS=on")
(define (have? prog) (= 0 (sh "command -v " prog " > /dev/null 2>&1")))

;; ---- the store: a.sh (one function) and p.sh (two functions) ----------------
(define (make-store! name files)
  (let ((s (string-append root "/" name "/store")) (src (string-append root "/" name "/src")))
    (sh "mkdir -p " (quoted s) " " (quoted src))
    (cli s "init")
    (for-each (lambda (f)
                (let ((p (string-append src "/" (car f))))
                  (sh "mkdir -p \"$(dirname " (quoted p) ")\"")
                  (put! p (cdr f))))
              files)
    (cli s "import-code" src)
    s))
(define A-TEXT "echo alpha committed\n")
(define S (make-store! "s1" (list (cons "a.sh" A-TEXT))))
(define A-ID (let ((m (find-headed (datum-of (cli S "grep" "alpha")) 'match))) (and m (cadr m))))
;; THE BLOCK IS a.sh's CHILD: the outline line after a.sh's own is indented
;; deeper and its id is the matched id, and a.sh's line's TITLE -- what
;; follows "- <id>  " -- is exactly "a.sh" (so neither ba.sh nor "b  a.sh"
;; is a.sh, and a sibling is not a child).
(define (outline-indent l)
  (let loop ((i 0)) (if (and (< i (string-length l)) (char=? (string-ref l i) #\space)) (loop (+ i 1)) i)))
(define (outline-id l)
  (let* ((n (string-length l)) (i (outline-indent l)))
    (and (<= (+ i 2) n) (string=? (substring l i (+ i 2)) "- ")
         (let loop ((j (+ i 2)))
           (cond ((= j n) (substring l (+ i 2) n))
                 ((char=? (string-ref l j) #\space) (substring l (+ i 2) j))
                 (else (loop (+ j 1))))))))
(define (outline-title l)
  (let* ((n (string-length l))
         (i (let loop ((i 0)) (if (and (< i n) (char=? (string-ref l i) #\space)) (loop (+ i 1)) i))))
    (and (<= (+ i 2) n) (string=? (substring l i (+ i 2)) "- ")
         (let loop ((j (+ i 2)))
           (cond ((>= (+ j 1) n) #f)
                 ((and (char=? (string-ref l j) #\space) (char=? (string-ref l (+ j 1)) #\space))
                  (substring l (+ j 2) n))
                 (else (loop (+ j 1))))))))
(want "L0 setup: the store's a.sh block holds its text, the whole line, and sits under a.sh in the outline"
      (let* ((m (find-headed (datum-of (cli S "grep" "alpha")) 'match))
             (o (find-headed (datum-of (cli S "outline")) 'text))
             (ls (if (and o (string? (cadr o))) (lines-of (cadr o)) '())))
        (list (and m (= (length m) 4) (cadddr m))
              (and A-ID
                   (let loop ((ls ls))
                     (cond ((or (null? ls) (null? (cdr ls))) #f)
                           ((and (equal? (outline-title (car ls)) "a.sh")
                                 (> (outline-indent (cadr ls)) (outline-indent (car ls)))
                                 (equal? (outline-id (cadr ls)) A-ID))
                            #t)
                           (else (loop (cdr ls))))))))
      (list "echo alpha committed" #t))

;; ---- L1: a shell runner, ok with exit data ------------------------------------
;; NOTE: ON THE BASE, --lang is not an option: parse-arguments makes it a
;; positional, and eval takes the first positional as the source -- the
;; text "--lang" -- so the base answers an eval-exception for this row.
(let ((a (ask ON S "cat a.sh; exit 3\n" "--lang" "shell")))
  (want "L1 shell: ok, (exit 3) as data, stdout the file's raw bytes, (lang shell), (projection (files 1)), no values clause"
        (list (head-of a) (clause-of a 'exit) (clause-of a 'stdout) (clause-of a 'lang)
              (clause-of a 'projection) (clause-of a 'values)
              (and (clause-of a 'working-view) #t))
        (list 'ok '(exit 3) (list 'stdout A-TEXT) '(lang shell) '(projection (files 1)) #f #t)))

;; ---- L2: the working view, pinned to the writer's baseline --------------------
;; Two blocks, f in p.sh and g in q.sh (a shell file imports as ONE code
;; block, so two functions in one file would be one block). w3 drafts f; then
;; w2 commits new text for g. w3's working view stands on its baseline: g's
;; OLD text with its own draft of f. The plain run shows the committed state.
;; In a second variant w2 also commits f itself; w3's view still shows its
;; draft.
(define P (make-store! "s2" (list (cons "p.sh" "echo fcommitted\n") (cons "q.sh" "echo gcommitted\n"))))
(define f-id (block-holding P "fcommitted"))
(define g-id (block-holding P "gcommitted"))
(cli P "write" "--writer" "w3" f-id "echo fdraft\n")
(cli P "write" "--writer" "w2" g-id "echo gpublished\n")
(cli P "commit" "--writer" "w2" g-id)
(let* ((w (ask ON P "cat p.sh q.sh\n" "--lang" "shell" "--working" "--writer" "w3"))
       (plain (ask ON P "cat p.sh q.sh\n" "--lang" "shell"))
       (text (lambda (a) (let ((c (clause-of a 'stdout))) (if c (cadr c) ""))))
       (cut (lambda (a) (let ((c (clause-of a 'working-view))) (and c (caddr c))))))
  (want "L2 working TWIN: w3's view has its draft of f and g's OLD text; the plain run has the committed g; both report a cut, and the two cuts differ"
        (list (and f-id g-id (not (equal? f-id g-id)))
              (has-substring? (text w) "fdraft") (has-substring? (text w) "gcommitted") (has-substring? (text w) "gpublished")
              (has-substring? (text plain) "fcommitted") (has-substring? (text plain) "gpublished")
              (and (pair? (cut w)) (pair? (cut plain)) (not (equal? (cut w) (cut plain))))
              (cadr (clause-of w 'working-view)))
        (list #t #t #t #f #t #t #t "w3")))
(cli P "write" "--writer" "w2" f-id "echo fpublished\n")
(cli P "commit" "--writer" "w2" f-id)
(let ((w (ask ON P "cat p.sh q.sh\n" "--lang" "shell" "--working" "--writer" "w3")))
  (want "L2 working TWIN, second variant: w2 commits f itself; w3's view still shows its draft of f"
        (let ((t (cadr (or (clause-of w 'stdout) '(stdout "")))))
          (list (has-substring? t "fdraft") (has-substring? t "fpublished")))
        (list #t #f)))

;; ---- L3: the limits, the signal, the interpreter, the ready boundary ----------
(let ((a (ask ON S "sleep 5\n" "--lang" "shell" "--timeout-ms" "1000")))
  (want "L3 time: a runner past --timeout-ms answers eval-limit (resource time)"
        (list (head-of a) (clause-of a 'resource)) (list '(error eval-limit) '(resource time))))
(let ((a (ask ON S "head -c 5000 /dev/zero | tr '\\0' x\n" "--lang" "shell" "--output-bytes" "1024")))
  (want "L3 output: printing past --output-bytes answers eval-limit (resource output)"
        (list (head-of a) (clause-of a 'resource)) (list '(error eval-limit) '(resource output))))
;; THE QUOTA IS BYTES OVER BOTH STREAMS, and the streams are UNEQUAL, so no
;; one-stream rule matches. Stop case: 100 two-byte characters (U+00E9) on
;; stdout and 450 on stderr, 1100 bytes together, past the 1024 quota --
;; while stdout alone is 200 bytes (400 counted twice), stderr alone 900,
;; and stdout 100 characters. The twin prints 100 and 350 (900 bytes) and
;; is not stopped; a rule that counted stderr twice (1400) would stop it.
(define (two-byte-chars out-n err-n)
  (string-append "i=0; while [ $i -lt " (number->string out-n) " ]; do printf '\\303\\251'; i=$((i+1)); done; "
                 "j=0; while [ $j -lt " (number->string err-n) " ]; do printf '\\303\\251' >&2; j=$((j+1)); done\n"))
(let ((a (ask ON S (two-byte-chars 100 450) "--lang" "shell" "--output-bytes" "1024"))
      (b (ask ON S (two-byte-chars 100 350) "--lang" "shell" "--output-bytes" "1024")))
  (want "L3 output in bytes over both streams: 200 + 900 bytes stop at a 1024 quota; 200 + 700 do not"
        (list (head-of a) (clause-of a 'resource) (head-of b))
        (list '(error eval-limit) '(resource output) 'ok)))
(let* ((t0 (real-time))
       (a (ask ON S "head -c 5000 /dev/zero | tr '\\0' x; sleep 30\n" "--lang" "shell"
               "--output-bytes" "1024" "--timeout-ms" "20000"))
       (ms (- (real-time) t0)))
  (want "L3 quota then sleep: stopped by the output quota within the timeout, not by the clock"
        (list (head-of a) (clause-of a 'resource) (< ms 15000)) (list '(error eval-limit) '(resource output) #t)))
(want "L3 memory (python3): an allocation past --memory-bytes answers eval-limit (resource memory)"
      (if (have? "python3")
          (let ((a (ask ON S "x = bytearray(400*1024*1024)\nimport time\ntime.sleep(3)\n" "--lang" "python"
                        "--memory-bytes" "67108864" "--timeout-ms" "10000")))
            (list (head-of a) (clause-of a 'resource)))
          'python3-is-not-on-this-machine)
      (list '(error eval-limit) '(resource memory)))
(let ((a (ask ON S "kill -TERM $$\n" "--lang" "shell")))
  (want "L3 signal: a runner killed by a signal answers ok with (exit (signal TERM))"
        (list (head-of a) (clause-of a 'exit)) (list 'ok '(exit (signal TERM)))))
;; A PATH holding sh and nothing else: the eval process finds Chez by its
;; absolute path (THEOURGIA_SCHEME), and python3 is not an interpreter here.
(define bin (string-append root "/bin"))
(sh "mkdir -p " (quoted bin) " && ln -sf /bin/sh " (quoted (string-append bin "/sh")))
(define marker (string-append outside "/ran"))
(let ((a (ask (string-append ON " THEOURGIA_SCHEME=" (quoted scheme-path) " PATH=" (quoted bin)) S
              (string-append "open('" marker "','w').write('ran')\n") "--lang" "python")))
  (want "L3 interpreter missing: spawn-refused (reason interpreter-missing) before anything ran (no marker)"
        (list a (file-exists? marker)) (list '(error spawn-refused (reason interpreter-missing)) #f)))
(let ((a (ask ON S "exit 3\n" "--lang" "shell")))
  (want "L3 after ready, exit 3 is the runner's own exit data, not interpreter-missing"
        (list (head-of a) (clause-of a 'exit)) (list 'ok '(exit 3))))

;; ---- L4: disabled; the Scheme path is today's ---------------------------------
(let* ((before (eval-dirs)) (a (ask "" S "echo hi\n" "--lang" "shell")) (after (eval-dirs)))
  (want "L4 without THEOURGIA_RUNNERS: runners-disabled, and no eval-* directory was made"
        (list a (equal? before after)) (list '(error runners-disabled) #t)))
;; THE GATE COMES BEFORE ANY DIRECTORY IS MADE, read on a run root that
;; cannot be written: a directory made and removed before the refusal would
;; leave the listing unchanged, but it cannot be made here at all, so the
;; answer would be that failure instead. The CONTROL shows the root really
;; refuses a directory: with the gate on, the same request is not ok.
(define ro-run (string-append root "/ro-run"))
(sh "mkdir -p " (quoted ro-run) " && chmod 500 " (quoted ro-run))
(let ((a (ask (string-append "THEOURGIA_RUN=" (quoted ro-run)) S "echo hi\n" "--lang" "shell"))
      (c (ask (string-append ON " THEOURGIA_RUN=" (quoted ro-run)) S "echo hi\n" "--lang" "shell")))
  (want "L4 the gate precedes any directory: on an unwritable run root, runners-disabled; CONTROL: with the gate on, the same request is refused by the root, which the answer names"
        (list a (and (pair? c) (not (eq? (car c) 'ok))) (equal? (head-of c) '(error runners-disabled))
              (has-substring? (let-values (((p get) (open-string-output-port))) (write c p) (get)) ro-run))
        (list '(error runners-disabled) #t #f #t)))
(sh "chmod 700 " (quoted ro-run))
(want "L4 THEOURGIA_RUNNERS=yes is not on"
      (ask "THEOURGIA_RUNNERS=yes" S "echo hi\n" "--lang" "shell") '(error runners-disabled))
(let ((a (ask "" S "(+ 1 2)\n" "--lang" "scheme")) (b (ask "" S "(+ 1 2)\n")))
  (want "L4 CONTROL: --lang scheme without the gate answers as today, (values (3)), the same as no --lang"
        (list (head-of a) (clause-of a 'values) (equal? (clause-of a 'values) (clause-of b 'values)))
        (list 'ok '(values (3)) #t)))

;; ---- L5: no runner ------------------------------------------------------------
(let ((a (ask ON S "x\n" "--lang" "typescript")))
  (want "L5 typescript has no runner: (error bad-request (reason no-runner) (lang typescript) ...)"
        (list (head-of a) (clause-of a 'reason) (clause-of a 'lang)) (list '(error bad-request) '(reason no-runner) '(lang typescript))))

;; ---- L6: what the runner reaches ----------------------------------------------
(let* ((a (ask ON S "env | cut -d= -f1 | sort | tr '\\n' ' '; echo; printf '%s|' \"$0\" \"$@\"; echo; pwd\n" "--lang" "shell"))
       (lines (let ((t (cadr (or (clause-of a 'stdout) '(stdout ""))))) (let loop ((cs (string->list t)) (cur '()) (acc '()))
                (cond ((null? cs) (reverse acc)) ((char=? (car cs) #\newline) (loop (cdr cs) '() (cons (list->string (reverse cur)) acc)))
                      (else (loop (cdr cs) (cons (car cs) cur) acc)))))))
  ;; THE ENVIRONMENT IS PATH, HOME AND LANG, plus what sh sets for itself
  ;; (PWD, SHLVL, _, OLDPWD): a launcher that passed its own environment on
  ;; would add the Chez library path here, and no THEOURGIA_ name ever reaches
  ;; the launcher, so "no THEOURGIA_" alone could not see that.
  (want "L6 reach: the environment names only PATH HOME LANG (and sh's own PWD SHLVL _ OLDPWD); argv names the source under source/ and no store path; pwd is tree/"
        (list (head-of a)
              ;; NOT VACUOUS: the list must hold PATH, so an env that printed
              ;; nothing does not pass as an environment with nothing extra.
              (and (pair? lines)
                   (let ((names (let split ((cs (string->list (car lines))) (cur '()) (acc '()))
                                  (cond ((null? cs) (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
                                        ((char=? (car cs) #\space) (split (cdr cs) '() (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
                                        (else (split (cdr cs) (cons (car cs) cur) acc))))))
                     (and (member "PATH" names)
                          (for-all (lambda (w) (member w '("PATH" "HOME" "LANG" "PWD" "SHLVL" "_" "OLDPWD"))) names)))
                   #t)
              (and (> (length lines) 1) (has-substring? (cadr lines) "/source/__eval.sh|")
                   (not (has-substring? (cadr lines) S)))
              (and (> (length lines) 2) (let ((p (caddr lines))) (and (> (string-length p) 5) (string=? (substring p (- (string-length p) 5) (string-length p)) "/tree")))))
        (list 'ok #t #t #t)))

;; ---- L7: cleanup, and a cleanup that fails -------------------------------------
(want "L7 every run above left no eval-* directory behind" (eval-dirs) '())
(let* ((plain (ask ON S "echo x\n" "--lang" "shell"))
       (faulted (ask (string-append ON " THEOURGIA_INJECT=on THEOURGIA_TRACE=1 THEOURGIA_FAULT=unlink-fail@eval-cleanup") S "echo x\n" "--lang" "shell"))
       (trace (err-of)))
  (want "L7 an injected removal failure leaves the whole answer as it was and says eval-cleanup-failed"
        (list (head-of faulted) (clause-of faulted 'stdout) (equal? plain faulted)
              (has-substring? trace "(trace eval-cleanup-failed"))
        (list 'ok '(stdout "x\n") #t #t)))
(sh "chmod -R u+rwX " (quoted run) " 2>/dev/null; rm -rf " (quoted run) "/eval-*")

;; ---- L8: a projection that cannot be made --------------------------------------
;; b.sh's code child is given the kind section, which is not code. A src set
;; through the CLI is stored as bytes, and `set` refuses a mode change
;; (mode-mismatch), so neither of those can make the block unexportable; its
;; kind can. The exporter refuses projection-invalid with the reason
;; unexportable-block, and the source -- which would write a marker OUTSIDE
;; the projection directory -- never runs.
(define B (make-store! "s3" (list (cons "b.sh" "echo beta\n"))))
(define b-child (block-holding B "beta"))
(define set-answer (cli B "set" b-child "kind" "section"))
(define marker8 (string-append outside "/ran8"))
(let ((a (ask ON B (string-append "touch " (quoted marker8) "\n") "--lang" "shell")))
  (want "L8 an export refusal: projection-failed with the exporter's unexportable-block inside, and the source never ran"
        (list (and b-child #t) (head-of (datum-of set-answer)) (head-of a)
              (and (pair? a) (> (length a) 2) (list (head-of (caddr a)) (clause-of (caddr a) 'reason)))
              (file-exists? marker8))
        (list #t 'ok '(error projection-failed) '((error projection-invalid) (reason unexportable-block)) #f)))

;; ---- L9: tree/ and source/ are disjoint ----------------------------------------
(define D (make-store! "s4" (list (cons "__eval.sh" "echo projected\n"))))
(let ((a (ask ON D "ls; cat __eval.sh; echo; cat \"$0\"\n" "--lang" "shell")))
  (want "L9 a projected file named like the source: tree/ holds it and the source is in source/; both are read"
        (list (head-of a) (clause-of a 'stdout))
        (list 'ok (list 'stdout "__eval.sh\necho projected\n\nls; cat __eval.sh; echo; cat \"$0\"\n"))))
(want "L9 python: a module imported by a path built from the cwd is found; a relative import is not (the stated limit)"
      (if (have? "python3")
          (let* ((M (make-store! "s5" (list (cons "m.py" "V = 'from tree'\n"))))
                 (good (ask ON M "import os, sys\nsys.path.insert(0, os.getcwd())\nimport m\nprint(m.V)\n" "--lang" "python"))
                 (rel (ask ON M "import m\nprint(m.V)\n" "--lang" "python")))
            (list (clause-of good 'stdout) (clause-of rel 'exit) (clause-of rel 'stdout)
                  (let ((e (clause-of rel 'stderr))) (and e (has-substring? (cadr e) "ModuleNotFoundError")))))
          'python3-is-not-on-this-machine)
      (list '(stdout "from tree\n") '(exit 1) '(stdout "") #t))

;; ---- L10: the option conflicts --------------------------------------------------
(want "L10 --lang with --cut, --latest, --under: bad-request, with the three reasons"
      (map (lambda (a) (list (head-of a) (clause-of a 'reason)))
           (list (ask ON S "x\n" "--lang" "shell" "--cut" "w:1")
                 (ask ON S "x\n" "--lang" "shell" "--latest")
                 (ask ON S "x\n" "--lang" "shell" "--under" "root")))
      '(((error bad-request) (reason lang-and-cut))
        ((error bad-request) (reason lang-and-cut))
        ((error bad-request) (reason lang-and-under))))

;; ---- L11: the documents ----------------------------------------------------------
(define (defined-datum file name)
  (call-with-input-file file
    (lambda (p) (let loop () (let ((x (read p)))
                               (cond ((eof-object? x) #f)
                                     ((and (pair? x) (eq? (car x) 'define) (pair? (cdr x)) (eq? (cadr x) name))
                                      (let ((v (caddr x))) (if (and (pair? v) (eq? (car v) 'quote)) (cadr v) v)))
                                     (else (loop))))))))
;; THE TABLE ROW IS READ INSIDE THE TABLE: the lines that start with "|"
;; directly after README's environment-table header. THE getenv IS READ AS
;; CODE: core.sc's forms as the reader gives them, so a comment or a string
;; holding the text does not count.
(define (prefix? s p) (and (>= (string-length s) (string-length p)) (string=? (substring s 0 (string-length p)) p)))
(define (environment-table-rows)
  (let ((after (member "| variable | read by | what it does |" (lines-of (text-of-file "../README.md")))))
    (if (not after)
        '()
        (let loop ((ls (cdr after)) (acc '()))
          (if (and (pair? ls) (prefix? (car ls) "|")) (loop (cdr ls) (cons (car ls) acc)) (reverse acc))))))
(define (forms-of file)
  (call-with-input-file file
    (lambda (p) (let loop ((acc '())) (let ((x (read p))) (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))
;; WHOLE FORMS ONLY: a form equal to d, or an ELEMENT of a form that holds
;; one -- never the tail of a list, so (list getenv "X") does not hold
;; (getenv "X"). A quoted or quasiquoted form is data, not code: its body
;; is not entered.
(define (holds-datum? x d)
  (cond ((equal? x d) #t)
        ((and (pair? x) (memq (car x) '(quote quasiquote))) #f)
        ((pair? x)
         (let loop ((l x))
           (cond ((pair? l) (or (holds-datum? (car l) d) (loop (cdr l))))
                 ((null? l) #f)
                 (else (holds-datum? l d)))))
        (else #f)))
(want "L11 docs: eval's usage names --lang; README's environment table has a THEOURGIA_RUNNERS row; runners-enabled? reads it by a literal getenv"
      (list (and (member "--lang" (map (lambda (x) (and (pair? x) (car x))) (or (defined-datum "../core.sc" 'eval-usage) '()))) #t)
            (and (exists (lambda (l) (prefix? l "| `THEOURGIA_RUNNERS` | `core.sc` |")) (environment-table-rows)) #t)
            ;; ONLY INSIDE THE GATE'S OWN DEFINITION: a binding elsewhere of the
            ;; same shape, (let ((getenv "THEOURGIA_RUNNERS")) ...), is not the read.
            (let ((d (find (lambda (x) (and (pair? x) (eq? (car x) 'define) (pair? (cdr x))
                                           (equal? (cadr x) '(runners-enabled?))))
                           (forms-of "../core.sc"))))
              (and d (holds-datum? (cddr d) '(getenv "THEOURGIA_RUNNERS")))))
      (list #t #t #t))

;; ---- L12, L13: a store missing a writer ------------------------------------------
;; The mirror M's directory is made unreadable. The projection is DECLARED: it
;; runs under eval's store key, projects what can be read, and the answer
;; carries ONE clause naming M. The exits after the projection's load carry
;; it; a refusal before any load has heard nothing and carries none.
;; w9 drafts a.sh's block BEFORE the mirror is made unreadable, so the
;; --working rows below read a view that differs from the committed one.
(cli S "write" "--writer" "w9" A-ID "echo alpha draft\n")
(define M "zzzzzzzz")
(define mirror (string-append S "/writers/" M))
(sh "mkdir -p " (quoted mirror) " && printf '' > " (quoted (string-append mirror "/000001.sexp")) " && chmod 000 " (quoted mirror))
(let ((a (ask ON S "cat a.sh\n" "--lang" "shell")))
  (want "L12 declared projection: an unreadable mirror still projects a.sh, the runner runs, ONE clause names the mirror"
        (list (head-of a) (clause-of a 'stdout) (count-incomplete a)
              (and (find-headed (clause-of a 'incomplete) 'writer) (cadr (find-headed (clause-of a 'incomplete) 'writer))))
        (list 'ok (list 'stdout A-TEXT) 1 M)))
;; ONE ROW PER EXIT, each reading the head, the reason when there is one,
;; the number of incomplete clauses and the writer the clause names: a
;; clause that lost its writer is not the clause. --working runs the exits
;; over w9's working view, which holds w9's draft of a.sh: the ok row reads
;; the draft, and the limit row's source sleeps only when it sees the draft
;; (the committed view would exit 1 at once).
(define (clause-writer a)
  (let ((w (find-headed (clause-of a 'incomplete) 'writer))) (and w (cadr w))))
(define (exit-reading a) (list (head-of a) (clause-of a 'reason) (count-incomplete a) (clause-writer a)))
(define NOPY (string-append ON " THEOURGIA_SCHEME=" (quoted scheme-path) " PATH=" (quoted bin)))
(want "L13 a limit after the projection's load carries ONE clause naming the mirror"
      (exit-reading (ask ON S "sleep 5\n" "--lang" "shell" "--timeout-ms" "1000"))
      (list '(error eval-limit) #f 1 M))
(want "L13 interpreter-missing after the projection's load carries ONE clause naming the mirror"
      (exit-reading (ask NOPY S "x\n" "--lang" "python"))
      (list '(error spawn-refused) '(reason interpreter-missing) 1 M))
(let ((a (ask ON S "cat a.sh\n" "--lang" "shell" "--working" "--writer" "w9")))
  (want "L13 --working: ok over the writer's view shows w9's draft and carries ONE clause naming the mirror"
        (list (exit-reading a) (clause-of a 'stdout))
        (list (list 'ok #f 1 M) '(stdout "echo alpha draft\n"))))
(want "L13 --working: a limit reached only in w9's view carries ONE clause naming the mirror"
      (exit-reading (ask ON S "grep -q draft a.sh && sleep 5\n" "--lang" "shell" "--timeout-ms" "1000" "--working" "--writer" "w9"))
      (list '(error eval-limit) #f 1 M))
;; NOTE: THIS ROW CANNOT SEE THE VIEW. The interpreter check refuses before
;; anything reads the projection, so --working is observable here only as
;; the path the exit takes; the row reads the clause on that exit.
(want "L13 --working: interpreter-missing carries ONE clause naming the mirror"
      (exit-reading (ask NOPY S "x\n" "--lang" "python" "--working" "--writer" "w9"))
      (list '(error spawn-refused) '(reason interpreter-missing) 1 M))
(want "L13 a refusal before any load (runners-disabled) carries no clause"
      (exit-reading (ask "" S "x\n" "--lang" "shell"))
      (list '(error runners-disabled) #f 0 #f))
(sh "chmod 700 " (quoted mirror))
;; projection-failed AFTER the load heard the mirror: store B's block is
;; unexportable (L8), and B gains the same unreadable mirror.
(define mirror-b (string-append B "/writers/" M))
(sh "mkdir -p " (quoted mirror-b) " && printf '' > " (quoted (string-append mirror-b "/000001.sexp")) " && chmod 000 " (quoted mirror-b))
(want "L13 projection-failed after the load carries ONE clause naming the mirror"
      (exit-reading (ask ON B "x\n" "--lang" "shell"))
      (list '(error projection-failed) #f 1 M))
(sh "chmod 700 " (quoted mirror-b))

;; ---- L14: the launcher's libraries are the calling process's ------------------
;; The store holds theourgia/ffi.sc: a library exporting the three names the
;; launcher imports, whose body writes a marker. The eval process is started
;; so that its OWN environment says nothing about the library path -- first
;; with CHEZSCHEMELIBDIRS unset and its cwd in a directory holding the
;; libraries (Chez's default "."), then with the variable unset and
;; --libdirs. A launcher that searched its own cwd, the projection's tree/,
;; would import the projected file and the marker would be written.
(define libdir
  (let loop ((ds (map car (library-directories))))
    (cond ((null? ds) #f)
          (else
           (let ((d (if (and (> (string-length (car ds)) 0) (char=? (string-ref (car ds) 0) #\/))
                        (car ds)
                        (string-append (current-directory) "/" (car ds)))))
             (if (and (file-exists? (string-append d "/theourgia/ffi.sc")) (file-exists? (string-append d "/igropyr")))
                 d
                 (loop (cdr ds))))))))
(define core-abs (string-append (current-directory) "/../core.sc"))
(define (fake-ffi marker)
  (string-append
    "(library (theourgia ffi)\n"
    "  (export path-executable? isolate-evaluation! exec-argv-env!)\n"
    "  (import (chezscheme))\n"
    "  (define loaded (call-with-output-file \"" marker "\" (lambda (p) (display \"ran\" p)) 'replace))\n"
    "  (define (path-executable? p) #f)\n"
    "  (define (isolate-evaluation! s) (values 0 0 0))\n"
    "  (define (exec-argv-env! a b c) #f))\n"))
;; (ask-without-libdirs cwd scheme-options store source args...): the eval
;; process with CHEZSCHEMELIBDIRS removed from its environment.
(define (ask-without-libdirs cwd options store source . args)
  (apply ask-without-libdirs-under run cwd options store source args))
(define (ask-without-libdirs-under run cwd options store source . args)
  (set! n (+ n 1))
  (let ((in (string-append root "/in-" (number->string n))) (out (string-append root "/out-" (number->string n))))
    (put! in source)
    (sh "cd " (quoted cwd) " && env -u CHEZSCHEMELIBDIRS THEOURGIA_LOCAL=1 THEOURGIA_HOME=" (quoted home)
        " THEOURGIA_RUN=" (quoted run) " " ON " " (quoted scheme-path) " " options " --script " (quoted core-abs) " eval "
        (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
        "--store " (quoted store) " --wire < " (quoted in) " > " (quoted out) " 2> " (quoted (string-append out ".err")))
    (guard (e (#t 'UNREADABLE))
      (let ((d (call-with-input-file out read))) (if (eof-object? d) 'NO-ANSWER d)))))
(define marker14a (string-append outside "/ran14a"))
(define marker14b (string-append outside "/ran14b"))
(define STORE-14A (make-store! "s7" (list (cons "theourgia/ffi.sc" (fake-ffi marker14a)) (cons "a.sh" "echo fourteen\n"))))
(define STORE-14B (make-store! "s8" (list (cons "theourgia/ffi.sc" (fake-ffi marker14b)) (cons "a.sh" "echo fourteen\n"))))
(want "L14 setup: a directory on this process's library path holds theourgia/ and igropyr/"
      (and libdir #t) #t)
(let ((a (ask-without-libdirs libdir "" STORE-14A "cat a.sh; test -f theourgia/ffi.sc && echo projected\n" "--lang" "shell")))
  (want "L14 CHEZSCHEMELIBDIRS unset (the libraries found by the default dot): the launcher loads the caller's libraries, not the projected theourgia/ffi.sc"
        (list (head-of a) (clause-of a 'stdout) (file-exists? marker14a))
        (list 'ok '(stdout "echo fourteen\nprojected\n") #f)))
(let ((a (ask-without-libdirs root (string-append "--libdirs " (quoted libdir)) STORE-14B
                              "cat a.sh; test -f theourgia/ffi.sc && echo projected\n" "--lang" "shell")))
  (want "L14 TWIN --libdirs with the variable unset: the launcher loads the caller's libraries, not the projected theourgia/ffi.sc"
        (list (head-of a) (clause-of a 'stdout) (file-exists? marker14b))
        (list 'ok '(stdout "echo fourteen\nprojected\n") #f)))

;; ---- L15: a store that cannot be loaded at all -----------------------------------
;; meta.sexp removed, then meta.sexp that is not a store's: the load raises a
;; log-error; the answer is projection-failed with the export verb's own
;; (error meta ...) inside, heard nothing, and the source never ran.
(define E1 (make-store! "s9" (list (cons "a.sh" "echo fifteen\n"))))
(define E2 (make-store! "s10" (list (cons "a.sh" "echo fifteen\n"))))
(sh "rm -f " (quoted (string-append E1 "/meta.sexp")))
(put! (string-append E2 "/meta.sexp") "(not a store)\n")
(define marker15 (string-append outside "/ran15"))
(let ((a (ask ON E1 (string-append "touch " (quoted marker15) "\n") "--lang" "shell")))
  (want "L15 meta.sexp removed: projection-failed with (error meta (path <meta.sexp>)) inside, no clause, and the source never ran"
        (list (head-of a) (and (pair? a) (> (length a) 2) (list (head-of (caddr a)) (clause-of (caddr a) 'path)))
              (count-incomplete a) (file-exists? marker15))
        (list '(error projection-failed) (list '(error meta) (list 'path (string-append E1 "/meta.sexp"))) 0 #f)))
(let ((a (ask ON E2 (string-append "touch " (quoted marker15) "\n") "--lang" "shell")))
  (want "L15 meta.sexp not a store's: projection-failed with (error meta ... (supported 1)) inside, and the source never ran"
        (list (head-of a) (and (pair? a) (> (length a) 2) (list (head-of (caddr a)) (clause-of (caddr a) 'supported)))
              (file-exists? marker15))
        (list '(error projection-failed) '((error meta) (supported 1)) #f)))

;; ---- L16: cleanup does not follow a link out of the scratch directory --------------
;; The runner links a directory outside to tree/link and to source/link2.
;; Cleanup removes the links and leaves the directory and its file alone,
;; and no eval-* directory remains.
(define keep-dir (string-append outside "/keep"))
(sh "mkdir -p " (quoted keep-dir) " && printf kept > " (quoted (string-append keep-dir "/f")))
(let ((a (ask ON S (string-append "ln -s " (quoted keep-dir) " link; ln -s " (quoted keep-dir)
                                  " \"$(dirname \"$0\")/link2\"; ls -d link \"$(dirname \"$0\")/link2\" > /dev/null && echo linked\n")
              "--lang" "shell")))
  (want "L16 a symbolic link the runner left is removed as a link: the directory it names keeps its file, and no eval-* directory remains"
        (list (head-of a) (clause-of a 'stdout) (text-of-file (string-append keep-dir "/f")) (eval-dirs))
        (list 'ok '(stdout "linked\n") "kept" '())))

;; ---- L18: a library directory the launcher's variable cannot hold ------------------
;; FROM A COMMAND LINE: the eval process runs with its cwd in root/lib:colon,
;; which holds the libraries (symbolic links to the library directory's
;; theourgia/ and igropyr/), CHEZSCHEMELIBDIRS unset, so it finds them by the
;; default "." -- and "." made absolute is a name holding ":". Its run root is
;; not writable: a directory made before the refusal would change the answer.
(define colon-dir (string-append root "/lib:colon"))
(define run18 (string-append root "/run18"))
(sh "mkdir -p " (quoted colon-dir) " " (quoted run18)
    " && ln -s " (quoted (string-append libdir "/theourgia")) " " (quoted (string-append colon-dir "/theourgia"))
    " && ln -s " (quoted (string-append libdir "/igropyr")) " " (quoted (string-append colon-dir "/igropyr"))
    " && chmod 500 " (quoted run18))
(let ((a (ask-without-libdirs-under run18 colon-dir "" S "echo x\n" "--lang" "shell")))
  (want "L18 a working directory holding a colon, libraries by the default dot: spawn-refused (reason library-directory-unrepresentable) naming it, on an unwritable run root, nothing made"
        (list (head-of a) (clause-of a 'reason)
              (let ((d (clause-of a 'directory))) (and d (string? (cadr d)) (has-substring? (cadr d) "/lib:colon")))
              (eval-dirs-of run18))
        (list '(error spawn-refused) '(reason library-directory-unrepresentable) #t '())))
(sh "chmod 700 " (quoted run18))

;; ---- L19: the library-path refusals a program reaches (tripwires) -----------------
;; An empty list, an extension holding ":" or NUL, and the pair ("" . ""), set
;; through the parameters themselves by a child program that then calls
;; run-foreign-eval on an unwritable run root. The first four are refused by
;; name; the last is carried: written "::", it reads back as itself, so there
;; is no refusal and the run goes on to fail on the run root instead. These
;; rows are tripwires for such a caller, not measurements of a command-line
;; path (L18 is the one a command line reaches); an ordinary configuration
;; that the checks wrongly refused would turn every --lang row above red.
(define (child-refusal name setup)
  (let ((child (string-append root "/" name ".sc")) (run-c (string-append root "/run-" name)))
    (sh "mkdir -p " (quoted run-c) " && chmod 500 " (quoted run-c))
    (put! child
          (string-append
            "(import (chezscheme) (theourgia eval-runner)\n"
            "        (only (theourgia ffi) fs-error? fs-error-target unreadable-entry? unreadable-entry-path))\n"
            setup "\n"
            "(write (guard (e ((fs-error? e) (list 'RAISED (fs-error-target e)))\n"
            "                 ((unreadable-entry? e) (list 'RAISED (unreadable-entry-path e)))\n"
            "                 (#t 'RAISED))\n"
            "  (run-foreign-eval \"" S "\" \"shell\" \"echo x\\n\" '())))\n"))
    (let ((a (datum-of (sh-out "THEOURGIA_LOCAL=1 THEOURGIA_HOME=" (quoted home) " THEOURGIA_RUN=" (quoted run-c) " "
                               (quoted scheme-path) " --script " (quoted child) " 2>/dev/null"))))
      (sh "chmod 700 " (quoted run-c))
      (list a (eval-dirs-of run-c) run-c))))
(let ((r (child-refusal "l19a" "(library-directories '())")))
  (want "L19 an empty library-directory list: spawn-refused (reason library-directories-empty), nothing made"
        (list (car r) (cadr r))
        (list '(error spawn-refused (reason library-directories-empty)) '())))
(let ((r (child-refusal "l19b" "(library-extensions (append (library-extensions) (list (cons \".x:y\" \".x:y\"))))")))
  (want "L19 an extension holding a colon: spawn-refused (reason library-extension-unrepresentable) naming it, nothing made"
        (list (car r) (cadr r))
        (list '(error spawn-refused (reason library-extension-unrepresentable) (extension ".x:y")) '())))
(let ((r (child-refusal "l19c" "(library-extensions '())")))
  (want "L19 an empty extension list: spawn-refused (reason library-extensions-empty), nothing made"
        (list (car r) (cadr r))
        (list '(error spawn-refused (reason library-extensions-empty)) '())))
(let ((r (child-refusal "l19d" "(library-extensions (append (library-extensions) (list (cons \"\" \"\"))))")))
  ;; CARRIED: the failure is the scratch directory's creation under the
  ;; unwritable run root, which comes after the refusals, so none refused it.
  (want "L19 the pair of two empty names is carried, not refused: the run goes on and fails creating eval-* under the unwritable run root, nothing made"
        (let ((a (car r)))
          (list (and (pair? a) (eq? (car a) 'RAISED) (pair? (cdr a)) (string? (cadr a))
                     (prefix? (cadr a) (string-append (caddr r) "/eval-")))
                (cadr r)))
        (list #t '())))
(let ((r (child-refusal "l19f" "(library-extensions (append (library-extensions) (list (cons \".sc\" \".b\\x0;d\"))))")))
  (want "L19 an extension pair whose OBJECT side holds NUL: the refusal names that side, not the source"
        (list (car r) (cadr r))
        (list '(error spawn-refused (reason library-extension-unrepresentable) (extension ".b\x0;d")) '())))
(let ((r (child-refusal "l19e" "(library-extensions (append (library-extensions) (list (cons \".x\\x0;y\" \".x\\x0;y\"))))")))
  (want "L19 an extension holding NUL: spawn-refused (reason library-extension-unrepresentable) naming it, nothing made"
        (list (car r) (cadr r))
        (list '(error spawn-refused (reason library-extension-unrepresentable) (extension ".x\x0;y")) '())))

;; ---- L20: nothing the runner started outlives the answer ---------------------------
;; The runner starts a child in the background, its output sent away (so the
;; streams close when the runner exits); the child first writes its own pid,
;; then would write the marker three seconds later. The runner waits a
;; moment, so the pid is written, and exits 0. Right after the answer the
;; child is asked for by kill -0 and must be gone -- the group is signalled
;; and waited for on this exit -- with no eval-group-survived trace; after
;; four seconds there is still no marker.
;; NOTE: END TO END THIS ROW CANNOT TELL A WAITED SIGNAL FROM ONE NOT WAITED
;; FOR: SIGKILL lands long before the kill -0 runs. L21 reads the wait itself.
(define pid20 (string-append outside "/pid20"))
(define marker20 (string-append outside "/ran20"))
(let* ((t0 (real-time))
       (a (ask (string-append ON " THEOURGIA_TRACE=1") S
               (string-append "sh -c 'echo $$ > " pid20 "; sleep 3; touch " marker20 "' > /dev/null 2>&1 &\n"
                              "sleep 0.3\nexit 0\n")
               "--lang" "shell"))
       (ms (- (real-time) t0))
       (trace (err-of))
       (pid (let ((t (text-of-file pid20))) (and (> (string-length t) 1) (substring t 0 (- (string-length t) 1)))))
       (alive (and pid (= 0 (sh "kill -0 " pid " 2>/dev/null")))))
  (sh "sleep 4")
  (want "L20 a background child of a runner that exited 0: the answer is ok at once, the child is gone at the answer (no eval-group-survived), and it never ran on"
        (list (head-of a) (clause-of a 'exit) (< ms 3000) (and pid #t) alive
              (has-substring? trace "eval-group-survived") (file-exists? marker20))
        (list 'ok '(exit 0) #t #t #f #f #f)))

;; ---- L21: the wait, read from its own text -----------------------------------------
;; group-wait-ms and stop-group-and-wait! are read out of eval-supervise.sc as
;; data and evaluated in an environment whose free names are stubs recording
;; each call: signal-pid!, sigkill, process-alive-signal0? (answering from a
;; script), sleep-ms (advancing a fake clock by a set step), real-time
;; (reading it) and trace-event!. Three cases:
;;   gone at once -- signal, one probe, #t, and no sleep;
;;   alive once -- signal, probe, sleep, probe, #t;
;;   never gone, each sleep taking 50 ms of the fake clock -- it stops at the
;;   deadline 2000 ms after the signal: 40 sleeps, the eval-group-survived
;;   trace, #f. A wait bounded by counting its asks would sleep 200 times.
(define (library-body-of file)
  (let ((lib (find (lambda (x) (and (pair? x) (eq? (car x) 'library))) (forms-of file))))
    (if (and lib (>= (length lib) 4)) (list-tail lib 4) '())))
(define (source-define name forms)
  (find (lambda (x) (and (pair? x) (eq? (car x) 'define) (pair? (cdr x))
                         (or (eq? (cadr x) name) (and (pair? (cadr x)) (eq? (caadr x) name)))))
        forms))
(define wait-env (copy-environment (scheme-environment) #t))
(eval '(begin
         (define calls '()) (define clock 0) (define step 10) (define answers '()) (define then #f)
         (define sigkill 9)
         (define (note! x) (set! calls (cons x calls)))
         (define (real-time) clock)
         (define (signal-pid! p s) (note! (list 'signal p s)) 0)
         (define (process-alive-signal0? p)
           (note! (list 'probe p))
           (if (null? answers) then (let ((a (car answers))) (set! answers (cdr answers)) a)))
         (define (sleep-ms n) (note! (list 'sleep n)) (set! clock (+ clock step)))
         (define (trace-event! op a b) (note! (list 'trace op a))))
      wait-env)
(define wait-forms (library-body-of "../eval-supervise.sc"))
(define wait-defines (list (source-define 'group-wait-ms wait-forms) (source-define 'stop-group-and-wait! wait-forms)))
(want "L21 setup: eval-supervise.sc defines group-wait-ms and stop-group-and-wait!"
      (map (lambda (d) (and d #t)) wait-defines) '(#t #t))
(when (for-all (lambda (d) d) wait-defines)
  (for-each (lambda (d) (eval d wait-env)) wait-defines))
(define (wait-case answers then step)
  (guard (e (#t (list 'RAISED (if (message-condition? e) (condition-message e) e))))
    (eval `(begin (set! calls '()) (set! clock 0) (set! answers ',answers) (set! then ,then) (set! step ,step)) wait-env)
    (let ((r (eval '(stop-group-and-wait! 7) wait-env)))
      (list r (reverse (eval 'calls wait-env))))))
(want "L21 gone at once: signal, one probe, #t, no sleep"
      (wait-case '(#f) #f 10)
      '(#t ((signal -7 9) (probe -7))))
(want "L21 alive once: signal, probe, sleep, probe, #t"
      (wait-case '(#t #f) #f 10)
      '(#t ((signal -7 9) (probe -7) (sleep 10) (probe -7))))
(want "L21 never gone, 50 ms a sleep: stops at the 2000 ms deadline after 40 sleeps, traces eval-group-survived, #f"
      (let ((r (wait-case '() #t 50)))
        (and (pair? r) (not (eq? (car r) 'RAISED))
             (list (car r)
                   (length (filter (lambda (c) (eq? (car c) 'sleep)) (cadr r)))
                   (car (reverse (cadr r))))))
      '(#f 40 (trace eval-group-survived 7)))

;; ---- L22: cleanup removes only what this evaluation made -------------------------
;; A child program learns its own scratch names -- the token is its process id
;; and a counter, and it takes the first token itself, so the evaluation's
;; next ones are known -- and plants something at them in a writable run root,
;; then calls run-foreign-eval with tracing on. (No scheduler runs in the
;; child, so the evaluation stops after its scratch directory is made; the
;; cleanup still runs.) A file planted at the next name survives, and the
;; trace shows the name after it created instead; a directory planted there
;; keeps its content; with the next eight names taken the answer is
;; scratch-unavailable, and every planted entry survives.
(define (hex8-of n)
  (let ((h (number->string (mod n 4294967296) 16)))
    (string-append (make-string (- 8 (string-length h)) #\0) (string-downcase h))))
(define (scratch-case name plant-code)
  (let ((child (string-append root "/" name ".sc")) (run-c (string-append root "/run-" name))
        (err (string-append root "/" name ".err")))
    (sh "mkdir -p " (quoted run-c))
    (put! child
          (string-append
            "(import (chezscheme) (theourgia eval-runner) (only (theourgia client) next-attempt-token))\n"
            "(define root \"" run-c "\")\n"
            "(define pid-hex (substring (next-attempt-token) 0 8))\n"
            "(define (hex8 n) (let ((h (number->string (mod n 4294967296) 16))) (string-append (make-string (- 8 (string-length h)) #\\0) (string-downcase h))))\n"
            "(define (name k) (string-append root \"/eval-\" pid-hex (hex8 k)))\n"
            "(define (plant-file! k) (call-with-output-file (name k) (lambda (p) (display \"planted\" p))))\n"
            "(define (plant-dir! k) (mkdir (name k)) (call-with-output-file (string-append (name k) \"/keep\") (lambda (p) (display \"kept\" p))))\n"
            plant-code "\n"
            "(define answer (guard (e (#t 'RAISED)) (run-foreign-eval \"" S "\" \"shell\" \"echo x\\n\" '())))\n"
            "(write (list answer pid-hex))\n"))
    (let* ((out (datum-of (sh-out "THEOURGIA_LOCAL=1 THEOURGIA_TRACE=1 THEOURGIA_HOME=" (quoted home) " THEOURGIA_RUN=" (quoted run-c) " "
                                  (quoted scheme-path) " --script " (quoted child) " 2> " (quoted err))))
           (pid-hex (and (pair? out) (= (length out) 2) (cadr out))))
      (list (and (pair? out) (car out)) pid-hex run-c (text-of-file err)))))
(define (scratch-name run-c pid-hex k) (string-append run-c "/eval-" pid-hex (hex8-of k)))
(let* ((r (scratch-case "l22a" "(plant-file! 2)"))
       (pid-hex (cadr r)) (run-c (caddr r)))
  (want "L22 a file at the next scratch name survives, and the evaluation creates the name after it instead (and removes only that)"
        (and pid-hex
             (list (equal? (car r) '(error spawn-refused (reason scratch-unavailable)))
                   (text-of-file (scratch-name run-c pid-hex 2))
                   (has-substring? (cadddr r) (string-append "(trace create " (scratch-name run-c pid-hex 3) " "))
                   (eval-dirs-of run-c)))
        (list #f "planted" #t (list (string-append "eval-" (cadr r) (hex8-of 2))))))
(let* ((r (scratch-case "l22b" "(plant-dir! 2)"))
       (pid-hex (cadr r)) (run-c (caddr r)))
  (want "L22 a directory at the next scratch name keeps its content, and the evaluation creates the name after it"
        (and pid-hex
             (list (text-of-file (string-append (scratch-name run-c pid-hex 2) "/keep"))
                   (has-substring? (cadddr r) (string-append "(trace create " (scratch-name run-c pid-hex 3) " "))))
        (list "kept" #t)))
(let* ((r (scratch-case "l22c" "(for-each plant-file! '(2 3 4 5 6 7 8 9))"))
       (pid-hex (cadr r)) (run-c (caddr r)))
  (want "L22 the next eight scratch names taken: spawn-refused (reason scratch-unavailable), and every planted file survives"
        (and pid-hex
             (list (car r)
                   (for-all (lambda (k) (equal? (text-of-file (scratch-name run-c pid-hex k)) "planted")) '(2 3 4 5 6 7 8 9))
                   (length (eval-dirs-of run-c))))
        (list '(error spawn-refused (reason scratch-unavailable)) #t 8)))

;; ---- L23: the run root is resolved at the claim ------------------------------------
;; The run root is a symbolic link L to directory A. The runner learns its
;; scratch name T from $0 (source/ under eval-T), points L at directory B,
;; makes B/eval-T/keep itself, and exits 0. The evaluation claimed A/eval-T
;; through L's real path, so it removes A/eval-T: B/eval-T/keep survives and
;; A holds no eval-* entry. Removing through L would have removed B's
;; directory and left A's.
(define dir-a (string-append root "/run-a"))
(define dir-b (string-append root "/run-b"))
(define link23 (string-append root "/run-link"))
(sh "mkdir -p " (quoted dir-a) " " (quoted dir-b) " && ln -s " (quoted dir-a) " " (quoted link23))
(let* ((src (string-append
              "d=$(dirname \"$(dirname \"$0\")\"); t=$(basename \"$d\"); "
              "rm " (quoted link23) " && ln -s " (quoted dir-b) " " (quoted link23) " && "
              "mkdir " (quoted dir-b) "/\"$t\" && printf keep > " (quoted dir-b) "/\"$t\"/keep && echo \"$t\"\n"))
       (a (ask (string-append ON " THEOURGIA_RUN=" (quoted link23)) S src "--lang" "shell"))
       (t (let ((o (clause-of a 'stdout))) (and o (> (string-length (cadr o)) 1)
                                               (substring (cadr o) 0 (- (string-length (cadr o)) 1))))))
  (want "L23 a run root reached through a link the runner re-points: the evaluation removes its own directory, not the one the link now reaches"
        (list (head-of a) (and t (prefix? t "eval-"))
              (and t (text-of-file (string-append dir-b "/" t "/keep")))
              (eval-dirs-of dir-a))
        (list 'ok #t "keep" '())))

;; ---- L17: the compiled layout carries every program ------------------------------
;; build.ss copies the programs beside the compiled libraries, and a program
;; left out cannot be started from the output (the launcher is found beside
;; core.sc). Its list is read against datum-self.sc's pinned PROGRAMS, the
;; list of the sources classified as programs.
(want "L17 build.ss's programs are exactly datum-self's pinned PROGRAMS"
      (let ((built (defined-datum "../build.ss" 'programs))
            (pinned (let ((w (find (lambda (x) (and (pair? x) (eq? (car x) 'want) (pair? (cdr x)) (string? (cadr x))
                                                    (prefix? (cadr x) "PROGRAMS the sources classified as programs are exactly these")))
                                   (forms-of "datum-self.sc"))))
                      (and w (= (length w) 4) (let ((q (cadddr w))) (and (pair? q) (eq? (car q) 'quote) (cadr q)))))))
        (list (and (pair? built) (pair? pinned) #t)
              (and (list? built) (list? pinned) (equal? (list-sort string<? built) (list-sort string<? pinned)))))
      (list #t #t))

(sh "chmod -R u+rwX " (quoted root) " 2>/dev/null; rm -rf " (quoted root))
(printf "\n~a failures\nrows: ~a\neval-lang complete\n" bad rows)
(exit (if (= bad 0) 0 1))
