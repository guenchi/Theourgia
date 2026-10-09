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
;; The RT rows, which run the compiled languages' real tools, are skipped by
;; name, with the reason, where a tool is absent (RT, below).

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
;; NEVER: WHAT A RUNNER'S OUTPUT LIMIT CARRIES IS AT MOST THE LIMIT IN BYTES,
;; cut on a character boundary: 600 two-byte characters (1200 bytes) at an
;; ODD quota, 1025, carry exactly the 512 whole characters that fit in 1024
;; bytes -- a cut in the middle of a character would carry a broken one --
;; and say (truncated #t).
(let* ((a (ask ON S (two-byte-chars 600 0) "--lang" "shell" "--output-bytes" "1025"))
       (out (clause-of a 'stdout))
       (text (and out (pair? (cdr out)) (string? (cadr out)) (cadr out))))
  (want "L3 output cut: a runner's carried stdout is the whole characters that fit in the quota's bytes, and the answer says (truncated #t)"
        (list (head-of a) (clause-of a 'resource)
              (and text (string=? text (make-string 512 (integer->char #xE9))))
              (clause-of a 'truncated))
        (list '(error eval-limit) '(resource output) #t '(truncated #t))))
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
(let ((a (ask ON S "x\n" "--lang" "markdown")))
  (want "L5 markdown has no runner: (error bad-request (reason no-runner) (lang markdown) ...)"
        (list (head-of a) (clause-of a 'reason) (clause-of a 'lang)) (list '(error bad-request) '(reason no-runner) '(lang markdown))))

;; ---- R: the default runners of typescript, go, rust, c and java ------------------
;; STAND-INS FIRST ON PATH: the runner keeps the calling process's PATH, so
;; scripts named node, go, rustc, cc and java in a directory put ahead of it
;; are what the table's argv reaches, on a machine with none of them. Each
;; prints its own name and its whole argv, one per line. node, go and java
;; then print -- and the source. The two compilers write a program at their
;; -o path that prints `ran`, the full path it runs as, --, then the source;
;; a source holding BAD they refuse with exit 3, writing nothing. A row
;; compares the whole of stdout, so an extra argument, a program run from
;; elsewhere or a source under another name is red.
(define stubs (string-append outside "/stubs"))
(sh "mkdir -p " (quoted stubs))
(define (stub! name text)
  (put! (string-append stubs "/" name) (string-append "#!/bin/sh\n" text))
  (sh "chmod 755 " (quoted (string-append stubs "/" name))))
(define (compiler-stub name)
  (string-append
    "printf '%s\\n' " name " \"$@\"\n"
    "[ \"$1\" = -o ] || exit 2\n"
    "if grep -q BAD \"$3\"; then echo '" name ": cannot compile' >&2; exit 3; fi\n"
    "cat > \"$2\" <<EOF\n"
    "#!/bin/sh\n"
    "printf '%s\\n' ran \"\\$0\"\n"
    "printf -- '--\\n'\n"
    "cat '$3'\n"
    "EOF\n"
    "chmod 755 \"$2\"\n"))
(stub! "node" "printf '%s\\n' node \"$@\"; printf -- '--\\n'; cat \"$1\"\n")
(stub! "java" "printf '%s\\n' java \"$@\"; printf -- '--\\n'; cat \"$1\"\n")
(stub! "go" "printf '%s\\n' go \"$@\"; printf -- '--\\n'; cat \"$2\"\n")
(stub! "rustc" (compiler-stub "rustc"))
(stub! "cc" (compiler-stub "cc"))
(define STUB-PATH (string-append "PATH=" (quoted stubs) ":\"$PATH\""))
(define (with-stubs . more) (apply string-append ON " " STUB-PATH more))
(define (stdout-of a) (let ((c (clause-of a 'stdout))) (if c (cadr c) "")))
(define (stderr-of a) (let ((c (clause-of a 'stderr))) (if c (cadr c) "")))
(define run-real (let ((s (sh-out "cd " (quoted run) " && pwd -P"))) (substring s 0 (max 0 (- (string-length s) 1)))))
;; (lang source-name at lines): the name the table writes the source as, the
;; line of stdout holding its full path, and, given that path, the lines the
;; stand-ins print before the source.
(define stand-ins
  (list (list "typescript" "__eval.mts" 1 (lambda (p) (list "node" p "--")))
        (list "go" "eval.go" 2 (lambda (p) (list "go" "run" p "--")))
        (list "rust" "__eval.rs" 3
              (lambda (p) (let ((bin (string-append p ".bin"))) (list "rustc" "-o" bin p "ran" bin "--"))))
        (list "c" "__eval.c" 3
              (lambda (p) (let ((bin (string-append p ".bin"))) (list "cc" "-o" bin p "ran" bin "--"))))
        (list "java" "__eval.java" 1 (lambda (p) (list "java" p "--")))))
(define (lines-text ls) (apply string-append (map (lambda (l) (string-append l "\n")) ls)))
(define (begins-with? s b) (and (>= (string-length s) (string-length b)) (string=? (substring s 0 (string-length b)) b)))
;; A SOURCE'S PATH: <run root>/eval-<token>/source/<name>, the run root as
;; given or as resolved.
(define (source-path? p name)
  (and (string? p)
       (ends-with? p (string-append "/source/" name))
       (or (begins-with? p (string-append run-real "/eval-")) (begins-with? p (string-append run "/eval-")))))
;; -> the full path of the source in what lang's stand-ins printed, or #f.
(define (printed-path lang out)
  (let ((s (assoc lang stand-ins)) (ls (lines-of out)))
    (and (> (length ls) (caddr s))
         (let ((p (list-ref ls (caddr s)))) (and (source-path? p (cadr s)) p)))))
;; -> #t when `out` is exactly what lang's stand-ins print for `source`, else `out`.
(define (stand-in-printed lang source out)
  (let ((p (printed-path lang out)))
    (or (and p (string=? out (string-append (lines-text ((cadddr (assoc lang stand-ins)) p)) source))) out)))
(for-each
  (lambda (c)
    (let* ((lang (car c)) (source (cadr c)) (a (ask (with-stubs) S source "--lang" lang)))
      (want (string-append "R1 " lang ": the table's runner runs the source through " (caddr c)
                           "; ok, (exit 0), and stdout is all the stand-ins printed, the full paths under this run root")
            (list (head-of a) (clause-of a 'exit) (stand-in-printed lang source (stdout-of a)) (clause-of a 'lang))
            (list 'ok '(exit 0) #t (list 'lang (string->symbol lang))))))
  (list (list "typescript" "let x: number = 1;\n" "node, as __eval.mts")
        (list "go" "package main\nfunc main() {}\n" "go run, as eval.go")
        (list "rust" "fn main() {}\n" "rustc, then the program it built beside it")
        (list "c" "int main(void) { return 0; }\n" "cc, then the program it built beside it")
        (list "java" "class C { public static void main(String[] a) {} }\n" "java, as __eval.java")))
;; A SOURCE THAT DOES NOT COMPILE: the compiler's exit and stderr are the
;; answer, and no program runs after it: stdout is the compiler's argv and
;; nothing more.
(for-each
  (lambda (lang)
    (let* ((a (ask (with-stubs) S "BAD\n" "--lang" lang))
           (out (stdout-of a))
           (p (printed-path lang out))
           (tool (if (string=? lang "rust") "rustc" "cc")))
      (want (string-append "R2 " lang ": a source the compiler refuses answers ok with its (exit 3) and its stderr; stdout is the compiler's argv alone, so nothing ran after it")
            (list (head-of a) (clause-of a 'exit)
                  (or (and p (string=? out (lines-text (list tool "-o" (string-append p ".bin") p)))) out)
                  (has-substring? (stderr-of a) (string-append tool ": cannot compile")))
            (list 'ok '(exit 3) #t #t))))
  '("rust" "c"))

;; ---- RT: the default runners with the real tools ----------------------------------
;; Where the tool is on PATH, the table's runner runs a program with it and
;; the row expects what it prints. Where it is not, the row prints SKIP with
;; the reason and is counted as skipped, never silently. node must also strip
;; types (process.features.typescript, Node 22.18 and 23.6 on), and java must
;; have a runtime (`java -version` succeeds; macOS has a java that only says
;; there is none), or the row is skipped saying which. The limits are wide:
;; a first build fills a compiler's cache.
(define skipped 0)
(define (skip! label why) (set! skipped (+ skipped 1)) (printf "SKIP ~a: ~a\n" label why))
(for-each
  (lambda (c)
    (let ((lang (list-ref c 0)) (tool (list-ref c 1)) (ready (list-ref c 2)) (unready (list-ref c 3)) (source (list-ref c 4))
          (label (string-append "RT-" (list-ref c 0))))
      (cond
        ((not (have? tool)) (skip! label (string-append tool " is not on PATH")))
        ((and ready (not (= 0 (sh ready " > /dev/null 2>&1")))) (skip! label unready))
        (else
         (let ((a (ask ON S source "--lang" lang "--timeout-ms" "60000" "--memory-bytes" "1073741824")))
           (want (string-append label ": the table's runner runs a program with the real " tool "; ok, (exit 0), stdout 42")
                 (list (head-of a) (clause-of a 'exit) (stdout-of a))
                 (list 'ok '(exit 0) "42\n")))))))
  (list (list "typescript" "node" "node -e 'process.exit(process.features.typescript ? 0 : 1)'"
              "node strips no types (Node 22.18 and 23.6 on do)"
              "const n: number = 6 * 7;\nconsole.log(n);\n")
        (list "go" "go" #f "" "package main\n\nimport \"fmt\"\n\nfunc main() { fmt.Println(6 * 7) }\n")
        (list "rust" "rustc" #f "" "fn main() { println!(\"{}\", 6 * 7); }\n")
        (list "c" "cc" #f "" "#include <stdio.h>\nint main(void) { printf(\"%d\\n\", 6 * 7); return 0; }\n")
        (list "java" "java" "java -version" "java has no runtime (java -version fails)"
              "class Main { public static void main(String[] a) { System.out.println(6 * 7); } }\n")))

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
;; A top-level define, or one in the body of a `(library ...)` form: eval's
;; usage form is rpc.sc's, and that file is one library form.
(define (defined-datum file name)
  (define (found x)
    (and (pair? x) (eq? (car x) 'define) (pair? (cdr x)) (eq? (cadr x) name) (pair? (cddr x))
         (let ((v (caddr x))) (if (and (pair? v) (eq? (car v) 'quote)) (cadr v) v))))
  (call-with-input-file file
    (lambda (p) (let loop () (let ((x (read p)))
                               (cond ((eof-object? x) #f)
                                     ((found x) => (lambda (v) v))
                                     ((and (pair? x) (eq? (car x) 'library) (list? x))
                                      (or (exists found (cdr x)) (loop)))
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
      (list (and (member "--lang" (map (lambda (x) (and (pair? x) (car x))) (or (defined-datum "../rpc.sc" 'eval-usage) '()))) #t)
            (and (exists (lambda (l) (prefix? l "| `THEOURGIA_RUNNERS` | `core.sc` |")) (environment-table-rows)) #t)
            ;; ONLY INSIDE THE GATE'S OWN DEFINITION: a binding elsewhere of the
            ;; same shape, (let ((getenv "THEOURGIA_RUNNERS")) ...), is not the read.
            (let ((d (find (lambda (x) (and (pair? x) (eq? (car x) 'define) (pair? (cdr x))
                                           (equal? (cadr x) '(runners-enabled?))))
                           (forms-of "../core.sc"))))
              (and d (holds-datum? (cddr d) '(getenv "THEOURGIA_RUNNERS")))))
      (list #t #t #t))

;; ---- L14: the writer an evaluation's view is for ---------------------------------
;; An explicit --writer, else THEOURGIA_WRITER when it is set and not empty,
;; never the actor -- the dispatcher's rule, now eval's in every branch: the
;; cut's baseline, the working view (--latest reaches it directly), and a
;; foreign runner's projection. Two writers draft the same block, so a row
;; that read the wrong one, or none, says which.
(define X5 (make-store! "x5" (list (cons "a.sh" "echo x5 committed\n"))))
(define X5-ID (block-holding X5 "x5 committed"))
(cli X5 "write" "--writer" "wx5" X5-ID "echo x5 draft of wx5\n")
(cli X5 "write" "--writer" "vx5" X5-ID "echo x5 draft of vx5\n")
(define (reads a) (list (head-of a)
                        (has-substring? (format "~s" a) "draft of wx5")
                        (has-substring? (format "~s" a) "draft of vx5")))
(define X5-BLOCK (string-append "(block \"" (or X5-ID "none") "\")"))
(want "L14 THEOURGIA_WRITER=wx5 and no --writer: eval --working (the cut) reads wx5's draft"
      (reads (ask "THEOURGIA_WRITER=wx5" X5 X5-BLOCK "--working"))
      '(ok #t #f))
(want "L14 THEOURGIA_WRITER=wx5 and no --writer: eval --working --latest (the view) reads wx5's draft"
      (reads (ask "THEOURGIA_WRITER=wx5" X5 X5-BLOCK "--working" "--latest"))
      '(ok #t #f))
(want "L14 THEOURGIA_WRITER=wx5 and no --writer: eval --lang shell --working (a runner's projection) reads wx5's draft"
      (reads (ask (string-append ON " THEOURGIA_WRITER=wx5") X5 "cat a.sh\n" "--lang" "shell" "--working"))
      '(ok #t #f))
(want "L14 an explicit --writer vx5 wins over THEOURGIA_WRITER=wx5, in the cut and in a runner's projection"
      (list (reads (ask "THEOURGIA_WRITER=wx5" X5 X5-BLOCK "--working" "--writer" "vx5"))
            (reads (ask (string-append ON " THEOURGIA_WRITER=wx5") X5 "cat a.sh\n" "--lang" "shell" "--working" "--writer" "vx5")))
      '((ok #f #t) (ok #f #t)))

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
;; Its admission pool is made first, while it still is, since the admission
;; comes before the runner: slot files 0 to 255, more than any pool this row
;; meets, so the evaluation is admitted and reaches the refusal the row is
;; about.
(define colon-dir (string-append root "/lib:colon"))
(define run18 (string-append root "/run18"))
(sh "mkdir -p " (quoted colon-dir) " " (quoted run18)
    " && ln -s " (quoted (string-append libdir "/theourgia")) " " (quoted (string-append colon-dir "/theourgia"))
    " && ln -s " (quoted (string-append libdir "/igropyr")) " " (quoted (string-append colon-dir "/igropyr"))
    " && mkdir -p " (quoted (string-append run18 "/admission"))
    " && i=0 && while [ $i -lt 256 ]; do : > " (quoted (string-append run18 "/admission")) "/$i; i=$((i+1)); done"
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
;; The claim traces the RESOLVED run root (its real path), so a trace is
;; looked for under the directory as `pwd -P` spells it: a fixture root
;; reached through a link (/tmp on macOS) is spelled differently there.
(define (resolved-dir p)
  (let ((s (sh-out "cd " (quoted p) " && pwd -P")))
    (if (and (> (string-length s) 0) (char=? (string-ref s (- (string-length s) 1)) #\newline))
        (substring s 0 (- (string-length s) 1))
        s)))
(let* ((r (scratch-case "l22a" "(plant-file! 2)"))
       (pid-hex (cadr r)) (run-c (caddr r)))
  (want "L22 a file at the next scratch name survives, and the evaluation creates the name after it instead (and removes only that)"
        (and pid-hex
             (list (equal? (car r) '(error spawn-refused (reason scratch-unavailable)))
                   (text-of-file (scratch-name run-c pid-hex 2))
                   (has-substring? (cadddr r) (string-append "(trace create " (scratch-name (resolved-dir run-c) pid-hex 3) " "))
                   (eval-dirs-of run-c)))
        (list #f "planted" #t (list (string-append "eval-" (cadr r) (hex8-of 2))))))
(let* ((r (scratch-case "l22b" "(plant-dir! 2)"))
       (pid-hex (cadr r)) (run-c (caddr r)))
  (want "L22 a directory at the next scratch name keeps its content, and the evaluation creates the name after it"
        (and pid-hex
             (list (text-of-file (string-append (scratch-name run-c pid-hex 2) "/keep"))
                   (has-substring? (cadddr r) (string-append "(trace create " (scratch-name (resolved-dir run-c) pid-hex 3) " "))))
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

;; ---- L15: the evaluation admission --------------------------------------------
;; Every evaluation takes one of K slots of its run root's pool before its cut,
;; view, projection or scratch, and holds it until its process ends. Each row
;; has a run root of its own, so its pool starts empty. A LIMIT: a slot
;; released when the evaluation returns but before core.sc ends is not told
;; apart from one released at the end -- nothing here keeps core.sc alive in
;; that interval.
(define SL (make-store! "slots" (list (cons "a.sh" "echo slots\n"))))
(define (wall-ms)
  (let ((t (current-time 'time-utc))) (+ (* 1000 (time-second t)) (quotient (time-nanosecond t) 1000000))))
(define (pause-ms ms) (sleep (make-time 'time-duration (* (mod ms 1000) 1000000) (div ms 1000))))
(define (within ms ok?)
  (let ((end (+ (wall-ms) ms)))
    (let loop () (let ((v (ok?))) (cond (v v) ((> (wall-ms) end) #f) (else (pause-ms 50) (loop)))))))
(define (fresh-run! name)
  (let ((d (string-append root "/" name))) (sh "rm -rf " (quoted d) "; mkdir -p " (quoted d)) d))
;; An evaluation started in the background: -> (pid out). The pid is the
;; evaluating process's own (env, when given, execs it).
(define (bg env store source . args)
  (set! n (+ n 1))
  (let ((in (string-append root "/in-" (number->string n))) (out (string-append root "/out-" (number->string n)))
        (pidf (string-append root "/pid-" (number->string n))))
    (put! in source)
    (sh "THEOURGIA_LOCAL=1 THEOURGIA_HOME=" (quoted home) " THEOURGIA_RUN=" (quoted run) " " env " "
        (quoted scheme-path) " --script ../core.sc eval "
        (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
        "--store " (quoted store) " --wire < " (quoted in) " > " (quoted out) " 2> " (quoted (string-append out ".err"))
        " & echo $! > " (quoted pidf))
    (list (within 5000 (lambda () (let ((t (text-of-file pidf))) (and (> (string-length t) 1) (string->number (substring t 0 (- (string-length t) 1)))))))
          out)))
(define (alive? pid) (and pid (= 0 (sh "kill -0 " (number->string pid) " 2>/dev/null"))))
;; -> the wall clock when the process was seen gone, or #f
(define (ended-at pid ms) (within ms (lambda () (and (not (alive? pid)) (wall-ms)))))
(define (answer-in out) (guard (e (#t 'UNREADABLE)) (let ((d (call-with-input-file out read))) (if (eof-object? d) 'NO-ANSWER d))))
(define (scratch-count r)
  (length (filter (lambda (f) (and (> (string-length f) 5) (string=? (substring f 0 5) "eval-")))
                  (directory-list r))))
(define (slots-env r . more) (apply string-append "THEOURGIA_RUN=" (quoted r) " " more))
;; A HOLDER'S SOURCE: it says it is running, then holds its slot until the row
;; creates the go file -- never for a fixed time, so a contender that starts
;; slowly cannot find the holder already gone. Every holder runs with a 60 s
;; deadline of its own, longer than any wait a row makes for its contender
;; (20 s at most). A LIMIT: a contender's own startup is not bounded by the
;; row, and one that took longer than 60 s would find its holder gone and
;; be let in. -> the source text.
(define (hold-until ready go) (string-append "touch " ready "; until [ -e " go " ]; do sleep 0.05; done"))

;; THE SECOND WAITS, AND MAKES NOTHING WHILE IT WAITS. A holds the only slot
;; until the row says go; B is held at its admission, whose .held file says
;; it got there, and the scratch listing is read then: only A's. Released
;; from the hold, B tries the slot; half a second later B is still running
;; with no answer, A still holds, and the scratch listing is still only A's
;; -- an implementation that let B in would show B's scratch or B's answer
;; there. Then A is let go, and B runs.
(want "L15 K=1: a second evaluation waits for the slot, has no scratch and no answer while it waits, and runs once the first ends"
      (let* ((r (fresh-run! "slots-wait"))
             (ready (string-append r ".ready")) (rel (string-append r ".release")) (go (string-append r ".go"))
             (a (bg (slots-env r "THEOURGIA_EVAL_SLOTS=1 " ON) SL (string-append (hold-until ready go) "; echo first")
                    "--lang" "shell" "--timeout-ms" "60000"))
             (ready-seen (within 15000 (lambda () (file-exists? ready))))
             (b (bg (slots-env r "THEOURGIA_EVAL_SLOTS=1 " ON " THEOURGIA_INJECT=on THEOURGIA_HOLD=eval-admission:" rel)
                    SL "echo second" "--lang" "shell" "--timeout-ms" "20000"))
             (held (within 20000 (lambda () (file-exists? (string-append rel ".held")))))
             (scratch (scratch-count r))
             (_ (sh "touch " (quoted rel)))
             (_ (pause-ms 500))
             (waiting (list (alive? (car b)) (alive? (car a)) (answer-in (cadr b)) (scratch-count r)))
             (_ (sh "touch " (quoted go)))
             (a-end (ended-at (car a) 20000))
             (b-end (ended-at (car b) 30000)))
        (list (and ready-seen held #t) scratch waiting
              (has-substring? (format "~s" (answer-in (cadr a))) "first")
              (has-substring? (format "~s" (answer-in (cadr b))) "second")
              (and a-end b-end #t)))
      '(#t 1 (#t #t NO-ANSWER 1) #t #t #t))

;; BUSY, WITH THE WAIT REPORTED: waited-ms is at least the budget and at most
;; the time the row itself measured around the request. (A LIMIT: a report
;; that is the budget itself, whatever the real wait, is not told apart; the
;; row has no clock inside the waiting process.)
(want "L15 K=1: a second evaluation whose timeout ends while the first holds the slot answers eval-busy, with K and the time it waited"
      (let* ((r (fresh-run! "slots-busy"))
             (ready (string-append r ".ready")) (go (string-append r ".go"))
             (a (bg (slots-env r "THEOURGIA_EVAL_SLOTS=1 " ON) SL (hold-until ready go)
                    "--lang" "shell" "--timeout-ms" "60000"))
             (ready-seen (within 15000 (lambda () (file-exists? ready))))
             (t0 (wall-ms))
             (busy (ask (slots-env r "THEOURGIA_EVAL_SLOTS=1 " ON) SL "echo x" "--lang" "shell" "--timeout-ms" "700"))
             (t1 (wall-ms))
             (_ (sh "touch " (quoted go)))
             (_ (ended-at (car a) 20000))
             (waited (let ((c (clause-of busy 'waited-ms))) (and c (cadr c)))))
        (list (and ready-seen #t) (head-of busy) (clause-of busy 'slots)
              (and (integer? waited) (<= 700 waited (- t1 t0)))))
      '(#t (error eval-busy) (slots 1) #t))

;; NO ATTEMPT AND NO SLEEP PAST THE BUDGET: with --timeout-ms 50 and the one
;; slot held, the refusal comes after the first attempt and a sleep cut to
;; what is left of the 50 ms, not after a whole 100 ms step. (50 and not 1:
;; a first attempt that itself took the whole of a 1 ms budget would refuse
;; before any sleep, and a whole-step sleep would go unseen. The same holds
;; at 50 ms, less often: a whole-step sleep is seen only when the first
;; attempt took under 50 ms.) (That a slot freed after the
;; budget is not taken, and that the wait is on the monotonic clock, are not
;; observable here: the first needs a release timed inside one step of
;; another process, the second a step of the machine's clock.)
(want "L15 K=1, --timeout-ms 50 while the slot is held: eval-busy with waited-ms from 50 to under one 100 ms step"
      (let* ((r (fresh-run! "slots-short"))
             (ready (string-append r ".ready")) (go (string-append r ".go"))
             (a (bg (slots-env r "THEOURGIA_EVAL_SLOTS=1 " ON) SL (hold-until ready go)
                    "--lang" "shell" "--timeout-ms" "60000"))
             (ready-seen (within 15000 (lambda () (file-exists? ready))))
             (busy (ask (slots-env r "THEOURGIA_EVAL_SLOTS=1 " ON) SL "echo x" "--lang" "shell" "--timeout-ms" "50"))
             (_ (sh "touch " (quoted go)))
             (_ (ended-at (car a) 20000))
             (waited (let ((c (clause-of busy 'waited-ms))) (and c (cadr c)))))
        (list (and ready-seen #t) (head-of busy) (clause-of busy 'slots) (and (integer? waited) (<= 50 waited 99))))
      '(#t (error eval-busy) (slots 1) #t))

;; THE REFUSALS THAT NEED NO STORE DO NOT QUEUE: with the one slot held,
;; cut-and-latest, eval-arguments, lang-and-cut and runners-disabled are
;; answered as themselves, not as eval-busy.
(want "L15 K=1, the slot held: the pre-admission refusals answer at once as themselves"
      (let* ((r (fresh-run! "slots-pre"))
             (ready (string-append r ".ready")) (go (string-append r ".go"))
             (a (bg (slots-env r "THEOURGIA_EVAL_SLOTS=1 " ON) SL (hold-until ready go)
                    "--lang" "shell" "--timeout-ms" "60000"))
             (ready-seen (within 15000 (lambda () (file-exists? ready))))
             (env (slots-env r "THEOURGIA_EVAL_SLOTS=1 " ON))
             (answers (list (ask env SL "(+ 1 2)" "--latest" "--cut" "w:1" "--timeout-ms" "500")
                            (ask env SL "(+ 1 2)" "--timeout-ms" "0")
                            (ask env SL "echo x" "--lang" "shell" "--cut" "w:1" "--timeout-ms" "500")
                            (ask (slots-env r "THEOURGIA_EVAL_SLOTS=1 THEOURGIA_RUNNERS=") SL "echo x" "--lang" "shell" "--timeout-ms" "500"))))
        (sh "touch " (quoted go))
        (ended-at (car a) 20000)
        (list (and ready-seen #t)
              (map (lambda (x) (list (head-of x) (clause-of x 'reason))) answers)))
      '(#t (((error bad-request) (reason cut-and-latest)) ((error bad-request) (reason eval-arguments))
            ((error bad-request) (reason lang-and-cut)) ((error runners-disabled) #f))))

;; THE WAIT IS NOT PART OF THE EVALUATION'S DEADLINE: B waits about 2 s for
;; the slot and then runs a 2.5 s source within a 4 s budget. A deadline that
;; counted the wait would leave B under 2 s and end it.
(want "L15 K=1: an evaluation that waited 2 s for the slot still has its whole --timeout-ms to run"
      (let* ((r (fresh-run! "slots-deadline"))
             (ready (string-append r ".ready")) (rel (string-append r ".release")) (go (string-append r ".go"))
             (a (bg (slots-env r "THEOURGIA_EVAL_SLOTS=1 " ON) SL (hold-until ready go)
                    "--lang" "shell" "--timeout-ms" "60000"))
             (ready-seen (within 15000 (lambda () (file-exists? ready))))
             (b (bg (slots-env r "THEOURGIA_EVAL_SLOTS=1 " ON " THEOURGIA_INJECT=on THEOURGIA_HOLD=eval-admission:" rel)
                    SL "sleep 2.5; echo late" "--lang" "shell" "--timeout-ms" "4000"))
             (held (within 20000 (lambda () (file-exists? (string-append rel ".held")))))
             (_ (sh "touch " (quoted rel)))
             (_ (pause-ms 2000))
             (_ (sh "touch " (quoted go)))
             (_ (ended-at (car a) 20000))
             (_ (ended-at (car b) 20000))
             (answer (answer-in (cadr b))))
        (list (and ready-seen held #t) (head-of answer) (has-substring? (format "~s" answer) "late")))
      '(#t ok #t))

;; NEVER: THE RUNNER DOES NOT HOLD THE SLOT. The holder -- core.sc itself, its
;; command line read back -- is killed with SIGKILL while its runner sleeps
;; on. The runner is known by its own pid, which its shell writes before
;; exec'ing the sleep (so the pid is the sleeping process), and it is seen
;; alive after the kill and again after the third request; a runner that had
;; inherited the lock's descriptor would keep the slot, and the third
;; evaluation would be busy. A LIMIT: the runner sleeps 31.5 s, so a row
;; delayed past that ends it first and is red on a correct product.
(want "L15 K=1: the holder killed while its runner sleeps frees the slot at once; the runner does not hold it"
      (let* ((r (fresh-run! "slots-kill"))
             (ready (string-append r ".ready")) (rp (string-append r ".runner"))
             (a (bg (slots-env r "THEOURGIA_EVAL_SLOTS=1 " ON) SL
                    (string-append "echo $$ > " rp "; touch " ready "; exec sleep 31.5")
                    "--lang" "shell" "--timeout-ms" "60000"))
             (ready-seen (within 15000 (lambda () (file-exists? ready))))
             (runner (let ((t (text-of-file rp))) (and (> (string-length t) 1) (string->number (substring t 0 (- (string-length t) 1))))))
             (target-core (and (car a) (has-substring? (sh-out "ps -o command= -p " (number->string (car a))) "core.sc")))
             (_ (when (car a) (sh "kill -9 " (number->string (car a)))))
             (gone (ended-at (car a) 5000))
             (runner-after-kill (alive? runner))
             (third (ask (slots-env r "THEOURGIA_EVAL_SLOTS=1 " ON) SL "echo third" "--lang" "shell" "--timeout-ms" "1500"))
             (runner-after-third (alive? runner)))
        (when runner (sh "kill " (number->string runner) " 2>/dev/null"))
        (list (and ready-seen #t) (and runner #t) target-core (and gone #t) runner-after-kill runner-after-third
              (head-of third) (has-substring? (format "~s" third) "third")))
      '(#t #t #t #t #t #t ok #t))

;; THE POOL'S SIZE UNSET: the online processors, through the one reading and
;; its seam. Two holders fill a pool of two, and the third is busy.
(want "L15 THEOURGIA_EVAL_SLOTS unset, THEOURGIA_EVAL_SLOTS_DEFAULT=2: two evaluations hold the pool and a third is busy with (slots 2)"
      (let* ((r (fresh-run! "slots-default"))
             (env (string-append "env -u THEOURGIA_EVAL_SLOTS THEOURGIA_EVAL_SLOTS_DEFAULT=2 THEOURGIA_RUN=" (quoted r) " " ON))
             (ra (string-append r ".a")) (rb (string-append r ".b")) (go (string-append r ".go"))
             (a (bg env SL (hold-until ra go) "--lang" "shell" "--timeout-ms" "60000"))
             (b (bg env SL (hold-until rb go) "--lang" "shell" "--timeout-ms" "60000"))
             (ready-seen (within 15000 (lambda () (and (file-exists? ra) (file-exists? rb)))))
             (third (ask env SL "echo x" "--lang" "shell" "--timeout-ms" "500")))
        (sh "touch " (quoted go))
        (ended-at (car a) 20000) (ended-at (car b) 20000)
        (list (and ready-seen #t) (head-of third) (clause-of third 'slots)))
      '(#t (error eval-busy) (slots 2)))

;; THE READING ITSELF, against the system's own count: getconf's
;; _NPROCESSORS_ONLN, in this process where the seam is unset. The reading is
;; asked inside a guard: a library without it (the base, a mutant) makes this
;; row red, not the fixture's end.
(let ((n-online (guard (e (#t 'NOT-AVAILABLE)) ((eval 'online-processors (environment '(theourgia ffi))))))
      (n-getconf (string->number (let ((t (sh-out "getconf _NPROCESSORS_ONLN 2>/dev/null")))
                                   (if (> (string-length t) 0) (substring t 0 (- (string-length t) 1)) "")))))
  (printf "L15 information: the online processors, by the pool's reading ~a, by getconf ~a\n" n-online n-getconf)
  (want "L15 the pool's reading of the online processors, with the seam unset, is getconf's _NPROCESSORS_ONLN"
        (list (not (getenv "THEOURGIA_EVAL_SLOTS_DEFAULT")) (and (integer? n-online) (> n-online 0) (eqv? n-online n-getconf)))
        '(#t #t)))

;; AN EMPTY SEAM IS AN UNSET ONE, as every other variable's empty value is
;; (env-or): the pool is the online processors, and the evaluation runs.
(want "L15 THEOURGIA_EVAL_SLOTS and THEOURGIA_EVAL_SLOTS_DEFAULT both empty: the pool is the online processors and the evaluation answers ok"
      (let ((r (fresh-run! "slots-empty")))
        (head-of (ask (slots-env r "THEOURGIA_EVAL_SLOTS= THEOURGIA_EVAL_SLOTS_DEFAULT=") SL "(+ 1 2)")))
      'ok)

(want "L15 a K that is not a positive integer is refused eval-slots"
      (let ((r (fresh-run! "slots-bad")))
        (list (ask (slots-env r "THEOURGIA_EVAL_SLOTS=0") SL "(+ 1 2)")
              (ask (slots-env r "THEOURGIA_EVAL_SLOTS=two") SL "(+ 1 2)")))
      '((error bad-request (reason eval-slots)) (error bad-request (reason eval-slots))))

;; THE SLOT FILES ARE MADE WHERE THEY CANNOT BE: an admission directory that
;; can be searched and not written, holding no slot. The creation's failure
;; is the table's unwritable. The instrument is checked first: a file this
;; process tries to create there must be refused (a process that bypasses
;; permissions would make the row say nothing).
(want "L15 an admission directory that can be searched and not written, and holds no slot, answers unwritable"
      (let* ((r (fresh-run! "slots-unwritable"))
             (d (string-append r "/admission"))
             (_ (sh "mkdir -p " (quoted d) "; chmod 555 " (quoted d)))
             (denied (not (= 0 (sh "touch " (quoted (string-append d "/probe")) " 2>/dev/null"))))
             (a (ask (slots-env r "THEOURGIA_EVAL_SLOTS=1") SL "(+ 1 2)")))
        (sh "chmod 755 " (quoted d))
        (list denied (head-of a)))
      '(#t (error unwritable)))

;; A MARK THAT FAILS is the descriptor's failure, unreadable-entry. (That the
;; slot is released before the answer is not observable from outside: the
;; process ends with its answer, and the system releases the lock then.)
(want "L15 the close-on-exec mark failing on the slot's descriptor answers unreadable"
      (let ((r (fresh-run! "slots-cloexec")))
        (head-of (ask (slots-env r "THEOURGIA_EVAL_SLOTS=1 THEOURGIA_INJECT=on THEOURGIA_FAULT=cloexec-fail@admission")
                      SL "(+ 1 2)")))
      '(error unreadable))

;; THE MARK, MEASURED THREE WAYS, on two descriptors of one file, one marked.
;; (a) THE MARK ITSELF: a child made by Chez's `system` (/bin/sh) runs
;; `test -e /dev/fd/<n>` for each; it must see the unmarked one (0, the
;; control) and not the marked one (1); the getter must read the mark.
;; (b) THE WORKER'S SPAWN PATH (proc-spawn!, by spawn-worker!: libuv), a
;; reading: libuv's child closes every descriptor it did not set up --
;; measured on macOS here, and on FreeBSD 15.0 with libuv 1.52.0 by a
;; compiled probe (2026-09-29) -- so its child sees neither, and there the
;; slot needs no mark. The row asserts only that the marked one is never
;; seen. A LIMIT: through that path the signal-9 row above cannot tell a
;; missing mark apart, on either platform.
;; (c) THE TREE'S OWN posix_spawn PATH (ffi.sc spawn-captured!, which sets
;; no POSIX_SPAWN_CLOEXEC_DEFAULT): its child inherits every descriptor not
;; marked, so where it sees the unmarked one, the marked one must be
;; unseen -- here the mark is the protection.
(define cloexec-probe (string-append root "/cloexec-probe.sc"))
(put! cloexec-probe
      (string-append
        "(import (chezscheme) (only (theourgia ffi) fd-open fd-close-on-exec! fd-close-on-exec? spawn-captured! waitpid-status)\n"
        "        (only (theourgia sched) start-scheduler receive self)\n"
        "        (only (theourgia proc) spawn-worker!))\n"
        "(define plain (fd-open \"/etc/hosts\" '(read)))\n"
        "(define marked (fd-open \"/etc/hosts\" '(read)))\n"
        "(fd-close-on-exec! marked)\n"
        "(define (test-argv fd) (list \"/bin/sh\" \"-c\" (string-append \"test -e /dev/fd/\" (number->string fd))))\n"
        "(define (seen-by-system fd) (system (caddr (test-argv fd))))\n"
        "(define (seen-by-ffi fd)\n"
        "  (let-values (((pid err) (spawn-captured! (test-argv fd) '() #f \"/dev/null\" \"/dev/null\")))\n"
        "    (if (not pid)\n"
        "        (list 'spawn-failed err)\n"
        "        (let loop ((i 0))\n"
        "          (let ((s (waitpid-status pid)))\n"
        "            (cond ((and s (eq? (car s) 'exit)) (cadr s))\n"
        "                  (s s)\n"
        "                  ((> i 1000) 'no-exit)\n"
        "                  (else (sleep (make-time 'time-duration 10000000 0)) (loop (+ i 1)))))))))\n"
        "(define (seen-by-worker fd)\n"
        "  (spawn-worker! \"/bin/sh\" (test-argv fd) '() self)\n"
        "  (receive (after 10000 'no-exit) (`(worker-exit ,r ,status ,signal) status)))\n"
        "(define before (list (seen-by-system plain) (seen-by-system marked) (seen-by-ffi plain) (seen-by-ffi marked)))\n"
        "(start-scheduler\n"
        "  (lambda ()\n"
        "    (let* ((p (seen-by-worker plain)) (m (seen-by-worker marked)))\n"
        "      (write (append (list (fd-close-on-exec? plain) (fd-close-on-exec? marked)) before (list p m)))\n"
        "      (newline) (flush-output-port (current-output-port))\n"
        "      (exit 0))))\n"))
;; -> (getter-plain getter-marked system-plain system-marked ffi-plain
;; ffi-marked worker-plain worker-marked); 0 seen, 1 unseen.
(let ((r (datum-of (sh-out (quoted scheme-path) " --script " (quoted cloexec-probe) " 2>/dev/null < /dev/null"))))
  (define (at i) (and (list? r) (= (length r) 8) (list-ref r i)))
  (printf "L15 information: seen by a child (0 seen, 1 unseen) -- ffi.sc's posix_spawn: unmarked ~a, marked ~a; libuv's spawn: unmarked ~a, marked ~a~a\n"
          (at 4) (at 5) (at 6) (at 7)
          (if (eqv? (at 6) 1) " (libuv closes every descriptor it did not set up)" ""))
  (want "L15 fd-close-on-exec!: the getter reads the mark, and a child made by Chez's system sees the unmarked descriptor and not the marked one"
        (list (at 0) (at 1) (at 2) (at 3))
        '(#f #t 0 1))
  (want "L15 the worker's spawn path (libuv) never shows the marked descriptor"
        (and (eqv? (at 7) 1) (memv (at 6) '(0 1)) #t)
        #t)
  (want "L15 ffi.sc's posix_spawn path shows the unmarked descriptor and not the marked one: there the mark is the protection"
        (list (at 4) (at 5))
        '(0 1)))

;; ---- C: --lang chez, a Scheme runner over the projection ---------------------------
;; A store holding the library (lib a) at lib/a.sc, imported by import-code.
;; On the base every chez row meets no-runner first: that is its red there,
;; and the property each row also asserts is for the mutants.
(define LIB-A "(library (lib a) (export f) (import (rnrs)) (define (f) 42))\n")
(define SC (make-store! "chez1" (list (cons "lib/a.sc" LIB-A))))
(define (chez-var text) (string-append "THEOURGIA_RUNNER_CHEZ=" (quoted text)))
(define (with-var text) (string-append ON " " (chez-var text)))
;; What a whitespace run is, in README: any run of spaces and newlines is one space.
(define (squash text)
  (let loop ((cs (string->list text)) (space? #f) (acc '()))
    (cond ((null? cs) (list->string (reverse acc)))
          ((memv (car cs) '(#\space #\newline #\tab))
           (loop (cdr cs) #t acc))
          (else (loop (cdr cs) #f (cons (car cs) (if (and space? (pair? acc)) (cons #\space acc) acc)))))))
(define (pwd-p dir) (let ((s (sh-out "cd " (quoted dir) " && pwd -P"))) (substring s 0 (max 0 (- (string-length s) 1)))))

(let ((a (ask ON SC "(import (lib a)) (display (f))" "--lang" "chez")))
  (want "C1 chez: a library the store holds as lib/a.sc is imported by the source; ok, (exit 0), stdout 42, (lang chez)"
        (list (head-of a) (clause-of a 'exit) (stdout-of a) (clause-of a 'lang))
        (list 'ok '(exit 0) "42" '(lang chez))))
;; THE PARENT'S LIBRARIES ARE ON THE PATH TOO: the interpreter's cwd is tree/,
;; and Chez searches "." by default, so a projected library alone would be
;; found by a runner that dropped the directories pair. (theourgia crc32) is
;; the calling process's and not the projection's.
(let ((a (ask ON SC "(import (theourgia crc32)) (display 1)" "--lang" "chez")))
  (want "C1b chez: a library only the calling process's directories hold is found through {libdirs}"
        (list (head-of a) (clause-of a 'exit) (stdout-of a))
        (list 'ok '(exit 0) "1")))
(define SC-SHADOW
  (make-store! "chez2" (list (cons "theourgia/crc32.sc"
                                   "(library (theourgia crc32) (export probe) (import (chezscheme)) (define probe \"projected\"))\n"))))
(let ((a (ask ON SC-SHADOW "(import (theourgia crc32)) (display probe)" "--lang" "chez")))
  (want "C1c chez: a library of the same name in the projection and in the calling process's directories: the projection's is found first"
        (list (head-of a) (clause-of a 'exit) (stdout-of a))
        (list 'ok '(exit 0) "projected")))
(let ((a (ask ON SC "(begin (import (lib a)) (f))" "--lang" "scheme")))
  (want "C2 --lang scheme is the sandbox, unchanged: the same import answers the fixed eval-exception"
        (list (head-of a) (clause-of a 'kind) (clause-of a 'message))
        (list '(error eval-exception) '(kind raised) '(message "Evaluation raised an exception"))))
(let ((a (ask (string-append ON " THEOURGIA_RUNNER_CHEZ=") SC "(import (lib a)) (display (f))" "--lang" "chez")))
  (want "C4b an empty THEOURGIA_RUNNER_CHEZ is unset: the table's runner runs"
        (list (head-of a) (stdout-of a))
        (list 'ok "42")))

;; A RECORDING INTERPRETER: a shell script that writes its argv, its
;; environment, the source's basename and the source's contents to files
;; outside the run root, then exits 0 -- whatever its first argument is: the
;; launcher rows hand it "--env" there, which no command below reads as an
;; option.
(define recorders 0)
(define (make-recorder!)
  (set! recorders (+ recorders 1))
  (let* ((base (string-append outside "/rec-" (number->string recorders)))
         (script (string-append base ".sh")))
    (put! script
          (string-append
            "#!/bin/sh\n"
            "{ for a in \"$@\"; do printf '%s\\n' \"$a\"; done; } > " (quoted (string-append base ".argv")) "\n"
            "env > " (quoted (string-append base ".env")) "\n"
            "pwd -P > " (quoted (string-append base ".pwd")) "\n"
            "printf '%s\\n' \"${1##*/}\" > " (quoted (string-append base ".base")) "\n"
            "cat < \"$1\" > " (quoted (string-append base ".src")) " 2>/dev/null\n"
            "exit 0\n"))
    (sh "chmod 755 " (quoted script))
    base))
(define (rec-script base) (string-append base ".sh"))
(define (rec-lines base ext)
  (let ((t (text-of-file (string-append base ext))))
    (if (string=? t "") '() (let ((ls (lines-of t))) (if (and (pair? ls) (string=? (car (last-pair ls)) "")) (reverse (cdr (reverse ls))) ls)))))
(define (rec-ran? base) (file-exists? (string-append base ".argv")))
(define (rec-env base)
  (map (lambda (l)
         (let loop ((i 0))
           (cond ((= i (string-length l)) (cons l #f))
                 ((char=? (string-ref l i) #\=) (cons (substring l 0 i) (substring l (+ i 1) (string-length l))))
                 (else (loop (+ i 1))))))
       (rec-lines base ".env")))
(define (rec-names base) (map car (rec-env base)))
(define (rec-value base name) (let ((p (assoc name (rec-env base)))) (and p (cdr p))))
(define sh-own-names '("PATH" "HOME" "LANG" "PWD" "SHLVL" "_" "OLDPWD"))
;; EVERY NAME THE LAUNCHER KEEPS IS THERE -- PATH, HOME and LANG, as far as
;; this fixture's own environment has them -- and nothing outside `allowed`.
(define kept-names (filter getenv '("PATH" "HOME" "LANG")))
(define (names-within? names allowed)
  (and (for-all (lambda (k) (member k names)) kept-names)
       (for-all (lambda (n) (member n allowed)) names)
       #t))
(define (theourgia-name? n) (and (>= (string-length n) 10) (string=? (substring n 0 10) "THEOURGIA_")))
(define SRC-C "(import (lib a)) (display (f))")
(let* ((r (make-recorder!))
       (a (ask (with-var (string-append "((argv (\"" (rec-script r) "\" \"{file}\")))")) SC SRC-C "--lang" "chez"))
       (pwd (let ((l (rec-lines r ".pwd"))) (and (pair? l) (car l)))))
  (want "C3 an argv-only override: the operator's argv runs; the environment is PATH, HOME, LANG and the table's pairs, {dir} expanded to the resolved tree/ ahead of the caller's directories; no THEOURGIA_ name; the source is __eval.ss with its text"
        (list (head-of a) (clause-of a 'exit)
              (let ((l (rec-lines r ".argv"))) (and (= 1 (length l)) (ends-with? (car l) "/source/__eval.ss")))
              (names-within? (rec-names r) (append sh-own-names '("CHEZSCHEMELIBDIRS" "CHEZSCHEMELIBEXTS")))
              (exists theourgia-name? (rec-names r))
              (let ((v (rec-value r "CHEZSCHEMELIBDIRS")))
                (and v pwd (prefix? v (string-append pwd ":")) (> (string-length v) (+ 1 (string-length pwd)))
                     (ends-with? pwd "/tree")))
              (rec-value r "CHEZSCHEMELIBEXTS")
              (rec-lines r ".base") (text-of-file (string-append r ".src")))
        (list 'ok '(exit 0)
              #t #t #f #t ".sc:.ss:.sls:.scm" '("__eval.ss") SRC-C)))
(let* ((r (make-recorder!))
       (a (ask (with-var (string-append "((argv (\"" (rec-script r) "\" \"{file}\")) (source-name \"run.ss\"))")) SC SRC-C "--lang" "chez")))
  (want "C3b a source-name override: the source is written as run.ss, {file} names it, and it holds the source's text"
        (list (head-of a) (rec-lines r ".base") (text-of-file (string-append r ".src")))
        (list 'ok '("run.ss") SRC-C)))
;; THE TABLE'S argv IS RECORDED AS A WHOLE: a recorder named `scheme` is put
;; first on PATH, which is where the launcher looks the interpreter up, while
;; THEOURGIA_SCHEME keeps the launcher itself on the real Chez.
(let* ((r (make-recorder!))
       (bin (string-append r "-bin"))
       (_ (sh "mkdir -p " (quoted bin) " && cp " (quoted (rec-script r)) " " (quoted (string-append bin "/scheme"))))
       (a (ask (string-append (with-var "((env ((\"FOO\" \"bar\"))))")
                              " THEOURGIA_SCHEME=" (quoted scheme-path) " PATH=" (quoted bin) ":\"$PATH\"")
               SC SRC-C "--lang" "chez")))
  (want "C3c an env-only override: the table's argv (scheme --script <file>) and source name stay, recorded whole; the environment is the override's whole, so the table's pairs are gone"
        (list (head-of a)
              (let ((l (rec-lines r ".argv")))
                (and (= 2 (length l)) (string=? (car l) "--script") (ends-with? (cadr l) "/source/__eval.ss")))
              (rec-value r "FOO") (rec-value r "CHEZSCHEMELIBDIRS") (rec-value r "CHEZSCHEMELIBEXTS"))
        (list 'ok #t "bar" #f #f)))
(let* ((r (make-recorder!))
       (a (ask (with-var (string-append "((argv (\"" (rec-script r) "\" \"{file}\")) (env ()))")) SC SRC-C "--lang" "chez")))
  (want "C3d an explicit (env ()) is an override, not absence: the interpreter has PATH, HOME and LANG only"
        (list (head-of a) (names-within? (rec-names r) sh-own-names))
        (list 'ok #t)))
(let* ((r (make-recorder!))
       (a (ask (with-var (string-append "((argv (\"" (rec-script r) "\" \"{file}\")) (env ((\"CHEZSCHEMELIBEXTS\" \".sls\"))))"))
               SC SRC-C "--lang" "chez")))
  (want "C3e an override naming only the extensions pair replaces env whole: CHEZSCHEMELIBDIRS is absent"
        (list (head-of a) (rec-value r "CHEZSCHEMELIBDIRS") (rec-value r "CHEZSCHEMELIBEXTS"))
        (list 'ok #f ".sls")))

;; {libdirs} AS A WHOLE argv ARGUMENT is the same text the default env puts
;; after the projection in CHEZSCHEMELIBDIRS, so the two are read against
;; each other.
(let* ((r (make-recorder!))
       (a (ask (with-var (string-append "((argv (\"" (rec-script r) "\" \"{file}\" \"{libdirs}\")))")) SC SRC-C "--lang" "chez"))
       (pwd (let ((l (rec-lines r ".pwd"))) (and (pair? l) (car l))))
       (dirs (rec-value r "CHEZSCHEMELIBDIRS")))
  (want "C3f {libdirs} as a whole argv argument is the launcher's library path: the text the default env puts after the projection"
        (list (head-of a)
              (let ((l (rec-lines r ".argv")))
                (and (= 2 (length l)) (string? dirs) pwd (not (string=? (cadr l) "{libdirs}"))
                     (equal? dirs (string-append pwd ":" (cadr l))))))
        (list 'ok #t)))

;; ---- C4: what the operator's variable may not be; nothing runs ----------------------
;; Each value names a recorder as argv[0] where it can, so a value wrongly
;; taken would run it and leave its record.
(define (config-refusal a) (list (head-of a) (clause-of a 'reason) (clause-of a 'variable) (clause-of a 'detail)))
(define (refused-with detail)
  (list '(error bad-request) '(reason runner-config-invalid) '(variable "THEOURGIA_RUNNER_CHEZ") (list 'detail detail)))
(for-each
  (lambda (c)
    (let* ((r (make-recorder!))
           (text ((cadr c) (rec-script r)))
           (a (ask (with-var text) SC SRC-C "--lang" "chez")))
      (want (string-append "C4 " (car c) ": runner-config-invalid naming the variable and " (format "~s" (caddr c)) "; the recorder never ran")
            (list (config-refusal a) (rec-ran? r))
            (list (refused-with (caddr c)) #f))))
  (list (list "not a datum" (lambda (p) (string-append "((argv (\"" p "\"")) 'not-one-datum)
        (list "two datums" (lambda (p) (string-append "((argv (\"" p "\" \"{file}\"))) ()")) 'not-one-datum)
        (list "an env name holding =" (lambda (p) (string-append "((argv (\"" p "\" \"{file}\")) (env ((\"A=B\" \"x\"))))")) '(field env))
        (list "PATH named in env" (lambda (p) (string-append "((argv (\"" p "\" \"{file}\")) (env ((\"PATH\" \"/x\"))))")) '(field env))
        (list "an argv holding NUL" (lambda (p) (string-append "((argv (\"" p "\" \"a\\x0;b\")))")) '(field argv))
        (list "an empty interpreter name" (lambda (p) "((argv (\"\" \"{file}\")))") '(field argv))
        (list "a field no runner has" (lambda (p) (string-append "((argv (\"" p "\" \"{file}\")) (cmd \"x\"))")) '(field cmd))
        (list "an empty override" (lambda (p) "()") '(field runner))))
(let ((a (ask (string-append ON " THEOURGIA_RUNNER_JAVASCRIPT=" (quoted "((")) S "console.log(1)" "--lang" "javascript")))
  (want "C4c THEOURGIA_RUNNER_JAVASCRIPT is not read: garbage in it leaves node's runner as the table has it (node present)"
        (list (have? "node") (head-of a) (stdout-of a))
        (list #t 'ok "1\n")))
(let ((a (ask (chez-var "((") SC SRC-C "--lang" "chez")))
  (want "C4d without THEOURGIA_RUNNERS, --lang chez answers runners-disabled before its configuration is read"
        a '(error runners-disabled)))
;; THE CONFIGURATION IS READ BEFORE ADMISSION: with the one slot held, an
;; invalid value is answered as itself, not as eval-busy; and on a fresh run
;; root it makes nothing, the admission directory included.
(want "C4e with the one slot held, an invalid THEOURGIA_RUNNER_CHEZ answers runner-config-invalid at once, not eval-busy"
      (let* ((r (fresh-run! "slots-chez"))
             (ready (string-append r ".ready")) (go (string-append r ".go"))
             (a (bg (slots-env r "THEOURGIA_EVAL_SLOTS=1 " ON) SL (hold-until ready go)
                    "--lang" "shell" "--timeout-ms" "60000"))
             (ready-seen (within 15000 (lambda () (file-exists? ready))))
             (x (ask (slots-env r "THEOURGIA_EVAL_SLOTS=1 " ON " " (chez-var "((")) SC SRC-C
                     "--lang" "chez" "--timeout-ms" "500")))
        (sh "touch " (quoted go))
        (ended-at (car a) 20000)
        (list (and ready-seen #t) (config-refusal x)))
      (list #t (refused-with 'not-one-datum)))
(want "C4f on a fresh run root, an invalid THEOURGIA_RUNNER_CHEZ is refused and the run root stays empty (no admission directory)"
      (let* ((r (fresh-run! "chez-noadmit"))
             (x (ask (slots-env r ON " " (chez-var "((")) SC SRC-C "--lang" "chez")))
        (list (config-refusal x) (directory-list r)))
      (list (refused-with 'not-one-datum) '()))

;; ---- V: the other languages' variables are whole runners ---------------------------
;; Each value names a recorder as argv[0], and the stand-ins are first on
;; PATH, so a value wrongly taken runs the recorder and a value wrongly
;; ignored runs the stand-in, which prints.
(define V-LANGS '("typescript" "go" "rust" "c" "java"))
(define (variable-of lang) (string-append "THEOURGIA_RUNNER_" (string-upcase lang)))
(define (lang-var lang text) (string-append (with-stubs) " " (variable-of lang) "=" (quoted text)))
(define (refused-by variable detail)
  (list '(error bad-request) '(reason runner-config-invalid) (list 'variable variable) (list 'detail detail)))
(for-each
  (lambda (lang)
    (let* ((r (make-recorder!))
           (a (ask (lang-var lang (string-append "((argv (\"" (rec-script r) "\" \"{file}\")) (source-name \"run.src\"))"))
                   S "x\n" "--lang" lang)))
      (want (string-append "V1 " lang ": a whole runner in " (variable-of lang)
                           " is what runs: ok, (exit 0), the recorder ran on the source written as run.src, and no stand-in printed")
            (list (head-of a) (clause-of a 'exit) (rec-lines r ".base") (rec-lines r ".src") (stdout-of a))
            (list 'ok '(exit 0) '("run.src") '("x") ""))))
  V-LANGS)
;; NOTHING OF THE TABLE'S IS MERGED IN: argv alone lacks a source-name, and
;; the table's is not taken to fill it.
(for-each
  (lambda (lang)
    (let* ((r (make-recorder!))
           (a (ask (lang-var lang (string-append "((argv (\"" (rec-script r) "\" \"{file}\")))")) S "x\n" "--lang" lang)))
      (want (string-append "V2 " lang ": argv alone answers runner-config-invalid naming " (variable-of lang)
                           " and (field source-name); nothing ran")
            (list (config-refusal a) (rec-ran? r) (stdout-of a))
            (list (refused-by (variable-of lang) '(field source-name)) #f ""))))
  V-LANGS)
(for-each
  (lambda (c)
    (let* ((r (make-recorder!))
           (lang (car c))
           (a (ask (lang-var lang ((caddr c) (rec-script r))) S "x\n" "--lang" lang)))
      (want (string-append "V3 " lang ", " (cadr c) ": runner-config-invalid naming " (variable-of lang)
                           " and " (format "~s" (cadddr c)) "; nothing ran")
            (list (config-refusal a) (rec-ran? r) (stdout-of a))
            (list (refused-by (variable-of lang) (cadddr c)) #f ""))))
  (list (list "rust" "not a datum" (lambda (p) (string-append "((argv (\"" p "\"")) 'not-one-datum)
        (list "go" "two datums" (lambda (p) (string-append "((argv (\"" p "\" \"{file}\")) (source-name \"a\")) ()")) 'not-one-datum)
        (list "java" "env alone" (lambda (p) "((env ()))") '(field argv))
        (list "java" "an empty runner" (lambda (p) "()") '(field argv))
        (list "c" "a source-name holding /" (lambda (p) (string-append "((argv (\"" p "\" \"{file}\")) (source-name \"a/b\"))")) '(field source-name))
        (list "typescript" "a field no runner has" (lambda (p) (string-append "((argv (\"" p "\" \"{file}\")) (source-name \"a\") (cmd \"x\"))")) '(field cmd))
        (list "typescript" "PATH named in env" (lambda (p) (string-append "((argv (\"" p "\" \"{file}\")) (source-name \"a\") (env ((\"PATH\" \"/x\"))))")) '(field env))))
(let ((a (ask (string-append (with-stubs) " THEOURGIA_RUNNER_GO=") S "x\n" "--lang" "go")))
  (want "V4 CONTROL an empty THEOURGIA_RUNNER_GO is unset: the table's runner runs the stand-in"
        (list (head-of a) (stand-in-printed "go" "x\n" (stdout-of a)))
        (list 'ok #t)))
(want "V5 on a fresh run root, an invalid THEOURGIA_RUNNER_JAVA is refused and the run root stays empty (no admission directory)"
      (let* ((r (fresh-run! "java-noadmit"))
             (x (ask (slots-env r (lang-var "java" "((")) S "x\n" "--lang" "java")))
        (list (config-refusal x) (directory-list r)))
      (list (refused-by "THEOURGIA_RUNNER_JAVA" 'not-one-datum) '()))

;; ---- C5: the launcher never loads from the projection; the interpreter does ----------
(define marker-c5a (string-append outside "/c5a"))
(define marker-c5b (string-append outside "/c5b"))
(define SC-PLANT
  (make-store! "chez3"
    (list (cons "theourgia/ffi.sc"
                (string-append
                  "(library (theourgia ffi) (export path-executable? isolate-evaluation! exec-argv-env!) (import (chezscheme))\n"
                  "  (define loaded (call-with-output-file \"" marker-c5a "\" (lambda (p) (display \"ran\" p)) 'replace))\n"
                  "  (define refused (raise 'planted-ffi))\n"
                  "  (define (path-executable? p) #f) (define (isolate-evaluation! s) (values 0 0 0)) (define (exec-argv-env! a b c) #f))\n"))
          (cons "lib/probe.sc"
                (string-append
                  "(library (lib probe) (export p) (import (chezscheme))\n"
                  "  (define p (begin (call-with-output-file \"" marker-c5b "\" (lambda (o) (display \"ran\" o)) 'replace) 1)))\n")))))
(let ((a (ask ON SC-PLANT "(display 1)" "--lang" "chez")))
  (want "C5a a planted theourgia/ffi.sc under tree/ is not what the launcher loads: ok, (exit 0), stdout 1, and its load marker is absent"
        (list (head-of a) (clause-of a 'exit) (stdout-of a) (file-exists? marker-c5a))
        (list 'ok '(exit 0) "1" #f)))
(let ((a (ask ON SC-PLANT "(import (lib probe)) (display p)" "--lang" "chez")))
  (want "C5b the interpreter reads the projection as a library directory: a projected library the source imports is loaded (its marker is written)"
        (list (head-of a) (clause-of a 'exit) (stdout-of a) (file-exists? marker-c5b))
        (list 'ok '(exit 0) "1" #t)))

;; ---- C7: the extensions and directories the interpreter reads ----------------------
(let ((a (ask ON SC "(write (library-extensions))" "--lang" "chez")))
  (want "C7 the interpreter's own (library-extensions): .sc, .ss, .sls, .scm, each with the default object .so"
        (list (head-of a) (datum-of (stdout-of a)))
        (list 'ok '((".sc" . ".so") (".ss" . ".so") (".sls" . ".so") (".scm" . ".so")))))
(let ((a (ask (with-var "((env ((\"CHEZSCHEMELIBDIRS\" \"{dir}:{libdirs}\") (\"CHEZSCHEMELIBEXTS\" \".sls\"))))") SC SRC-C "--lang" "chez")))
  (want "C7 extensions set to .sls only: lib/a.sc is not found; a non-zero exit, and stderr names the library"
        (list (head-of a) (equal? (clause-of a 'exit) '(exit 0)) (has-substring? (stderr-of a) "library (lib a) not found"))
        (list 'ok #f #t)))
;; THE DIRECTORIES, READ BY THE INTERPRETER: the calling process is given a
;; library pair whose source and object directories differ, so a runner that
;; dropped the object side of {libdirs} is seen.
(define objdir (string-append root "/objects"))
(sh "mkdir -p " (quoted objdir))
(let ((a (ask (string-append ON " CHEZSCHEMELIBDIRS=" (quoted (string-append libdir "::" objdir))) SC
              "(write (list (current-directory) (library-directories)))" "--lang" "chez")))
  (want "C7b the interpreter's (library-directories): the projection first, as (tree . tree), then the calling process's pair with both sides intact"
        (let ((d (datum-of (stdout-of a))))
          (list (head-of a)
                (and (pair? d) (string? (car d)) (pair? (cdr d))
                     (equal? (cadr d) (list (cons (car d) (car d)) (cons libdir objdir))))))
        (list 'ok #t)))

;; ---- C8: the limits are a runner's ------------------------------------------------
(let ((a (ask ON SC "(let loop () (loop))" "--lang" "chez" "--timeout-ms" "1500")))
  (want "C8 time: a loop past --timeout-ms answers eval-limit (resource time)"
        (list (head-of a) (clause-of a 'resource)) (list '(error eval-limit) '(resource time))))
;; 400 characters of two bytes each: 800 bytes over a 600 quota, while the
;; characters (400) are under it, so a quota in characters would let it pass.
(let ((a (ask ON SC "(display (make-string 400 #\\xe9))" "--lang" "chez" "--output-bytes" "600")))
  (want "C8 output in bytes: 400 two-byte characters stop at a 600-byte quota"
        (list (head-of a) (clause-of a 'resource)) (list '(error eval-limit) '(resource output))))

;; ---- C9: --working ------------------------------------------------------------------
(define lib-a-id (block-holding SC "define (f) 42"))
(cli SC "write" "--writer" "wc" lib-a-id "(library (lib a) (export f) (import (rnrs)) (define (f) 43))\n")
(let ((w (ask ON SC SRC-C "--lang" "chez" "--working" "--writer" "wc"))
      (plain (ask ON SC SRC-C "--lang" "chez")))
  (want "C9 --working: wc's draft of lib/a.sc answers 43; the committed state still answers 42"
        (list (and lib-a-id #t) (stdout-of w) (stdout-of plain))
        (list #t "43" "42")))

;; ---- C6: an env field on another language's runner (in process) ---------------------
;; THE REGISTRATION IS IN THE PROCESS THAT EVALUATES: a child program that
;; registers javascript's entry with an env field and runs the evaluation
;; itself, as core.sc does, under the scheduler; the fixture's own `ask`
;; starts a fresh core.sc, which never sees a registration made elsewhere.
(define launcher-abs (string-append (current-directory) "/../eval-runner-exec.sc"))
(define (in-process-eval name lang entry-edit source)
  (let ((child (string-append root "/" name ".sc")))
    (put! child
          (string-append
            "(import (chezscheme) (theourgia languages) (theourgia eval-runner) (theourgia sched))\n"
            entry-edit "\n"
            "(start-scheduler (lambda ()\n"
            "  (write (guard (e (#t 'RAISED))\n"
            "    (run-foreign-eval \"" SC "\" \"" lang "\" " (format "~s" source) "\n"
            "      (list (cons 'working? #f) (cons 'timeout-ms 10000) (cons 'memory-bytes 268435456)\n"
            "            (cons 'output-bytes 65536) (cons 'scheme " (format "~s" scheme-path) ")\n"
            "            (cons 'launcher " (format "~s" launcher-abs) ")))))\n"
            "  (newline) (exit 0)))\n"))
    (datum-of (sh-out "THEOURGIA_LOCAL=1 THEOURGIA_HOME=" (quoted home) " THEOURGIA_RUN=" (quoted run) " "
                      (quoted scheme-path) " --script " (quoted child) " 2>/dev/null"))))
(let ((a (in-process-eval "c6" "javascript"
           (string-append
             "(register-language!\n"
             "  (map (lambda (f) (if (eq? (car f) 'runner)\n"
             "                       '(runner ((argv (\"node\" \"{file}\")) (source-name \"__eval.mjs\") (env ((\"GREETING\" \"hi {dir}\")))))\n"
             "                       f))\n"
             "       (language-for-name \"javascript\")))")
           "console.log(process.env.GREETING)")))
  (want "C6 javascript given an env field in the evaluating process: node sees the pair, {dir} replaced inside the value by the tree/ it runs in (node present)"
        (list (have? "node") (head-of a)
              (let ((o (stdout-of a))) (and (prefix? o "hi /") (ends-with? o "/tree\n"))))
        (list #t 'ok #t)))

;; THE CONTROL: node through the table's own entry, with no env field, sees
;; the kept names and nothing that came from outside it. On macOS node adds
;; __CF_USER_TEXT_ENCODING to its own environment as it starts (measured: the
;; shell row L6, handed the same environment, never shows it), as sh adds PWD
;; and SHLVL; it is allowed, and nothing else is.
(define node-own-names '("__CF_USER_TEXT_ENCODING"))
(let* ((a (ask ON S "console.log(Object.keys(process.env).sort().join(' '))" "--lang" "javascript"))
       (names (let split ((cs (string->list (stdout-of a))) (cur '()) (acc '()))
                (cond ((null? cs) (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
                      ((memv (car cs) '(#\space #\newline))
                       (split (cdr cs) '() (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
                      (else (split (cdr cs) (cons (car cs) cur) acc))))))
  (want "C6 control: node with the table's entry (no env field) sees every kept name (PATH, HOME, LANG as this fixture has them) and nothing else but node's own (node present)"
        (list (have? "node") (head-of a) (names-within? names (append kept-names node-own-names)))
        (list #t 'ok #t)))

;; ---- the launcher's arguments, read by the launcher itself --------------------------
;; The launcher is started directly, as the supervisor starts it, with a
;; recording interpreter; `go` on its stdin.
;; -> (status . stdout): the status as text, and what the launcher printed
;; before it became the interpreter -- (ready <pgid>) or nothing.
(define (launcher-run . args)
  (let* ((out (string-append root "/launcher-out"))
         (status (sh-out "printf 'go\\n' | " (quoted scheme-path) " --script " (quoted launcher-abs) " "
                         (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                         " > " (quoted out) " 2>/dev/null; echo $?")))
    (cons status (text-of-file out))))
(let* ((r (make-recorder!))
       (status (launcher-run "5" "--env" "A=1" "--env" "B=" "--env" "C=x=y z --" "--" (rec-script r) "--env" "--" "tail")))
  (want "G1 the launcher: pairs up to the first standalone --, each split at its first =, an empty value kept; everything after -- is the interpreter's verbatim"
        (list (car status) (prefix? (cdr status) "(ready ")
              (rec-lines r ".argv") (rec-value r "A") (rec-value r "B") (rec-value r "C"))
        (list "0\n" #t '("--env" "--" "tail") "1" "" "x=y z --")))
(for-each
  (lambda (c)
    (let ((r (make-recorder!)))
      (want (string-append "G2 the launcher, " (car c) ": exit " (cadr c) " before ready (nothing printed), and the interpreter never ran")
            (let ((s (apply launcher-run (map (lambda (a) (if (equal? a "REC") (rec-script r) a)) (caddr c)))))
              (list (car s) (cdr s) (rec-ran? r)))
            (list (string-append (cadr c) "\n") "" #f))))
  (list (list "a pair with no =" "3" '("5" "--env" "NOEQ" "--" "REC"))
        (list "a pair with an empty name" "3" '("5" "--env" "=v" "--" "REC"))
        (list "no -- at all (usage)" "2" '("5" "--env" "A=1" "REC"))
        (list "--env with nothing after it (usage)" "2" '("5" "--env"))))

;; ---- the table's checks at construction ---------------------------------------------
(define (child-datum name program)
  (let ((child (string-append root "/" name ".sc")))
    (put! child program)
    (datum-of (sh-out (quoted scheme-path) " --script " (quoted child) " 2>/dev/null"))))
(want "T1 every runner of the built-in table passes runner-valid?, and nine languages have one"
      (child-datum "t1"
        (string-append
          "(import (chezscheme) (theourgia languages))\n"
          "(write (let ((rs (filter values (map language-runner (language-table)))))\n"
          "  (list (for-all runner-valid? rs) (length rs)\n"
          "        (map (lambda (e) (language-property e 'lang #f)) (filter language-runner (language-table))))))\n"))
      '(#t 9 ("javascript" "typescript" "python" "go" "rust" "c" "java" "shell" "chez")))
(want "T2 a table built with an entry whose runner the checks refuse stops at construction, naming the entry and the field"
      (child-datum "t2"
        (string-append
          "(import (chezscheme) (theourgia languages))\n"
          "(write (guard (e ((assertion-violation? e) (cons 'REFUSED (condition-irritants e))))\n"
          "  (checked-catalogue (list '((lang \"bad\") (extensions ()) (def-heads ())\n"
          "                             (runner ((argv (\"\" \"{file}\")) (source-name \"x.ss\"))))))\n"
          "  'BUILT))\n"))
      '(REFUSED "bad" argv))

;; ---- a projection directory the library variable cannot carry -------------------------
;; A run root whose path holds ":": a runner without {dir} in a library
;; variable (shell) runs there; chez is refused before anything is exported,
;; and the run root keeps what it held (the admission's directory) with no
;; eval-* left.
(define run-colon (string-append root "/run:colon"))
(define (entries-of dir) (if (file-exists? dir) (list-sort string<? (directory-list dir)) '()))
(let* ((a (ask (string-append ON " THEOURGIA_RUN=" (quoted run-colon)) S "echo shell\n" "--lang" "shell"))
       (before (entries-of run-colon))
       (b (ask (string-append ON " THEOURGIA_RUN=" (quoted run-colon)) SC SRC-C "--lang" "chez"))
       (after (entries-of run-colon))
       (resolved (pwd-p run-colon))
       (d (let ((c (clause-of b 'directory))) (and c (cadr c)))))
  (want "U1 a run root holding a colon: shell runs; chez answers projection-directory-unrepresentable naming <resolved root>/eval-<token>/tree, and the run root is as before with no eval-*"
        (list (head-of a) (head-of b) (clause-of b 'reason)
              (and (string? d) (prefix? d (string-append resolved "/eval-")) (ends-with? d "/tree"))
              (equal? before after) (eval-dirs-of run-colon))
        (list 'ok '(error spawn-refused) '(reason projection-directory-unrepresentable) #t #t '())))

;; ---- the documents --------------------------------------------------------------------
(define readme (squash (text-of-file "../README.md")))
(define (library-defines file name)
  (let loop ((fs (forms-of file)))
    (cond ((null? fs) #f)
          ((and (pair? (car fs)) (eq? (caar fs) 'library))
           (or (find (lambda (x) (and (pair? x) (eq? (car x) 'define) (pair? (cdr x)) (equal? (cadr x) name))) (cdar fs))
               (loop (cdr fs))))
          (else (loop (cdr fs))))))
(define RUNNER-VARIABLES
  '("THEOURGIA_RUNNER_CHEZ" "THEOURGIA_RUNNER_C" "THEOURGIA_RUNNER_RUST" "THEOURGIA_RUNNER_GO"
    "THEOURGIA_RUNNER_JAVA" "THEOURGIA_RUNNER_TYPESCRIPT"))
(want "D1 docs: README's environment table has a row read by eval-runner.sc for each runner variable; runner-variables reads each by a literal getenv"
      (let ((d (library-defines "../eval-runner.sc" '(runner-variables))))
        (map (lambda (v)
               (list v
                     (and (exists (lambda (l) (prefix? l (string-append "| `" v "` | `eval-runner.sc` |"))) (environment-table-rows)) #t)
                     (and d (holds-datum? (cddr d) (list 'getenv v)))))
             RUNNER-VARIABLES))
      (map (lambda (v) (list v #t #t)) RUNNER-VARIABLES))
(want "D3 docs: README states the five variables are whole runners, argv alone is refused rather than completed, the refusal names its own variable, and the tsx example"
      (map (lambda (t) (has-substring? readme (squash t)))
           (list "Unlike chez's, the value is a WHOLE runner"
                 "a value naming `argv` alone is refused with `(detail (field source-name))`, not completed from the table."
                 "naming its own language's variable"
                 "THEOURGIA_RUNNER_TYPESCRIPT='((argv (\"tsx\" \"{file}\")) (source-name \"__eval.ts\"))'"))
      '(#t #t #t #t))
(want "D2 docs: README states chez's default runner, the field-wise replacement, empty is unset, an interpreter and a compiler example with {file} as $1, the reach sentence in place of the old one, and the argv placeholders"
      (map (lambda (t) (has-substring? readme (squash t)))
           (list "(runner ((argv (\"scheme\" \"--script\" \"{file}\")) (source-name \"__eval.ss\") (env ((\"CHEZSCHEMELIBDIRS\" \"{dir}:{libdirs}\") (\"CHEZSCHEMELIBEXTS\" \".sc:.ss:.sls:.scm\")))))"
                 "The fields it names replace the table's, field by field, and a named field replaces the table's whole"
                 "An empty value is unset."
                 "THEOURGIA_RUNNER_CHEZ='((argv (\"petite\" \"--script\" \"{file}\")))'"
                 "THEOURGIA_RUNNER_CHEZ='((argv (\"sh\" \"-c\" \"goeteia build \\\"$1\\\" && ./a.out\" \"compile\" \"{file}\")))'"
                 "nothing the projection holds is ever loaded as a library by the launcher; a runner configured to search it, as `chez` is, loads from it after exec, and that is the runner's reach."
                 "In `argv`, `{file}`, `{dir}` and `{libdirs}` -- the launcher's own library path, as written for it -- are replaced only as whole arguments"
                 "is ever loaded as a library."))
      '(#t #t #t #t #t #t #t #f))
;; BEFORE THE EXPORT, NOT AFTER IT: a store that cannot be exported (L8's) is
;; refused for the directory; a check made after the export would answer the
;; exporter's projection-failed instead.
(let ((a (ask (string-append ON " THEOURGIA_RUN=" (quoted run-colon)) B SRC-C "--lang" "chez")))
  (want "U1b under the same run root, a store the exporter refuses answers projection-directory-unrepresentable: the check precedes the export"
        (list (head-of a) (clause-of a 'reason) (eval-dirs-of run-colon))
        (list '(error spawn-refused) '(reason projection-directory-unrepresentable) '())))
(want "C every chez run above left no eval-* directory behind" (eval-dirs) '())

(sh "chmod -R u+rwX " (quoted root) " 2>/dev/null; rm -rf " (quoted root))
(printf "\nskipped: ~a\n" skipped)
(printf "\n~a failures\nrows: ~a\neval-lang complete\n" bad rows)
(exit (if (= bad 0) 0 1))
