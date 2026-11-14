;; Copyright 2018 - 2026 guenchi
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;     http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; NO HOME UNDER /tmp (ffi.sc machine-home, client.sc run-root and
;; missing-home, and the check at the start of four programs).
;;
;;   NH-0  read from the product's sources: no string datum is "/tmp" or
;;         begins "/tmp/", and the string "HOME" is read in exactly five
;;         places, so a new reader of HOME is a red census
;;   NH-1  in children that only ask and print: with no HOME (absent or
;;         empty) and no explicit variable, each definition raises its
;;         no-home datum; an explicit variable, relative or absolute, is
;;         answered as given; a HOME as before; missing-home names the
;;         first missing home, the machine home's first
;;   NH-2  with no home, each of the four programs, started directly,
;;         answers the machine home's datum on its stream with its status,
;;         for a read, a write, init, its own verbs, the local route, an
;;         explicit --socket, serve (also under --detach) and the MCP
;;         shell's start; init leaves nothing, serve no socket, lock,
;;         private namespace or log, and the core under --frame answers
;;         before it reads the frame
;;   NH-3  with one home only, each program names the other, and the home
;;         that is there gains nothing; a thin client with THEOURGIA_RUN
;;         alone pointing at a live daemon's run root reaches it with
;;         nothing, where one with both homes is dispatched
;;   NH-4  HOME absent and both variables set: the store verbs, a relative
;;         run root, eval, eval --lang with a foreign runner, and the MCP
;;         shell with its daemon and its core child all work
;;   NH-6  help and a verb's usage are answered with no home, status 0; a
;;         --help for a verb with no usage meets the core's check (status 1)
;;
;; NEVER: NOTHING HERE RUNS WITH A FALLBACK IN PLACE. On the old tree, or
;; under a mutant that puts a fallback back, a program with no home makes
;; or uses /tmp/.theourgia, which on a shared machine may be another
;; build's live home. NH-0 and NH-1 make nothing, and every other row runs
;; only when both are green; each of those cells also reads the existence
;; and modification time of /tmp/.theourgia before and after and wants
;; them equal.
(import (chezscheme) (only (theourgia client) request-frame))

(define bad 0)
(define rows-run 0)
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) (caught x)))))))

(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define (absolute p) (if (and (> (string-length p) 0) (char=? (string-ref p 0) #\/)) p (string-append (current-directory) "/" p)))
(define tree-dir (absolute (string-append script-dir "/..")))
(define root
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (string-append (absolute (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test"))
                   "/no-tmp-home-" (number->string (get-process-id)))))
;; Sockets live under the runner's short socket directory, so a run root's
;; socket path fits in sun_path.
(define sock-root
  (let ((v (getenv "THEOURGIA_TEST_SOCK")))
    (string-append (absolute (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                   "/nth" (number->string (get-process-id)))))
(define (quoted s)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list s))) "'"))
(define (sh . xs) (system (apply string-append xs)))
(sh "rm -rf " (quoted root) " " (quoted sock-root) " && mkdir -p " (quoted root) " " (quoted sock-root))

(define (file-text p)
  (if (file-exists? p) (let ((t (call-with-input-file p get-string-all))) (if (string? t) t "")) ""))
(define (put! p text) (call-with-output-file p (lambda (o) (put-string o text)) 'replace))
(define (contains? text needle)
  (let ((n (string-length needle)))
    (let loop ((i 0))
      (cond ((> (+ i n) (string-length text)) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (count-of text needle)
  (let ((n (string-length needle)))
    (let loop ((i 0) (k 0))
      (cond ((> (+ i n) (string-length text)) k)
            ((string=? (substring text i (+ i n)) needle) (loop (+ i 1) (+ k 1)))
            (else (loop (+ i 1) k))))))
(define (split-colons s)
  (let loop ((cs (string->list s)) (cur '()) (acc '()))
    (cond ((null? cs) (reverse (cons (list->string (reverse cur)) acc)))
          ((char=? (car cs) #\:) (loop (cdr cs) '() (cons (list->string (reverse cur)) acc)))
          (else (loop (cdr cs) (cons (car cs) cur) acc)))))
(define (join-colons l)
  (if (null? l) "" (fold-left (lambda (a b) (string-append a ":" b)) (car l) (cdr l))))

;; The library path, made absolute, so a child started in another directory
;; finds the same libraries.
(define libdirs
  (join-colons (map (lambda (d) (if (string=? d "") d (absolute d)))
                    (split-colons (or (getenv "CHEZSCHEMELIBDIRS") "..")))))
(define libexts (or (getenv "CHEZSCHEMELIBEXTS") ".sc::.no-obj"))

;; ---- children -------------------------------------------------------------
;; One program in a child: HOME, THEOURGIA_HOME, THEOURGIA_RUN and the
;; routing variables absent, then `assignments` (NAME=VALUE words, already
;; quoted), the program from this tree, its words; stdin from `input` when
;; given. -> (status stdout stderr).
(define c-n 0)
(define (child cwd assignments program words . input)
  (set! c-n (+ c-n 1))
  (let* ((base (string-append root "/c" (number->string c-n)))
         (in (if (pair? input) (begin (put! (string-append base ".in") (car input)) (string-append base ".in")) "/dev/null"))
         (rc (sh "cd " (quoted cwd) " && env -u HOME -u THEOURGIA_HOME -u THEOURGIA_RUN"
                 " -u THEOURGIA_STORE -u THEOURGIA_LOCAL -u THEOURGIA_TRACE "
                 assignments " CHEZSCHEMELIBDIRS=" (quoted libdirs) " CHEZSCHEMELIBEXTS=" (quoted libexts)
                 " perl -e 'alarm 120; exec @ARGV' scheme --script " (quoted (string-append tree-dir "/" program)) " "
                 (apply string-append (map (lambda (w) (string-append (quoted w) " ")) words))
                 "> " (quoted (string-append base ".out")) " 2> " (quoted (string-append base ".err"))
                 " < " (quoted in))))
    (list rc (file-text (string-append base ".out")) (file-text (string-append base ".err")))))
(define (assign name value) (string-append name "=" (quoted value) " "))

;; ---- NH-0, from the sources ------------------------------------------------
;; The product's sources: every .sc and .ss at the tree's root, and every .sc
;; in mcp/ and tools/. Each is read as data by the reader, so a comment is
;; not a datum and a string is exactly the string.
(define (suffix? s x) (let ((n (string-length s)) (k (string-length x))) (and (>= n k) (string=? (substring s (- n k) n) x))))
(define (sources-in dir rel exts)
  (map (lambda (f) (if (string=? rel "") f (string-append rel "/" f)))
       (filter (lambda (f) (exists (lambda (x) (suffix? f x)) exts))
               (list-sort string<? (directory-list (if (string=? rel "") dir (string-append dir "/" rel)))))))
(define product-sources
  (append (sources-in tree-dir "" '(".sc" ".ss"))
          (sources-in tree-dir "mcp" '(".sc"))
          (sources-in tree-dir "tools" '(".sc"))))
(define (forms-of rel)
  (guard (e (#t 'UNREADABLE))
    (call-with-input-file (string-append tree-dir "/" rel)
      (lambda (p) (let loop ((acc '())) (let ((x (read p))) (if (eof-object? x) (reverse acc) (loop (cons x acc)))))))))
;; Every string in the forms, each with the name of the innermost
;; (define (name ...) ...) or (define name ...) around it, or #f.
(define (strings-with-definers forms)
  (let walk ((x forms) (definer #f) (out '()))
    (cond
      ((string? x) (cons (cons definer x) out))
      ((and (pair? x) (eq? (car x) 'define) (pair? (cdr x)))
       (let ((name (cond ((symbol? (cadr x)) (cadr x))
                         ((and (pair? (cadr x)) (symbol? (car (cadr x)))) (car (cadr x)))
                         (else definer))))
         (walk (cddr x) name out)))
      ((pair? x) (walk (cdr x) definer (walk (car x) definer out)))
      ((vector? x) (walk (vector->list x) definer out))
      (else out))))
(define source-strings
  (map (lambda (rel) (cons rel (let ((fs (forms-of rel))) (if (eq? fs 'UNREADABLE) 'UNREADABLE (reverse (strings-with-definers fs))))))
       product-sources))
(define (tmp-string? s) (or (string=? s "/tmp") (and (>= (string-length s) 5) (string=? (substring s 0 5) "/tmp/"))))
(printf "NH-0 product sources read: ~a\n" (length product-sources))
(want "NH-0 the product's sources were read: more than sixty files, none unreadable"
      (list (> (length product-sources) 60)
            (map car (filter (lambda (e) (eq? (cdr e) 'UNREADABLE)) source-strings)))
      (list #t '()))
(want "NH-0 no string datum in the product's sources is \"/tmp\" or begins \"/tmp/\""
      (apply append (map (lambda (e) (if (eq? (cdr e) 'UNREADABLE) '()
                                         (map (lambda (d) (list (car e) (car d) (cdr d)))
                                              (filter (lambda (d) (tmp-string? (cdr d))) (cdr e)))))
                         source-strings))
      '())
;; THE CENSUS OF "HOME": (file definer count), sorted. The two definitions
;; read it, and so do the three that pass PATH, HOME and LANG alone to a
;; foreign runner; a sixth reader, or a fallback built another way, has to
;; change this row.
(define home-census
  (let ((hits (apply append (map (lambda (e) (if (eq? (cdr e) 'UNREADABLE) '()
                                                 (map (lambda (d) (cons (car e) (car d)))
                                                      (filter (lambda (d) (string=? (cdr d) "HOME")) (cdr e)))))
                                 source-strings))))
    (let loop ((hs hits) (out '()))
      (if (null? hs)
          (list-sort (lambda (a b) (string<? (format "~s" a) (format "~s" b))) out)
          (let* ((h (car hs)) (k (length (filter (lambda (x) (equal? x h)) hits))))
            (loop (filter (lambda (x) (not (equal? x h))) hs) (cons (list (car h) (cdr h) k) out)))))))
(printf "NH-0 the readers of \"HOME\": ~s\n" home-census)
(want "NH-0 the string \"HOME\" is read in exactly five places: the two definitions and the three foreign-runner environments"
      home-census
      '(("client.sc" run-root 2) ("eval-runner-exec.sc" kept-environment 1) ("eval-runner.sc" launcher-environment 1)
        ("ffi.sc" machine-home 2) ("languages.sc" runner-env-valid? 1)))

;; ---- NH-1, the two definitions, in children that only ask ---------------
(define ask-definitions
  (string-append
    "(import (chezscheme) (only (theourgia ffi) machine-home) (only (theourgia client) run-root))\n"
    "(define (asked f) (guard (e (#t (list 'raised e))) (f)))\n"
    "(write (list (asked machine-home) (asked run-root)))\n"))
(define ask-missing
  (string-append
    "(import (chezscheme) (only (theourgia client) missing-home))\n"
    "(write (guard (e (#t (list 'raised e))) (missing-home)))\n"))
(put! (string-append root "/ask-definitions.ss") ask-definitions)
(put! (string-append root "/ask-missing.ss") ask-missing)
(define (asked script assignments)
  (set! c-n (+ c-n 1))
  (let* ((out (string-append root "/a" (number->string c-n) ".out"))
         (rc (sh "cd " (quoted root) " && env -u HOME -u THEOURGIA_HOME -u THEOURGIA_RUN " assignments
                 " CHEZSCHEMELIBDIRS=" (quoted libdirs) " CHEZSCHEMELIBEXTS=" (quoted libexts)
                 " scheme --script " (quoted (string-append root "/" script)) " > " (quoted out) " 2>/dev/null < /dev/null")))
    (if (= rc 0)
        (guard (e (#t (list 'unreadable (file-text out)))) (read (open-string-input-port (file-text out))))
        (list 'status rc))))
(define machine-datum '(error no-home (needs "HOME" "THEOURGIA_HOME")))
(define run-datum '(error no-home (needs "HOME" "THEOURGIA_RUN")))
(define both-raised (list (list 'raised machine-datum) (list 'raised run-datum)))
(want "NH-1 HOME absent, no THEOURGIA variable: each definition raises its datum"
      (asked "ask-definitions.ss" "") both-raised)
(want "NH-1 HOME empty, no THEOURGIA variable: each definition raises its datum"
      (asked "ask-definitions.ss" "HOME= ") both-raised)
(want "NH-1 HOME absent, both THEOURGIA variables empty: an empty value is unset, and each raises"
      (asked "ask-definitions.ss" "THEOURGIA_HOME= THEOURGIA_RUN= ") both-raised)
(want "NH-1 HOME absent, both variables relative: each is answered as given"
      (asked "ask-definitions.ss" "THEOURGIA_HOME=rel/h THEOURGIA_RUN=rel/r ") '("rel/h" "rel/r"))
(want "NH-1 HOME absent, both variables absolute: each is answered as given"
      (asked "ask-definitions.ss" "THEOURGIA_HOME=/abs/h THEOURGIA_RUN=/abs/r ") '("/abs/h" "/abs/r"))
(want "NH-1 a HOME and no variable: the home's .theourgia and its run, as before"
      (asked "ask-definitions.ss" "HOME=/x/y ") '("/x/y/.theourgia" "/x/y/.theourgia/run"))
(want "NH-1 a HOME and both variables empty: the HOME's, as before"
      (asked "ask-definitions.ss" "HOME=/x/y THEOURGIA_HOME= THEOURGIA_RUN= ") '("/x/y/.theourgia" "/x/y/.theourgia/run"))
(want "NH-1 missing-home: no home at all names the machine home first"
      (asked "ask-missing.ss" "") machine-datum)
(want "NH-1 missing-home: THEOURGIA_RUN alone names the machine home"
      (asked "ask-missing.ss" "THEOURGIA_RUN=/abs/r ") machine-datum)
(want "NH-1 missing-home: THEOURGIA_HOME alone names the run root"
      (asked "ask-missing.ss" "THEOURGIA_HOME=/abs/h ") run-datum)
(want "NH-1 missing-home: both variables, or a HOME, answer #f"
      (list (asked "ask-missing.ss" "THEOURGIA_HOME=/abs/h THEOURGIA_RUN=/abs/r ") (asked "ask-missing.ss" "HOME=/x/y "))
      '(#f #f))

;; ---- the gate --------------------------------------------------------------
(define gate-open (= bad 0))
(printf "the gate: ~a\n" (if gate-open "NH-0 and NH-1 green, the rest runs" "NH-0 or NH-1 red, NOTHING BELOW RUNS"))
(want "the gate: NH-0 and NH-1 are green, so no program below can run with a fallback in place" gate-open #t)

;; Around each cell: /tmp/.theourgia's existence and modification time,
;; before and after. -> (result tmp-home-unchanged) or (result (tmp-home ...)).
(define tmp-home "/tmp/.theourgia")
(define (tmp-home-state)
  (if (file-exists? tmp-home #f)
      (let ((t (file-modification-time tmp-home #f))) (list 'present (time-second t) (time-nanosecond t)))
      'absent))
(define (guarded thunk)
  (let* ((before (tmp-home-state)) (r (thunk)) (after (tmp-home-state)))
    (list r (if (equal? before after) 'tmp-home-unchanged (list 'tmp-home before after)))))
(define-syntax gated
  (syntax-rules ()
    ((_ label got expect)
     (when gate-open (want label (guarded (lambda () got)) (list expect 'tmp-home-unchanged))))))

(define H (string-append root "/home"))
(define R (string-append sock-root "/run"))
(define S (string-append root "/store"))
(define homes (string-append (assign "THEOURGIA_HOME" H) (assign "THEOURGIA_RUN" R)))
(define machine-line (string-append (format "~s" machine-datum) "\n"))
(define run-line (string-append (format "~s" run-datum) "\n"))
(define announce-line (string-append "(theourgia machine-home " H ")\n"))
(define (listing dir) (if (file-directory? dir) (list-sort string<? (directory-list dir)) 'no-directory))
(define (serve-pids store)
  (let ((out (string-append root "/pids.txt")))
    (sh "pgrep -f " (quoted (string-append "theourgiad.s[c] serve " store)) " > " (quoted out) " 2>/dev/null")
    (filter (lambda (l) (> (string-length l) 0))
            (let loop ((p (open-input-string (file-text out))) (acc '()))
              (let ((l (get-line p))) (if (eof-object? l) (reverse acc) (loop p (cons l acc))))))))
(define (stop-daemons! store)
  (for-each (lambda (pid) (sh "kill " pid " 2>/dev/null")) (serve-pids store))
  (let wait ((k 0)) (when (and (pair? (serve-pids store)) (< k 100)) (sleep (make-time 'time-duration 50000000 0)) (wait (+ k 1)))))

(when gate-open
  (sh "mkdir -p " (quoted H) " " (quoted R) " " (quoted S))
  (child root homes "core.sc" (list "init" "--store" S))
  (child root (string-append homes "THEOURGIA_LOCAL=1 ") "core.sc" (list "insert" "--title" "NH-CANARY" "--store" S)))

;; ---- NH-2: no home at all --------------------------------------------------
(define (fresh-dir name) (let ((d (string-append root "/" name))) (sh "mkdir -p " (quoted d)) d))
(define client-refusal (list 2 machine-line ""))
(gated "NH-2 thin client, a read (outline): the datum on stdout, status 2"
       (child root "" "theourgia.sc" (list "outline" "--store" S)) client-refusal)
(gated "NH-2 thin client, a write (insert)"
       (child root "" "theourgia.sc" (list "insert" "--title" "NH2" "--store" S)) client-refusal)
(gated "NH-2 thin client, init: refused, and the store's directory is left empty"
       (let ((d (fresh-dir "init-thin"))) (list (child root "" "theourgia.sc" (list "init" "--store" d)) (listing d)))
       (list client-refusal '()))
(gated "NH-2 thin client, its own verbs (eval, scope, collect)"
       (list (child root "" "theourgia.sc" (list "eval" "--store" S) "(+ 1 2)")
             (child root "" "theourgia.sc" (list "scope" "--store" S))
             (child root "" "theourgia.sc" (list "collect" "--store" S)))
       (list client-refusal client-refusal client-refusal))
(gated "NH-2 thin client, the local route (THEOURGIA_LOCAL=1)"
       (child root "THEOURGIA_LOCAL=1 " "theourgia.sc" (list "outline" "--store" S)) client-refusal)
(gated "NH-2 thin client, an explicit --socket: refused, and nothing is made at the socket's path"
       (let ((sock (string-append sock-root "/explicit.sock")))
         (list (child root "" "theourgia.sc" (list "outline" "--store" S "--socket" sock)) (file-exists? sock #f)))
       (list client-refusal #f))
(define core-refusal (list 1 machine-line ""))
(gated "NH-2 core, a read: the datum through finish, status 1"
       (child root "" "core.sc" (list "outline" "--store" S)) core-refusal)
(gated "NH-2 core, a write"
       (child root "" "core.sc" (list "insert" "--title" "NH2" "--store" S)) core-refusal)
(gated "NH-2 core, init: refused, and the store's directory is left empty"
       (let ((d (fresh-dir "init-core"))) (list (child root "" "core.sc" (list "init" "--store" d)) (listing d)))
       (list core-refusal '()))
(gated "NH-2 core, its own verbs (eval, scope, collect)"
       (list (child root "" "core.sc" (list "eval" "--store" S) "(+ 1 2)")
             (child root "" "core.sc" (list "scope" "--store" S))
             (child root "" "core.sc" (list "collect" "--store" S)))
       (list core-refusal core-refusal core-refusal))
(gated "NH-2 core, the local route and an explicit --socket"
       (list (child root "THEOURGIA_LOCAL=1 " "core.sc" (list "outline" "--store" S))
             (child root "" "core.sc" (list "outline" "--store" S "--socket" (string-append sock-root "/explicit.sock"))))
       (list core-refusal core-refusal))
(define framed-refusal
  (list 1 (format "~s\n" (list 'answer (list 'stdout machine-line) '(stderr "") '(exit 1) '(origin core))) ""))
;; A FRAME AS THE CLIENT BUILDS IT (request-frame), so the answer is the
;; check's and not a refusal of the frame
(define real-frame
  (let* ((f (request-frame S 'outline '() '())) (t (if (bytevector? f) (utf8->string f) f)))
    (if (and (> (string-length t) 0) (char=? (string-ref t (- (string-length t) 1)) #\newline)) t (string-append t "\n"))))
(gated "NH-2 core under --frame, a frame as the client builds it: the datum as the envelope's answer, status 1"
       (child root "" "core.sc" (list "--frame") real-frame)
       framed-refusal)
;; BEFORE THE FRAME IS READ: input that is no frame at all gets the same
;; answer, where a core that read it first would refuse the frame
(gated "NH-2 core under --frame, with input that is not a frame: the same datum, before the frame is read"
       (child root "" "core.sc" (list "--frame") "not a frame\n")
       framed-refusal)
;; WHAT A SERVE LEAVES BEHIND, for an explicit socket <dir>/<name>: the
;; socket, its lock <dir>/.<name>.lock and its private namespace <socket>.d
(define (serve-leftovers sock)
  (let* ((cut (let loop ((i (- (string-length sock) 1))) (if (char=? (string-ref sock i) #\/) i (loop (- i 1)))))
         (lock (string-append (substring sock 0 cut) "/." (substring sock (+ cut 1) (string-length sock)) ".lock")))
    (list (file-exists? sock #f) (file-exists? lock #f) (file-exists? (string-append sock ".d") #f))))
(gated "NH-2 serve: the datum on stdout, status 2; no socket, no lock and no private namespace at the given path, and no daemon"
       (let* ((sock (string-append sock-root "/serve.sock"))
              (r (child root "" "theourgiad.sc" (list "serve" S "--socket" sock))))
         (list r (serve-leftovers sock) (serve-pids S) (listing S)))
       (list (list 2 machine-line "") '(#f #f #f) '() (listing S)))
;; UNDER --detach the check is still before the detach: the datum on the
;; caller's stdout, and no log opened
(gated "NH-2 serve --detach --log: the datum on stdout, status 2, before it detaches: no log, no socket, no lock"
       (let* ((sock (string-append sock-root "/detach.sock"))
              (log (string-append root "/detach.log"))
              (r (child root "" "theourgiad.sc" (list "serve" S "--socket" sock "--detach" "--log" log))))
         (list r (file-exists? log #f) (serve-leftovers sock) (serve-pids S)))
       (list (list 2 machine-line "") #f '(#f #f #f) '()))
(gated "NH-2 the MCP shell: the datum on stderr, nothing on stdout, status 2"
       (child root "" "mcp/server.sc" (list "--store" S)) (list 2 "" machine-line))

;; ---- NH-3: one home only ---------------------------------------------------
(define run-only (assign "THEOURGIA_RUN" R))
(define home-only (assign "THEOURGIA_HOME" H))
;; AND THE ONE HOME THERE GAINS NOTHING: the run root's listing, or the
;; machine home's, the same before and after
(define (tree-listing dir)
  (let ((out (string-append root "/listing.txt")))
    (sh "cd " (quoted dir) " 2>/dev/null && find . | sort > " (quoted out))
    (file-text out)))
(gated "NH-3 THEOURGIA_RUN alone: each program names the machine home, and the run root gains nothing"
       (let* ((before (tree-listing R))
              (rs (list (child root run-only "theourgia.sc" (list "outline" "--store" S))
                        (child root run-only "core.sc" (list "outline" "--store" S))
                        (child root run-only "theourgiad.sc" (list "serve" S))
                        (child root run-only "mcp/server.sc" (list "--store" S)))))
         (list rs (string=? before (tree-listing R))))
       (list (list (list 2 machine-line "") (list 1 machine-line "") (list 2 machine-line "") (list 2 "" machine-line)) #t))
(gated "NH-3 THEOURGIA_HOME alone: each program names the run root, and the machine home gains nothing"
       (let* ((before (tree-listing H))
              (rs (list (child root home-only "theourgia.sc" (list "outline" "--store" S))
                        (child root home-only "core.sc" (list "outline" "--store" S))
                        (child root home-only "theourgiad.sc" (list "serve" S))
                        (child root home-only "mcp/server.sc" (list "--store" S)))))
         (list rs (string=? before (tree-listing H))))
       (list (list (list 2 run-line announce-line) (list 1 run-line announce-line) (list 2 run-line announce-line)
                   (list 2 "" (string-append announce-line run-line)))
             #t))
;; THE LIVE DAEMON: serve started with both homes and its trace on, so its
;; log names each dispatch; the thin client finds it by the run root.
;; (The thin client starts no daemon: with none it runs the core itself.)
(define (socket-under run)
  (let ((dir (if (file-directory? run) run (string-append root "/" run))))
    (and (file-directory? dir)
         (exists (lambda (d) (file-exists? (string-append dir "/" d "/socket") #f)) (directory-list dir)))))
(define (start-serve! assignments store run log)
  ;; In the background and without the alarm: --detach keeps this process,
  ;; which is the daemon, so the alarm a child carries would end it.
  (sh "cd " (quoted root) " && env -u HOME -u THEOURGIA_HOME -u THEOURGIA_RUN"
      " -u THEOURGIA_STORE -u THEOURGIA_LOCAL " assignments "THEOURGIA_TRACE=1"
      " CHEZSCHEMELIBDIRS=" (quoted libdirs) " CHEZSCHEMELIBEXTS=" (quoted libexts)
      " scheme --script " (quoted (string-append tree-dir "/theourgiad.sc")) " serve " (quoted store)
      " --detach --log " (quoted log) " > /dev/null 2>&1 < /dev/null &")
  (let wait ((k 0))
    (when (and (not (socket-under run)) (< k 200)) (sleep (make-time 'time-duration 50000000 0)) (wait (+ k 1))))
  (and (socket-under run) #t))
(define (dispatches log) (count-of (file-text log) "(trace daemon-dispatch"))
(define log3 (string-append root "/serve3.log"))
(define live (and gate-open (start-serve! homes S R log3)))
(gated "NH-3 a live daemon: the thin client with THEOURGIA_RUN alone is refused and the daemon gains no dispatch; with both homes it gains one"
       (let* ((d0 (dispatches log3))
              (refused (child root run-only "theourgia.sc" (list "outline" "--store" S)))
              (d1 (dispatches log3))
              (served (car (child root homes "theourgia.sc" (list "outline" "--store" S))))
              (d2 (dispatches log3)))
         (list live (pair? (serve-pids S)) refused (- d1 d0) served (- d2 d1)))
       (list #t #t (list 2 machine-line "") 0 0 1))
(when gate-open (stop-daemons! S))

;; ---- NH-4: HOME absent, both variables set ----------------------------------
(define S4 (string-append root "/store4"))
(define log4 (string-append root "/serve4.log"))
(gated "NH-4 the store verbs through the thin client: init, insert and outline, then outline again served by a daemon"
       (begin
         (sh "mkdir -p " (quoted S4))
         (let* ((i (car (child root homes "theourgia.sc" (list "init" "--store" S4))))
                (w (car (child root homes "theourgia.sc" (list "insert" "--title" "NH4-BLOCK" "--store" S4))))
                (o (child root homes "theourgia.sc" (list "outline" "--store" S4)))
                (up (start-serve! homes S4 R log4))
                (d0 (dispatches log4))
                (o2 (child root homes "theourgia.sc" (list "outline" "--store" S4))))
           (list i w (car o) (contains? (cadr o) "NH4-BLOCK") up (car o2) (contains? (cadr o2) "NH4-BLOCK")
                 (- (dispatches log4) d0))))
       (list 0 0 0 #t #t 0 #t 1))
(when gate-open (stop-daemons! S4))
(define log4r (string-append root "/serve4r.log"))
(gated "NH-4 a relative run root: serve and the client, from the same working directory, meet under it"
       (let* ((rel (string-append home-only "THEOURGIA_RUN=rel-run "))
              (up (start-serve! rel S "rel-run" log4r))
              (d0 (dispatches log4r))
              (r (child root rel "theourgia.sc" (list "outline" "--store" S))))
         (list up (file-directory? (string-append root "/rel-run")) (car r) (contains? (cadr r) "NH-CANARY")
               (- (dispatches log4r) d0)))
       (list #t #t 0 #t 1))
(when gate-open (stop-daemons! S))
(gated "NH-4 eval in the core: answered"
       (let ((r (child root homes "core.sc" (list "eval" "--store" S "--wire") "(+ 40 2)")))
         (list (car r) (contains? (cadr r) "42")))
       (list 0 #t))
(gated "NH-4 eval --lang shell with a foreign runner (its launcher has PATH, HOME and LANG alone): answered"
       (let ((r (child root (string-append homes "THEOURGIA_RUNNERS=on ") "core.sc"
                       (list "eval" "--lang" "shell" "--store" S "--wire") "echo nh4-shell\n")))
         (list (car r) (contains? (cadr r) "nh4-shell")))
       (list 0 #t))
(define hello
  (string-append "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":"
                 "{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},"
                 "\"clientInfo\":{\"name\":\"probe\",\"version\":\"1\"}}}"))
(define ready "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")
(define (tool-call id name argv)
  (string-append "{\"jsonrpc\":\"2.0\",\"id\":" (number->string id) ",\"method\":\"tools/call\",\"params\":{\"name\":\""
                 name "\",\"arguments\":{\"argv\":[" argv "]}}}"))
(gated "NH-4 the MCP shell: outline through its on-demand daemon and eval through its core child are answered"
       (let ((r (child root homes "mcp/server.sc" (list "--store" S)
                       (string-append hello "\n" ready "\n"
                                      (tool-call 2 "theourgia_outline" "") "\n"
                                      (tool-call 3 "theourgia_eval" "\"(+ 40 2)\"") "\n"))))
         ;; each answer read from its own frame: the eval's line carries id 3
         (let* ((lines (let loop ((p (open-input-string (cadr r))) (acc '()))
                         (let ((l (get-line p))) (if (eof-object? l) (reverse acc) (loop p (cons l acc))))))
                (frame (lambda (id) (let ((m (filter (lambda (l) (contains? l (string-append "\"id\":" id ","))) lines)))
                                      (if (pair? m) (car m) ""))))
                (outline (frame "2")) (evaluated (frame "3")))
           (list (car r) (contains? outline "NH-CANARY") (contains? evaluated "42")
                 (contains? outline "\"isError\":true") (contains? evaluated "\"isError\":true"))))
       (list 0 #t #t #f #f))
(when gate-open (stop-daemons! S))

;; ---- NH-6: help and usage with no home --------------------------------------
(gated "NH-6 help and a verb's usage with no home: the thin client, the core and serve answer, status 0"
       (map (lambda (r) (list (car r) (contains? (cadr r) "usage")))
            (list (child root "" "theourgia.sc" (list "outline" "--help"))
                  (child root "" "core.sc" (list "outline" "-h"))
                  (child root "" "theourgiad.sc" (list "serve" "--help"))))
       '((0 #t) (0 #t) (0 #t)))
;; PINNED AS IT IS: the thin client hands any --help to the core before its
;; own check, and for a verb with no usage the core goes on to its check
(gated "NH-6 a --help for a verb with no usage (bogus --help) with no home: the core's check answers the datum, status 1"
       (child root "" "theourgia.sc" (list "bogus" "--help"))
       (list 1 machine-line ""))

(when gate-open (stop-daemons! S) (stop-daemons! S4))
(sh "rm -rf " (quoted sock-root))
(printf "\n~a failures\nrows: ~a\nno-tmp-home complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
