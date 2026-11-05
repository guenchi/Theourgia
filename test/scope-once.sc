#!chezscheme
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

;; scope reduces the store ONCE per run at a literal cut, not once per root:
;; channel.sc's scope-read obtains the state with store-state-at-cut, as each
;; root's read at a cut did, and reads every root from it. Counted, not timed:
;; store-state-at-cut's restricted replay is the seam phase `cut-replay`, so a
;; child run with THEOURGIA_TIMING=1 prints one line per reduction at a cut.
;; A tag cut keeps the routed read per root. The reads from the one state are
;; the routed reads, root by root; the two paths write the same scope; and a
;; store missing a writer answers alike on both.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch register-verbs!)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce store-state-at-cut)
        (only (theourgia reduce) state-read state-block-ids reduction?)
        (only (theourgia arguments) parse-arguments argument-positionals)
        (only (theourgia client) socket-path serve-log-path)
        (only (theourgia log) log-publish! segment-sha writer-directory)
        (only (theourgia wire) encode-record storable-encode)
        (only (theourgia reduce) block-id reduce-applied-cut)
        (only (theourgia ffi) entry-type))

(register-verbs! extension-verbs)

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
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e))))
       e0))))
(include "expected.ss")
(include "signal-send.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/scope-once-" (number->string (get-process-id))))
(when (file-exists? root) (assertion-violation 'scope-once "scratch directory already exists" root))
(define (sh . xs) (system (apply string-append xs)))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(sh "mkdir -p " (quoted root))
;; THE SOCKETS UNDER THE RUNNER'S SOCKET ROOT, whose path is short enough for one
(define sock-base (let ((v (getenv "THEOURGIA_TEST_SOCK"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define run-root (string-append sock-base "/so-" (number->string (get-process-id))))
(sh "mkdir -p " (quoted run-root))
(putenv "THEOURGIA_RUN" (string-append run-root "/run"))
(define (write-file! path text)
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p (string->utf8 text)))))
(define (file-text path)
  (call-with-port (open-file-input-port path) (lambda (p) (let ((b (get-bytevector-all p))) (if (eof-object? b) "" (utf8->string b))))))
(define (clause a k) (let ((p (and (pair? a) (list? a) (find (lambda (x) (and (pair? x) (eq? (car x) k))) (cdr a))))) (and p (cdr p))))
(define (head-of a)
  (cond ((not (pair? a)) a)
        ((and (eq? (car a) 'error) (pair? (cdr a)) (pair? (cddr a)) (pair? (caddr a)) (eq? (car (caddr a)) 'reason))
         (list 'error (cadr a) (caddr a)))
        ((eq? (car a) 'error) (list 'error (cadr a)))
        (else (car a))))
;; a fresh store with its home; -> the store's path
(define (fresh-store! name)
  (let ((st (string-append root "/" name "/store")))
    (sh "mkdir -p " (quoted st) " " (quoted (string-append root "/" name "/home")) " " (quoted (string-append root "/" name "/src")))
    (putenv "THEOURGIA_HOME" (string-append root "/" name "/home"))
    (rpc-dispatch st '(init) "test")
    st))
(define (ask st actor . r) (rpc-dispatch st r actor))
(define (top-ids st)
  (let ((q (ask st "test" 'query "(under ?b \"root\")")))
    (values (map cadr (filter (lambda (r) (and (pair? r) (eq? (car r) 'row))) (or (clause q 'items) '())))
            (format "~s" (car (clause q 'cut))))))
;; scope in this process, as review-channel.sc runs it: through its library
(define (scope-in st actor dir cut roots)
  (let ((nodes (parse-arguments 'scope (append (list dir)
                                               (if (eq? roots 'all) '("--roots" "all")
                                                   (apply append (map (lambda (r) (list "--roots" r)) roots)))
                                               (list "--for" "reviewer" "--cut" cut "--home" (string-append dir "-home"))))))
    ((eval 'scope-verb (environment '(theourgia channel))) st actor (argument-positionals nodes) nodes #f)))
;; THE SAME SCOPE IN A CHILD with THEOURGIA_TIMING=1, which the seam reads when
;; the libraries expand: -> (<answer's head> <cut-replay lines>)
(define child (string-append root "/count.ss"))
(write-file! child
  (string-append
    "(import (chezscheme) (only (theourgia arguments) parse-arguments argument-positionals)\n"
    "        (only (theourgia rpc) register-verbs!) (only (theourgia extensions) extension-verbs))\n"
    "(register-verbs! extension-verbs)\n"
    "(define args (cdr (command-line)))\n"
    "(define nodes (parse-arguments 'scope (cddr args)))\n"
    "(define sock (if (equal? (cadr args) \"-\") #f (cadr args)))\n"
    "(define a ((eval 'scope-verb (environment '(theourgia channel))) (car args) \"test\" (argument-positionals nodes) nodes #f sock))\n"
    "(write a)\n"))
(define (counted st dir cut roots . options)
  (let ((out (string-append dir ".out")) (err (string-append dir ".err"))
        (routed (memq 'routed options)) (sock (let ((s (find string? options))) (or s "-"))))
    (sh (if routed "env -u THEOURGIA_LOCAL " "THEOURGIA_LOCAL=1 ") "THEOURGIA_TIMING=1 THEOURGIA_HOME=" (quoted (string-append (path-parent st) "/home"))
        " scheme --script " (quoted child) " " (quoted st) " " (quoted sock) " " (quoted dir)
        (if (eq? roots 'all) " --roots all" (apply string-append (map (lambda (r) (string-append " --roots " (quoted r))) roots)))
        " --for reviewer --cut " (quoted cut) " --home " (quoted (string-append dir "-home"))
        " > " (quoted out) " 2> " (quoted err) " < /dev/null")
    (list (let ((a (guard (e (#t 'UNREADABLE)) (read (open-string-input-port (file-text out)))))) (if (pair? a) (car a) a))
          (let loop ((s (file-text err)) (n 0))
            (let ((i (let find ((j 0)) (and (<= (+ j 27) (string-length s))
                                            (if (string=? "(timing request cut-replay " (substring s j (+ j 27))) j (find (+ j 1)))))))
              (if i (loop (substring s (+ i 27) (string-length s)) (+ n 1)) n))))))
(define (path-parent p)
  (let loop ((i (- (string-length p) 1))) (cond ((< i 0) ".") ((char=? (string-ref p i) #\/) (substring p 0 i)) (else (loop (- i 1))))))

;; ---- the store: 24 roots, of 2 to 25 blocks ---------------------------------------
(define S (fresh-store! "s"))
;; each file split at its definitions (split-suggest's copy imported), so the
;; roots hold 2 to 25 blocks: the file and one block per definition
(sh "mkdir -p " (quoted (string-append root "/s/raw")))
(do ((k 0 (+ k 1))) ((= k 24))
  (let ((name (string-append "f" (number->string k) ".sc")))
    (write-file! (string-append root "/s/raw/" name)
                 (apply string-append (map (lambda (j) (format "(define (d~a) ~a)\n" j j)) (iota (+ k 1)))))
    (ask S "test" 'split-suggest (string-append root "/s/raw/" name) "--output" (string-append root "/s/src/" name))))
(ask S "test" 'import-code (string-append root "/s/src"))
(ask S "test" 'tag "t0")
(define-values (roots cut) (top-ids S))

;; ---- SO-1, SO-2: the reductions, counted -----------------------------------------------
;; SO-1 FIRST: its scope writes the letter into S, which moves the store's cut,
;; and `--roots all` is refused at any cut but the store's own
(printf "== SO-1, SO-2: reductions at a cut per scope run ==\n")
;; THE BASE: 0 and 0, no such seam phase; the per-root mutant: 24
(want "SO-1 scope --roots all at a literal cut reduces the store once: one cut-replay line for 24 roots"
      (list (length roots) (counted S (string-append root "/s/literal") cut 'all))
      '(24 (ok 1)))
(want "SO-2 at a tag cut it reads root by root as before: one cut-replay line per root"
      (counted S (string-append root "/s/tagged") "t0" roots)
      '(ok 24))

;; WITH A SOCKET AT THE STORE'S PATH a routed read would go to it, so scope
;; reads root by root through it as before (a stale socket: each read then
;; finds no daemon and is answered locally, root by root). The roots are 26
;; by then: the 24 files and the letters SO-1's and SO-2's scopes wrote.
(want "SO-6 with a socket at the store's path and THEOURGIA_LOCAL unset, scope reads root by root: one line per root"
      (let-values (((rs c) (top-ids S)))
        (sh "mkdir -p " (quoted (path-parent (socket-path S))))
        (write-file! (socket-path S) "")
        (let ((r (counted S (string-append root "/s/socketed") c 'all 'routed)))
          (sh "rm -f " (quoted (socket-path S)))
          (list (length rs) r)))
      '(26 (ok 26)))

;; ---- SO-3: the reads from the one state are the routed reads -------------------------------
(printf "== SO-3: every root's read from the one state equals its routed read ==\n")
;; looked up when the row calls it, so the file loads on the base, where it reads red
(define (scope-state-read . args) (apply (eval 'scope-state-read (environment '(theourgia channel))) args))
(want "SO-3 for every root, items, versions and the cut clause agree between the two reads"
      (let ((state (store-state-at-cut S #f cut)))
        (list (reduction? state)
              (filter (lambda (x) x)
                      (map (lambda (r)
                             (let ((old (ask S "test" 'read r "--cut" cut "--recursive"))
                                   (new (scope-state-read S "test" r state)))
                               (and (not (and (equal? (car old) (car new))
                                              (equal? (clause old 'items) (clause new 'items))
                                              (equal? (clause old 'versions) (clause new 'versions))
                                              (equal? (clause old 'cut) (clause new 'cut))))
                                    r)))
                           roots))))
      '(#t ()))

;; ---- SO-4: the two paths write the same scope --------------------------------------------
(printf "== SO-4: the scope the two paths write ==\n")
;; a scoped store's copies, each as its origin and fields, the run-specific ones left out
(define (copies dir)
  (define home (getenv "THEOURGIA_HOME"))
  (putenv "THEOURGIA_HOME" (string-append dir "-home"))
  (let* ((s (open-and-reduce dir))
         (rows (map (lambda (id) (state-read s id)) (state-block-ids s))))
    (putenv "THEOURGIA_HOME" home)
    (list-sort (lambda (a b) (string<? (format "~s" a) (format "~s" b)))
               (filter (lambda (x) x)
                       (map (lambda (b)
                              (let* ((fs (cdr (assq 'fields b))) (o (assq 'origin fs)))
                                (and o (cons (cdr o) (filter (lambda (f) (not (memq (car f) '(origin origin-cut)))) fs)))))
                            rows)))))
(want "SO-4 CONTROL the literal path's scope and the tag path's hold the same copies, and the same count"
      (let ((a (copies (string-append root "/s/literal"))) (b (copies (string-append root "/s/tagged"))))
        (list (length a) (equal? a b)))
      (list (apply + (map (lambda (k) (+ k 2)) (iota 24))) #t))

;; ---- SO-5: a store missing a writer answers alike on both paths ------------------------------
(printf "== SO-5: a writer's directory gone after the cut ==\n")
;; A second machine adopts the store and commits, a log writer of its own; the cut
;; names both; then that writer's directory is moved away.
(define M (fresh-store! "m"))
(write-file! (string-append root "/m/src/a.sc") "(define (a) 1)\n(define (b) 2)\n")
(ask M "test" 'import-code (string-append root "/m/src"))
(define m-block (let ((b (find (lambda (r) (and (pair? r) (eq? (car r) 'row) (eq? (caddr r) 'code)))
                               (cdr (or (clause (ask M "test" 'outline) 'rows) '(rows))))))
                  (and b (cadr b))))
(sh "mkdir -p " (quoted (string-append root "/m/home2")))
(putenv "THEOURGIA_HOME" (string-append root "/m/home2"))
(define m2 (let ((a (ask M "m2" 'adopt))) (and (clause a 'to) (car (clause a 'to)))))
(ask M "m2" 'write (or m-block "none") "(define (a) 3)\n" "--writer" "m2")
(ask M "m2" 'commit "--writer" "m2")
(putenv "THEOURGIA_HOME" (string-append root "/m/home"))
(ask M "test" 'tag "t0")
(define-values (m-roots m-cut) (top-ids M))
(sh "mv " (quoted (string-append M "/writers/" (or m2 "none"))) " " (quoted (string-append root "/m/removed")))
;; THE BASE gives these three, and the new path must give them too
(want "SO-5 CONTROL with the second machine's writer gone: roots all refused at another cut, the tag unknown, the named roots' cut unavailable"
      (list (and m2 #t) (length m-roots)
            (head-of (scope-in M "test" (string-append root "/m/s1") m-cut 'all))
            (head-of (scope-in M "test" (string-append root "/m/s2") "t0" m-roots))
            (head-of (scope-in M "test" (string-append root "/m/s3") m-cut m-roots))
            (let ((a (scope-in M "test" (string-append root "/m/s4") m-cut m-roots))) (clause a 'reason)))
      '(#t 1 (error bad-request (reason roots-all-at-another-cut)) (error unknown-tag) (error cut-unavailable)
        (not-received)))

;; ---- helpers for SO-7..SO-10 ------------------------------------------------------------
;; One record of another writer, published as that writer's segment SEQ.
(define (mirror! st writer seq ts deps payload)
  (let ((bytes (encode-record seq ts "peer" deps (storable-encode payload))))
    (car (log-publish! st writer seq bytes (segment-sha bytes)))))
(define (section title) (list 'put (list (cons 'kind 'section) (cons 'title title))))
(define (new-id answer)
  (let ((ev (and (pair? answer) (clause answer 'events)))) (and ev (let ((e (car (car ev)))) (block-id (car e) (cdr e))))))
;; a store of COUNT files split at their definitions, file k holding k + 2 blocks; -> the store
(define (split-store! name count)
  (let ((st (fresh-store! name)))
    (sh "mkdir -p " (quoted (string-append root "/" name "/raw")))
    (do ((k 0 (+ k 1))) ((= k count))
      (let ((f (string-append "g" (number->string k) ".sc")))
        (write-file! (string-append root "/" name "/raw/" f)
                     (apply string-append (map (lambda (j) (format "(define (e~a) ~a)\n" j j)) (iota (+ k 1)))))
        (ask st "test" 'split-suggest (string-append root "/" name "/raw/" f) "--output" (string-append root "/" name "/src/" f))))
    (ask st "test" 'import-code (string-append root "/" name "/src"))
    st))
(define (count-in text needle)
  (let loop ((s text) (n 0))
    (let ((i (let find ((j 0)) (and (<= (+ j (string-length needle)) (string-length s))
                                    (if (string=? needle (substring s j (+ j (string-length needle)))) j (find (+ j 1)))))))
      (if i (loop (substring s (+ i (string-length needle)) (string-length s)) (+ n 1)) n))))
;; the root reads the daemons of this run served, by their trace
(define (daemon-reads)
  (let ((f (string-append root "/daemon-logs.txt")))
    (sh "cat " (quoted run-root) "/run/*/serve.log " (quoted run-root) "/custom-*.log > " (quoted f) " 2>/dev/null; true")
    (count-in (file-text f) "(trace daemon-dispatch read")))
(include "serve-for.ss")
(define d-stores '())
(define (stop-daemon! store)
  (signal-processes! (serve-processes store) "TERM")
  (let loop ((k 0)) (when (and (< k 50) (pair? (serve-pids store))) (sh "sleep 0.1") (loop (+ k 1))))
  (signal-processes! (serve-processes store) "KILL"))
(let ((previous (base-exception-handler)))
  (base-exception-handler (lambda (e) (for-each stop-daemon! d-stores) (previous e))))
(define (top-count st) (let-values (((rs c) (top-ids st))) (length rs)))
;; the events this store's own writers applied: what a scope writes (its letter),
;; and what a fork marker on a mirrored writer does not change
(define (own-events st mirrored)
  (apply + (map cdr (filter (lambda (e) (not (member (car e) mirrored))) (reduce-applied-cut (open-and-reduce st))))))

;; ---- SO-7: the routing matrix, with a live daemon --------------------------------------------
;; The roots are named and the cut is one literal, so each run reads the same 6 roots
;; at the same cut, whoever answers: this process (one cut-replay line in its own
;; stderr) or a daemon (no line here, 6 reads in its log).
(printf "== SO-7: who reads the roots, with a daemon serving ==\n")
(define D (split-store! "d" 6))
(define-values (d-roots d-cut) (top-ids D))
(set! d-stores (cons D d-stores))
(define (through! label sock . options)
  (let* ((before (daemon-reads))
         (r (apply counted D (string-append root "/d/" label) d-cut d-roots (if sock (cons sock options) options))))
    (list r (- (daemon-reads) before))))
(serve-for! root D #f "THEOURGIA_TRACE=1")
(want "SO-7 a live daemon at the store's socket, THEOURGIA_LOCAL=1: scope reduces once here and the daemon reads no root"
      (through! "local-live" #f)
      '((ok 1) 0))
(want "SO-7 a live daemon at the store's socket, THEOURGIA_LOCAL unset: the daemon reads each root, none is reduced here"
      (through! "routed-live" #f 'routed)
      '((ok 0) 6))
(want "SO-7 a supplied socket where nothing is, the store's own daemon live: the supplied path wins, scope reduces once here"
      (through! "supplied-absent" (string-append run-root "/nothing-here.sock") 'routed)
      '((ok 1) 0))
(stop-daemon! D)
(define custom (string-append run-root "/custom.sock"))
(serve-for! root D custom "THEOURGIA_TRACE=1")
(want "SO-7 a live daemon at a supplied socket, none at the store's: the roots are read through the supplied one"
      (list (eq? (entry-type (socket-path D)) 'absent) (through! "custom-live" custom 'routed))
      '(#t ((ok 0) 6)))
(stop-daemon! D)
(want "SO-7 no daemon anywhere and no socket file, THEOURGIA_LOCAL unset: scope reduces once here"
      (list (eq? (entry-type (socket-path D)) 'absent) (through! "none" #f 'routed))
      '(#t ((ok 1) 0)))

;; ---- SO-8: the one state's hold and re-check --------------------------------------------------
;; read-at-cut RC-8's twin, for scope's one state: held at read-cut-before-replay
;; (the injection build's hold seam), a fork marker set aside the mirrored writer's
;; records from 2 on; store-state-at-cut refuses cut-moved, so the roots take the
;; routed read, whose own refusal is the answer (measured: the cut is not received
;; once the records are set aside). Nothing is written on a refusal. The twin is
;; held the same way with nothing set aside and copies the three blocks.
;; THE BASE ANSWERS cut-moved here: its first root's own read is the one held,
;; so the record is set aside between that read's judgement and its replay;
;; today's code answers cut-unavailable too when the record is set aside before
;; the reads, as the fallback's reads find it. Either way nothing is written.
(printf "== SO-8: a cut that moves while scope holds its one state ==\n")
(define (held-scope! fork?)
  (let* ((name (if fork? "hm" "hs")) (st (fresh-store! name)) (Q "qqqqqqqq")
         (_ (for-each (lambda (k) (mirror! st Q k (+ 1757300000000 k) '() (section (format "q~a" k)))) '(1 2 3)))
         (rs (map (lambda (k) (block-id Q k)) '(1 2 3)))
         (cut (format "~s" (list (cons Q 3))))
         (release (string-append root "/" name "-release"))
         (dir (string-append root "/" name "/scope"))
         (out (string-append root "/" name ".out"))
         (tops (own-events st (list Q))))
    (sh "( THEOURGIA_INJECT=on THEOURGIA_LOCAL=1 THEOURGIA_HOLD=read-cut-before-replay:" release
        " THEOURGIA_HOME=" (quoted (string-append root "/" name "/home"))
        " perl -e 'alarm 120; exec @ARGV' scheme --script " (quoted child) " " (quoted st) " - " (quoted dir)
        (apply string-append (map (lambda (r) (string-append " --roots " (quoted r))) rs))
        " --for reviewer --cut " (quoted cut) " --home " (quoted (string-append dir "-home"))
        " > " (quoted out) " 2> " (quoted (string-append out ".err")) " < /dev/null; echo $? > " (quoted (string-append out ".rc")) " ) &")
    (let ((held (let wait ((k 0))
                  (cond ((file-exists? (string-append release ".held")) 'held)
                        ((> k 600) 'never-held)
                        (else (sh "sleep 0.1") (wait (+ k 1)))))))
      (when (and fork? (eq? held 'held))
        (call-with-output-file (string-append (writer-directory st Q) "/quarantine.sexp")
          (lambda (p) (put-string p "((format 1) (fork 2) (ours \"o\") (theirs \"t\"))\n")) 'truncate))
      (sh "touch " (quoted release))
      (let wait ((k 0))
        (unless (or (file-exists? (string-append out ".rc")) (> k 1200)) (sh "sleep 0.1") (wait (+ k 1))))
      (list held (guard (e (#t 'UNREADABLE)) (read (open-string-input-port (file-text out))))
            (eq? (entry-type dir) 'absent) (= tops (own-events st (list Q)))))))
(want "SO-8 a record set aside while the one state is held: refused as the routed read refuses it, nothing written"
      (let ((r (held-scope! #t)))
        (list (car r) (head-of (cadr r)) (clause (cadr r) 'reason) (caddr r) (cadddr r)))
      '(held (error cut-unavailable) (not-received) #t #t))
(want "SO-8 TWIN held the same way with nothing set aside: the three blocks are copied"
      (let ((r (held-scope! #f)))
        (list (car r) (head-of (cadr r)) (let ((b (clause (cadr r) 'blocks))) (and b (car b)))))
      '(held ok 3))

;; ---- SO-9: the one state is the cut's, after the store has moved on ------------------------------
;; A state taken at C; then a root's block rewritten, a block added under a root and one
;; deleted under another, and a snapshot taken at the new cut. Every original root's
;; read from the state at C is its routed read at C, whole; the current reads differ;
;; and scope at C copies C's blocks.
(printf "== SO-9: a state at C after the store moved to C2 ==\n")
(define H (split-store! "h" 4))
(define-values (h-roots C) (top-ids H))
(define state-C (store-state-at-cut H #f C))
(define (items-of a) (or (clause a 'items) '()))
(define at-C (map (lambda (r) (ask H "test" 'read r "--cut" C "--recursive")) h-roots))
(define (kid r k) (let ((is (items-of (list-ref at-C (list-index (lambda (x) (equal? x r)) h-roots))))) (cdr (assq 'id (list-ref is k)))))
(define (list-index p xs) (let loop ((xs xs) (i 0)) (cond ((null? xs) #f) ((p (car xs)) i) (else (loop (cdr xs) (+ i 1))))))
(define rewritten (kid (list-ref h-roots 1) 1))
(define old-src (let ((b (state-read state-C rewritten))) (cdr (assq 'src (cdr (assq 'fields b))))))
(ask H "w9" 'write rewritten "(define (e0) 99)\n" "--writer" "w9")
(ask H "w9" 'commit "--writer" "w9")
(ask H "test" 'insert "--under" (list-ref h-roots 2) "--title" "added later")
(ask H "test" 'del (kid (list-ref h-roots 3) 2))
(ask H "test" 'snapshot)
(define touched (list (list-ref h-roots 1) (list-ref h-roots 2) (list-ref h-roots 3)))
(want "SO-9 for every original root, the read from the state at C is the routed read at C, whole"
      (filter (lambda (x) x)
              (map (lambda (r old) (and (not (equal? old (scope-state-read H "test" r state-C))) r)) h-roots at-C))
      '())
(want "SO-9 and the store has moved: the three touched roots read otherwise now"
      (map (lambda (r) (not (equal? (items-of (ask H "test" 'read r "--recursive"))
                                    (items-of (list-ref at-C (list-index (lambda (x) (equal? x r)) h-roots))))))
           touched)
      '(#t #t #t))
(want "SO-9 scope with the roots named at C copies C's blocks: the rewritten block's copy holds its old source"
      (let* ((dir (string-append root "/h/at-C"))
             (a (scope-in H "test" dir C h-roots))
             (cs (copies dir))
             (c (find (lambda (x) (equal? (car x) rewritten)) cs)))
        (list (car a) (length cs) (and c (equal? (cdr (assq 'src (cdr c))) old-src))))
      (list 'ok (apply + (map (lambda (a) (length (items-of a))) at-C)) #t))

;; ---- SO-10: a writer present but unreadable -----------------------------------------------------
;; read-at-cut RC-D2's store: a local block, a mirrored writer's block, the mirror's
;; directory made unreadable. scope's one state is loaded outside the read verb's
;; declaration, so it would raise there; the roots take the routed read, declared,
;; and answer as before: at a cut without the mirror, the read served with the
;; incomplete clause and the root copied, then the main letter refused (no view: a
;; predecessor not applied), scope-failed at its letter step, as the base answers;
;; at a cut with it, refused, and nothing written.
(printf "== SO-10: an unreadable writer ==\n")
(define U (fresh-store! "u"))
(define u-insert (ask U "test" 'insert "--under" "root" "--title" "local block"))
(define u-b1 (new-id u-insert))
;; the cut at the insert, which the mirrored writer's record is not in
(define u-early (format "~s" (list (car (car (clause u-insert 'events))))))
(define u-M "zzzzzzzz")
(mirror! U u-M 1 50 '() (section "mirrored block"))
(ask U "test" 'set u-b1 "title" "local block renamed")
(define-values (u-roots u-now) (top-ids U))
(define u-dir (writer-directory U u-M))
(define (u-scope label cut) (scope-in U "test" (string-append root "/u/" label) cut (list u-b1)))
(sh "chmod 000 " (quoted u-dir))
(define u-routed-early (ask U "test" 'read u-b1 "--cut" u-early "--recursive"))
(define u-scope-early (guard (e (#t (list 'RAISED (condition-message e)))) (u-scope "early" u-early)))
(define u-tops (top-count U))
(define u-routed-now (ask U "test" 'read u-b1 "--cut" u-now "--recursive"))
(define u-scope-now (guard (e (#t (list 'RAISED (condition-message e)))) (u-scope "now" u-now)))
(define u-tops-after (top-count U))
(sh "chmod 700 " (quoted u-dir))
(define (incomplete-paths a)
  (let ((c (clause a 'incomplete)))
    (if (not c) 'no-clause (map (lambda (u) (let ((p (and (pair? u) (assq 'path (cdr u))))) (and p (cadr p)))) c))))
(want "SO-10 at a cut without the unreadable writer: the read is served with its incomplete clause, scope copies the root and fails at its letter"
      (list (head-of u-routed-early) (incomplete-paths u-routed-early)
            (head-of u-scope-early) (clause u-scope-early 'step)
            ;; the refused letter's own answer carries the same diagnostic
            (let ((a (clause u-scope-early 'answer))) (and a (incomplete-paths (car a))))
            (guard (e (#t 'no-scope)) (length (copies (string-append root "/u/early")))))
      (list 'ok (list u-dir) '(error scope-failed) '(letter) (list u-dir) 1))
(want "SO-10 at a cut with it: scope answers the routed read's refusal whole, cut-unavailable not-received with its incomplete clause, and writes nothing"
      (list (equal? u-scope-now u-routed-now)
            (head-of u-scope-now) (clause u-scope-now 'reason) (incomplete-paths u-scope-now)
            (eq? (entry-type (string-append root "/u/now")) 'absent) (= u-tops u-tops-after))
      (list #t '(error cut-unavailable) '(not-received) (list u-dir) #t #t))

(for-each stop-daemon! d-stores)
(sh "rm -rf " (quoted root) " " (quoted run-root))
(printf "rows: ~a\n~a failures\nscope-once complete\n" rows bad)
(exit (if (zero? bad) 0 1))
