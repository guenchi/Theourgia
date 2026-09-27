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

;; A store kept in git, with one writer: backup, restore and move.
;;
;; What a checkout carries is decided by the .gitignore that init writes;
;; a checkout has no instance.sexp, so it reads, refuses its first write
;; with the remedy, and adopts an identity of its own; a copy that still
;; writes under its old identity is refused by name once it has pulled the
;; successor. Every product call is a subprocess of core.sc with
;; THEOURGIA_LOCAL=1 and one THEOURGIA_HOME per checkout, so no daemon is
;; left behind; the missing-lock row is the one that starts a daemon on
;; purpose, through theourgia.sc, and that start fails.
;;
;; THE DAEMON'S ANSWER FOR A MISSING LOCK IS PINNED AS MEASURED, AND IT IS A
;; MISNAMING: the start answers kind store-busy, which says somebody else
;; holds the store, while the lock file is in fact absent. The base answered
;; the same. The in-process read answers absent, which is right. The rows
;; pin the daemon's answer so a change to it is seen, not to endorse it.
;;
;; git runs with the global and system configuration shut out
;; (GIT_CONFIG_GLOBAL=/dev/null, GIT_CONFIG_NOSYSTEM=1): an excludes file
;; in the person's own configuration would make "not staged" rows pass
;; with no .gitignore at all. git itself is required; without it the file
;; is red by name, not skipped.

(import (chezscheme)
        (only (theourgia client) socket-path)
        (only (theourgia reduce) block-id)
        (only (theourgia wire) storable-decode storable-encode)
        (only (theourgia log) store-id-of)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (igropyr sexpr) string->sexpr-extended sexpr->string-extended))

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
                 "/store-in-git-" (number->string (get-process-id))))
(when (file-exists? root)
  (assertion-violation 'store-in-git "scratch directory already exists" root))
(system (string-append "mkdir -p " root))

;; Run roots for a daemon start live under the short socket directory the
;; runner provides: a socket path under the test root is longer than a
;; unix socket name may be, and the start answers socket-path-too-long.
(define sock-base
  (let ((v (getenv "THEOURGIA_TEST_SOCK")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(define (sh . xs) (system (apply string-append xs)))
(define (sh-out . xs)
  (let* ((p (process (apply string-append xs)))
         (s (get-string-all (car p))))
    (close-port (car p)) (close-port (cadr p))
    (if (eof-object? s) "" s)))
(define (lines-of s)
  (let loop ((cs (string->list s)) (cur '()) (acc '()))
    (cond ((null? cs) (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
          ((char=? (car cs) #\newline) (loop (cdr cs) '() (cons (list->string (reverse cur)) acc)))
          (else (loop (cdr cs) (cons (car cs) cur) acc)))))
(define (text-of-file p)
  (if (file-exists? p)
      (let ((t (call-with-input-file p get-string-all))) (if (eof-object? t) "" t))
      ""))
(define (has-substring? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (clause-of answer head)
  (and (pair? answer) (list? answer)
       (find (lambda (c) (and (pair? c) (eq? (car c) head))) (cdr answer))))
(define (head-of a)
  (cond ((not (pair? a)) a)
        ((and (eq? (car a) 'error) (pair? (cdr a))) (list 'error (cadr a)))
        (else (car a))))

;; ---- git, isolated ------------------------------------------------------------
(define git-env
  (string-append "GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 HOME=" (quoted root)
                 " GIT_AUTHOR_NAME=t GIT_AUTHOR_EMAIL=t@t GIT_COMMITTER_NAME=t GIT_COMMITTER_EMAIL=t@t"))
(define (git dir . args)
  (sh-out "cd " (quoted dir) " && " git-env " git -c init.defaultBranch=main -c commit.gpgsign=false "
          (apply string-append (map (lambda (a) (string-append a " ")) args)) " 2>&1"))
(define git-present? (= 0 (sh "command -v git > /dev/null 2>&1")))
(want "G0 git is on this machine (these rows need it; without it the file is red by name)"
      git-present? #t)
(unless git-present?
  (printf "\n~a failures\nrows: ~a\nstore-in-git complete\n" bad rows)
  (exit 1))

;; ---- the product, one home per checkout ---------------------------------------
(define (ask-in home store in verb . args)
  (let ((out (string-append root "/ask.out")))
    (sh "THEOURGIA_LOCAL=1 THEOURGIA_HOME=" (quoted home) " scheme --script ../core.sc " verb " "
        (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
        "--store " (quoted store) " --wire > " (quoted out) " 2> /dev/null < " (if in (quoted in) "/dev/null"))
    (guard (e (#t 'UNREADABLE))
      (let ((d (call-with-input-file out read))) (if (eof-object? d) 'NO-ANSWER d)))))
(define (ask home store verb . args) (apply ask-in home store #f verb args))
(define (home! name) (let ((h (string-append root "/home-" name))) (sh "mkdir -p " (quoted h)) h))
(define (writer-of init-answer) (let ((c (clause-of init-answer 'writer))) (and c (cadr c))))
(define (new-id answer)
  (let ((ev (and (pair? answer) (assq 'events (cdr answer)))))
    (and ev (let ((e (car (cadr ev)))) (block-id (car e) (cdr e))))))
(define (files-under dir)
  (list-sort string<? (filter (lambda (l) (> (string-length l) 0))
                              (lines-of (sh-out "cd " (quoted dir) " && find . -type f | sed 's#^\\./##'")))))
(define (tracked repo) (list-sort string<? (filter (lambda (l) (> (string-length l) 0)) (lines-of (git repo "ls-files")))))
;; A path relative to the store, split at "/".
(define (path-parts s)
  (let loop ((cs (string->list s)) (cur '()) (acc '()))
    (cond ((null? cs) (reverse (cons (list->string (reverse cur)) acc)))
          ((char=? (car cs) #\/) (loop (cdr cs) '() (cons (list->string (reverse cur)) acc)))
          (else (loop (cdr cs) (cons (car cs) cur) acc)))))
;; A segment file name as the log reads one: six ASCII digits naming a
;; number above zero, then ".sexp".
(define (segment-name? f)
  (and (= (string-length f) 11) (string=? (substring f 6 11) ".sexp")
       (for-all (lambda (c) (char<=? #\0 c #\9)) (string->list (substring f 0 6)))
       (> (string->number (substring f 0 6)) 0)))
;; R2's exclusions, written a second time from the patterns' meaning rather
;; than from the file, as git applies them: to EACH ENTRY along a path (each
;; prefix, a directory unless it is the last), the last matching pattern
;; winning; a path is excluded when any of its entries is. The patterns:
;; /instance.sexp and /request-index.sexp (a root entry of either kind),
;; /snap/ (a root directory), /writers/*/working/ (a directory),
;; /writers/*/draft.lock (either kind), *.tmp-* (a name at any depth), and
;; !/writers/*/incoming/* (re-includes an incoming entry of either kind).
(define (glob-tmp? name) (has-substring? name ".tmp-"))
(define (entry-excluded? ps dir?)
  (let* ((k (length ps)) (name (list-ref ps (- k 1)))
         (writer-level? (and (= k 3) (string=? (car ps) "writers")))
         (incoming-entry? (and (= k 4) (string=? (car ps) "writers") (string=? (caddr ps) "incoming"))))
    (cond (incoming-entry? #f)
          ((glob-tmp? name) #t)
          ((and (= k 1) (member name '("instance.sexp" "request-index.sexp"))) #t)
          ((and (= k 1) dir? (string=? name "snap")) #t)
          ((and writer-level? dir? (string=? name "working")) #t)
          ((and writer-level? (string=? name "draft.lock")) #t)
          (else #f))))
(define (excluded-by-r2? rel)
  (let ((ps (path-parts rel)))
    (let loop ((k 1))
      (and (<= k (length ps))
           (or (entry-excluded? (list-head ps k) (< k (length ps)))
               (loop (+ k 1)))))))
(define (segment-md5s store writer)
  (sh-out "cd " (quoted store) "/writers/" (quoted writer) " && ls [0-9]*.sexp 2>/dev/null | sort | xargs md5 -r 2>/dev/null"))

;; ---- G1: a missing lock ---------------------------------------------------------
;; The lock is never recreated but by init. In process, a read answers
;; `absent` on the lock's path. Through a daemon start the answer is
;; serve-start-failed of kind store-busy, on a fresh run root and on a
;; pre-created one alike (see the note at the top of the file).
(let* ((h (home! "g1")) (s (string-append root "/g1/store")))
  (sh "mkdir -p " (quoted s))
  (ask h s "init")
  (sh "rm -f " (quoted (string-append s "/lock")))
  (let ((a (ask h s "read" "root")))
    (want "G1 in process: a read of a store with no lock answers absent on the lock's path"
          (list (head-of a) (clause-of a 'path))
          (list '(error absent) (list 'path (string-append s "/lock"))))))
(define (client-read home store run)
  (let ((out (string-append root "/g1c.out")))
    (let ((rc (sh "THEOURGIA_HOME=" (quoted home) " THEOURGIA_RUN=" (quoted run)
                  " perl -e 'alarm 60; exec @ARGV' scheme --script ../theourgia.sc read root --store "
                  (quoted store) " --wire > " (quoted out) " 2> /dev/null < /dev/null")))
      (list rc (guard (e (#t 'UNREADABLE)) (call-with-input-file out read))))))
(define (key-dir-for store run)
  (putenv "THEOURGIA_RUN" run)
  (let ((p (socket-path store))) (substring p 0 (- (string-length p) (string-length "/socket")))))
(let* ((h (home! "g1d")) (s (string-append root "/g1d/store")) (run (string-append sock-base "/sg-f")))
  (sh "mkdir -p " (quoted s) " " (quoted run))
  (ask h s "init")
  (sh "rm -f " (quoted (string-append s "/lock")))
  (let* ((r (client-read h s run)) (a (cadr r)) (failed (clause-of a 'failed)))
    (want "G1 daemon, fresh run root: serve-start-failed kind store-busy AS MEASURED (a misnaming carried from the base); rc 75"
          (list (car r) (head-of a) (clause-of a 'kind))
          (list 75 '(error serve-start-failed) '(kind store-busy)))))
(let* ((h (home! "g1p")) (s (string-append root "/g1p/store")) (run (string-append sock-base "/sg-p")))
  (sh "mkdir -p " (quoted s) " " (quoted run))
  (ask h s "init")
  (sh "rm -f " (quoted (string-append s "/lock")))
  (let ((kd (key-dir-for s run)))
    (sh "mkdir -p " (quoted kd) " && touch " (quoted (string-append kd "/.socket.lock"))))
  (let* ((r (client-read h s run)) (a (cadr r)))
    (want "G1 daemon, startup directory and lock pre-created: serve-start-failed kind store-busy AS MEASURED (a misnaming carried from the base), on the lock's path; rc 75"
          (list (car r) (head-of a) (clause-of a 'kind) (clause-of a 'path))
          (list 75 '(error serve-start-failed) '(kind store-busy) (list 'path (string-append s "/lock"))))))

;; ---- G2: what init keeps out, and what `git add <store>` stages --------------------
(define gitignore-lines
  '("/instance.sexp" "/request-index.sexp" "/snap/" "/writers/*/working/" "/writers/*/draft.lock" "*.tmp-*"
    "!/writers/*/incoming/*"))
(let* ((h (home! "g2")) (repo (string-append root "/g2")) (s (string-append repo "/store")))
  (sh "mkdir -p " (quoted s))
  (git repo "init" "-q")
  (let* ((i (ask h s "init")) (w (writer-of i))
         (b (new-id (ask h s "insert" "--under" "root" "--title" "Alpha" "--text" "first")))
         (snap (ask h s "snapshot"))
         (draft (and b (ask h s "write" b "a draft" "--writer" w)))
         ;; NESTED CASES the anchoring must tell apart: a blob that happens
         ;; to be named instance.sexp (store data, tracked), a staged
         ;; candidate retained in incoming/ (evidence, tracked), and a
         ;; retained temporary beside the segments (excluded).
         (nested-blob "blobs/instance.sexp")
         (incoming-tmp (string-append "writers/" w "/incoming/000002.sexp.ab12.seg.tmp-1-1"))
         (writer-tmp (string-append "writers/" w "/000001.sexp.tmp-1-2")))
    (for-each (lambda (r) (sh "mkdir -p " (quoted (string-append s "/" (let loop ((i (- (string-length r) 1)))
                                                                         (if (char=? (string-ref r i) #\/) (substring r 0 i) (loop (- i 1))))))
                              " && printf 'x' > " (quoted (string-append s "/" r))))
              (list nested-blob incoming-tmp writer-tmp))
    (want "G2 setup: the store has an instance, a checkpoint, a snapshot, a draft and its lock on disk"
          (list (head-of snap) (head-of draft)
                (map (lambda (p) (file-exists? (string-append s "/" p)))
                     (list "instance.sexp" "request-index.sexp"
                           (string-append "writers/" w "/draft.lock")))
                (and (pair? (filter (lambda (f) (has-substring? f "/working/")) (files-under s))) #t)
                (and (pair? (filter (lambda (f) (has-substring? f "snap/")) (files-under s))) #t))
          (list 'ok 'ok '(#t #t #t) #t #t))
    (want "G2 init writes .gitignore holding exactly the seven patterns, in order"
          (lines-of (text-of-file (string-append s "/.gitignore")))
          gitignore-lines)
    (git repo "add" "store")
    (let ((t (tracked repo)))
      (want "G2 `git add <store>` stages meta.sexp, lock, .gitignore, the segment and owner.sexp, and nothing R2 excludes"
            (list (and (member "store/meta.sexp" t) #t) (and (member "store/lock" t) #t)
                  (and (member "store/.gitignore" t) #t)
                  (and (member (string-append "store/writers/" w "/000001.sexp") t) #t)
                  (and (member (string-append "store/writers/" w "/owner.sexp") t) #t)
                  (filter (lambda (f) (and (> (string-length f) 6) (excluded-by-r2? (substring f 6 (string-length f))))) t))
            (list #t #t #t #t #t '()))
      (want "G2 the anchoring: a blob named instance.sexp and a candidate kept in incoming/ are staged; a temporary beside the segments is not"
            (list (and (member (string-append "store/" nested-blob) t) #t)
                  (and (member (string-append "store/" incoming-tmp) t) #t)
                  (and (member (string-append "store/" writer-tmp) t) #t)
                  (file-exists? (string-append s "/" writer-tmp)))
            (list #t #t #f #t)))))

;; ---- G3: a rolled segment is staged as a whole ------------------------------------
;; A segment rolls at 1 MiB; one append over that and a second append give
;; two segments. The names are never listed: `git add <store>` takes both.
;; The big text goes through `batch` on standard input: as an argument it
;; would exceed the command line's own limit (ARG_MAX, 1 MiB here).
(let* ((h (home! "g3")) (repo (string-append root "/g3")) (s (string-append repo "/store"))
       (clone (string-append root "/g3-clone")))
  (sh "mkdir -p " (quoted s))
  (git repo "init" "-q")
  (let* ((i (ask h s "init")) (w (writer-of i))
         (in (let ((f (string-append root "/g3-big.in")))
               (call-with-output-file f
                 (lambda (o) (write (list (list 'insert 'root #f
                                                (list '(kind . section) '(title . "Big")
                                                      (cons 'src (make-string 1100000 #\a))))) o)))
               f))
         (t0 (real-time))
         (big (ask-in h s in "batch"))
         (small (ask h s "insert" "--under" "root" "--title" "Small" "--text" "b"))
         (ms (- (real-time) t0))
         (segs (filter segment-name? (files-under (string-append s "/writers/" w)))))
    (printf "note G3 the two appends took ~a ms\n" ms)
    (git repo "add" "store")
    (git repo "commit" "-q" "-m" "store")
    (sh git-env " git clone -q " (quoted repo) " " (quoted clone) " > /dev/null 2>&1")
    (want "G3 two appends past 1 MiB leave two or more segments, all staged by `git add <store>` and all in a clone"
          (list (and (pair? big) (eq? (car big) 'batch) (pair? (cdr big)) (pair? (cadr big)) (head-of (car (cadr big))))
                (head-of small) (>= (length segs) 2)
                (map (lambda (f) (and (member (string-append "store/writers/" w "/" f) (tracked repo)) #t)) segs)
                (map (lambda (f) (file-exists? (string-append clone "/store/writers/" w "/" f))) segs))
          (list 'ok 'ok #t (map (lambda (f) #t) segs) (map (lambda (f) #t) segs)))))

;; ---- G4 and G5: restore, and the stale twin -----------------------------------------
(define origin (string-append root "/origin.git"))
(define A (string-append root "/A"))
(define B (string-append root "/B"))
(define sA (string-append A "/store"))
(define sB (string-append B "/store"))
(define hA (home! "A"))
(define hB (home! "B"))
(sh "mkdir -p " (quoted sA))
(git root "init" "-q" "--bare" (quoted origin))
(git A "init" "-q")
(define initA (ask hA sA "init"))
(define wA (writer-of initA))
(ask hA sA "insert" "--under" "root" "--title" "One" "--text" "first")
(git A "add" "store")
(git A "commit" "-q" "-m" "one")
(git A "remote" "add" "origin" (quoted origin))
(git A "push" "-q" "origin" "HEAD:main")
(sh git-env " git clone -q -b main " (quoted origin) " " (quoted B) " > /dev/null 2>&1")
(define old-segments-in-B (segment-md5s sB wA))

(let ((r (ask hB sB "outline")))
  (want "G4 a clone reads the history before anything else (reads check no instance)"
        (list (head-of r) (has-substring? (format "~s" r) "One"))
        (list 'ok #t)))
(want "G4 a clone's first write is refused no-instance, with the remedy adopt"
      (ask hB sB "insert" "--under" "root" "--title" "Refused" "--text" "x")
      '(error refused no-instance (remedy adopt)))
(define adoptB (ask hB sB "adopt"))
(define wB (let ((c (clause-of adoptB 'to))) (and c (cadr c))))
(want "G4 adopt on the clone answers ok from the old writer to a new one, reason identity, (identity instance-absent)"
      (list (head-of adoptB) (clause-of adoptB 'from) (and wB (not (equal? wB wA)))
            (clause-of adoptB 'reason) (clause-of adoptB 'identity))
      (list 'ok (list 'from wA) #t '(reason identity) '(identity instance-absent)))
(let ((r (ask hB sB "outline")))
  (want "G4 after adopt the clone still reads the history"
        (list (head-of r) (has-substring? (format "~s" r) "One"))
        (list 'ok #t)))
(let ((ins (ask hB sB "insert" "--under" "root" "--title" "Two" "--text" "second")))
  (want "G4 an insert after adopt lands in the NEW generation; the old writer's segments are unchanged"
        (list (head-of ins)
              (and wB (> (string-length (text-of-file (string-append sB "/writers/" wB "/000001.sexp"))) 0))
              (equal? (segment-md5s sB wA) old-segments-in-B))
        (list 'ok #t #t)))

;; The new reason needs a local writer. A store with neither an instance
;; nor a local writer is not a checkout to adopt as an identity: adopt does
;; not take the identity path for it, so it mints no instance -- instance.sexp
;; is still absent afterwards. (What adopt answers for such a store is the
;; base's answer, unchanged here.)
(let* ((h (home! "g4c")) (s (string-append root "/g4c/store")))
  (sh "mkdir -p " (quoted s))
  (let* ((i (ask h s "init")) (w (writer-of i)))
    (sh "rm -f " (quoted (string-append s "/instance.sexp")) " "
        (quoted (string-append s "/writers/" w "/owner.sexp")))
    (let ((a (ask h s "adopt")))
      (want "G4c a store with no instance AND no local writer is not adopted as instance-absent: no identity clause, no instance minted"
            (list (and (pair? a) (not (eq? (car a) 'ok))) (clause-of a 'identity)
                  (file-exists? (string-append s "/instance.sexp")))
            (list #t #f #f)))))

;; The reason needs the FILE to be absent. An instance.sexp that is present
;; but holds the datum #f verifies as absent too; adopt must not mint over
;; it: no identity clause, and the file is left as it was.
(let* ((h (home! "g4d")) (s (string-append root "/g4d/store")))
  (sh "mkdir -p " (quoted s))
  (ask h s "init")
  (call-with-output-file (string-append s "/instance.sexp") (lambda (o) (put-string o "#f\n")) 'truncate)
  (let ((a (ask h s "adopt")))
    (want "G4d an instance.sexp that is present but holds #f is not adopted as instance-absent: the base's not-needed, no identity clause, the file unchanged"
          (list (head-of a) (clause-of a 'identity)
                (text-of-file (string-append s "/instance.sexp")))
          (list '(error not-needed) #f "#f\n"))))

;; A store directory that can be searched but not listed holds no proof that
;; instance.sexp is absent, so adopt answers as the base does. The G4d state
;; (a present instance.sexp holding #f) under such a directory.
(let* ((h (home! "g4f")) (s (string-append root "/g4f/store")))
  (sh "mkdir -p " (quoted s))
  (ask h s "init")
  (call-with-output-file (string-append s "/instance.sexp") (lambda (o) (put-string o "#f\n")) 'truncate)
  (sh "chmod 300 " (quoted s))
  ;; THE SETUP IS ASSERTED: the directory really cannot be listed by this
  ;; caller (a failed chmod, or privileges that bypass directory read,
  ;; would leave the fallback unexercised and the row green regardless).
  (let* ((unlistable (not (= 0 (sh "ls " (quoted s) " > /dev/null 2>&1"))))
         (a (ask h s "adopt")))
    (sh "chmod 755 " (quoted s))
    (want "G4f a store directory that cannot be listed (asserted: ls fails): adopt answers the base's not-needed, no identity clause, the file unchanged"
          (list unlistable (head-of a) (clause-of a 'identity) (text-of-file (string-append s "/instance.sexp")))
          (list #t '(error not-needed) #f "#f\n"))))

;; Nor a symlink at instance.sexp that points nowhere: a stat follows it and
;; answers absent, but the name is in the store's directory.
(let* ((h (home! "g4e")) (s (string-append root "/g4e/store")) (target (string-append root "/g4e/nowhere")))
  (sh "mkdir -p " (quoted s))
  (ask h s "init")
  (sh "rm -f " (quoted (string-append s "/instance.sexp")) " && ln -s " (quoted target) " "
      (quoted (string-append s "/instance.sexp")))
  (let ((a (ask h s "adopt")))
    (want "G4e a dangling symlink at instance.sexp is not adopted as instance-absent: the base's not-needed, no identity clause, still the same link"
          (list (head-of a) (clause-of a 'identity)
                (sh-out "readlink " (quoted (string-append s "/instance.sexp"))))
          (list '(error not-needed) #f (string-append target "\n")))))

;; The stale twin: A still has its own valid instance and writes to the
;; OLD generation; once it pulls B's commit it holds the successor and the
;; predecessor's retirement, and its next write is refused by name.
(git B "add" "store")
(git B "commit" "-q" "-m" "adopted")
(git B "push" "-q" "origin" "HEAD:main")
(define (size-of p) (if (file-exists? p) (string-length (text-of-file p)) -1))
(let* ((seg (string-append sA "/writers/" wA "/000001.sexp"))
       (size0 (size-of seg))
       (before (ask hA sA "insert" "--under" "root" "--title" "Three" "--text" "third"))
       (size1 (size-of seg)))
  (git A "pull" "-q" "--ff-only" "origin" "main")
  (let ((after (ask hA sA "insert" "--under" "root" "--title" "Four" "--text" "fourth"))
        (status (lines-of (git A "status" "--porcelain" "store"))))
    (want "G5 the old checkout writes to the old generation (its segment grows), then after the pull is refused (instance nonce); git reports the segment modified ( M)"
          (list (head-of before) (> size1 size0) after
                (and (member (string-append " M store/writers/" wA "/000001.sexp") status) #t))
          (list 'ok #t '(error refused (instance nonce)) #t))))

;; ---- G6: a retained temporary is never staged ----------------------------------------
;; An atomic write that fails after its temporary is written keeps the
;; temporary for a reader. init writes .gitignore before meta.sexp, so a
;; failed meta.sexp write leaves `meta.sexp.tmp-<pid>-<n>` in a store that
;; already excludes it.
(let* ((h (home! "g6")) (repo (string-append root "/g6")) (s (string-append repo "/store")))
  (sh "mkdir -p " (quoted s))
  (git repo "init" "-q")
  (sh "THEOURGIA_LOCAL=1 THEOURGIA_INJECT=on THEOURGIA_FAULT=fsync-fail@registry:file=meta.sexp THEOURGIA_HOME="
      (quoted h) " scheme --script ../core.sc init --store " (quoted s) " --wire > /dev/null 2>&1 < /dev/null")
  (let ((temps (filter (lambda (f) (has-substring? f ".tmp-")) (files-under s))))
    (git repo "add" "store")
    (want "G6 an injected failure leaves a temporary on disk; `git add <store>` stages the store's .gitignore and lock but not the temporary"
          (list (and (pair? temps) #t)
                (let ((t (tracked repo))) (list (and (member "store/.gitignore" t) #t) (and (member "store/lock" t) #t)))
                (filter (lambda (f) (has-substring? f ".tmp-")) (tracked repo)))
          (list #t '(#t #t) '()))))

;; ---- G7: the checkpoint is rebuilt, not carried ---------------------------------------
;; The original answers a tracked request and takes a snapshot, so its
;; checkpoint exists. A clone carries no checkpoint, has none after a read,
;; and has one only after its own first snapshot.
;;
;; A REPLAY OF A REQUEST MADE BEFORE THE RESTORE ANSWERS unknown ON THE CLONE,
;; before adopt and after it, pinned as measured. The replay barrier raises
;; the request writer's written frontier in the machine registry: without an
;; instance there is no identity to raise it under, and after adopt the
;; writer that answered is the retired predecessor, which the new home's
;; registry has no entry for. unknown is a non-answer -- the store does not
;; guess -- where the original answers (replay #t).
(let* ((h (home! "g7")) (hc (home! "g7c")) (repo (string-append root "/g7")) (s (string-append repo "/store"))
       (clone (string-append root "/g7-clone")) (sc (string-append clone "/store")))
  (sh "mkdir -p " (quoted s))
  (git repo "init" "-q")
  ;; A tracked request carries its id AND the cursor where the store stood
  ;; when the client sent it; one plain insert first makes sequence 1.
  (let* ((w (writer-of (ask h s "init")))
         (plain (ask h s "insert" "--under" "root" "--title" "Plain" "--text" "p"))
         (cursor (string-append w ":1"))
         (send-tracked (lambda (home store) (ask home store "insert" "--under" "root" "--title" "Req" "--text" "r"
                                            "--req" "G7R1" "--cursor" cursor)))
         (first (send-tracked h s))
         (snap (ask h s "snapshot"))
         (replay-original (send-tracked h s)))
    (git repo "add" "store")
    (git repo "commit" "-q" "-m" "store")
    (sh git-env " git clone -q " (quoted repo) " " (quoted clone) " > /dev/null 2>&1")
    (let* ((checkpoint? (lambda () (file-exists? (string-append sc "/request-index.sexp"))))
           (carried (checkpoint?))
           (before-adopt (send-tracked hc sc))
           (after-read (begin (ask hc sc "outline") (checkpoint?)))
           (adopt (ask hc sc "adopt"))
           (replay-clone (send-tracked hc sc))
           (after-replay (checkpoint?))
           (snapc (ask hc sc "snapshot"))
           (ck (text-of-file (string-append sc "/request-index.sexp")))
           ;; The file's header and digest, read as the product writes them:
           ;; (request-index 1 <this store's id> <digest of the body> <body>),
           ;; the digest being sha256-hex of the body's extended encoding,
           ;; and the body two lists.
           (ck-datum? (guard (e (#t #f))
                        (let* ((x (storable-decode (string->sexpr-extended ck)))
                               (body (and (list? x) (= (length x) 5) (list-ref x 4)))
                               (digest (lambda (v) (bytevector->hex (sha256 (string->utf8 (sexpr->string-extended (storable-encode v))))))))
                          (and body (eq? (car x) 'request-index) (eqv? (cadr x) 1)
                               (equal? (caddr x) (store-id-of sc)) (equal? (cadddr x) (digest body))
                               (list? body) (= (length body) 2) (list? (car body)) (list? (cadr body)))))))
      ;; WHAT THE LAST TWO ELEMENTS MEASURE, AND WHAT THEY DO NOT: the file
      ;; is a request index for THIS store -- its store id, the digest of its
      ;; body, a body of two lists -- and names the clone's store path. They
      ;; do not claim the product would load it: the loader also checks each
      ;; entry, and copying more of it here would be a third copy of the
      ;; loader rather than a measurement of it.
      (want "G7 the checkpoint is rebuilt, not carried: the original has one, the clone none, none after a read or a replay, and after its first snapshot a request index for THIS store (its id, its body's digest, a two-list body) naming the clone's store path"
            (list (head-of plain) (head-of first) (head-of snap) (file-exists? (string-append s "/request-index.sexp"))
                  carried after-read (head-of adopt) after-replay (head-of snapc) (checkpoint?)
                  ck-datum? (has-substring? ck sc))
            (list 'ok 'ok 'ok #t #f #f 'ok #f 'ok #t #t #t))
      (want "G7 a pre-restore request replays (replay #t) on the original and answers unknown on the clone, before adopt and after it (as measured; the store does not guess)"
            (list (clause-of replay-original 'replay) before-adopt replay-clone)
            (list '(replay #t)
                  '(error unknown (replay-barrier-failed "cannot raise the written frontier without the store's identity"))
                  '(error unknown (replay-barrier-failed "no registry entry to raise the written frontier on")))))))

(sh "chmod -R u+rwX " (quoted root) " 2>/dev/null; rm -rf " (quoted root))
(printf "\n~a failures\nrows: ~a\nstore-in-git complete\n" bad rows)
(exit (if (= bad 0) 0 1))
