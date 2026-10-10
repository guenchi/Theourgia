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
        (only (theourgia log) store-id-of verify-instance)
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
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

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
;; /snap/ and /derived/ (root directories), /writers/*/working/ (a directory),
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
          ((and (= k 1) dir? (member name '("snap" "derived"))) #t)
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
  '("/instance.sexp" "/request-index.sexp" "/snap/" "/derived/" "/writers/*/working/" "/writers/*/draft.lock"
    "*.tmp-*" "!/writers/*/incoming/*"))
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
    (want "G2 init writes .gitignore holding exactly the eight patterns, in order"
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

;; ---- G2d: the tables a supply writes are not staged ---------------------------------
;; A supply keeps its facts in derived/ (a table and the lock beside it); the
;; store's own files around it are staged as before.
(let* ((h (home! "g2d")) (repo (string-append root "/g2d")) (s (string-append repo "/store"))
       (f (string-append root "/g2d-supply.sexp")))
  (sh "mkdir -p " (quoted s))
  (git repo "init" "-q")
  (ask h s "init")
  (call-with-output-file f
    (lambda (p) (put-string p "(supply signatures (writer \"-\") (language \"javascript\") (source (vscode \"1.140.0\")) (files ()) (replaces ()))\n"))
    'truncate)
  (let ((a (ask h s "supply" "signatures" f)))
    (git repo "add" "store")
    (let ((t (tracked repo)))
      (want "G2d after a supply, derived/ holds the table and the lock, and `git add <store>` stages neither while meta.sexp and .gitignore are staged"
            (list a (map (lambda (n) (file-exists? (string-append s "/derived/" n)))
                         '("signatures-%2D-javascript.sexp" "lock"))
                  (filter (lambda (x) (has-substring? x "derived")) t)
                  (and (member "store/meta.sexp" t) #t) (and (member "store/.gitignore" t) #t))
            (list '(ok (supplied (facts 0) (files 0))) '(#t #t) '() #t #t)))))

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

;; The G4d state (a present instance.sexp holding #f) under a store directory
;; that can be searched but not listed: the name is present, asked of the
;; name, so adopt answers not-needed; the listing is no longer what decides.
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

;; ---- the name instance.sexp, asked of the name -------------------------------
(define (ask-env env home store verb . args)
  (let ((out (string-append root "/ask.out")))
    (sh env " THEOURGIA_LOCAL=1 THEOURGIA_HOME=" (quoted home) " scheme --script ../core.sc " verb " "
        (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
        "--store " (quoted store) " --wire > " (quoted out) " 2> /dev/null < /dev/null")
    (guard (e (#t 'UNREADABLE))
      (let ((d (call-with-input-file out read))) (if (eof-object? d) 'NO-ANSWER d)))))
;; -> (home store instance-path), a checkout with a local writer and NO
;; instance.sexp.
(define (checkout-without-instance! name)
  (let* ((h (home! name)) (s (string-append root "/" name "/store")))
    (sh "mkdir -p " (quoted s))
    (ask h s "init")
    (sh "rm -f " (quoted (string-append s "/instance.sexp")))
    (list h s (string-append s "/instance.sexp"))))

;; H2: a checkout without instance.sexp under a root that can be searched and
;; not read. The QUESTION -- is the name instance.sexp there -- is asked of
;; the name, and needs only search permission: adopt judges the instance
;; absent and goes on to mint one. The MINT needs more. A new entry is durable
;; only once its directory is flushed, and flushing a directory means opening
;; it, which needs the same read permission a listing needs. So on this root
;; the mint is written and renamed into place and cannot be made durable, and
;; adopt says so: `incomplete`, the root and EACCES, and the three steps it
;; did take. The base answered `not-needed` here, reading the unlistable root
;; as holding an instance it does not have.
(let* ((c (checkout-without-instance! "h2")) (h (car c)) (s (cadr c)))
  (sh "chmod 300 " (quoted s))
  (let* ((unlistable (not (= 0 (sh "ls " (quoted s) " > /dev/null 2>&1"))))
         (searchable (= 0 (sh "test -e " (quoted (string-append s "/meta.sexp")))))
         (writer (and (file-exists? (string-append s "/writers")) #t))
         (a (ask h s "adopt")))
    (sh "chmod 755 " (quoted s))
    (want "H2 a checkout with no instance.sexp under a root that can be searched but not read is judged instance-absent and minted, and the mint that cannot be made durable is answered incomplete: the root, EACCES, and create, write, rename of instance.sexp"
          (list unlistable searchable writer (head-of a)
                (clause-of a 'failed)
                (let ((w (clause-of a 'written)))
                  (and w (pair? (cdr w)) (list? (cadr w))
                       (list (map car (cadr w))
                             (let ((r (assq 'rename (cadr w))))
                               (and r (pair? (cdr r)) (pair? (cddr r)) (equal? (caddr r) (caddr c)))))))
                (file-exists? (caddr c)))
          (list #t #t #t '(error incomplete)
                (list 'failed (list 'path s) (list 'reason "Permission denied") (list 'errno 'EACCES))
                '((create write rename) #t)
                #t))))

;; H3: the inventory's question fails (stat-fail in its stage): adopt refuses
;; metadata-unreadable naming instance.sexp, and mints nothing. A second
;; adopt without the fault mints: the refusal left the store as it was.
(let* ((c (checkout-without-instance! "h3")) (h (car c)) (s (cadr c)) (inst (caddr c))
       (a (ask-env "THEOURGIA_INJECT=on THEOURGIA_FAULT=stat-fail@presence:instance.sexp" h s "adopt"))
       (after-a (file-exists? inst))
       (b (ask h s "adopt")))
  (want "H3 the inventory's presence question failing refuses metadata-unreadable naming instance.sexp, mints nothing, and a second adopt mints"
        (list (head-of a) (clause-of a 'path) after-a (head-of b) (file-exists? inst))
        (list '(error metadata-unreadable) (list 'path inst) #f 'ok #t)))

;; H3b: the decision's question fails (its own stage), after the inventory's
;; passed: the request's failure table names it, and nothing is minted.
(let* ((c (checkout-without-instance! "h3b")) (h (car c)) (s (cadr c)) (inst (caddr c))
       (a (ask-env "THEOURGIA_INJECT=on THEOURGIA_FAULT=stat-fail@presence-decision:instance.sexp" h s "adopt")))
  (want "H3b the decision's presence question failing answers unreadable with the path and errno EIO, and mints nothing"
        (list (head-of a) (clause-of a 'path) (clause-of a 'errno) (file-exists? inst))
        (list '(error unreadable) (list 'path inst) '(errno EIO) #f)))

;; H5: on a volume that folds case, INSTANCE.SEXP is the name instance.sexp.
;; The lowercase file init wrote is removed and INSTANCE.SEXP written holding
;; #f; the listing holds only the uppercase name. adopt must not mint over it.
(let* ((probe (string-append root "/h5-fold"))
       (_ (sh "mkdir -p " (quoted probe) " && touch " (quoted (string-append probe "/A"))))
       (folds (file-exists? (string-append probe "/a"))))
  (if (not folds)
      (printf "n/a  H5 not applicable: case-sensitive volume (A created, a not found)\n")
      (let* ((h (home! "h5")) (s (string-append root "/h5/store")))
        (sh "mkdir -p " (quoted s))
        (ask h s "init")
        (sh "rm -f " (quoted (string-append s "/instance.sexp")))
        (call-with-output-file (string-append s "/INSTANCE.SEXP") (lambda (o) (put-string o "#f\n")))
        (let* ((listing (lines-of (sh-out "ls " (quoted s))))
               (a (ask h s "adopt")))
          (want "H5 on a case-folding volume, INSTANCE.SEXP holding #f is the name instance.sexp: adopt answers not-needed and leaves it"
                (list (and (member "INSTANCE.SEXP" listing) #t) (and (member "instance.sexp" listing) #t)
                      (head-of a) (clause-of a 'identity) (text-of-file (string-append s "/INSTANCE.SEXP")))
                (list #t #f '(error not-needed) #f "#f\n"))))))

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
;; A REPLAY OF A REQUEST MADE BEFORE THE RESTORE answers unknown on the clone
;; before adopt -- without an instance there is no identity to count the
;; acknowledgement under, and the store does not guess -- and after adopt it
;; answers as the original does, (replay #t): the acknowledgement creates the
;; count it needs under the new instance (the P rows below read that count).
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
      (want "G7 a pre-restore request replays (replay #t) on the original; on the clone it answers unknown before adopt and (replay #t) after it"
            (list (clause-of replay-original 'replay) before-adopt (head-of replay-clone) (clause-of replay-clone 'replay))
            (list '(replay #t)
                  '(error unknown (replay-barrier-failed "cannot raise the written frontier without the store's identity"))
                  'ok '(replay #t))))))

;; ---- P: a pre-restore request is replayed after an identity adopt ---------------
;; The original O answers three tracked requests (R1 at sequence 2, R2 at 3,
;; R3 at 4). A clone C is adopted by identity, so every writer of O is under a
;; nonce C's machine registry holds no water mark for (only the adopt's
;; generation). A replay then acknowledges a
;; record this log holds and the barrier flushed, and the acknowledgement
;; creates the count it needs: the entry (store-id nonce writer seq active seq)
;; under C's NEW nonce, raised by max afterwards.
(define (read-sexpr-file p)
  (guard (e (#t 'UNREADABLE))
    (if (file-exists? p) (string->sexpr-extended (text-of-file p)) 'ABSENT)))
(define (marks-of home)
  (let ((r (read-sexpr-file (string-append home "/instances.sexp"))))
    (cond ((eq? r 'ABSENT) '())
          ((list? r) (filter (lambda (e) (and (list? e) (pair? e) (string? (car e)))) r))
          (else r))))
(define (gens-of home)
  (let ((r (read-sexpr-file (string-append home "/instances.sexp"))))
    (if (list? r) (filter (lambda (e) (and (pair? e) (eq? (car e) 'gen))) r) '())))
(define (marks-for home id nonce writer)
  (let ((ms (marks-of home)))
    (if (list? ms)
        (filter (lambda (e) (and (equal? (car e) id) (equal? (cadr e) nonce) (equal? (caddr e) writer))) ms)
        ms)))
(define (nonce-of store)
  (let ((d (read-sexpr-file (string-append store "/instance.sexp"))))
    (and (list? d)
         (let ((c (find (lambda (x) (and (list? x) (= 2 (length x)) (eq? (car x) 'nonce))) d)))
           (and c (cadr c))))))
;; (ask-rc env home store verb args...) -> (rc answer), with extra environment.
(define (ask-rc env home store verb . args)
  (let* ((out (string-append root "/ask-rc.out"))
         (rc (sh "THEOURGIA_LOCAL=1 THEOURGIA_HOME=" (quoted home) " " env " scheme --script ../core.sc " verb " "
                 (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                 "--store " (quoted store) " --wire > " (quoted out) " 2> /dev/null < /dev/null")))
    (list rc (guard (e (#t 'UNREADABLE))
               (let ((d (call-with-input-file out read))) (if (eof-object? d) 'NO-ANSWER d))))))
(define (exit-of rc) (if (> rc 255) (quotient rc 256) rc))

(define p-repo (string-append root "/p-orig"))
(define sO (string-append p-repo "/store"))
(define hO (home! "pO"))
(sh "mkdir -p " (quoted sO))
(git p-repo "init" "-q")
(define wO (writer-of (ask hO sO "init")))
(ask hO sO "insert" "--under" "root" "--title" "Plain" "--text" "p")
;; A tracked request is replayed with its own id and the cursor it was sent with.
(define (tracked-args req cursor) (list "insert" "--under" "root" "--title" req "--text" req "--req" req "--cursor" cursor))
(define (send! home store req cursor) (apply ask home store (tracked-args req cursor)))
(define cur1 (string-append wO ":1"))
(define cur2 (string-append wO ":2"))
(define cur3 (string-append wO ":3"))
(define r1-original (send! hO sO "PR1" cur1))
(define r2-original (send! hO sO "PR2" cur2))
(define r3-original (send! hO sO "PR3" cur3))
(want "P setup: the original answers three tracked requests at sequences 2, 3 and 4"
      (map (lambda (a) (clause-of a 'events)) (list r1-original r2-original r3-original))
      (list (list 'events (list (cons wO 2))) (list 'events (list (cons wO 3))) (list 'events (list (cons wO 4)))))
(git p-repo "add" "store")
(git p-repo "commit" "-q" "-m" "store")
(define (clone-of repo name)
  (let ((c (string-append root "/" name)))
    (sh git-env " git clone -q " (quoted repo) " " (quoted c) " > /dev/null 2>&1")
    c))
(define cC (clone-of p-repo "p-clone"))
(define sC (string-append cC "/store"))
(define hC (home! "pC"))
(define idO (store-id-of sO))
(want "P4 before adopt the replay is refused by name (no identity yet)"
      (send! hC sC "PR2" cur2)
      '(error unknown (replay-barrier-failed "cannot raise the written frontier without the store's identity")))
(define adoptC (ask hC sC "adopt"))
(define nC (nonce-of sC))
(define marks-before (marks-of hC))
(define entry-before (marks-for hC idO nC wO))
(define r2-replay (send! hC sC "PR2" cur2))
(define entry-after (marks-for hC idO nC wO))
(want "P1 after an identity adopt the replay answers as the original did: ok, (replay #t), (event (w-old . 3))"
      (list (head-of adoptC) (clause-of adoptC 'identity) (head-of r2-replay) (clause-of r2-replay 'replay)
            (clause-of r2-replay 'event))
      (list 'ok '(identity instance-absent) 'ok '(replay #t) (list 'event (cons wO 3))))
(want "P2 the registry held no entry for (id, new nonce, w-old) before the replay and exactly one after, the whole entry (id nonce w-old 3 active 3); entries under any other nonce are unchanged"
      (list (and nC #t) entry-before entry-after
            (equal? (filter (lambda (e) (not (equal? (cadr e) nC))) (if (list? marks-before) marks-before '()))
                    (filter (lambda (e) (not (equal? (cadr e) nC))) (let ((m (marks-of hC))) (if (list? m) m '())))))
      (list #t '() (list (list idO nC wO 3 'active 3)) #t))
(send! hC sC "PR3" cur3)
(define entry-m (marks-for hC idO nC wO))
(send! hC sC "PR1" cur1)
(define entry-k (marks-for hC idO nC wO))
(want "P3 a replay of a later record raises written (and authorised) to 4; a replay of an earlier one leaves it at 4 (max)"
      (list entry-m entry-k)
      (list (list (list idO nC wO 4 'active 4)) (list (list idO nC wO 4 'active 4))))

;; P5: A RECOVERY ADOPT UNDER THE SAME NONCE. The local writer's log is
;; restored from end 10 to a consistent prefix 6 (its segment copied at 6
;; and put back), keeping the tracked request at 4; the registry says 10, so
;; adopt answers registry-ahead and keeps the instance. The replay of the
;; request at 4 answers ok, and the entry is never lowered.
(define s5 (string-append root "/p5/store"))
(define h5 (home! "p5"))
(sh "mkdir -p " (quoted s5))
(define w5 (writer-of (ask h5 s5 "init")))
(for-each (lambda (t) (ask h5 s5 "insert" "--under" "root" "--title" t "--text" t)) '("a" "b" "c"))
(define r5 (send! h5 s5 "PR5" (string-append w5 ":3")))
(for-each (lambda (t) (ask h5 s5 "insert" "--under" "root" "--title" t "--text" t)) '("e" "f"))
(define seg5 (string-append s5 "/writers/" w5 "/000001.sexp"))
(sh "cp -p " (quoted seg5) " " (quoted (string-append root "/p5-seg-at-6")))
(for-each (lambda (t) (ask h5 s5 "insert" "--under" "root" "--title" t "--text" t)) '("g" "h" "i" "j"))
(define id5 (store-id-of s5))
(define n5 (nonce-of s5))
(define mark5-before (marks-for h5 id5 n5 w5))
(sh "cp -p " (quoted (string-append root "/p5-seg-at-6")) " " (quoted seg5))
(define adopt5 (ask h5 s5 "adopt"))
;; THE INSTANCE CHECK, ASKED IN THIS PROCESS under p5's machine home: the
;; recovery adopt kept the instance, so it still verifies -- the replay
;; below raises an entry that exists and creates nothing.
(define verify5
  (let ((old (getenv "THEOURGIA_HOME")))
    (putenv "THEOURGIA_HOME" h5)
    (let ((v (guard (e (#t 'RAISED)) (verify-instance s5))))
      (putenv "THEOURGIA_HOME" (or old ""))
      v)))
(define r5-replay (send! h5 s5 "PR5" (string-append w5 ":3")))
(define mark5-after (marks-for h5 id5 n5 w5))
(want "P5 setup: the request was at 4, the registry said authorised 10 and written 10 before the restore, and the instance still verifies after the recovery adopt"
      (list (clause-of r5 'events)
            (and (pair? mark5-before) (list-ref (car mark5-before) 3))
            (and (pair? mark5-before) (list-ref (car mark5-before) 5))
            verify5)
      (list (list 'events (list (cons w5 4))) 10 10 'ok))
(want "P5 a recovery adopt (reason registry-ahead, no identity clause, the nonce kept), then the replay of the request at 4 answers ok (replay #t)"
      (list (head-of adopt5) (clause-of adopt5 'reason) (clause-of adopt5 'identity) (equal? (nonce-of s5) n5)
            (head-of r5-replay) (clause-of r5-replay 'replay) (clause-of r5-replay 'event))
      (list 'ok '(reason registry-ahead) #f #t 'ok '(replay #t) (list 'event (cons w5 4))))
(define (count-of ms) (if (list? ms) (length ms) ms))
(want "P5 the entry is not lowered: exactly one entry before and after; authorised as before, written max(before, 4)"
      (list (count-of mark5-before) (count-of mark5-after)
            (and (pair? mark5-after) (list (list-ref (car mark5-after) 3) (list-ref (car mark5-after) 5))))
      (list 1 1
            (if (pair? mark5-before)
                (list (list-ref (car mark5-before) 3) (max 4 (list-ref (car mark5-before) 5)))
                'NO-ENTRY-BEFORE)))

;; P6: FAIL CLOSED AT THE FLUSH. On a fresh identity-adopted clone, the
;; replay's commit-stage fsync of the segment holding the record is made to
;; fail: the answer is unknown (the guard turns the raise into it), exit 1,
;; no written clause, and NO entry for w-old appears under the new nonce --
;; the count is created only after the flush returned.
(define c6 (clone-of p-repo "p6-clone"))
(define s6 (string-append c6 "/store"))
(define h6 (home! "p6"))
(ask h6 s6 "adopt")
(define n6 (nonce-of s6))
(define r6 (apply ask-rc (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=fsync-fail@commit:file="
                                        s6 "/writers/" wO "/000001.sexp")
                  h6 s6 (tracked-args "PR2" cur2)))
(want "P6 an injected fsync failure at the replay's flush: unknown (replay-barrier-failed \"unexpected failure\"), exit 1, no written clause, and no entry for w-old under the new nonce"
      (list (cadr r6) (exit-of (car r6)) (clause-of (cadr r6) 'written) (and n6 #t) (marks-for h6 idO n6 wO))
      (list '(error unknown (replay-barrier-failed "unexpected failure")) 1 #f #t '()))

;; P7: A MIRROR'S RECORD. A first clone C1 is adopted (writer w2) and answers
;; a tracked request; w2's segment is published into O, so O holds w2 as a
;; mirror. A second clone C2 of O is adopted, and the replay of that request
;; answers ok naming (w2 . k); C2's registry gains (id, nonce, w2) with
;; written k, and no owner file and no generation record for w2.
;; THE REQUEST'S CURSOR IS ON w2, NOT ON THE ORIGINAL'S WRITER. The identity
;; adopt records the original's writer as uncertain above the clone's copy
;; of it, open at the top -- the original may have gone on writing -- so a
;; new request whose cursor is on that writer cannot be told from one that
;; ran there, and is answered unknown (range-overlaps). A request sent to
;; the clone speaks to its own writer, from its start.
(define c71 (clone-of p-repo "p7-first"))
(define s71 (string-append c71 "/store"))
(define h71 (home! "p71"))
(define adopt71 (ask h71 s71 "adopt"))
(define w2 (let ((c (clause-of adopt71 'to))) (and c (cadr c))))
(define cur7 (string-append (or w2 "none") ":0"))
(define r7 (send! h71 s71 "PR7" cur7))
(define k7 (let ((c (clause-of r7 'events))) (and c (pair? (cadr c)) (cdar (cadr c)))))
(define pub7 (ask hO sO "publish" (or w2 "none") "1" (string-append s71 "/writers/" (or w2 "none") "/000001.sexp")))
(git p-repo "add" "store")
(git p-repo "commit" "-q" "-m" "mirror")
(define c72 (clone-of p-repo "p7-second"))
(define s72 (string-append c72 "/store"))
(define h72 (home! "p72"))
(define adopt72 (ask h72 s72 "adopt"))
(define n72 (nonce-of s72))
(define r7-replay (send! h72 s72 "PR7" cur7))
(want "P7 setup: the first clone answers the request as w2, and O publishes w2's segment"
      (list (and w2 (not (equal? w2 wO))) (and k7 #t) (head-of pub7))
      (list #t #t 'ok))
(want "P7 a mirror's record replays on the second clone: ok (replay #t) (event (w2 . k)); the entry (id nonce w2 k active k); no owner file and no generation record for w2"
      (list (head-of r7-replay) (clause-of r7-replay 'replay) (clause-of r7-replay 'event)
            (marks-for h72 idO n72 w2)
            (file-exists? (string-append s72 "/writers/" (or w2 "none") "/owner.sexp"))
            (filter (lambda (g) (member w2 g)) (gens-of h72)))
      (list 'ok '(replay #t) (list 'event (cons w2 k7)) (list (list idO n72 w2 k7 'active k7)) #f '()))

;; P8: TWO IDENTITY ADOPTS ON ONE MACHINE. C's adopted store is committed and
;; cloned again (D), adopted by identity under the SAME machine home as C; the
;; replay of the original's request answers ok and creates the entry under
;; D's newest nonce only -- C's entry is untouched.
(git cC "add" "store")
(git cC "commit" "-q" "-m" "adopted")
(define cD (clone-of cC "p8-clone"))
(define sD (string-append cD "/store"))
(define adoptD (ask hC sD "adopt"))
(define nD (nonce-of sD))
(define entry-D-before (marks-for hC idO nD wO))
(define rD (send! hC sD "PR2" cur2))
(want "P8 a checkout of a checkout: the replay answers ok, the entry under the newest nonce is absent before it and created by it, and the previous checkout's entry is unchanged"
      (list (head-of adoptD) (and nD (not (equal? nD nC))) entry-D-before (head-of rD) (clause-of rD 'replay)
            (marks-for hC idO nD wO) (marks-for hC idO nC wO))
      (list 'ok #t '() 'ok '(replay #t) (list (list idO nD wO 3 'active 3)) (list (list idO nC wO 4 'active 4))))

;; P9: A COPY THAT KEEPS instance.sexp IS NOT AN IDENTITY. C's adopted store
;; is copied file by file, as rsync would, instance.sexp included, and the
;; copy is used under a fresh machine home without an adopt. The identity
;; file is present but names another machine and another inode, so the
;; replay is refused by name as it was before replays could create an
;; entry, and no entry is created.
(define s9 (string-append root "/p9-copy/store"))
(define h9 (home! "p9"))
(sh "mkdir -p " (quoted (string-append root "/p9-copy")) "; cp -Rp " (quoted sC) " " (quoted s9))
(define r9 (send! h9 s9 "PR2" cur2))
;; The refusal's first three elements: a refusal also lists, in its written
;; clause, what it created on the way (the fresh home's lock), which is not
;; what these rows are about.
(define (refusal-of a) (if (and (list? a) (>= (length a) 3)) (list-head a 3) a))
(want "P9 an rsync-style copy under a fresh home: instance.sexp is carried, the replay is refused by name, and the home's registry holds no entry"
      (list (equal? (nonce-of s9) nC) (refusal-of r9) (marks-of h9))
      (list #t '(error unknown (replay-barrier-failed "no registry entry to raise the written frontier on")) '()))

;; P9b: THE SAME COPY UNDER A HOME THAT HOLDS THE WITNESS. The home carries
;; C's machine id and C's generation records for (id, nC) and no water mark,
;; so the generation condition holds and only the instance check can refuse:
;; the copy's instance.sexp names C's inode, not the copy's. The replay is
;; refused by name and the home gains no water mark.
(define s9b (string-append root "/p9b-copy/store"))
(define h9b (home! "p9b"))
(define gens9b (filter (lambda (g) (and (equal? (list-ref g 1) idO) (equal? (list-ref g 2) nC))) (gens-of hC)))
(sh "mkdir -p " (quoted (string-append root "/p9b-copy")) "; cp -Rp " (quoted sC) " " (quoted s9b)
    "; cp -p " (quoted (string-append hC "/machine.sexp")) " " (quoted (string-append h9b "/machine.sexp")))
(call-with-output-file (string-append h9b "/instances.sexp") (lambda (o) (write gens9b o) (newline o)))
(define r9b (send! h9b s9b "PR2" cur2))
(want "P9b a copy under a home holding this machine's id and the instance's generation: refused by name, and no water mark appears"
      (list (pair? gens9b) (equal? (nonce-of s9b) nC) (refusal-of r9b) (marks-of h9b))
      (list #t #t '(error unknown (replay-barrier-failed "no registry entry to raise the written frontier on")) '()))

;; P10: A KEYED ROW TOO SHORT TO BE A WATER MARK. A fresh identity-adopted
;; clone's registry is given (id nonce w-old 12) -- four fields under the
;; replay's key, which the raise leaves alone. It reads as NO entry: with the
;; adopt's generation present the replay creates the proper entry beside it;
;; in a home holding the row and this machine's id but no generation, the
;; replay is refused by name and the registry is left as it was.
(define (append-registry-row! home row)
  (let* ((r (read-sexpr-file (string-append home "/instances.sexp")))
         (rows (if (list? r) r '())))
    (call-with-output-file (string-append home "/instances.sexp")
      (lambda (o) (write (append rows (list row)) o) (newline o))
      'replace)))
(define c10 (clone-of p-repo "p10-clone"))
(define s10 (string-append c10 "/store"))
(define h10 (home! "p10"))
(define adopt10 (ask h10 s10 "adopt"))
(define n10 (nonce-of s10))
(append-registry-row! h10 (list idO n10 wO 12))
(define r10 (send! h10 s10 "PR2" cur2))
(want "P10 a four-field row under the key is no entry: with the generation present the replay answers ok and creates the proper entry beside the row"
      (list (head-of adopt10) (head-of r10) (clause-of r10 'replay) (marks-for h10 idO n10 wO))
      (list 'ok 'ok '(replay #t) (list (list idO n10 wO 12) (list idO n10 wO 3 'active 3))))
(define h10b (home! "p10b"))
(sh "cp -p " (quoted (string-append h10 "/machine.sexp")) " " (quoted (string-append h10b "/machine.sexp")))
(append-registry-row! h10b (list idO n10 wO 12))
;; ONLY THE GENERATION CONDITION CAN REFUSE HERE: the instance verifies
;; under this home (asked in this process, as P5 does), and the home holds
;; no generation. The registry is compared whole, before and after.
(define verify10b
  (let ((old (getenv "THEOURGIA_HOME")))
    (putenv "THEOURGIA_HOME" h10b)
    (let ((v (guard (e (#t 'RAISED)) (verify-instance s10))))
      (putenv "THEOURGIA_HOME" (or old ""))
      v)))
(define registry10b-before (read-sexpr-file (string-append h10b "/instances.sexp")))
(define r10b (send! h10b s10 "PR3" cur3))
(want "P10b the same row in a home with this machine's id and no generation: the instance verifies, the replay is refused by name, and the registry is unchanged"
      (list verify10b (gens-of h10b) (refusal-of r10b)
            (equal? (read-sexpr-file (string-append h10b "/instances.sexp")) registry10b-before)
            registry10b-before)
      (list 'ok '() '(error unknown (replay-barrier-failed "no registry entry to raise the written frontier on"))
            #t (list (list idO n10 wO 12))))

(sh "chmod -R u+rwX " (quoted root) " 2>/dev/null; rm -rf " (quoted root))
(printf "\n~a failures\nrows: ~a\nstore-in-git complete\n" bad rows)
(exit (if (= bad 0) 0 1))
