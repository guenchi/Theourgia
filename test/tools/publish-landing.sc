#!r6rs
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

;; THE PUBLICATION OF A LANDED CUT: the store's tree at the cut, written over a
;; git worktree and committed there. An operator script, not a fixture.
;;
;;   scheme --script test/tools/publish-landing.sc <store> <cut> <item> <worktree> --freeze <dir> [--push <remote> <branch>]
;;   scheme --script test/tools/publish-landing.sc --self-test
;;
;; <cut> is the landed cut, a literal as `tag` answers it; <item> names the
;; landing in the commit message; <worktree> is a git working copy (a linked
;; worktree too); --freeze names the freeze's export of the same cut, and is
;; required: every landing is compared.
;;
;; EVERY CHECK COMES BEFORE THE WORKTREE IS TOUCHED:
;;   1. the paths are made absolute; the worktree must be its repository's top
;;      level; the store, the freeze export and the scratch directory (a fresh
;;      mktemp -d) must lie outside the worktree;
;;   2. export-code --cut <cut>, marked, into a fresh directory inside the
;;      scratch one (an export removes nothing it did not write);
;;   3. the export compared with the freeze export: the same paths, and each
;;      file the same bytes but for the one header line (@file) of a marked
;;      file, whose decoded header may differ in its cut and nothing else;
;;   4. the PUBLICATION-ONLY paths of the worktree -- never in the store, kept in
;;      git: every path with a component that starts with a dot (.git, .github,
;;      .gitignore, .gitattributes: the import's walk does not enter them), and
;;      every file a text import skips (not UTF-8, decoded strictly, or holding a
;;      NUL). A file that cannot be read is refused, not kept: an import would
;;      have failed on it, not skipped it. A path holding a newline is refused.
;; THEN:
;;   5. rsync -a --checksum --delete from the export over the worktree, the
;;      publication-only paths excluded (each written as a literal pattern);
;;   6. the worktree, the publication-only paths left out, checked to equal the
;;      export byte for byte;
;;   7. git add -A, and every exported path added with -f (a .gitignore must not
;;      keep a published file out); the index checked to hold every exported
;;      path; commit "landing <item> at <cut>" (nothing to commit is said, not
;;      refused);
;;   8. with --push <remote> <branch>, and only when this run committed: the
;;      branch fetched, the new commit's parent required to be exactly the
;;      remote branch, then pushed to it.
;; A refusal says the state it leaves: untouched (steps 1-4), synced (5-6: the
;; worktree changed, nothing staged or committed), staged (7), or committed
;; <sha> (8: committed, not pushed).
;;
;;   (publication (cut <cut served>) (files <n>) (kept (<path> ...)) (comparison ok)
;;                (commit <sha>|none) (pushed #t|#f))
;;   (error <reason> (state untouched|synced|staged|(committed <sha>)) ...)
;;
;; --self-test makes a scratch store, a scratch git repository and a bare
;; remote, and publishes into them, printing one row each.

(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia languages) language-table language-for-path)
        (only (theourgia code-markers) projection-decode projection-header-wrapper?))

(register-verbs! extension-verbs)

;; ---- files and commands ---------------------------------------------------------------------------

(define (quoted s)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list s))) "'"))
(define (file-text path) (if (file-exists? path) (call-with-input-file path get-string-all) ""))
(define (first-line text)
  (let loop ((i 0)) (cond ((= i (string-length text)) text) ((char=? (string-ref text i) #\newline) (substring text 0 i)) (else (loop (+ i 1))))))
;; COMMAND with its output in OUT. -> its exit status, as the shell reports it
;; (written by the shell itself, so it is the command's own number).
(define (shell-out command out)
  (system (string-append "{ " command "; } > " (quoted out) " 2>&1 < /dev/null; echo $? > " (quoted (string-append out ".rc"))))
  (let ((n (string->number (first-line (file-text (string-append out ".rc"))))))
    (or n 255)))
;; The first line a command prints, read through a pipe (no file is written).
(define (command-first-line command)
  (guard (e (#t #f))
    (let* ((p (process (string-append command " 2>/dev/null")))
           (line (get-line (car p))))
      (close-port (car p)) (close-port (cadr p))
      (and (string? line) line))))
;; A directory's absolute, symlink-resolved path, or #f.
(define (real-dir dir scratch)
  (let ((out (string-append scratch "/real")))
    (and (= 0 (shell-out (string-append "cd " (quoted dir) " && pwd -P") out))
         (let ((p (first-line (file-text out)))) (and (> (string-length p) 0) (char=? (string-ref p 0) #\/) p)))))
(define (inside? path dir)
  (let ((d (string-append dir "/")))
    (or (string=? path dir)
        (and (>= (string-length path) (string-length d)) (string=? (substring path 0 (string-length d)) d)))))
;; A file's bytes, or 'unreadable.
(define (bytes-of path)
  (guard (e (#t 'unreadable))
    (call-with-port (open-file-input-port path)
      (lambda (i) (let ((b (get-bytevector-all i))) (if (eof-object? b) (make-bytevector 0) b))))))
(define (split-path p)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i (string-length p)) (reverse (cons (substring p start i) out)))
          ((char=? (string-ref p i) #\/) (loop (+ i 1) (+ i 1) (cons (substring p start i) out)))
          (else (loop (+ i 1) start out)))))
(define (dot-named? p)
  (exists (lambda (c) (and (> (string-length c) 0) (char=? (string-ref c 0) #\.))) (split-path p)))
;; Every regular file (or symbolic link) under DIR, relative, sorted; the entry
;; named .git at the top is the repository, not the tree, and is not entered.
(define (files-under dir)
  (define (walk rel out)
    (let ((d (if (string=? rel "") dir (string-append dir "/" rel))))
      (fold-left (lambda (out name)
                   (let* ((r (if (string=? rel "") name (string-append rel "/" name)))
                          (p (string-append dir "/" r)))
                     (cond ((member name '("." "..")) out)
                           ((and (string=? rel "") (string=? name ".git")) out)
                           ((file-symbolic-link? p) (cons r out))
                           ((file-directory? p) (walk r out))
                           (else (cons r out)))))
                 out (directory-list d))))
  (if (file-directory? dir) (list-sort string<? (walk "" '())) '()))
(define strict-utf8 (make-transcoder (utf-8-codec) (eol-style none) (error-handling-mode raise)))
;; Not text to an import: not UTF-8 (decoded strictly), or holding a NUL byte.
(define (not-text? b)
  (or (guard (e (#t #t)) (bytevector->string b strict-utf8) #f)
      (let loop ((i 0)) (and (< i (bytevector-length b)) (or (= 0 (bytevector-u8-ref b i)) (loop (+ i 1)))))))
;; TEXT split at its NUL characters, the empty piece after the last one dropped.
(define (split-nul text)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i (string-length text)) (reverse (if (< start i) (cons (substring text start i) out) out)))
          ((char=? (string-ref text i) #\nul) (loop (+ i 1) (+ i 1) (cons (substring text start i) out)))
          (else (loop (+ i 1) start out)))))
;; A path as an rsync pattern matching only itself: anchored, wildcards escaped.
(define (literal-pattern p)
  (string-append "/" (apply string-append (map (lambda (c) (if (memv c '(#\* #\? #\[ #\] #\\)) (string #\\ c) (string c))) (string->list p)))))

;; ---- the comparison -------------------------------------------------------------------------------

(define (lines-of b)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i (bytevector-length b))
           (reverse (if (< start i) (cons (subbytes b start i) out) out)))
          ((= 10 (bytevector-u8-ref b i)) (loop (+ i 1) (+ i 1) (cons (subbytes b start (+ i 1)) out)))
          (else (loop (+ i 1) start out)))))
(define (subbytes b from to) (let ((out (make-bytevector (- to from)))) (bytevector-copy! b from out 0 (- to from)) out))
(define (holds? b needle)
  (let ((n (bytevector-length needle)) (m (bytevector-length b)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((let check ((k 0)) (or (= k n) (and (= (bytevector-u8-ref b (+ i k)) (bytevector-u8-ref needle k)) (check (+ k 1))))) #t)
            (else (loop (+ i 1)))))))
(define (header-of p b)
  (let* ((entry (or (find (lambda (candidate) (projection-header-wrapper? candidate b)) (language-table)) (language-for-path p)))
         (d (guard (e (#t #f)) (projection-decode entry b))))
    (and (list? d) (= 2 (length d)) (list? (car d)) (= 7 (length (car d))) (car d))))
;; The same bytes, or the same but for the one @file header line, whose decoded
;; headers differ in the cut alone (the header is (code-projection 1 <store> <id>
;; <cut> text <pad>)).
(define (same-but-cut? p a b)
  (or (bytevector=? a b)
      (let ((la (lines-of a)) (lb (lines-of b)))
        (and (= (length la) (length lb))
             (let ((differing (filter (lambda (pair) (not (bytevector=? (car pair) (cdr pair)))) (map cons la lb))))
               (and (= 1 (length differing))
                    (holds? (car (car differing)) (string->utf8 "@file "))
                    (holds? (cdr (car differing)) (string->utf8 "@file "))
                    (let ((ha (header-of p a)) (hb (header-of p b)))
                      (and ha hb
                           (equal? (list-head ha 4) (list-head hb 4))
                           (equal? (list-tail ha 5) (list-tail hb 5))))))))))
;; The paths of two trees that are not the same but for the cut: missing on one
;; side, or differing.
(define (tree-differences left right)
  (let ((paths (list-sort string<? (fold-left (lambda (acc p) (if (member p acc) acc (cons p acc))) (files-under left) (files-under right)))))
    (filter (lambda (p)
              (let ((a (bytes-of (string-append left "/" p))) (b (bytes-of (string-append right "/" p))))
                (not (and (bytevector? a) (bytevector? b) (same-but-cut? p a b)))))
            paths)))

;; ---- the publication ------------------------------------------------------------------------------

(define (publish! store cut item worktree freeze push)
  (call/cc
    (lambda (return)
      (define state 'untouched)
      (define (refuse . what) (return (cons 'error (append what (list (list 'state state))))))
      (unless (and (string? freeze) (> (string-length freeze) 0)) (refuse 'no-freeze-export))
      (let* ((tmp (or (getenv "TMPDIR") "/tmp"))
             (scratch (command-first-line (string-append "mktemp -d " (quoted (string-append tmp "/publish-landing.XXXXXX")))))
             (_ (unless (and scratch (file-directory? scratch)) (refuse 'no-scratch-directory)))
             (scratch (or (real-dir scratch scratch) (refuse 'no-scratch-directory)))
             (git-out (string-append scratch "/git.out"))
             (result
               (call/cc
                 (lambda (done)
                   (define (refuse . what) (done (cons 'error (append what (list (list 'state state))))))
                   (let* ((tree (or (real-dir worktree scratch) (refuse 'not-a-directory worktree)))
                          (store-real (or (real-dir store scratch) (refuse 'not-a-directory store)))
                          (freeze-real (or (real-dir freeze scratch) (refuse 'not-a-directory freeze)))
                          (git (lambda (args) (shell-out (string-append "git -C " (quoted tree) " " args) git-out)))
                          (_ (unless (and (= 0 (git "rev-parse --show-toplevel"))
                                          (equal? (real-dir (first-line (file-text git-out)) scratch) tree))
                               (refuse 'not-a-git-worktree-top tree)))
                          (_ (for-each (lambda (p) (when (inside? p tree) (refuse 'inside-the-worktree p)))
                                       (list store-real freeze-real scratch)))
                          (fresh (string-append scratch "/export"))
                          (exported (rpc-dispatch store-real (list 'export-code fresh "--cut" cut) "publish-landing"))
                          (served (and (pair? exported) (eq? (car exported) 'ok) (assq 'cut (cdr exported))))
                          (_ (unless served (refuse 'export-failed exported)))
                          (frozen-diff (tree-differences fresh freeze-real))
                          (_ (unless (null? frozen-diff) (refuse 'comparison-failed (list 'differ frozen-diff))))
                          (worktree-files (files-under tree))
                          (_ (for-each (lambda (p) (when (memv #\newline (string->list p)) (refuse 'path-holds-a-newline p))) worktree-files))
                          (kept (filter (lambda (p)
                                          (or (dot-named? p)
                                              (let ((b (bytes-of (string-append tree "/" p))))
                                                (when (eq? b 'unreadable) (refuse 'unreadable p))
                                                (not-text? b))))
                                        worktree-files))
                          (exported-files (files-under fresh))
                          (exclude (string-append scratch "/exclude"))
                          (listing (string-append scratch "/exported")))
                     (call-with-output-file exclude
                       (lambda (o) (display ".*\n" o) (for-each (lambda (p) (unless (dot-named? p) (display (literal-pattern p) o) (newline o))) kept))
                       'truncate)
                     (set! state 'synced)
                     (unless (= 0 (shell-out (string-append "rsync -a --checksum --delete --exclude-from=" (quoted exclude) " "
                                                            (quoted (string-append fresh "/")) " " (quoted (string-append tree "/")))
                                             (string-append scratch "/rsync.out")))
                       (refuse 'rsync-failed (file-text (string-append scratch "/rsync.out"))))
                     (let ((published (filter (lambda (p) (not (member p kept))) (files-under tree))))
                       (unless (and (equal? published exported-files)
                                    (for-all (lambda (p) (equal? (bytes-of (string-append tree "/" p)) (bytes-of (string-append fresh "/" p))))
                                             published))
                         (refuse 'worktree-not-the-export (list 'published published))))
                     (set! state 'staged)
                     (call-with-output-file listing (lambda (o) (for-each (lambda (p) (display p o) (newline o)) exported-files)) 'truncate)
                     (unless (and (= 0 (git "add -A"))
                                  (= 0 (shell-out (string-append "git --literal-pathspecs -C " (quoted tree) " add -f --pathspec-from-file=" (quoted listing))
                                                  git-out)))
                       (refuse 'git-add-failed (file-text git-out)))
                     (let* ((index (begin (git "ls-files -z --cached") (split-nul (file-text git-out))))
                            (missing (filter (lambda (p) (not (member p index))) exported-files)))
                       (unless (null? missing) (refuse 'not-in-the-index missing)))
                     (let* ((diff-rc (git "diff --cached --quiet"))
                            (commit (cond ((= diff-rc 0) 'none)
                                          ((= diff-rc 1)
                                           (unless (= 0 (git (string-append "commit -q -m " (quoted (string-append "landing " item " at " cut)))))
                                             (refuse 'git-commit-failed (file-text git-out)))
                                           (unless (= 0 (git "rev-parse HEAD")) (refuse 'git-rev-parse-failed (file-text git-out)))
                                           (first-line (file-text git-out)))
                                          (else (refuse 'git-diff-failed (file-text git-out))))))
                       (when (string? commit) (set! state (list 'committed commit)))
                       (let ((pushed
                               (and push (string? commit)
                                    (let ((remote (car push)) (branch (cadr push)))
                                      (unless (= 0 (git (string-append "fetch -q " (quoted remote) " " (quoted branch))))
                                        (refuse 'git-fetch-failed (file-text git-out)))
                                      (let ((remote-head (begin (git "rev-parse FETCH_HEAD") (first-line (file-text git-out))))
                                            (parent (begin (git "rev-parse HEAD~1") (first-line (file-text git-out)))))
                                        (unless (equal? remote-head parent)
                                          (refuse 'remote-is-not-the-parent (list 'remote remote-head) (list 'parent parent))))
                                      (unless (= 0 (git (string-append "push -q " (quoted remote) " " (quoted (string-append "HEAD:" branch)))))
                                        (refuse 'git-push-failed (file-text git-out)))
                                      #t))))
                         (list 'publication served (list 'files (length exported-files)) (list 'kept kept)
                               '(comparison ok) (list 'commit commit) (list 'pushed pushed)))))))))
        (system (string-append "rm -rf " (quoted scratch)))
        result))))

;; ---- the self-test -------------------------------------------------------------------------------

(define (self-test)
  (define bad 0)
  (define rows 0)
  (define (row name got expected)
    (set! rows (+ rows 1))
    (if (equal? got expected)
        (printf "ok   ~a\n" name)
        (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
  (define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/publish-landing-" (number->string (get-process-id))))
  (define store (string-append root "/store"))
  (define src (string-append root "/src"))
  (define repo (string-append root "/repo"))
  (define bare (string-append root "/remote.git"))
  (define (run . args) (rpc-dispatch store args "self-test"))
  (define (parent-of p) (let loop ((i (- (string-length p) 1))) (if (char=? (string-ref p i) #\/) (substring p 0 i) (loop (- i 1)))))
  (define (put-bytes! path b)
    (system (string-append "mkdir -p " (quoted (parent-of path))))
    (when (file-exists? path) (delete-file path))
    (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (o) (put-bytevector o b))))
  (define (put! path text) (put-bytes! path (string->utf8 text)))
  (define (git . args) (system (string-append "git -C " (quoted repo) " " (apply string-append args) " > /dev/null 2>&1 < /dev/null")))
  (define (git-text args) (let ((o (string-append root "/git-text"))) (shell-out (string-append "git -C " (quoted repo) " " args) o) (first-line (file-text o))))
  (define (guarded thunk) (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e)))) (thunk)))
  (define (part r k) (and (pair? r) (list? r) (find (lambda (c) (and (pair? c) (eq? (car c) k))) (cdr r))))
  (define (head2 r) (and (pair? r) (list? r) (list-head r (min 2 (length r)))))
  ;; Every file of the repository's tree (not .git), with its bytes.
  (define (snapshot) (map (lambda (p) (cons p (bytes-of (string-append repo "/" p)))) (files-under repo)))
  (define (export-cut dir c) (run 'export-code dir "--cut" c))
  (define (cut-of answer) (let ((c (and (pair? answer) (eq? (car answer) 'ok) (assq 'cut (cdr answer))))) (and c (format "~s" (cadr c)))))
  (define (copy-tree from to) (system (string-append "rm -rf " (quoted to) "; cp -R " (quoted from) " " (quoted to))))
  (system (string-append "rm -rf " (quoted root) "; mkdir -p " (quoted (string-append root "/home"))))
  (putenv "THEOURGIA_HOME" (string-append root "/home"))
  ;; The store: three files imported (one of them *.tmp, which the repository's
  ;; .gitignore would hide), the freeze exported; then an event that changes
  ;; no file (a later cut, the same tree), and a later change to a.sc.
  (put! (string-append src "/a.sc") "(define (a) 'frozen)\n")
  (put! (string-append src "/notes.txt") "notes, frozen\n")
  (put! (string-append src "/keep.tmp") "kept although ignored\n")
  (run 'init)
  (run 'import-code src)
  (let* ((freeze (string-append root "/freeze"))
         (c1 (cut-of (export-cut freeze (begin (run 'tag "freeze-x-1") "freeze-x-1"))))
         (_ (run 'batch "((insert root #f ((kind . section) (title . \"not a file\"))))"))
         (c2 (cut-of (export-cut (string-append root "/c2-probe") (begin (run 'tag "later-x") "later-x"))))
         (_ (let ((edit (string-append root "/edit")))
              (run 'export-code edit)
              (let* ((p (string-append edit "/a.sc")) (t (file-text p))
                     (at (let loop ((i 0)) (cond ((> (+ i 8) (string-length t)) #f) ((string=? (substring t i (+ i 8)) "'frozen)") i) (else (loop (+ i 1)))))))
                (when at (put! p (string-append (substring t 0 at) "'later)" (substring t (+ at 8) (string-length t))))))
              (run 'import-code edit))))
    ;; The repository: an older tree; a stale file; publication-only paths: a
    ;; .gitignore, a nested dot file, a file with a NUL that is valid UTF-8, a
    ;; file that is not UTF-8 and holds no NUL, and one named with rsync's
    ;; wildcard characters; a bare remote it tracks.
    (system (string-append "mkdir -p " (quoted repo) "; git init -q --bare " (quoted bare) " > /dev/null 2>&1"))
    (git "init -q")
    (git "config user.name self-test")
    (git "config user.email self-test@localhost")
    (put! (string-append repo "/a.sc") "(define (a) 'old)\n")
    (put! (string-append repo "/gone.sc") "(define (gone) 'old)\n")
    (put! (string-append repo "/.gitignore") "*.tmp\n")
    (put! (string-append repo "/sub/.hidden") "hidden\n")
    (put-bytes! (string-append repo "/nul.dat") (bytevector 97 0 98))
    (put-bytes! (string-append repo "/bad.dat") (bytevector 255 254 65))
    (put-bytes! (string-append repo "/blob[1].bin") (bytevector 0 255))
    (put! (string-append repo "/blob1.bin") "(define (blob1) 'old)\n")
    (git "add -A")
    (git "commit -q -m initial")
    (git "remote add origin " (quoted bare))
    (git "push -q origin HEAD:main")
    (let* ((first (guarded (lambda () (publish! store c1 "x" repo freeze #f)))))
      (row "P-0 the freeze and the cuts: the freeze cut, a later cut with the same tree, the head's a.sc changed after both"
           (list (and c1 #t) (and c2 #t) (not (equal? c1 c2))
                 (let ((now (string-append root "/now")))
                   (run 'export-code now "--raw")
                   (equal? (bytes-of (string-append now "/a.sc")) (string->utf8 "(define (a) 'later)\n"))))
           '(#t #t #t #t))
      (row "P-1 published at the freeze cut: the frozen files, the stale gone.sc and blob1.bin deleted, the publication-only files kept byte for byte, compared ok"
           (list (and (pair? first) (car first))
                 (map (lambda (p) (equal? (bytes-of (string-append repo "/" p)) (bytes-of (string-append freeze "/" p))))
                      '("a.sc" "notes.txt" "keep.tmp"))
                 (map (lambda (p) (file-exists? (string-append repo "/" p))) '("gone.sc" "blob1.bin"))
                 (map (lambda (p) (bytes-of (string-append repo "/" p))) '(".gitignore" "sub/.hidden" "nul.dat" "bad.dat" "blob[1].bin"))
                 (and (part first 'kept) (cadr (part first 'kept)))
                 (and (part first 'comparison) (cadr (part first 'comparison))))
           (list 'publication '(#t #t #t) '(#f #f)
                 (list (string->utf8 "*.tmp\n") (string->utf8 "hidden\n") (bytevector 97 0 98) (bytevector 255 254 65) (bytevector 0 255))
                 '(".gitignore" "bad.dat" "blob[1].bin" "nul.dat" "sub/.hidden") 'ok))
      (row "P-2 the commit is \"landing x at <cut>\", the status clean, and keep.tmp in the index although .gitignore hides it"
           (list (git-text "log -1 --format=%s") (git-text "status --porcelain")
                 (git-text "ls-files --cached keep.tmp"))
           (list (string-append "landing x at " (or c1 "")) "" "keep.tmp"))
      (let ((again (guarded (lambda () (publish! store c1 "x" repo freeze #f)))))
        (row "P-3 published again at the same cut: nothing to commit, said, not refused"
             (list (and (pair? again) (car again)) (and (part again 'commit) (cadr (part again 'commit))))
             '(publication none)))
      (let ((later (guarded (lambda () (publish! store c2 "y" repo freeze #f)))))
        (row "P-4 published at a later cut with the same tree, against the freeze export: the marked headers differ in their cut alone, compared ok"
             (list (and (pair? later) (car later)) (and (part later 'comparison) (cadr (part later 'comparison))))
             '(publication ok)))
      ;; Each refusal before the worktree is touched leaves it and HEAD as they were.
      (let* ((wrong-text (string-append root "/freeze-text")) (wrong-mark (string-append root "/freeze-mark"))
             (extra (string-append root "/freeze-extra")) (missing (string-append root "/freeze-missing")))
        (copy-tree freeze wrong-text) (put! (string-append wrong-text "/notes.txt") "notes, not what landed\n")
        (copy-tree freeze wrong-mark)
        (let* ((p (string-append wrong-mark "/a.sc")) (t (file-text p))
               (at (let loop ((i 0)) (cond ((> (+ i 6) (string-length t)) #f) ((string=? (substring t i (+ i 6)) " pad 0") i) (else (loop (+ i 1)))))))
          (when at (put! p (string-append (substring t 0 at) (substring t (+ at 6) (string-length t))))))
        (copy-tree freeze extra) (put! (string-append extra "/extra.sc") "(define (extra) 1)\n")
        (copy-tree freeze missing) (delete-file (string-append missing "/notes.txt"))
        (let* ((before (snapshot)) (head (git-text "rev-parse HEAD"))
               (answers (map (lambda (f) (guarded (lambda () (publish! store c1 "x" repo f #f)))) (list wrong-text wrong-mark extra missing))))
          (row "P-5 a freeze export differing in text, in a marker line, by an extra file, by a missing file: each refused comparison-failed naming the path, untouched"
               (map (lambda (a) (list (head2 a) (let ((d (part a 'differ))) (and d (cadr d))) (let ((s (part a 'state))) (and s (cadr s))))) answers)
               '(((error comparison-failed) ("notes.txt") untouched)
                 ((error comparison-failed) ("a.sc") untouched)
                 ((error comparison-failed) ("extra.sc") untouched)
                 ((error comparison-failed) ("notes.txt") untouched)))
          (row "P-5 CONTROL: the worktree and HEAD are as they were after the four refusals"
               (list (equal? before (snapshot)) (equal? head (git-text "rev-parse HEAD")))
               '(#t #t))))
      (row "P-6 no freeze export, or a store inside the worktree: refused, untouched"
           (list (head2 (guarded (lambda () (publish! store c1 "x" repo #f #f))))
                 (head2 (guarded (lambda () (copy-tree store (string-append repo "/inner-store"))
                                            (let ((r (publish! (string-append repo "/inner-store") c1 "x" repo freeze #f)))
                                              (system (string-append "rm -rf " (quoted (string-append repo "/inner-store"))))
                                              r)))))
           '((error no-freeze-export) (error inside-the-worktree)))
      ;; The push: to the named remote and branch, only when this run committed,
      ;; and only onto the commit's own parent.
      ;; The earlier rows' commits are pushed first: the remote branch must be
      ;; the new commit's parent.
      (git "push -q origin HEAD:main")
      (let* ((edit (string-append root "/edit2"))
             (_ (run 'export-code edit))
             (_ (let* ((p (string-append edit "/a.sc")) (t (file-text p))
                       (at (let loop ((i 0)) (cond ((> (+ i 7) (string-length t)) #f) ((string=? (substring t i (+ i 7)) "'later)") i) (else (loop (+ i 1)))))))
                  (when at (put! p (string-append (substring t 0 at) "'pushed)" (substring t (+ at 7) (string-length t)))))
                  (run 'import-code edit)))
             (c3-freeze (string-append root "/freeze3"))
             (c3 (cut-of (export-cut c3-freeze (begin (run 'tag "freeze-x-3") "freeze-x-3"))))
             (pushed (guarded (lambda () (publish! store c3 "z" repo c3-freeze (list "origin" "main")))))
             (remote-head (let ((o (string-append root "/remote-head")))
                            (shell-out (string-append "git -C " (quoted bare) " rev-parse main") o) (first-line (file-text o)))))
        (row "P-7 --push origin main: committed, pushed, and the remote branch is the new commit"
             (list (and (pair? pushed) (car pushed)) (and (part pushed 'pushed) (cadr (part pushed 'pushed)))
                   (equal? remote-head (git-text "rev-parse HEAD")))
             '(publication #t #t)))))
  (system (string-append "rm -rf " (quoted root)))
  (printf "\n~a failures\nrows: ~a\npublish-landing self-test complete\n" bad rows)
  (exit (if (= bad 0) 0 1)))

;; ---- the command line ----------------------------------------------------------------------------

(define args (cdr (command-line)))
(define (after name) (let ((m (member name args))) (and m (cdr m))))
(cond
  ((and (pair? args) (string=? (car args) "--self-test")) (self-test))
  ((and (>= (length args) 6) (after "--freeze") (pair? (after "--freeze"))
        (not (string=? (car (after "--freeze")) "--push"))
        (or (not (after "--push")) (>= (length (after "--push")) 2)))
   (let ((r (publish! (list-ref args 0) (list-ref args 1) (list-ref args 2) (list-ref args 3)
                      (car (after "--freeze"))
                      (let ((p (after "--push"))) (and p (list (car p) (cadr p)))))))
     (write r) (newline)
     (exit (if (and (pair? r) (eq? (car r) 'publication)) 0 1))))
  (else
   (write '(usage "scheme --script test/tools/publish-landing.sc <store> <cut> <item> <worktree> --freeze <dir> [--push <remote> <branch>] | --self-test"))
   (newline)
   (exit 2)))
