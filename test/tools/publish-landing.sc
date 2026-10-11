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
;; git worktree and committed there.
;;
;;   scheme --script test/tools/publish-landing.sc <store> <cut> <item> <worktree> [--freeze <dir>] [--push]
;;   scheme --script test/tools/publish-landing.sc --self-test
;;
;; <cut> is the landed cut, a literal as `tag` answers it (a tag name works
;; too); <item> names the landing in the commit message; <worktree> is a git
;; working copy. The steps, each refusing on failure:
;;
;;   1. export-code --cut <cut>, marked (the published form carries the marker
;;      lines), into a FRESH directory: an export writes its files and removes
;;      nothing else, so a fresh directory is the only one that holds the tree
;;      and nothing more;
;;   2. the PUBLICATION-ONLY paths of the worktree -- never in the store, kept in
;;      git: every path with a component that starts with a dot (.git, .github,
;;      .gitignore, .gitattributes: the import's walk does not enter them), and
;;      every file a text import skips (not UTF-8, or holding a NUL byte);
;;   3. rsync with deletion from the fresh directory over the worktree, the
;;      publication-only paths excluded, so they stay as they are;
;;   4. with --freeze <dir> (the freeze's export of the same cut), the worktree's
;;      files, the publication-only ones left out, are compared with the freeze
;;      export: the same set of paths, and for each the same bytes, a marked
;;      file's header compared without its cut. A difference refuses the commit;
;;   5. git add and commit "landing <item> at <cut>" (nothing to commit is said,
;;      not refused); with --push, git push.
;;
;; It prints what it did:
;;
;;   (publication (cut <cut served>) (files <n>) (kept (<path> ...))
;;                (comparison ok|skipped|(differ (<path> ...))) (commit <sha>|none) (pushed #t|#f))
;;
;; --self-test makes a scratch store and a scratch git repository and publishes
;; into it, printing one row each.

(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia languages) language-table language-for-path)
        (only (theourgia code-markers) projection-decode projection-header-wrapper?))

(register-verbs! extension-verbs)

;; ---- files ----------------------------------------------------------------------------------------

(define (bytes-of path)
  (guard (e (#t #f))
    (and (file-exists? path)
         (call-with-port (open-file-input-port path)
           (lambda (i) (let ((b (get-bytevector-all i))) (if (eof-object? b) (make-bytevector 0) b)))))))
(define (split-path p)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i (string-length p)) (reverse (cons (substring p start i) out)))
          ((char=? (string-ref p i) #\/) (loop (+ i 1) (+ i 1) (cons (substring p start i) out)))
          (else (loop (+ i 1) start out)))))
(define (dot-named? p)
  (exists (lambda (c) (and (> (string-length c) 0) (char=? (string-ref c 0) #\.))) (split-path p)))
;; Every regular file under DIR, relative, sorted; a directory named .git is
;; not entered (it is the repository, not the tree).
(define (files-under dir)
  (define (walk rel out)
    (let ((d (if (string=? rel "") dir (string-append dir "/" rel))))
      (fold-left (lambda (out name)
                   (let* ((r (if (string=? rel "") name (string-append rel "/" name)))
                          (p (string-append dir "/" r)))
                     (cond ((member name '("." ".." ".git")) out)
                           ((file-symbolic-link? p) (cons r out))
                           ((file-directory? p) (walk r out))
                           ((file-regular? p) (cons r out))
                           (else out))))
                 out (directory-list d))))
  (if (file-directory? dir) (list-sort string<? (walk "" '())) '()))
;; Not text to an import: not UTF-8 (decoded strictly: utf8->string would
;; replace a bad sequence and not say so), or holding a NUL byte.
(define strict-utf8 (make-transcoder (utf-8-codec) (eol-style none) (error-handling-mode raise)))
(define (not-text? b)
  (or (not (bytevector? b))
      (guard (e (#t #t)) (bytevector->string b strict-utf8) #f)
      (let loop ((i 0)) (and (< i (bytevector-length b)) (or (= 0 (bytevector-u8-ref b i)) (loop (+ i 1)))))))
(define (quoted s)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list s))) "'"))
(define (shell-out command out)
  (system (string-append command " > " (quoted out) " 2>&1 < /dev/null")))
(define (file-text path) (if (file-exists? path) (call-with-input-file path get-string-all) ""))
(define (first-line text)
  (let loop ((i 0)) (cond ((= i (string-length text)) text) ((char=? (string-ref text i) #\newline) (substring text 0 i)) (else (loop (+ i 1))))))

;; A file's bytes as the comparison reads them: a marked file's header without
;; its cut, and its blocks; any other file, its bytes.
(define (comparable p b)
  (let* ((entry (or (find (lambda (candidate) (projection-header-wrapper? candidate b)) (language-table)) (language-for-path p)))
         (d (guard (e (#t #f)) (projection-decode entry b))))
    (if (and (list? d) (= 2 (length d)) (list? (car d)) (>= (length (car d)) 5))
        (list 'marked (list-head (car d) 4) (list-tail (car d) 5) (cadr d))
        (list 'bytes b))))

;; ---- the publication ------------------------------------------------------------------------------

(define (publish! store cut item worktree freeze push? scratch)
  (call/cc
    (lambda (return)
      (define (refuse . what) (return (cons 'error what)))
      (define fresh (string-append scratch "/export"))
      (system (string-append "rm -rf " (quoted fresh) "; mkdir -p " (quoted fresh)))
      (unless (file-directory? (string-append worktree "/.git")) (refuse 'not-a-git-worktree worktree))
      (let* ((exported (rpc-dispatch store (list 'export-code fresh "--cut" cut) "publish-landing"))
             (served (and (pair? exported) (eq? (car exported) 'ok) (assq 'cut (cdr exported)))))
        (unless served (refuse 'export-failed exported))
        (let* ((kept (filter (lambda (p) (or (dot-named? p) (not-text? (bytes-of (string-append worktree "/" p)))))
                             (files-under worktree)))
               (exclude (string-append scratch "/exclude"))
               (_ (call-with-output-file exclude
                    (lambda (o)
                      (display ".*\n" o)
                      (for-each (lambda (p) (unless (dot-named? p) (display (string-append "/" p "\n") o))) kept))
                    'truncate))
               (rsync-out (string-append scratch "/rsync.out")))
          (unless (= 0 (shell-out (string-append "rsync -a --delete --exclude-from=" (quoted exclude) " "
                                                 (quoted (string-append fresh "/")) " " (quoted (string-append worktree "/")))
                                  rsync-out))
            (refuse 'rsync-failed (file-text rsync-out)))
          (let* ((published (filter (lambda (p) (not (member p kept))) (files-under worktree)))
                 (comparison
                   (if (not freeze)
                       'skipped
                       (let* ((frozen (files-under freeze))
                              (paths (list-sort string<? (fold-left (lambda (acc p) (if (member p acc) acc (cons p acc)))
                                                                    published frozen)))
                              (differ (filter (lambda (p)
                                                (let ((a (bytes-of (string-append worktree "/" p)))
                                                      (b (bytes-of (string-append freeze "/" p))))
                                                  (not (and (bytevector? a) (bytevector? b)
                                                            (equal? (comparable p a) (comparable p b))))))
                                              paths)))
                         (if (null? differ) 'ok (list 'differ differ))))))
            (unless (memq comparison '(ok skipped))
              (refuse 'comparison-failed comparison (list 'kept kept)))
            (let* ((git-out (string-append scratch "/git.out"))
                   (git (lambda (args) (shell-out (string-append "git -C " (quoted worktree) " " args) git-out))))
              (unless (= 0 (git "add -A")) (refuse 'git-add-failed (file-text git-out)))
              (let* ((clean? (= 0 (git "diff --cached --quiet")))
                     (commit (if clean?
                                 'none
                                 (begin
                                   (unless (= 0 (git (string-append "commit -q -m " (quoted (string-append "landing " item " at " cut)))))
                                     (refuse 'git-commit-failed (file-text git-out)))
                                   (git "rev-parse HEAD")
                                   (first-line (file-text git-out)))))
                     (pushed (and push?
                                  (begin (unless (= 0 (git "push -q")) (refuse 'git-push-failed (file-text git-out)))
                                         #t))))
                (list 'publication served (list 'files (length published)) (list 'kept kept)
                      (list 'comparison comparison) (list 'commit commit) (list 'pushed pushed))))))))))

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
  (define freeze (string-append root "/freeze"))
  (define (run . args) (rpc-dispatch store args "self-test"))
  (define (put! path text)
    (system (string-append "mkdir -p " (quoted (let loop ((i (- (string-length path) 1)))
                                                   (if (char=? (string-ref path i) #\/) (substring path 0 i) (loop (- i 1)))))))
    (call-with-output-file path (lambda (o) (put-string o text)) 'truncate))
  (define (git . args) (system (string-append "git -C " (quoted repo) " " (apply string-append args) " > /dev/null 2>&1 < /dev/null")))
  (define (git-text args) (let ((o (string-append root "/git-text"))) (shell-out (string-append "git -C " (quoted repo) " " args) o) (first-line (file-text o))))
  (define (guarded thunk) (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e)))) (thunk)))
  (system (string-append "rm -rf " (quoted root) "; mkdir -p " (quoted (string-append root "/home"))))
  (putenv "THEOURGIA_HOME" (string-append root "/home"))
  ;; The store: two files imported, the freeze taken, then a later change the
  ;; landed cut does not hold.
  (put! (string-append src "/a.sc") "(define (a) 'frozen)\n")
  (put! (string-append src "/notes.txt") "notes, frozen\n")
  (run 'init)
  (run 'import-code src)
  (run 'tag "freeze-x-1")
  (let* ((frozen-answer (run 'export-code freeze "--cut" "freeze-x-1"))
         (cut (let ((c (and (pair? frozen-answer) (eq? (car frozen-answer) 'ok) (assq 'cut (cdr frozen-answer)))))
                (and c (format "~s" (cadr c))))))
    ;; The later change goes through a marked export, so the import lands on
    ;; the same blocks (a file without its header would be a new file).
    (let ((edit (string-append root "/edit")))
      (run 'export-code edit)
      (let* ((p (string-append edit "/a.sc")) (t (file-text p)) (at (let loop ((i 0)) (cond ((> (+ i 8) (string-length t)) #f) ((string=? (substring t i (+ i 8)) "'frozen)") i) (else (loop (+ i 1)))))))
        (when at (put! p (string-append (substring t 0 at) "'later)" (substring t (+ at 8) (string-length t))))))
      (run 'import-code edit))
    ;; The repository: an older tree, a file the store does not hold, a
    ;; .gitignore and a binary file, both publication-only.
    (system (string-append "mkdir -p " (quoted repo)))
    (git "init -q")
    (git "config user.name self-test")
    (git "config user.email self-test@localhost")
    (put! (string-append repo "/a.sc") "(define (a) 'old)\n")
    (put! (string-append repo "/gone.sc") "(define (gone) 'old)\n")
    (put! (string-append repo "/.gitignore") "*.tmp\n")
    (call-with-port (open-file-output-port (string-append repo "/blob.bin") (file-options no-fail))
      (lambda (o) (put-bytevector o #vu8(0 1 2 255))))
    (git "add -A")
    (git "commit -q -m initial")
    (let* ((first (guarded (lambda () (publish! store cut "x" repo freeze #f (string-append root "/scratch1")))))
           (part (lambda (r k) (and (pair? r) (assq k (cdr r))))))
      (row "P-0 the freeze export answered a cut, and the later change landed after it (the head's a.sc says later)"
           (list (and cut #t) (and (pair? frozen-answer) (car frozen-answer))
                 (let ((now (string-append root "/now")))
                   (run 'export-code now "--raw")
                   (equal? (bytes-of (string-append now "/a.sc")) (string->utf8 "(define (a) 'later)\n"))))
           '(#t ok #t))
      (row "P-1 the publication writes the tree at the landed cut: the frozen a.sc and notes.txt, gone.sc deleted, .gitignore and blob.bin kept, compared equal to the freeze export"
           (list (and (pair? first) (car first))
                 (equal? (bytes-of (string-append repo "/a.sc")) (bytes-of (string-append freeze "/a.sc")))
                 (equal? (bytes-of (string-append repo "/notes.txt")) (bytes-of (string-append freeze "/notes.txt")))
                 (file-exists? (string-append repo "/gone.sc"))
                 (file-exists? (string-append repo "/.gitignore"))
                 (equal? (bytes-of (string-append repo "/blob.bin")) #vu8(0 1 2 255))
                 (and (part first 'kept) (cadr (part first 'kept)))
                 (and (part first 'comparison) (cadr (part first 'comparison))))
           '(publication #t #t #f #t #t (".gitignore" "blob.bin") ok))
      (row "P-2 the commit is \"landing x at <cut>\" and leaves nothing uncommitted"
           (list (git-text "log -1 --format=%s") (git-text "status --porcelain"))
           (list (string-append "landing x at " (or cut "")) ""))
      (let ((again (guarded (lambda () (publish! store cut "x" repo freeze #f (string-append root "/scratch2"))))))
        (row "P-3 published again at the same cut: nothing to commit, said, not refused"
             (list (and (pair? again) (car again)) (and (part again 'commit) (cadr (part again 'commit))))
             '(publication none)))
      ;; A freeze export that differs in one file refuses the commit.
      (let* ((wrong (string-append root "/freeze-wrong"))
             (_ (system (string-append "cp -R " (quoted freeze) " " (quoted wrong))))
             (_ (put! (string-append wrong "/notes.txt") "notes, not what landed\n"))
             (head-before (git-text "rev-parse HEAD"))
             (refused (guarded (lambda () (publish! store cut "x" repo wrong #f (string-append root "/scratch3"))))))
        (row "P-4 a freeze export that differs refuses the commit, naming the file"
             (list (and (pair? refused) (list-head refused (min 2 (length refused))))
                   (let ((c (and (pair? refused) (list? refused) (find (lambda (x) (and (pair? x) (eq? (car x) 'differ))) refused))))
                     (and c (cadr c)))
                   (equal? head-before (git-text "rev-parse HEAD")))
             '((error comparison-failed) ("notes.txt") #t)))))
  (system (string-append "rm -rf " (quoted root)))
  (printf "\n~a failures\nrows: ~a\npublish-landing self-test complete\n" bad rows)
  (exit (if (= bad 0) 0 1)))

;; ---- the command line ----------------------------------------------------------------------------

(define (option args name)
  (let loop ((xs args)) (cond ((null? xs) #f) ((string=? (car xs) name) (if (pair? (cdr xs)) (cadr xs) #t)) (else (loop (cdr xs))))))
(define args (cdr (command-line)))
(cond
  ((and (pair? args) (string=? (car args) "--self-test")) (self-test))
  ((>= (length args) 4)
   (let* ((scratch (string-append (or (getenv "TMPDIR") "/tmp") "/publish-landing-" (number->string (get-process-id))))
          (r (begin (system (string-append "mkdir -p " (quoted scratch)))
                    (publish! (list-ref args 0) (list-ref args 1) (list-ref args 2) (list-ref args 3)
                              (let ((f (option (list-tail args 4) "--freeze"))) (and (string? f) f))
                              (and (option (list-tail args 4) "--push") #t)
                              scratch))))
     (system (string-append "rm -rf " (quoted scratch)))
     (write r) (newline)
     (exit (if (and (pair? r) (eq? (car r) 'publication)) 0 1))))
  (else
   (write '(usage "scheme --script test/tools/publish-landing.sc <store> <cut> <item> <worktree> [--freeze <dir>] [--push] | --self-test"))
   (newline)
   (exit 2)))
