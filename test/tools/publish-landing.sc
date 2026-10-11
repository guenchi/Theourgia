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
;; required: every landing is compared. A path or a TMPDIR holding a newline is
;; refused. --push takes a remote name and a branch name, both checked.
;;
;; EVERY CHECK COMES BEFORE THE WORKTREE IS TOUCHED:
;;   1. the paths are made absolute; the worktree must be its repository's top
;;      level; the store, the freeze export and the scratch directory (a fresh
;;      mktemp -d) must lie outside the worktree;
;;   2. export-code --cut <cut>, marked, into a fresh directory made in the
;;      scratch one (an export removes nothing it did not write);
;;   3. the export and the freeze export required to hold regular files only,
;;      then compared: the same paths, and each
;;      file the same bytes but for its header line: the freeze's line must be
;;      exactly the export's header line with the cut replaced (the same
;;      wrapping, every other field equal); an exported file that would be
;;      publication-only (a dot path, or not import text) is refused;
;;   4. the worktree read: outside the dot paths, an entry that is not a
;;      regular file (a symbolic link, a fifo) is refused, a file that cannot be read is refused, a path holding a
;;      newline is refused (in the worktree and in the export); the
;;      PUBLICATION-ONLY paths -- never in the store, kept in git -- are every
;;      path with a component that starts with a dot (.git, .github, .gitignore,
;;      .gitattributes: the import's walk does not enter them) and every file
;;      the import's own test (import-text?) does not take as text; such a
;;      file whose path holds a backslash is refused (it cannot be written as
;;      an rsync pattern every rsync reads alike).
;; THEN:
;;   5. rsync -a --checksum --delete from the export over the worktree, the
;;      publication-only paths excluded (each written as a literal pattern);
;;   6. the worktree checked: the publication-only files as they were, the rest
;;      equal to the export byte for byte;
;;   7. git add -A, and every exported path added with -f (a .gitignore must not
;;      keep a published file out); each exported path's staged blob compared
;;      with the export (a clean filter must not change what is committed);
;;      commit "landing <item> at <cut>" (nothing to commit is said, not refused);
;;      the commit made with --no-verify (no hook changes it), then its whole
;;      tree, the publication-only paths left out, compared with the export;
;;      an index entry marked skip-worktree or assume-unchanged is refused
;;      before the worktree is touched;
;;   8. with --push, and only when this run committed: the remote's push URL
;;      read, refs/heads/<branch> fetched from it (no tags), the new commit's
;;      parent required to be exactly that, then pushed to it (no hooks, no
;;      tags).
;; SCOPE: the script runs on our worktree under our git configuration. A
;; hostile configuration -- hooks, skip-worktree, a pushurl, push.followTags,
;; submodule recursion, core.fileMode and the like -- is out of scope; the
;; flags and options above are belts, not claims. The backstop is the
;; comparison of the commit's tree with the export, by paths and bytes (not
;; modes): whatever a configuration does to them, the publication is refused
;; committed-not-the-export rather than pushed.
;; A refusal says the state it leaves: untouched (1-4), synced (5-6: the worktree
;; changed, nothing staged), staged (7) or committed <sha> (8; a failed push
;; also says whether the remote ref was read back at the commit). An
;; unexpected failure is answered the same way, (error raised <message> (state
;; ...)), and the scratch directory is removed in every case.
;;
;;   (publication (cut <cut served>) (files <n>) (kept (<path> ...)) (comparison ok)
;;                (commit <sha>|none) (pushed #t|#f))
;;
;; --self-test makes a scratch store, a scratch git repository and a bare
;; remote, and publishes into them, printing one row each.

(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia languages) language-table language-for-path)
        (only (theourgia markers) import-text? marker-line)
        (only (theourgia wire) sexpr->string-extended storable-encode)
        (only (theourgia digest) bytevector->hex)
        (only (theourgia code-markers) projection-decode projection-header-wrapper?))

(register-verbs! extension-verbs)

;; ---- files and commands ---------------------------------------------------------------------------

(define (quoted s)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list s))) "'"))
;; A line break of either kind: rsync's filter files and git's path lists
;; end a line at either.
(define (has-newline? s) (and (string? s) (exists (lambda (c) (memv c '(#\newline #\return))) (string->list s)) #t))
(define (file-text path) (if (file-exists? path) (call-with-input-file path get-string-all) ""))
(define (first-line text)
  (let loop ((i 0)) (cond ((= i (string-length text)) text) ((char=? (string-ref text i) #\newline) (substring text 0 i)) (else (loop (+ i 1))))))
;; COMMAND with its output in OUT. -> its exit status, as the shell reports it.
(define (shell-out command out)
  (system (string-append "{ " command "; } > " (quoted out) " 2>&1 < /dev/null; echo $? > " (quoted (string-append out ".rc"))))
  (or (string->number (first-line (file-text (string-append out ".rc")))) 255))
;; The output of a command whose output is one line: the line, read through a
;; pipe, or #f when it printed more than one line or nothing.
(define (command-one-line command)
  (guard (e (#t #f))
    (let* ((p (process (string-append command " 2>/dev/null")))
           (text (get-string-all (car p))))
      (close-port (car p)) (close-port (cadr p))
      (and (string? text)
           (let ((line (first-line text)))
             (and (> (string-length line) 0)
                  (or (string=? text line) (string=? text (string-append line "\n")))
                  line))))))
;; A directory's absolute, symlink-resolved path, or #f.
(define (real-dir dir)
  (and (not (has-newline? dir))
       (let ((p (command-one-line (string-append "cd -P -- " (quoted dir) " && pwd -P"))))
         (and p (char=? (string-ref p 0) #\/) p))))
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
;; The entries under DIR, relative and sorted, as (files . others): files are
;; regular files and symbolic links; others (a fifo, a socket, a device) are
;; listed so a caller can refuse them. In a WORKTREE the entry named .git at
;; the top is the repository and is not entered; in an export or a freeze
;; export it is an entry like any other.
(define (entries-under dir . worktree?)
  (define (walk rel acc)
    (let ((d (if (string=? rel "") dir (string-append dir "/" rel))))
      (fold-left (lambda (acc name)
                   (let* ((r (if (string=? rel "") name (string-append rel "/" name)))
                          (p (string-append dir "/" r)))
                     (cond ((member name '("." "..")) acc)
                           ((and (pair? worktree?) (car worktree?) (string=? rel "") (string=? name ".git")) acc)
                           ((file-symbolic-link? p) (cons (cons r (car acc)) (cdr acc)))
                           ((file-directory? p) (walk r acc))
                           ((file-regular? p) (cons (cons r (car acc)) (cdr acc)))
                           (else (cons (car acc) (cons r (cdr acc)))))))
                 acc (directory-list d))))
  (if (file-directory? dir)
      (let ((r (walk "" (cons '() '()))))
        (cons (list-sort string<? (car r)) (list-sort string<? (cdr r))))
      (cons '() '())))
(define (files-under dir) (car (entries-under dir)))
(define (worktree-files-under dir) (car (entries-under dir #t)))
;; The entries under DIR that are not regular files: symbolic links and others.
(define (not-regular-under dir)
  (let ((e (entries-under dir)))
    (list-sort string<? (append (cdr e) (filter (lambda (p) (file-symbolic-link? (string-append dir "/" p))) (car e))))))
;; TEXT split at its NUL characters, the empty piece after the last one dropped.
(define (split-nul text)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i (string-length text)) (reverse (if (< start i) (cons (substring text start i) out) out)))
          ((char=? (string-ref text i) #\nul) (loop (+ i 1) (+ i 1) (cons (substring text start i) out)))
          (else (loop (+ i 1) start out)))))
;; Not text to an import: the import's own test, import-text? (markers.sc).
(define (not-text? b) (not (import-text? b)))
;; A path as an rsync pattern matching only itself: anchored, and each wildcard
;; character (* ? [) escaped by a backslash. A path holding a backslash is not
;; written as a pattern (the caller refuses it): rsync implementations disagree
;; on what a backslash before a backslash means.
(define (literal-pattern p)
  (string-append "/" (apply string-append (map (lambda (c) (if (memv c '(#\* #\? #\[)) (string #\\ c) (string c))) (string->list p)))))

;; Why an export cannot be published, or #f. A file of it that the
;; publication would classify as publication-only -- a dot path (rsync would
;; not write it, git would keep the worktree's), or a file the import does not
;; take as text (a store can hold such a src; the next publication would keep
;; it as the worktree's) -- would make the published tree not the export.
(define (export-refusal dir files)
  (let ((dots (filter dot-named? files)))
    (if (pair? dots)
        (list 'export-holds-a-dot-path dots)
        (let ((odd (filter (lambda (p) (let ((b (bytes-of (string-append dir "/" p)))) (or (not (bytevector? b)) (not-text? b)))) files)))
          (and (pair? odd) (list 'export-holds-not-text odd))))))

;; ---- the comparison -------------------------------------------------------------------------------

(define (subbytes b from to) (let ((out (make-bytevector (- to from)))) (bytevector-copy! b from out 0 (- to from)) out))
(define (lines-of b)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i (bytevector-length b))
           (reverse (if (< start i) (cons (subbytes b start i) out) out)))
          ((= 10 (bytevector-u8-ref b i)) (loop (+ i 1) (+ i 1) (cons (subbytes b start (+ i 1)) out)))
          (else (loop (+ i 1) start out)))))
;; The header line the projection writes for header H in ENTRY's wrapping.
(define (header-line entry h)
  (marker-line entry (string-append "@file " (bytevector->hex (string->utf8 (sexpr->string-extended (storable-encode h)))))))
;; A marked file's language entry, its decoded header and the header line the
;; projection writes for it, or #f. The header is (code-projection 1 <store>
;; <id> <cut> text <pad>).
(define (header-and-line p b)
  (let* ((entry (or (find (lambda (candidate) (projection-header-wrapper? candidate b)) (language-table)) (language-for-path p)))
         (d (guard (e (#t #f)) (projection-decode entry b))))
    (and (list? d) (= 2 (length d)) (list? (car d)) (= 7 (length (car d)))
         (let ((h (car d)))
           (guard (e (#t #f))
             (list entry h (header-line entry h)))))))
;; The same bytes, or the same but for the one header line: A's line is A's own
;; header line, and B's line is EXACTLY the line A's wrapping gives A's header
;; with its cut replaced by B's -- the same wrapping, every other field equal.
(define (same-but-cut? p a b)
  (or (bytevector=? a b)
      (let ((la (lines-of a)) (lb (lines-of b)))
        (and (= (length la) (length lb))
             (let ((differing (filter (lambda (pair) (not (bytevector=? (car pair) (cdr pair)))) (map cons la lb))))
               (and (= 1 (length differing))
                    (let ((ha (header-and-line p a)) (hb (header-and-line p b)))
                      (and ha hb
                           (let* ((entry (car ha)) (h (cadr ha))
                                  (with-b-cut (append (list-head h 4) (list (list-ref (cadr hb) 4)) (list-tail h 5))))
                             (and (bytevector=? (car (car differing)) (caddr ha))
                                  (bytevector=? (cdr (car differing)) (header-line entry with-b-cut))))))))))))
;; The paths of two trees that are not the same but for the cut.
(define (tree-differences left right)
  (let ((paths (list-sort string<? (fold-left (lambda (acc p) (if (member p acc) acc (cons p acc))) (files-under left) (files-under right)))))
    (filter (lambda (p)
              (let ((a (bytes-of (string-append left "/" p))) (b (bytes-of (string-append right "/" p))))
                (not (and (bytevector? a) (bytevector? b) (same-but-cut? p a b)))))
            paths)))

;; ---- names a push may take ------------------------------------------------------------------------

(define (remote-name? s)
  (and (string? s) (> (string-length s) 0) (not (char=? (string-ref s 0) #\-))
       (for-all (lambda (c) (or (char-alphabetic? c) (char-numeric? c) (memv c '(#\. #\_ #\-)))) (string->list s))))
(define (branch-name? s scratch)
  (and (string? s) (> (string-length s) 0) (not (char=? (string-ref s 0) #\-))
       (not (exists (lambda (c) (memv c '(#\: #\+ #\space #\newline))) (string->list s)))
       (= 0 (shell-out (string-append "git check-ref-format --branch " (quoted s)) (string-append scratch "/ref.out")))))

;; ---- the publication ------------------------------------------------------------------------------

(define (publish! store cut item worktree freeze push)
  (call/cc
    (lambda (return)
      (define state 'untouched)
      (define (refuse . what) (return (cons 'error (append what (list (list 'state state))))))
      (unless (and (string? freeze) (> (string-length freeze) 0)) (refuse 'no-freeze-export))
      (for-each (lambda (s) (when (has-newline? s) (refuse 'argument-holds-a-newline s))) (list store worktree freeze cut item))
      (let* ((tmp (or (getenv "TMPDIR") "/tmp"))
             (_ (when (has-newline? tmp) (refuse 'tmpdir-holds-a-newline)))
             (made (command-one-line (string-append "mktemp -d " (quoted (string-append tmp "/publish-landing.XXXXXX")))))
             (_ (unless (and made (file-directory? made)) (refuse 'no-scratch-directory)))
             (scratch (or (real-dir made) (refuse 'no-scratch-directory made))))
        (dynamic-wind
          (lambda () (if #f #f))
          (lambda ()
            (guard (e (#t (cons 'error (list 'raised (if (and (condition? e) (message-condition? e)) (condition-message e) e)
                                             (list 'state state)))))
              (call/cc
                (lambda (done)
                  (define (refuse . what) (done (cons 'error (append what (list (list 'state state))))))
                  (let* ((git-out (string-append scratch "/git.out"))
                         (tree (or (real-dir worktree) (refuse 'not-a-directory worktree)))
                         (store-real (or (real-dir store) (refuse 'not-a-directory store)))
                         (freeze-real (or (real-dir freeze) (refuse 'not-a-directory freeze)))
                         (git (lambda (args) (shell-out (string-append "git -C " (quoted tree) " " args) git-out)))
                         (_ (unless (and (= 0 (git "rev-parse --show-toplevel"))
                                         (equal? (real-dir (first-line (file-text git-out))) tree))
                              (refuse 'not-a-git-worktree-top tree)))
                         (_ (for-each (lambda (p) (when (inside? p tree) (refuse 'inside-the-worktree p)))
                                      (list store-real freeze-real scratch)))
                         ;; A merge in progress would make the publication a merge commit.
                         (_ (when (= 0 (git "rev-parse -q --verify MERGE_HEAD")) (refuse 'merge-in-progress)))
                         ;; An index entry marked skip-worktree or assume-unchanged is one
                         ;; git add -A leaves as it is: the commit would not be the export.
                         (flagged (begin (unless (= 0 (git "ls-files -v -z")) (refuse 'git-ls-files-failed (file-text git-out)))
                                         (filter (lambda (e) (and (> (string-length e) 2)
                                                                  (let ((tag (string-ref e 0))) (or (char=? tag #\S) (char-lower-case? tag)))))
                                                 (split-nul (file-text git-out)))))
                         (_ (unless (null? flagged) (refuse 'index-entry-flagged (map (lambda (e) (substring e 2 (string-length e))) flagged))))
                         (_ (when push
                              (unless (remote-name? (car push)) (refuse 'not-a-remote-name (car push)))
                              (unless (branch-name? (cadr push) scratch) (refuse 'not-a-branch-name (cadr push)))))
                         (fresh (string-append scratch "/export"))
                         (_ (unless (= 0 (shell-out (string-append "mkdir " (quoted fresh)) (string-append scratch "/mkdir.out")))
                              (refuse 'no-export-directory fresh)))
                         (exported (rpc-dispatch store-real (list 'export-code fresh "--cut" cut) "publish-landing"))
                         (served (and (pair? exported) (eq? (car exported) 'ok) (assq 'cut (cdr exported))))
                         (_ (unless served (refuse 'export-failed exported)))
                         (exported-files (files-under fresh))
                         (_ (for-each (lambda (p) (when (has-newline? p) (refuse 'path-holds-a-newline p))) exported-files))
                         (_ (let ((r (export-refusal fresh exported-files))) (when r (apply refuse r))))
                         (_ (for-each (lambda (d) (let ((odd (not-regular-under d))) (unless (null? odd) (refuse 'not-a-plain-tree d odd))))
                                      (list fresh freeze-real)))
                         (frozen-diff (tree-differences fresh freeze-real))
                         (_ (unless (null? frozen-diff) (refuse 'comparison-failed (list 'differ frozen-diff))))
                         (worktree-entries (entries-under tree #t))
                         (worktree-files (car worktree-entries))
                         ;; Outside the dot paths (which rsync leaves alone), every entry must
                         ;; be a regular file: the import refuses anything else, and reading a
                         ;; fifo, or a link to one, would block.
                         (not-files (filter (lambda (p) (not (dot-named? p)))
                                            (append (cdr worktree-entries)
                                                    (filter (lambda (p) (file-symbolic-link? (string-append tree "/" p))) worktree-files))))
                         (_ (unless (null? not-files) (refuse 'not-a-file (list-sort string<? not-files))))
                         (_ (for-each (lambda (p) (when (has-newline? p) (refuse 'path-holds-a-newline p))) worktree-files))
                         (kept (filter (lambda (p)
                                         (or (dot-named? p)
                                             (let ((b (bytes-of (string-append tree "/" p))))
                                               (when (eq? b 'unreadable) (refuse 'unreadable p))
                                               (not-text? b))))
                                       worktree-files))
                         (_ (for-each (lambda (p) (when (and (not (dot-named? p)) (memv #\\ (string->list p))) (refuse 'kept-path-holds-a-backslash p)))
                                      kept))
                         ;; Kept regular files, byte for byte; a kept symbolic link (in a dot
                         ;; path) is not followed.
                         (kept-before (map (lambda (p) (cons p (bytes-of (string-append tree "/" p))))
                                           (filter (lambda (p) (not (file-symbolic-link? (string-append tree "/" p)))) kept)))
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
                    (unless (for-all (lambda (k) (equal? (bytes-of (string-append tree "/" (car k))) (cdr k))) kept-before)
                      (refuse 'publication-only-file-changed (map car (filter (lambda (k) (not (equal? (bytes-of (string-append tree "/" (car k))) (cdr k)))) kept-before))))
                    (let ((published (filter (lambda (p) (not (member p kept))) (worktree-files-under tree))))
                      (unless (and (equal? published exported-files)
                                   (for-all (lambda (p) (equal? (bytes-of (string-append tree "/" p)) (bytes-of (string-append fresh "/" p))))
                                            published))
                        (refuse 'worktree-not-the-export (list 'published published))))
                    (set! state 'staged)
                    (call-with-port (open-file-output-port listing (file-options no-fail))
                      (lambda (o) (for-each (lambda (p) (put-bytevector o (string->utf8 p)) (put-u8 o 0)) exported-files)))
                    (unless (and (= 0 (git "add -A"))
                                 (= 0 (shell-out (string-append "git --literal-pathspecs -C " (quoted tree)
                                                                " add -f --pathspec-file-nul --pathspec-from-file=" (quoted listing))
                                                 git-out)))
                      (refuse 'git-add-failed (file-text git-out)))
                    ;; Each exported path's staged blob, as git stores it, against the export.
                    (let ((staged-differ
                            (filter (lambda (p)
                                      (let ((blob (string-append scratch "/blob")))
                                        (not (and (= 0 (shell-out (string-append "git -C " (quoted tree) " cat-file blob " (quoted (string-append ":0:" p))
                                                                                 " > " (quoted blob))
                                                                  git-out))
                                                  (equal? (bytes-of blob) (bytes-of (string-append fresh "/" p)))))))
                                    exported-files)))
                      (unless (null? staged-differ) (refuse 'staged-not-the-export staged-differ)))
                    (let* ((diff-rc (git "diff --cached --quiet"))
                           (commit (cond ((= diff-rc 0) 'none)
                                         ((= diff-rc 1)
                                          (unless (= 0 (git (string-append "commit -q --no-verify -m " (quoted (string-append "landing " item " at " cut)))))
                                            (refuse 'git-commit-failed (file-text git-out)))
                                          (set! state '(committed unknown))
                                          (unless (= 0 (git "rev-parse HEAD")) (refuse 'git-rev-parse-failed (file-text git-out)))
                                          (first-line (file-text git-out)))
                                         (else (refuse 'git-diff-failed (file-text git-out))))))
                      (when (string? commit) (set! state (list 'committed commit)))
                      ;; The commit's tree, the kept paths left out, against the export:
                      ;; the same paths, each blob the same bytes.
                      (let* ((head-paths (begin (unless (= 0 (git "ls-tree -r -z --name-only HEAD")) (refuse 'git-ls-tree-failed (file-text git-out)))
                                                (list-sort string<? (filter (lambda (p) (not (member p kept))) (split-nul (file-text git-out))))))
                             (blob (string-append scratch "/head-blob"))
                             (differ (filter (lambda (p)
                                               (not (and (= 0 (shell-out (string-append "git -C " (quoted tree) " cat-file blob " (quoted (string-append "HEAD:" p))
                                                                                        " > " (quoted blob))
                                                                         git-out))
                                                         (equal? (bytes-of blob) (bytes-of (string-append fresh "/" p))))))
                                             exported-files)))
                        (unless (and (equal? head-paths exported-files) (null? differ))
                          (refuse 'committed-not-the-export (list 'paths head-paths) (list 'differ differ))))
                      (let ((pushed
                              (and push (string? commit)
                                   ;; Fetched from and pushed to the one URL the push goes to (a
                                   ;; remote's pushurl may differ from its fetch url), with no tags.
                                   (let* ((url (or (command-one-line (string-append "git -C " (quoted tree) " remote get-url --push " (quoted (car push))))
                                                   (refuse 'no-push-url (car push))))
                                          (ref (string-append "refs/heads/" (cadr push))))
                                     (unless (= 0 (git (string-append "fetch -q --no-tags -- " (quoted url) " " (quoted ref))))
                                       (refuse 'git-fetch-failed (file-text git-out)))
                                     (let ((remote-head (and (= 0 (git "rev-parse FETCH_HEAD")) (first-line (file-text git-out))))
                                           (parent (and (= 0 (git "rev-parse HEAD~1")) (first-line (file-text git-out)))))
                                       (unless (and remote-head parent (equal? remote-head parent))
                                         (refuse 'remote-is-not-the-parent (list 'remote remote-head) (list 'parent parent))))
                                     ;; A push that fails may still have moved the remote (a reply lost
                                     ;; after the update): the remote ref is read back and named.
                                     (unless (= 0 (git (string-append "push -q --no-verify --no-follow-tags -- " (quoted url) " " (quoted (string-append "HEAD:" ref)))))
                                       (let ((said (file-text git-out))
                                             (now (command-one-line (string-append "git -C " (quoted tree) " ls-remote -- " (quoted url) " " (quoted ref)))))
                                         (refuse 'git-push-failed said
                                                 (list 'remote (cond ((not now) 'unknown)
                                                                     ((and (>= (string-length now) (string-length commit))
                                                                           (string=? (substring now 0 (string-length commit)) commit))
                                                                      'at-the-commit)
                                                                     (else 'not-at-the-commit))))))
                                     #t))))
                        (list 'publication served (list 'files (length exported-files)) (list 'kept kept)
                              '(comparison ok) (list 'commit commit) (list 'pushed pushed)))))))))
          (lambda () (system (string-append "rm -rf " (quoted scratch)))))))))

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
  (define other (string-append root "/other"))
  (define (run . args) (rpc-dispatch store args "self-test"))
  (define (parent-of p) (let loop ((i (- (string-length p) 1))) (if (char=? (string-ref p i) #\/) (substring p 0 i) (loop (- i 1)))))
  (define (put-bytes! path b)
    (system (string-append "mkdir -p " (quoted (parent-of path))))
    (when (file-exists? path) (delete-file path))
    (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (o) (put-bytevector o b))))
  (define (put! path text) (put-bytes! path (string->utf8 text)))
  (define (bytes-append . bs)
    (let ((out (make-bytevector (apply + (map bytevector-length bs)))))
      (let loop ((bs bs) (at 0))
        (if (null? bs) out
            (begin (bytevector-copy! (car bs) 0 out at (bytevector-length (car bs))) (loop (cdr bs) (+ at (bytevector-length (car bs)))))))))
  (define (git-in dir . args) (system (string-append "git -C " (quoted dir) " " (apply string-append args) " > /dev/null 2>&1 < /dev/null")))
  (define (git . args) (apply git-in repo args))
  (define (git-text-in dir args) (let ((o (string-append root "/git-text"))) (shell-out (string-append "git -C " (quoted dir) " " args) o) (first-line (file-text o))))
  (define (git-text args) (git-text-in repo args))
  (define (guarded thunk) (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e)))) (thunk)))
  (define (part r k) (and (pair? r) (list? r) (find (lambda (c) (and (pair? c) (eq? (car c) k))) (cdr r))))
  (define (head2 r) (and (pair? r) (list? r) (list-head r (min 2 (length r)))))
  (define (snapshot) (map (lambda (p) (cons p (bytes-of (string-append repo "/" p)))) (worktree-files-under repo)))
  (define (export-cut dir c) (run 'export-code dir "--cut" c))
  (define (cut-of answer) (let ((c (and (pair? answer) (eq? (car answer) 'ok) (assq 'cut (cdr answer))))) (and c (format "~s" (cadr c)))))
  (define (copy-tree from to) (system (string-append "rm -rf " (quoted to) "; cp -R " (quoted from) " " (quoted to))))
  ;; FILE's text with its first occurrence of OLD replaced by NEW, written back.
  (define (replace-in! file old new)
    (let* ((t (file-text file)) (n (string-length old))
           (at (let loop ((i 0)) (cond ((> (+ i n) (string-length t)) #f) ((string=? (substring t i (+ i n)) old) i) (else (loop (+ i 1)))))))
      (when at (put! file (string-append (substring t 0 at) new (substring t (+ at n) (string-length t)))))
      (and at #t)))
  ;; a.sc's source changed in the store, from OLD to NEW, through a marked export
  ;; (a file without its header would be imported as a new file).
  ;; -> #t when the edit was made and the import answered ok.
  (define (change-a! old new dir)
    (run 'export-code dir)
    (and (replace-in! (string-append dir "/a.sc") old new)
         (let ((a (run 'import-code dir))) (and (pair? a) (eq? (car a) 'ok)))))
  (define (holds-text? file text)
    (let ((t (file-text file)) (n (string-length text)))
      (let loop ((i 0)) (cond ((> (+ i n) (string-length t)) #f) ((string=? (substring t i (+ i n)) text) #t) (else (loop (+ i 1)))))))
  (system (string-append "rm -rf " (quoted root) "; mkdir -p " (quoted (string-append root "/home"))))
  (putenv "THEOURGIA_HOME" (string-append root "/home"))
  ;; The store: three files (keep.tmp is hidden by the repository's .gitignore),
  ;; the freeze; then an event that changes no file (a later cut, the same
  ;; tree); then a change to a.sc.
  (put! (string-append src "/a.sc") "(define (a) 'frozen)\n")
  (put! (string-append src "/notes.txt") "notes, frozen\n")
  (put! (string-append src "/keep.tmp") "kept although ignored\n")
  (put! (string-append src "/at.sc") "(define (at) \"@file one\")\n")
  (run 'init)
  (run 'import-code src)
  (let* ((freeze (string-append root "/freeze"))
         (c1 (cut-of (export-cut freeze (begin (run 'tag "freeze-x-1") "freeze-x-1"))))
         (_ (run 'batch "((insert root #f ((kind . section) (title . \"not a file\"))))"))
         (c2 (cut-of (export-cut (string-append root "/c2-probe") (begin (run 'tag "later-x") "later-x"))))
         (edit1 (change-a! "'frozen)" "'later)" (string-append root "/edit1"))))
    ;; The repository: an older tree; two stale files; publication-only files: a
    ;; .gitignore, a nested dot file, a file with a NUL that is valid UTF-8, a
    ;; file that is not UTF-8 and holds no NUL, a file with two byte-order marks,
    ;; and two named with rsync's pattern characters; a bare remote.
    (system (string-append "mkdir -p " (quoted repo) "; git init -q --bare -b main " (quoted bare) " > /dev/null 2>&1"))
    (git "init -q")
    (git "config user.name self-test")
    (git "config user.email self-test@localhost")
    (put! (string-append repo "/a.sc") "(define (a) 'old)\n")
    (put! (string-append repo "/gone.sc") "(define (gone) 'old)\n")
    (put! (string-append repo "/.gitignore") "*.tmp\n")
    (put! (string-append repo "/sub/.hidden") "hidden\n")
    (put-bytes! (string-append repo "/nul.dat") (bytevector 97 0 98))
    (put-bytes! (string-append repo "/bad.dat") (bytevector 255 254 65))
    (put-bytes! (string-append repo "/two-marks.txt") (bytevector #xEF #xBB #xBF #xEF #xBB #xBF 97))
    (put-bytes! (string-append repo "/blob[1].bin") (bytevector 0 255))
    (put-bytes! (string-append repo "/blob].bin") (bytevector 0 254))
    (put! (string-append repo "/blob1.bin") "(define (blob1) 'old)\n")
    (git "add -A")
    (git "commit -q -m initial")
    (git "remote add origin " (quoted bare))
    (git "push -q origin HEAD:refs/heads/main")
    (let* ((first (guarded (lambda () (publish! store c1 "x" repo freeze #f))))
           (kept-paths '(".gitignore" "bad.dat" "blob[1].bin" "blob].bin" "nul.dat" "sub/.hidden" "two-marks.txt")))
      (row "P-0 the freeze and the cuts: the freeze cut, a later cut with the same tree, the head's a.sc changed after both"
           (list (and c1 #t) (and c2 #t) (not (equal? c1 c2)) edit1
                 (let ((now (string-append root "/now")))
                   (run 'export-code now "--raw")
                   (equal? (bytes-of (string-append now "/a.sc")) (string->utf8 "(define (a) 'later)\n"))))
           '(#t #t #t #t #t))
      (row "P-1 published at the freeze cut: the frozen files, the stale files deleted, the publication-only files kept byte for byte, compared ok"
           (list (and (pair? first) (car first))
                 (map (lambda (p) (equal? (bytes-of (string-append repo "/" p)) (bytes-of (string-append freeze "/" p))))
                      '("a.sc" "notes.txt" "keep.tmp"))
                 (map (lambda (p) (file-exists? (string-append repo "/" p))) '("gone.sc" "blob1.bin"))
                 (map (lambda (p) (bytes-of (string-append repo "/" p))) kept-paths)
                 (and (part first 'kept) (cadr (part first 'kept)))
                 (and (part first 'comparison) (cadr (part first 'comparison))))
           (list 'publication '(#t #t #t) '(#f #f)
                 (list (string->utf8 "*.tmp\n") (bytevector 255 254 65) (bytevector 0 255) (bytevector 0 254)
                       (bytevector 97 0 98) (string->utf8 "hidden\n") (bytevector #xEF #xBB #xBF #xEF #xBB #xBF 97))
                 kept-paths 'ok))
      (row "P-2 the commit is \"landing x at <cut>\", the status clean, and keep.tmp in the index although .gitignore hides it"
           (list (git-text "log -1 --format=%s") (git-text "status --porcelain") (git-text "ls-files --cached keep.tmp"))
           (list (string-append "landing x at " (or c1 "")) "" "keep.tmp"))
      (let ((again (guarded (lambda () (publish! store c1 "x" repo freeze #f)))))
        (row "P-3 published again at the same cut: nothing to commit, said, not refused"
             (list (and (pair? again) (car again)) (and (part again 'commit) (cadr (part again 'commit))))
             '(publication none)))
      (let* ((head (git-text "rev-parse HEAD"))
             (later (guarded (lambda () (publish! store c2 "y" repo freeze #f)))))
        (row "P-4 published at a later cut with the same tree, against the freeze export: served at that cut, a.sc's header differs (a new commit), compared ok"
             (list (and (pair? later) (car later))
                   (equal? (part later 'cut) (list 'cut (with-input-from-string c2 read)))
                   (not (equal? (bytes-of (string-append repo "/a.sc")) (bytes-of (string-append freeze "/a.sc"))))
                   (let ((c (part later 'commit))) (and c (string? (cadr c)) (not (equal? (cadr c) head))))
                   (and (part later 'comparison) (cadr (part later 'comparison))))
             '(publication #t #t #t ok)))
      ;; Each refusal before the worktree is touched leaves it and HEAD as they were.
      (let* ((wrong-text (string-append root "/freeze-text")) (wrong-mark (string-append root "/freeze-mark"))
             (wrong-at (string-append root "/freeze-at")) (extra (string-append root "/freeze-extra"))
             (missing (string-append root "/freeze-missing")) (wrong-id (string-append root "/freeze-id"))
             (wrong-wrap (string-append root "/freeze-wrap")) (with-git (string-append root "/freeze-git"))
             ;; FILE's header line replaced by the line ENTRY-OF gives the header
             ;; MUTATE makes. -> #t when the line was found and replaced by another.
             (rewrite-header!
               (lambda (file p mutate entry-of)
                 (let* ((b (bytes-of file)) (hal (and (bytevector? b) (header-and-line p b))))
                   (and hal
                        (let* ((old (caddr hal)) (h (cadr hal))
                               (new (header-line (entry-of (car hal) h) (mutate h))))
                          (and (not (bytevector=? old new))
                               (begin
                                 (put-bytes! file (apply bytes-append (map (lambda (l) (if (bytevector=? l old) new l)) (lines-of b))))
                                 #t))))))))
        (copy-tree freeze wrong-text)
        (copy-tree freeze wrong-mark)
        (copy-tree freeze wrong-at)
        (copy-tree freeze extra) (put! (string-append extra "/extra.sc") "(define (extra) 1)\n")
        (copy-tree freeze missing) (delete-file (string-append missing "/notes.txt"))
        (copy-tree freeze wrong-id)
        (copy-tree freeze wrong-wrap)
        (copy-tree freeze with-git) (put! (string-append with-git "/.git/x") "not the export\n")
        (let* ((edits (list (replace-in! (string-append wrong-text "/notes.txt") "notes, frozen" "notes, Frozen")
                            (replace-in! (string-append wrong-mark "/a.sc") " pad 0" "")
                            (replace-in! (string-append wrong-at "/at.sc") "@file one" "@file two")
                            ;; a validly encoded header naming another store
                            (rewrite-header! (string-append wrong-id "/a.sc") "a.sc"
                                             (lambda (h) (append (list-head h 2) (list "another-store") (list-tail h 3)))
                                             (lambda (entry h) entry))
                            ;; the same header in another language's wrapping
                            (rewrite-header! (string-append wrong-wrap "/a.sc") "a.sc" (lambda (h) h)
                                             (lambda (entry h)
                                               (or (find (lambda (e) (not (bytevector=? (header-line e h) (header-line entry h)))) (language-table))
                                                   entry)))))
               (before (snapshot)) (head (git-text "rev-parse HEAD"))
               ;; At the freeze's own cut the headers are equal, so each copy differs
               ;; from the export in its one edit alone.
               (answers (map (lambda (f) (guarded (lambda () (publish! store c1 "x" repo f #f))))
                             (list wrong-text wrong-mark wrong-at extra missing wrong-id wrong-wrap with-git))))
          (row "P-5 PREMISE: each freeze copy was changed as its row says (a body byte, a marker line, a source line holding \"@file \" on both sides, the header's store, the header's wrapping)"
               edits '(#t #t #t #t #t))
          (row "P-5 a freeze export differing in a body byte, a marker line, a source line holding @file, by an extra file, by a missing file, in a header field other than the cut, in the header's wrapping, by a .git entry: each refused comparison-failed naming the path, untouched"
               (map (lambda (a) (list (head2 a) (let ((d (part a 'differ))) (and d (cadr d))) (let ((s (part a 'state))) (and s (cadr s))))) answers)
               '(((error comparison-failed) ("notes.txt") untouched)
                 ((error comparison-failed) ("a.sc") untouched)
                 ((error comparison-failed) ("at.sc") untouched)
                 ((error comparison-failed) ("extra.sc") untouched)
                 ((error comparison-failed) ("notes.txt") untouched)
                 ((error comparison-failed) ("a.sc") untouched)
                 ((error comparison-failed) ("a.sc") untouched)
                 ((error comparison-failed) (".git/x") untouched)))
          (row "P-5 CONTROL: the worktree and HEAD are as they were after the eight refusals"
               (list (equal? before (snapshot)) (equal? head (git-text "rev-parse HEAD")))
               '(#t #t))))
      (let ((probe (string-append root "/export-probe")))
        (put! (string-append probe "/a.sc") "(define (a) 1)\n")
        (put-bytes! (string-append probe "/n.dat") (bytevector 110 0 10))
        (put! (string-append probe "/sub/.x") "dot\n")
        (let* ((with-dot (export-refusal probe (files-under probe)))
               (_ (delete-file (string-append probe "/sub/.x")))
               (with-nul (export-refusal probe (files-under probe))))
          (row "P-10 an export holding a dot path, then one holding a file with a NUL, then the freeze export: refused, refused, taken"
               (list with-dot with-nul (export-refusal freeze (files-under freeze)))
               '((export-holds-a-dot-path ("sub/.x")) (export-holds-not-text ("n.dat")) #f))))
      ;; Each case: the answer's head, its state, and whether the worktree, the
      ;; index (with its flags) and HEAD read the same after it as before.
      (let* ((index-text (lambda () (let ((o (string-append root "/index-text")))
                                      (shell-out (string-append "git -C " (quoted repo) " ls-files -s -v") o) (file-text o))))
             (measured (lambda (thunk)
                         (let* ((before (snapshot)) (head (git-text "rev-parse HEAD")) (index (index-text))
                                (r (guarded thunk)))
                           (list (head2 r) (let ((s (part r 'state))) (and s (cadr s)))
                                 (and (equal? before (snapshot)) (equal? head (git-text "rev-parse HEAD")) (equal? index (index-text)))))))
             (inner (string-append repo "/inner-store"))
             (no-freeze (measured (lambda () (publish! store c1 "x" repo #f #f))))
             (inside (begin (copy-tree store inner)
                            (let ((m (measured (lambda () (publish! inner c1 "x" repo freeze #f)))))
                              (system (string-append "rm -rf " (quoted inner)))
                              m)))
             (refspec (measured (lambda () (publish! store c1 "x" repo freeze (list "origin" "+main:refs/heads/victim")))))
             (flagged (begin (git "update-index --skip-worktree notes.txt")
                             (let ((m (measured (lambda () (publish! store c1 "x" repo freeze #f)))))
                               (git "update-index --no-skip-worktree notes.txt")
                               m))))
        (row "P-6 no freeze export; a store inside the worktree; a branch that is a refspec; an index entry marked skip-worktree: each refused, state untouched, worktree, index and HEAD unchanged"
             (list no-freeze inside refspec flagged)
             '(((error no-freeze-export) untouched #t) ((error inside-the-worktree) untouched #t)
               ((error not-a-branch-name) untouched #t) ((error index-entry-flagged) untouched #t))))
      ;; The push: to the named remote and branch, only when this run committed,
      ;; and only onto the commit's own parent. The earlier commits are pushed
      ;; first.
      (git "push -q origin HEAD:refs/heads/main")
      (let* ((head-before (git-text "rev-parse HEAD"))
             (edit2 (change-a! "'later)" "'pushed)" (string-append root "/edit2")))
             (freeze3 (string-append root "/freeze3"))
             (c3 (cut-of (export-cut freeze3 (begin (run 'tag "freeze-x-3") "freeze-x-3"))))
             (pushed (guarded (lambda () (publish! store c3 "z" repo freeze3 (list "origin" "main")))))
             (head-after (git-text "rev-parse HEAD")))
        (row "P-7 --push origin main: a new commit, carrying the change, pushed, and the remote branch is that commit"
             (list edit2 (holds-text? (string-append freeze3 "/a.sc") "'pushed)")
                   (and (pair? pushed) (car pushed)) (not (equal? head-before head-after))
                   (let ((c (part pushed 'commit))) (and c (equal? (cadr c) head-after)))
                   (equal? (bytes-of (string-append repo "/a.sc")) (bytes-of (string-append freeze3 "/a.sc")))
                   (and (part pushed 'pushed) (cadr (part pushed 'pushed)))
                   (equal? (git-text-in bare "rev-parse main") head-after))
             '(#t #t publication #t #t #t #t #t))
        (let ((again (guarded (lambda () (publish! store c3 "z" repo freeze3 (list "origin" "main"))))))
          (row "P-8 --push with nothing new to commit: commit none, nothing pushed"
               (list (and (pair? again) (car again)) (and (part again 'commit) (cadr (part again 'commit)))
                     (and (part again 'pushed) (cadr (part again 'pushed))))
               '(publication none #f)))
        ;; Another clone moves the remote branch on; the next publication's parent
        ;; is not the remote branch: committed, not pushed.
        (system (string-append "git clone -q --branch main " (quoted bare) " " (quoted other) " > /dev/null 2>&1"))
        (git-in other "config user.name other")
        (git-in other "config user.email other@localhost")
        (put! (string-append other "/elsewhere.txt") "from another clone\n")
        (git-in other "add -A")
        (git-in other "commit -q -m elsewhere")
        (git-in other "push -q origin HEAD:refs/heads/main")
        (let* ((moved-on (equal? (git-text-in bare "rev-parse main") (git-text-in other "rev-parse HEAD")))
               (edit3 (change-a! "'pushed)" "'fourth)" (string-append root "/edit3")))
               (freeze4 (string-append root "/freeze4"))
               (c4 (cut-of (export-cut freeze4 (begin (run 'tag "freeze-x-4") "freeze-x-4"))))
               (moved (guarded (lambda () (publish! store c4 "w" repo freeze4 (list "origin" "main"))))))
          (row "P-9 the remote branch moved on elsewhere: refused remote-is-not-the-parent, committed and not pushed"
               (list moved-on edit3 (head2 moved) (let ((s (part moved 'state))) (and s (pair? (cadr s)) (car (cadr s))))
                     (not (equal? (git-text-in bare "rev-parse main") (git-text "rev-parse HEAD"))))
               '(#t #t (error remote-is-not-the-parent) committed #t))))))
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
