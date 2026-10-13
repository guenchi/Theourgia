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

;; tools/publish-landing.sc: a landing published from a small store into a
;; throwaway git repository. Each row builds a repository from files, imports
;; its worktree into a fresh store, tags start, changes the store, writes the
;; landing block, tags freeze and runs the script as a child, reading the datum
;; it prints and the repository after it. The volume rows call the pure
;; ambiguity function (code-project.sc, landing-collision) with a GIVEN probe
;; result, so both answers run on any machine; the real probe has its own row.
;; PL-26 puts a git in front of the real one that restores a removed file, the
;; one way to stage a removal as a modification. PL-33 needs a directory on
;; another volume (THEOURGIA_TEST_OTHER_VOLUME, else /tmp, /dev/shm, /var/tmp
;; when one is); without one it prints why it was skipped.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) block-id)
        (only (theourgia code-project) code-files code-field code-children landing-collision volume-insensitivity)
        (only (theourgia log) log-publish! segment-sha)
        (only (theourgia wire) encode-record storable-encode)
        (only (theourgia ffi) path-stat))

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
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))


(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/publish-landing-" (number->string (get-process-id))))
(when (file-exists? root) (assertion-violation 'publish-landing "scratch directory already exists" root))
(define (sh . xs) (system (apply string-append xs)))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(sh "mkdir -p " (quoted root))
;; a command's standard output as text
(define (out-of cmd)
  (let-values (((to from err pid) (open-process-ports cmd (buffer-mode block) (native-transcoder))))
    (close-port to)
    (let ((s (get-string-all from))) (get-string-all err) (close-port from) (close-port err) (if (eof-object? s) "" s))))
(define (chomp s) (if (and (> (string-length s) 0) (char=? #\newline (string-ref s (- (string-length s) 1)))) (substring s 0 (- (string-length s) 1)) s))
(define (git wt . args) (chomp (out-of (string-append "git -C " (quoted wt) " " (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args)) "2>/dev/null"))))
(define (path-parent p)
  (let loop ((i (- (string-length p) 1))) (cond ((< i 0) ".") ((char=? (string-ref p i) #\/) (substring p 0 i)) (else (loop (- i 1))))))
(define (write-file! path text)
  (sh "mkdir -p " (quoted (path-parent path)))
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p (string->utf8 text)))))
(define (file-text path) (call-with-port (open-file-input-port path) (lambda (p) (let ((b (get-bytevector-all p))) (if (eof-object? b) "" (utf8->string b))))))
(define (ask st . req) (rpc-dispatch st req "test"))
(define (head-of a)
  (cond ((not (pair? a)) a)
        ((eq? (car a) 'error) (list 'error (cadr a)))
        (else (car a))))
;; a list whose elements are evaluated left to right: R6RS leaves the order
;; of a call's arguments open, and a row that publishes and then reads the
;; repository must read it after
(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))
(define (clause-of a head) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) head))) (cdr a))))

;; ---- a repository and its store -----------------------------------------------
;; -> (dir wt store base); FILES ((<path> . <text>) ...), SETUP run in the
;; worktree before the base commit. The store holds the worktree imported
;; (dot-named paths are not), tagged start.
(define n 0)
(define (repo! files . setup)
  (set! n (+ n 1))
  (let* ((d (string-append root "/r" (number->string n))) (wt (string-append d "/wt")) (st (string-append d "/store")))
    (sh "mkdir -p " (quoted wt) " " (quoted st) " " (quoted (string-append d "/home")))
    (putenv "THEOURGIA_HOME" (string-append d "/home"))
    (sh "git init -q " (quoted wt) " && git -C " (quoted wt) " config user.name fixture && git -C " (quoted wt)
        " config user.email fixture@example.invalid && git -C " (quoted wt) " config commit.gpgsign false")
    (for-each (lambda (f) (write-file! (string-append wt "/" (car f)) (cdr f))) files)
    (when (pair? setup) ((car setup) wt))
    (sh "git -C " (quoted wt) " add -A && git -C " (quoted wt) " commit -q -m base")
    (ask st 'init)
    (let ((a (ask st 'import-code wt)))
      (unless (eq? 'ok (head-of a)) (assertion-violation 'publish-landing "import failed" a)))
    (ask st 'tag "start")
    (list d wt st (git wt "rev-parse" "HEAD"))))
(define (dir-of r) (car r)) (define (wt-of r) (cadr r)) (define (st-of r) (caddr r)) (define (base-of r) (cadddr r))
(define (file-at st path)
  (let ((state (open-and-reduce st)))
    (find (lambda (id) (equal? (code-field state id 'path) path)) (code-files state))))
(define (children-at st path) (code-children (open-and-reduce st) (file-at st path)))
(define (checked what a) (unless (eq? 'ok (head-of a)) (assertion-violation 'publish-landing what a)) a)
;; the store edits: a file's text, a new file, a removal, a rename
(define (set-text! st path text)
  (let ((kids (children-at st path)))
    (unless (= 1 (length kids)) (assertion-violation 'publish-landing "not one block" path kids))
    (ask st 'write (car kids) text "--writer" "w1")
    (checked "commit" (ask st 'commit "--writer" "w1"))))
(define adds 0)
(define (add-file! r path text)
  (set! adds (+ adds 1))
  (let ((src (string-append (dir-of r) "/add" (number->string adds))))
    (write-file! (string-append src "/" path) text)
    (checked "import" (ask (st-of r) 'import-code src))))
(define (remove-file! st path)
  (let ((f (file-at st path)))
    (for-each (lambda (k) (checked "del child" (ask st 'del k))) (children-at st path))
    (checked "del file" (ask st 'del f))))
(define (rename! st path new) (checked "rename" (ask st 'set (file-at st path) "path" new)))
;; THE LANDING BLOCK: -> its id. FIELDS overrides ((<field> . <value or #f>) ...)
(define (landing! r text . fields)
  (let* ((st (st-of r)) (a (checked "insert" (ask st 'insert "--title" "landing" "--text" text)))
         (id (let ((ev (car (cadr (assq 'events (cdr a)))))) (block-id (car ev) (cdr ev))))
         (over (if (pair? fields) (car fields) '())))
    (for-each (lambda (f)
                (let ((v (let ((o (assoc (car f) over))) (if o (cdr o) (cdr f)))))
                  (when v (checked (car f) (ask st 'set id (car f) v)))))
              (list (cons "kind" "decision") (cons "slot" "landing") (cons "freeze" "freeze")
                    (cons "base" (base-of r)) (cons "status" "done")))
    id))
(define (freeze! r) (checked "freeze tag" (ask (st-of r) 'tag "freeze")))
;; the script, as a child: -> its datum
(define script (string-append (path-parent (current-directory)) "/tools/publish-landing.sc"))
(define (publish! r . env)
  (let ((o (string-append (dir-of r) "/publish.out")))
    (sh (if (pair? env) (car env) "") " scheme --script " (quoted script) " " (quoted (st-of r)) " start freeze " (quoted (wt-of r))
        " > " (quoted o) " 2> " (quoted (string-append (dir-of r) "/publish.err")) " < /dev/null")
    (guard (e (#t (list 'UNREADABLE (file-text o)))) (call-with-input-file o read))))
(define (head-commit r) (git (wt-of r) "rev-parse" "HEAD"))
(define (name-status r) (git (wt-of r) "show" "--no-renames" "--name-status" "--format=" "HEAD"))
(define (blob r path) (out-of (string-append "git -C " (quoted (wt-of r)) " cat-file blob " (quoted (string-append "HEAD:" path)))))
(define (mode-at r path) (let ((l (git (wt-of r) "ls-tree" "HEAD" "--" path))) (if (> (string-length l) 6) (substring l 0 6) l)))

;; ---- PL-1, PL-4, PL-5, PL-6: the landing itself ---------------------------------
(printf "== PL-1, PL-4..PL-6: a landing lands ==\n")
(define data "publish-landing-data/")
(let ((r (repo! (list (cons "test/client-program.sc" (file-text "locate-data/client-program-20b4878.txt"))
                      (cons "test/rows-baseline.txt" (file-text (string-append data "rows-baseline-20b4878.txt")))))))
  (set-text! (st-of r) "test/client-program.sc" (file-text (string-append data "client-program-46f14c8.txt")))
  (set-text! (st-of r) "test/rows-baseline.txt" (file-text (string-append data "rows-baseline-46f14c8.txt")))
  (landing! r "test: client-program's daemon count does not match its own shell\n")
  (freeze! r)
  (let ((a (publish! r)))
    (want "PL-1 the trial's landing reproduces the next commit's two files byte for byte, as content changes"
          (list (head-of a) (string=? (blob r "test/client-program.sc") (file-text (string-append data "client-program-46f14c8.txt")))
                (string=? (blob r "test/rows-baseline.txt") (file-text (string-append data "rows-baseline-46f14c8.txt")))
                (name-status r) (and (clause-of a 'commit) (equal? (cadr (clause-of a 'commit)) (head-commit r))))
          '(ok #t #t "M\ttest/client-program.sc\nM\ttest/rows-baseline.txt" #t))
    (want "PL-4 the commit message is the block's text, verbatim"
          ;; not through git, which drops the output's last LF
          (let ((c (out-of (string-append "git -C " (quoted (wt-of r)) " cat-file commit HEAD"))))
            (let loop ((i 0)) (if (string=? "\n\n" (substring c i (+ i 2))) (substring c (+ i 2) (string-length c)) (loop (+ i 1)))))
          "test: client-program's daemon count does not match its own shell\n")
    (want "PL-1 nothing is left behind: the worktree clean, no scratch directory, the parent the base"
          (list (git (wt-of r) "status" "--porcelain" "--untracked-files=all" "--ignored")
                (git (wt-of r) "rev-parse" "HEAD~1"))
          (list "" (base-of r)))))
(let ((r (repo! '(("a.txt" . "one\n") ("gone.txt" . "bye\n") ("old.txt" . "same\n")))))
  (remove-file! (st-of r) "gone.txt")
  (rename! (st-of r) "old.txt" "new.txt")
  (landing! r "remove and rename")
  (freeze! r)
  (let ((a (publish! r)))
    (want "PL-5 a deletion is staged as a removal; PL-6 a rename is a removal plus an addition"
          (list (head-of a) (name-status r) (blob r "new.txt"))
          '(ok "D\tgone.txt\nA\tnew.txt\nD\told.txt" "same\n"))))

;; a repository of two files whose store has changed a.txt
(define (simple! . setup)
  (let ((r (apply repo! '(("a.txt" . "one\n") ("b.txt" . "two\n")) setup)))
    (set-text! (st-of r) "a.txt" "uno\n")
    r))
;; a hook that rewrites the message, and a tracked file hard-linked to the one
;; the landing changes
(let ((r (repo! '(("a.txt" . "one\n")) (lambda (wt)
                                          (sh "ln " (quoted (string-append wt "/a.txt")) " " (quoted (string-append wt "/b.txt")))
                                          (write-file! (string-append wt "/.git/hooks/prepare-commit-msg") "#!/bin/sh\nprintf 'HOOK: ' > \"$1.x\"; cat \"$1\" >> \"$1.x\"; mv \"$1.x\" \"$1\"\n")
                                          (sh "chmod 755 " (quoted (string-append wt "/.git/hooks/prepare-commit-msg")))))))
  (set-text! (st-of r) "a.txt" "uno\n")
  (landing! r "plain message")
  (freeze! r)
  (want "PL-4 a prepare-commit-msg hook does not run: the message is the block's text; a hard-linked tracked file is left as it was"
        (in-order (head-of (publish! r)) (git (wt-of r) "log" "-1" "--format=%B")
              (file-text (string-append (wt-of r) "/b.txt")) (name-status r))
        '(ok "plain message" "one\n" "M\ta.txt")))

;; a git in front of the real one that amends the commit with another
;; message; and a TMPDIR whose name holds an LF and 70 repeated bytes
(let* ((r (simple!)) (shim (string-append (dir-of r) "/shim")) (real (chomp (out-of "command -v git"))))
  (landing! r "the block's text") (freeze! r)
  (write-file! (string-append shim "/git")
               (string-append "#!/bin/sh\n# the publisher calls git -c <config> -C <worktree> <command> ...\n"
                              "if [ \"$5\" = commit ]; then " (quoted real) " \"$@\" || exit $?; exec " (quoted real)
                              " -C \"$4\" commit -q --amend -m other; fi\nexec " (quoted real) " \"$@\"\n"))
  (sh "chmod 755 " (quoted (string-append shim "/git")))
  (let ((a (publish! r (string-append "PATH=" (quoted shim) ":\"$PATH\""))))
    (want "PL-4 a commit whose message is not the block's text refuses commit-message-changed, saying it is committed"
          (list (head-of a) (clause-of a 'state))
          (list '(error commit-message-changed) (list 'state 'committed (head-commit r))))))
(let* ((r (simple!)) (tmp (string-append (dir-of r) "/tmp\n" (make-string 70 #\a))))
  (sh "mkdir -p " (quoted tmp))
  (landing! r "odd tmpdir") (freeze! r)
  (want "PL-4 a TMPDIR whose name holds an LF and a run of one byte still lands, git's error file read and removed"
        (let* ((answer (publish! r (string-append "TMPDIR=" (quoted tmp))))
               (left (out-of (string-append "ls -A " (quoted tmp)))))
          (list (head-of answer) left))
        '(ok "")))

;; ---- PL-2, PL-3, PL-21: the worktree --------------------------------------------
(printf "== PL-2, PL-3, PL-21, PL-35: the worktree before anything ==\n")
(let ((r (simple!)))
  (landing! r "x") (freeze! r)
  (write-file! (string-append (wt-of r) "/b.txt") "changed\n")
  (want "PL-2 a modified tracked file is a dirty worktree, refused with HEAD unchanged"
        (in-order (head-of (publish! r)) (head-commit r))
        (list '(error dirty) (base-of r))))
(let ((r (simple!)))
  (landing! r "x") (freeze! r)
  (write-file! (string-append (wt-of r) "/stray.txt") "new\n")
  (want "PL-21 an untracked file counts as dirty"
        (head-of (publish! r))
        '(error dirty)))
(let ((r (simple!)))
  (landing! r "x") (freeze! r)
  (sh "git -C " (quoted (wt-of r)) " commit -q --allow-empty -m other")
  (want "PL-3 a HEAD that is not the landing's base is refused"
        (head-of (publish! r))
        '(error wrong-head)))
(let ((r (simple!)))
  (landing! r "x") (freeze! r)
  (sh "mkdir " (quoted (string-append (wt-of r) "/.theourgia-landing-x")))
  (want "PL-35 an empty leftover .theourgia-landing-x/ refuses leftover-scratch, naming it"
        (publish! r)
        '(error leftover-scratch (names ".theourgia-landing-x"))))

;; ---- PL-7, PL-8, PL-9, PL-13, PL-14, PL-20: the landing block and the tags ----------
(printf "== PL-7..PL-9, PL-13, PL-14, PL-20: the block and the tags ==\n")
(let ((r (simple!)))
  (freeze! r)
  (want "PL-7 no landing block at the freeze cut refuses missing-landing"
        (publish! r)
        '(error missing-landing "freeze")))
(let ((r (simple!)))
  (landing! r "x") (landing! r "y") (freeze! r)
  (want "PL-8 two landing blocks refuse ambiguous-landing"
        (head-of (publish! r))
        '(error ambiguous-landing)))
(let* ((r (simple!)) (st (st-of r)))
  (landing! r "x") (freeze! r)
  ;; a peer writer binds the same name to another cut
  (let* ((bytes (encode-record 1 1757300000001 "peer" '() (storable-encode (list 'tag "freeze" (list (cons "zzzzzzzz" 1)))))))
    (log-publish! st "zzzzzzzz" 1 bytes (segment-sha bytes)))
  (want "PL-9 a freeze tag bound to two cuts refuses tag-unsettled"
        (publish! r)
        '(error tag-unsettled "freeze")))
(define (invalid-reason a) (let ((c (clause-of a 'reason))) (and c (cadr c))))
(let ((answers
        (map (lambda (over)
               (let ((r (simple!))) (landing! r "x" over) (freeze! r)
                 (let ((a (publish! r))) (list (head-of a) (invalid-reason a)))))
             (list '(("kind" . "task")) '(("base" . #f)) '(("status" . #f)) '(("status" . "dropped"))))))
  (want "PL-13 a landing block that is a task, a decision with no base, or with an open status, refuses invalid-landing"
        (list-head answers 3)
        '(((error invalid-landing) not-a-decision) ((error invalid-landing) no-base) ((error invalid-landing) not-done)))
  (want "PL-20 a landing block whose status is dropped refuses invalid-landing"
        (list-ref answers 3)
        '((error invalid-landing) not-done)))
(let* ((r (simple!)) (id (landing! r "the frozen text")))
  (freeze! r)
  (ask (st-of r) 'write id "edited after the freeze" "--writer" "w1")
  (ask (st-of r) 'commit "--writer" "w1")
  (let ((a (publish! r)))
    (want "PL-14 a landing block edited after the freeze is matched at the freeze cut, with its frozen text"
          (list (code-field (open-and-reduce (st-of r)) id 'src) (head-of a) (git (wt-of r) "log" "-1" "--format=%s"))
          '("edited after the freeze" ok "the frozen text"))))

;; ---- PL-10, PL-27, PL-28: a directory and a file trade places --------------------
(printf "== PL-10, PL-27, PL-28: directories and files ==\n")
(let ((r (repo! '(("x/y" . "inside\n") ("a.txt" . "one\n")))))
  (remove-file! (st-of r) "x/y")
  (add-file! r "x" "now a file\n")
  (landing! r "x/y becomes x") (freeze! r)
  (want "PL-10 a directory becoming a file lands"
        (in-order (head-of (publish! r)) (name-status r) (blob r "x"))
        '(ok "A\tx\nD\tx/y" "now a file\n")))
(let ((r (repo! '(("x" . "a file\n") ("a.txt" . "one\n")))))
  (remove-file! (st-of r) "x")
  (add-file! r "x/y" "now inside\n")
  (landing! r "x becomes x/y") (freeze! r)
  (want "PL-27 a file becoming a directory lands"
        (in-order (head-of (publish! r)) (name-status r) (blob r "x/y"))
        '(ok "D\tx\nA\tx/y" "now inside\n")))
(let ((r (repo! '(("x/y" . "inside\n") (".gitignore" . "*.log\n")))))
  (write-file! (string-append (wt-of r) "/x/junk.log") "ignored\n")
  (remove-file! (st-of r) "x/y")
  (add-file! r "x" "now a file\n")
  (landing! r "blocked") (freeze! r)
  (want "PL-28 an ignored file inside a directory becoming a file refuses destination-exists, with HEAD unchanged"
        (in-order (head-of (publish! r)) (head-commit r))
        (list '(error destination-exists) (base-of r))))

;; ---- PL-11, PL-12, PL-16, PL-17, PL-22: the base ------------------------------
(printf "== PL-11, PL-12, PL-16, PL-17, PL-22: the base ==\n")
(let ((r (simple!)))
  (write-file! (string-append (wt-of r) "/b.txt") "not the store's\n")
  (sh "git -C " (quoted (wt-of r)) " commit -q -am drift")
  (landing! r "x" (list (cons "base" (head-commit r)))) (freeze! r)
  (let ((a (publish! r)))
    (want "PL-11 a base whose tree is not the start export refuses base-mismatch, naming the path"
          (list (head-of a) (clause-of a 'paths))
          '((error base-mismatch) (paths "b.txt")))))
(let ((r (repo! '(("a.txt" . "one\n")))))
  (checked "title" (ask (st-of r) 'set (car (children-at (st-of r) "a.txt")) "title" "renamed"))
  (landing! r "nothing") (freeze! r)
  (want "PL-12 a landing whose changed-files is empty refuses nothing-to-publish and makes no commit"
        (in-order (head-of (publish! r)) (head-commit r))
        (list '(error nothing-to-publish) (base-of r))))
(let ((r (simple! (lambda (wt) (write-file! (string-append wt "/.gitattributes") "b.txt export-subst\n")
                                (write-file! (string-append wt "/b.txt") "$Format:%H$\n")))))
  (landing! r "subst") (freeze! r)
  (let ((a (publish! r)))
    (want "PL-16 a base with an export-subst attribute verifies by its literal blobs"
          (list (head-of a) (blob r "b.txt"))
          '(ok "$Format:%H$\n"))))
(let ((r (simple! (lambda (wt) (sh "ln -s b.txt " (quoted (string-append wt "/link")))))))
  (landing! r "link") (freeze! r)
  (let ((a (publish! r)))
    (want "PL-17 a base with an untouched symbolic link verifies and lists it as skipped"
          (list (head-of a) (clause-of a 'skipped) (mode-at r "link"))
          '(ok (skipped ("link" |120000|)) "120000"))))
(let ((r (simple! (lambda (wt) (write-file! (string-append wt "/.github/workflow.yml") "on: push\n")))))
  (landing! r "github") (freeze! r)
  (let ((a (publish! r)))
    (want "PL-22 a base with a tracked .github file verifies, listed as skipped dot-named"
          (list (head-of a) (clause-of a 'skipped))
          '(ok (skipped (".github/workflow.yml" dot-named))))))

;; ---- PL-15, PL-18, PL-19: what git does to bytes and modes -----------------------
(printf "== PL-15, PL-18, PL-19: bytes and modes ==\n")
(let ((r (simple! (lambda (wt) (write-file! (string-append wt "/.gitattributes") "*.txt text eol=lf\n")))))
  (set-text! (st-of r) "b.txt" "two\r\nlines\r\n")
  (landing! r "crlf") (freeze! r)
  (let ((a (publish! r)))
    (want "PL-15 a path with a text attribute and CRLF bytes refuses staging-converted, the worktree left staged"
          (list (head-of a) (clause-of a 'state) (head-commit r))
          (list '(error staging-converted) '(state staged) (base-of r)))))
(let ((r (simple! (lambda (wt) (write-file! (string-append wt "/run.sh") "echo one\n") (sh "chmod 755 " (quoted (string-append wt "/run.sh")))
                                (write-file! (string-append wt "/old.sh") "echo old\n") (sh "chmod 755 " (quoted (string-append wt "/old.sh")))))))
  (set-text! (st-of r) "run.sh" "echo two\n")
  (landing! r "exec") (freeze! r)
  (let ((refused (publish! r)))
    (want "PL-18 a content change to a 100755 file refuses unsupported-entry"
          (list (head-of refused) (head-commit r))
          (list '(error unsupported-entry) (base-of r)))))
(let ((r (simple! (lambda (wt) (write-file! (string-append wt "/old.sh") "echo old\n") (sh "chmod 755 " (quoted (string-append wt "/old.sh")))))))
  (remove-file! (st-of r) "old.sh")
  (add-file! r "new.sh" "echo new\n")
  (landing! r "exec") (freeze! r)
  (want "PL-18 the removal of a 100755 file lands; PL-19 an added .sh lands 0644"
        (in-order (head-of (publish! r)) (name-status r) (mode-at r "new.sh"))
        '(ok "M\ta.txt\nA\tnew.sh\nD\told.sh" "100644")))

;; ---- PL-23, PL-24, PL-25, PL-26: destinations and the staged set -------------------
(printf "== PL-23..PL-26: destinations and the staged set ==\n")
(let ((r (simple! (lambda (wt) (write-file! (string-append wt "/real/x.txt") "x\n") (sh "ln -s real " (quoted (string-append wt "/d")))))))
  (add-file! r "d/new.txt" "under a link\n")
  (landing! r "link dir") (freeze! r)
  (want "PL-23 an added path under a symbolically linked directory refuses unsafe-destination"
        (in-order (head-of (publish! r)) (head-commit r))
        (list '(error unsafe-destination) (base-of r))))
(let ((r (simple! (lambda (wt) (write-file! (string-append wt "/.gitignore") "secret.txt\n")))))
  (write-file! (string-append (wt-of r) "/secret.txt") "ignored\n")
  (add-file! r "secret.txt" "from the store\n")
  (landing! r "ignored") (freeze! r)
  (want "PL-24 an ignored file at an added path refuses destination-exists, the file untouched"
        (in-order (head-of (publish! r)) (file-text (string-append (wt-of r) "/secret.txt")))
        '((error destination-exists) "ignored\n")))
(let ((r (simple!)))
  (rename! (st-of r) "b.txt" ".git/config")
  (landing! r "git path") (freeze! r)
  (want "PL-25 a store path .git/config refuses git-path"
        (publish! r)
        '(error git-path ".git/config")))
(let* ((r (repo! '(("a.txt" . "one\n") ("gone.txt" . "bye\n"))))
       (shim (string-append (dir-of r) "/shim")) (real (chomp (out-of "command -v git"))))
  (remove-file! (st-of r) "gone.txt")
  (landing! r "removal") (freeze! r)
  (write-file! (string-append shim "/git")
               (string-append "#!/bin/sh\n# after an rm, the removed file comes back with other bytes\n"
                              "# the publisher calls git -c <config> -C <worktree> <command> ...\n"
                              "if [ \"$5\" = rm ]; then " (quoted real) " \"$@\"; rc=$?; printf 'back\\n' > \"$4/gone.txt\"; "
                              (quoted real) " -C \"$4\" add -- gone.txt; exit $rc; fi\nexec " (quoted real) " \"$@\"\n"))
  (sh "chmod 755 " (quoted (string-append shim "/git")))
  (let ((a (publish! r (string-append "PATH=" (quoted shim) ":\"$PATH\""))))
    (want "PL-26 an intended removal staged as a modification refuses staged-set-mismatch, the worktree left staged"
          (list (head-of a) (clause-of a 'state) (head-commit r))
          (list '(error staged-set-mismatch) '(state staged) (base-of r)))))

;; ---- PL-29..PL-31, PL-34, PL-36: the ambiguity rule, a pure function ---------------
(printf "== PL-29..PL-31, PL-34, PL-36: the ambiguity rule with a given probe ==\n")
(want "PL-29 added A.md and a.md: case-insensitive refuses case-collision, sensitive lands"
      (list (landing-collision #t #f '("A.md" "a.md") '()) (landing-collision #f #f '("A.md" "a.md") '()))
      '((error case-collision (paths "A.md" "a.md")) #f))
(want "PL-30 an untouched ambiguous pair elsewhere does not block an unrelated landing"
      (landing-collision #t #t '("z.txt") '("B.md" "b.md" "z.txt"))
      #f)
(want "PL-31 unchanged A/x with added a/y refuses on an insensitive volume, at the directory"
      (list (landing-collision #t #f '("a/y") '("A" "A/x")) (landing-collision #f #f '("a/y") '("A" "A/x")))
      '((error case-collision (paths "A" "a")) #f))
(want "PL-34 a case rename (A removed, a added) is no collision on either kind of volume: the removed A is not staying"
      (list (landing-collision #t #t '("a") '("b")) (landing-collision #f #f '("a") '("b")))
      '(#f #f))
(want "PL-36 U+00C9.sc added beside an existing e-plus-U+0301.sc: case-only lands, both refuses case-collision"
      (list (landing-collision #t #f (list (string #\xC9 #\. #\s #\c)) (list (string #\e #\x301 #\. #\s #\c)))
            (head-of (landing-collision #t #t (list (string #\xC9 #\. #\s #\c)) (list (string #\e #\x301 #\. #\s #\c)))))
      '(#f (error case-collision)))
(let ((r (repo! '(("A.txt" . "one\n") ("b.txt" . "two\n")))))
  (rename! (st-of r) "A.txt" "a.txt")
  (landing! r "case rename") (freeze! r)
  (want "PL-34 a case rename lands on this machine's volume, whatever it is"
        (in-order (head-of (publish! r)) (name-status r) (blob r "a.txt"))
        '(ok "D\tA.txt\nA\ta.txt" "one\n")))

;; ---- PL-32 the real probe; PL-33 another volume ---------------------------------
(printf "== PL-32, PL-33: the probe and the scratch directory ==\n")
(let ((d (string-append root "/probe")) (ro (string-append root "/probe-ro")))
  (sh "mkdir -p " (quoted d) " " (quoted ro) " && chmod 555 " (quoted ro))
  (let-values (((c n) (volume-insensitivity d)))
    (let ((failed (guard (e ((pair? e) e)) (volume-insensitivity ro) 'answered)))
      (sh "chmod 755 " (quoted ro))
      (want "PL-32 the probe answers two booleans and leaves nothing in its directory; a read-only one refuses probe-failed"
            (list (boolean? c) (boolean? n) (out-of (string-append "ls -A " (quoted d))) (head-of failed))
            '(#t #t "" (error probe-failed))))))
(define other-volume
  (let ((here (car (path-stat root))))
    (find (lambda (p) (and p (file-directory? p) (not (equal? (car (path-stat p)) here))
                           (= 0 (system (string-append "test -w " (quoted p))))))
          (list (getenv "THEOURGIA_TEST_OTHER_VOLUME") "/tmp" "/dev/shm" "/var/tmp"))))
(if (not other-volume)
    (printf "SKIP PL-33: no writable directory on another volume than ~a (tried THEOURGIA_TEST_OTHER_VOLUME, /tmp, /dev/shm, /var/tmp); set THEOURGIA_TEST_OTHER_VOLUME to one to run it\n" root)
    (let* ((main (string-append other-volume "/publish-landing-main-" (number->string (get-process-id))))
           (r (repo! '(("a.txt" . "one\n")))))
      ;; the worktree becomes a linked one of a repository on the other volume
      (sh "git clone -q " (quoted (wt-of r)) " " (quoted main) " && git -C " (quoted main) " config user.name fixture"
          " && git -C " (quoted main) " config user.email fixture@example.invalid && rm -rf " (quoted (wt-of r))
          " && git -C " (quoted main) " worktree add -q --detach " (quoted (wt-of r)) " " (base-of r))
      (set-text! (st-of r) "a.txt" "uno\n")
      (landing! r "linked") (freeze! r)
      (let* ((a (publish! r))
             (seen (list (head-of a) (name-status r) (out-of (string-append "ls -A " (quoted (wt-of r)))))))
        (sh "rm -rf " (quoted main))
        (want "PL-33 a linked worktree whose repository is on another volume lands through the hidden directory, gone before the commit"
              seen
              '(ok "M\ta.txt" ".git\na.txt\n")))))

(sh "rm -rf " (quoted root))
(printf "rows: ~a\n~a failures\npublish-landing complete\n" rows bad)
(exit (if (zero? bad) 0 1))
