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

;; Publish a landing: the files that changed in the store between two tags,
;; committed in a git worktree whose HEAD is the tree the first tag exports.
;;
;;   scheme --script tools/publish-landing.sc <store> <start-tag> <freeze-tag> <worktree>
;;
;; THE LANDING IS A BLOCK, found by its fields at the freeze cut: kind
;; decision, status done, and the fields slot "landing", freeze "<freeze-tag>"
;; and base "<the 40-hex git id of the start tree>"; its text is the commit
;; message. It is written and committed before the freeze tag is made, so the
;; tag's cut holds it. Tags carry no note and titles are not unique, which is
;; why the block is found by fields.
;;
;; EVERYTHING IS CHECKED BEFORE THE WORKTREE CHANGES: each tag settled; one
;; landing block, valid; the worktree has no leftover scratch directory, is
;; clean (untracked files included) and its HEAD is the base; the base equals
;; the start cut's raw export path by path (the literal blobs: dot-named paths,
;; symbolic links and submodules skipped and listed); changed-files between
;; the cuts is not empty; every touched path is a plain file at the base;
;; every destination is outside the git directory, under real directories and
;; free, but for what the planned removals clear; and no two spellings the
;; landing writes, or one it writes and one that stays, would be merged by the
;; volume, which a probe in the scratch directory says.
;;
;; THEN, in two passes: git rm of every removed path and of any empty directory
;; left in the way, then every added or content path copied from the freeze
;; cut's export (an added file lands 0644). The staged set is read back -- the
;; exact paths and letters, and each staged blob equal to the projection's
;; bytes -- and committed with the block's text, author and committer from the
;; worktree's configuration. It never pushes.
;;
;; SCRATCH: the probe and the two exports need a directory on the worktree's
;; volume. That is the git directory when it is on the same device as the
;; worktree (a linked worktree's git directory need not be), else a hidden
;; `.theourgia-landing-<pid>/` at the worktree root, made after the dirty check
;; and removed before anything is staged. A run that dies leaves it behind, and
;; the next run refuses leftover-scratch until the operator removes it.
;;
;; A refusal prints (error <kind> ...) and exits 1; one after the worktree
;; changed adds (state staged). Success prints (ok (commit <id>) ...).
(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia store) store-state-at-cut)
        (only (theourgia reduce) reduction? state-read)
        (only (theourgia query) make-query-session session-answer)
        (only (theourgia lifecycle) lifecycle decision-state-of)
        (only (theourgia landing-files) raw-projection volume-insensitivity landing-collision spelling-key)
        (only (theourgia ffi) entry-bytes entry-name-type directory-entries mkdir-p! mkdir-exclusive!
              rmdir! unlink! overwrite-entry! path-stat process-id))

(define actor "publish-landing")
(define (refuse kind . details) (raise (cons 'error (cons kind details))))
;; what the worktree has been through, for a refusal to say: #f before the
;; first git mutation, then staged, then (committed <id>)
(define touched #f)

;; ---- small tools -----------------------------------------------------------

(define (prefix? s p) (and (>= (string-length s) (string-length p)) (string=? (substring s 0 (string-length p)) p)))
(define (split-on s ch)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i (string-length s)) (reverse (cons (substring s start i) out)))
          ((char=? (string-ref s i) ch) (loop (+ i 1) (+ i 1) (cons (substring s start i) out)))
          (else (loop (+ i 1) start out)))))
(define (join parts sep)
  (if (null? parts) ""
      (fold-left (lambda (acc p) (string-append acc sep p)) (car parts) (cdr parts))))
(define (parent-of rel) (let ((cs (split-on rel #\/))) (join (reverse (cdr (reverse cs))) "/")))
(define (base-of rel) (car (reverse (split-on rel #\/))))
(define (ancestors rel)
  (let loop ((cs (reverse (cdr (reverse (split-on rel #\/))))) (out '()) (at #f))
    (if (null? cs) (reverse out)
        (let ((p (if at (string-append at "/" (car cs)) (car cs))))
          (loop (cdr cs) (cons p out) p)))))
(define (dot-named? rel) (exists (lambda (c) (prefix? c ".")) (split-on rel #\/)))
(define (absolute p) (if (prefix? p "/") p (string-append (current-directory) "/" p)))
(define (hex40? s)
  (and (string? s) (= (string-length s) 40)
       (for-all (lambda (c) (or (char<=? #\0 c #\9) (char<=? #\a c #\f))) (string->list s))))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(define (chomp s) (if (and (> (string-length s) 0) (char=? #\newline (string-ref s (- (string-length s) 1))))
                      (substring s 0 (- (string-length s) 1)) s))
(define (hex->string h)
  (let loop ((i 0) (out '()))
    (if (>= (+ i 1) (string-length h))
        (utf8->string (u8-list->bytevector (reverse out)))
        (loop (+ i 2) (cons (string->number (substring h i (+ i 2)) 16) out)))))
(define (string-set-of l) (let ((h (make-hashtable string-hash string=?))) (for-each (lambda (s) (hashtable-set! h s #t)) l) h))
(define (in? h s) (hashtable-ref h s #f))

;; ---- git -------------------------------------------------------------------
;;
;; One process per command, through /bin/sh: -> (values rc stdout-bytes).
;; The exit status and the file git's standard error went to (its name in
;; hex, so no byte of it is an LF) are printed after the output as a last line
;; of their own, so the output's bytes are everything before the final LF. Pathspecs are literal, so a path is never
;; a pattern, and no hook runs (core.hooksPath is /dev/null).
;;
;; NEVER: STANDARD ERROR IS NOT A SECOND PIPE READ AFTER THE FIRST. Read in
;; turn, a git that fills the error pipe while its output is still being
;; waited for blocks for good (conversion warnings from an add can). It goes
;; to a temporary file, copied to this process's standard error afterwards.
(define (git wt args . input)
  (let-values (((to from errs pid)
                (open-process-ports
                  (string-append "e=$(mktemp) || { printf '\\n125 '; exit 0; }; "
                                 "LC_ALL=C GIT_LITERAL_PATHSPECS=1 git -c core.hooksPath=/dev/null -C " (quoted wt) " "
                                 (join (map quoted args) " ") " 2>\"$e\"; s=$?; printf '\\n%s %s' \"$s\" \"$(printf %s \"$e\" | od -An -v -tx1 | tr -d ' \\n')\"")
                  (buffer-mode block) #f)))
    (when (pair? input) (put-bytevector to (car input)))
    (close-port to)
    (let ((out (let ((b (get-bytevector-all from))) (if (eof-object? b) (make-bytevector 0) b))))
      (get-bytevector-all errs)
      (close-port from) (close-port errs)
      (let loop ((i (- (bytevector-length out) 1)))
        (cond ((< i 0) (values -1 out))
              ((= 10 (bytevector-u8-ref out i))
               (let* ((last (utf8->string (let ((t (make-bytevector (- (bytevector-length out) i 1))))
                                            (bytevector-copy! out (+ i 1) t 0 (bytevector-length t)) t)))
                      (space (let find ((j 0)) (cond ((= j (string-length last)) #f) ((char=? (string-ref last j) #\space) j) (else (find (+ j 1))))))
                      (rc (and space (string->number (substring last 0 space))))
                      (err-file (and space (hex->string (substring last (+ space 1) (string-length last)))))
                      (body (let ((b (make-bytevector i))) (bytevector-copy! out 0 b 0 i) b)))
                 (when (and err-file (> (string-length err-file) 0))
                   (let ((e (standard-error-port))) (put-bytevector e (entry-bytes err-file)) (flush-output-port e))
                   (unlink! err-file))
                 (values (or rc -1) body)))
              (else (loop (- i 1))))))))
;; the output as text, a nonzero status refused as git-failed
(define (git-text wt . args)
  (let-values (((rc out) (git wt args)))
    (unless (= rc 0) (refuse 'git-failed (cons 'command args) (list 'status rc)))
    (utf8->string out)))
(define (git-bytes wt . args) (git-run wt args))
;; ARGS as a list, and the bytes for its standard input when given
(define (git-run wt args . input)
  (let-values (((rc out) (apply git wt args input)))
    (unless (= rc 0) (refuse 'git-failed (cons 'command args) (list 'status rc)))
    out))
;; NUL-separated fields
(define (nul-fields s) (filter (lambda (x) (> (string-length x) 0)) (split-on s #\nul)))

;; the base tree: path -> (mode sha), from `git ls-tree -r -z`
(define (base-tree wt base)
  (let ((h (make-hashtable string-hash string=?)))
    (for-each (lambda (rec)
                (let* ((tab (let loop ((i 0)) (if (char=? (string-ref rec i) #\tab) i (loop (+ i 1)))))
                       (meta (split-on (substring rec 0 tab) #\space)))
                  (hashtable-set! h (substring rec (+ tab 1) (string-length rec)) (list (car meta) (caddr meta)))))
              (nul-fields (git-text wt "ls-tree" "-r" "-z" base)))
    h))

;; ---- the worktree's entries, read through the door ---------------------------

(define (type-at path) (entry-name-type path))
(define (names-in dir) (filter (lambda (n) (not (member n '("." "..")))) (directory-entries dir)))
;; every regular file under DIR: ((<relative path> <bytes>) ...), sorted
(define (files-under dir)
  (list-sort (lambda (x y) (string<? (car x) (car y)))
    (let walk ((rel ""))
      (let ((path (if (string=? rel "") dir (string-append dir "/" rel))))
        (case (type-at path)
          ((directory) (apply append (map (lambda (n) (walk (if (string=? rel "") n (string-append rel "/" n))))
                                          (names-in path))))
          ((regular) (list (list rel (entry-bytes path))))
          ((absent) '())
          (else (refuse 'unexpected-entry (list 'path path))))))))
(define (remove-tree! path)
  (case (type-at path)
    ((absent) #f)
    ((directory) (for-each (lambda (n) (remove-tree! (string-append path "/" n))) (names-in path)) (rmdir! path))
    (else (unlink! path))))

;; ---- the store -----------------------------------------------------------------

(define (ask store . req) (rpc-dispatch store req actor))
(define (answer-ok a) (unless (and (pair? a) (eq? (car a) 'ok)) (raise a)) a)
(define (clause a head) (find (lambda (c) (and (pair? c) (eq? (car c) head))) (cdr a)))

;; THE CUT A TAG NAMES, read from the tag listing: unknown-tag, or
;; tag-unsettled when the name lists more than one cut.
(define (tag-cut store name)
  (let ((mine (filter (lambda (t) (equal? (cadr (assq 'name (cdr t))) name))
                      (cdr (cadr (answer-ok (ask store 'tag)))))))
    (cond ((null? mine) (refuse 'unknown-tag name))
          ((or (> (length mine) 1) (assq 'unsettled (cdr (car mine)))) (refuse 'tag-unsettled name))
          (else (cadr (assq 'cut (cdr (car mine))))))))
(define (cut-text cut) (format "~s" cut))

;; THE LANDING BLOCK AT THE FREEZE CUT, by its fields, verified.
;; -> (values <id> <message> <base>)
(define (landing-at state freeze)
  (let* ((a (session-answer (make-query-session state)
                            (list 'and (list 'field '?b "slot" "landing") (list 'field '?b "freeze" freeze))))
         (ids (map car (cadr (answer-ok a)))))
    (cond ((null? ids) (refuse 'missing-landing freeze))
          ((> (length ids) 1) (refuse 'ambiguous-landing freeze (cons 'ids ids))))
    (let* ((id (car ids))
           (row (state-read state id))
           (field (lambda (n) (let ((f (assq n (cdr (assq 'fields row))))) (and f (cdr f)))))
           ;; a bytevector src is text only when it is its own UTF-8
           (src (let ((s (field 'src)))
                  (if (bytevector? s) (let ((t (utf8->string s))) (and (bytevector=? (string->utf8 t) s) t)) s)))
           (bad (lambda (why) (refuse 'invalid-landing (list 'id id) (list 'reason why)))))
      (unless (eq? (field 'kind) 'decision) (bad 'not-a-decision))
      (unless (equal? (decision-state-of (lifecycle state) id) '(closed done)) (bad 'not-done))
      (unless (hex40? (field 'base)) (bad 'no-base))
      (unless (and (string? src) (> (string-length src) 0)) (bad 'no-text))
      (values id src (field 'base)))))

;; ---- the checks on the worktree ----------------------------------------------------

;; THE ON-DISK NAME IN DIR that a volume of these kinds would take NAME for,
;; read from the directory's listing (an existence test under NAME's own
;; spelling answers yes for the other spelling on an insensitive volume, and
;; says nothing of which entry it is). -> the name, or #f.
(define (entry-for dir name case? normalisation?)
  (and (eq? 'directory (type-at dir))
       (let ((names (names-in dir))
             (kind (cond ((and case? normalisation?) 'both) (case? 'case) (normalisation? 'normalisation) (else #f))))
         (or (find (lambda (n) (string=? n name)) names)
             (and kind (find (lambda (n) (string=? (spelling-key kind n) (spelling-key kind name))) names))))))

;; A DIRECTORY THE REMOVAL PASS EMPTIES AND DELETES: every entry in it, read
;; from the directory itself (ignored and untracked files included, which git
;; rm would leave), is a removed path or such a directory.
(define (cleared? wt rel removed)
  (let ((path (string-append wt "/" rel)))
    (and (eq? 'directory (type-at path))
         (for-all (lambda (n)
                    (let ((r (string-append rel "/" n)))
                      (if (eq? 'directory (type-at (string-append wt "/" r))) (cleared? wt r removed) (in? removed r))))
                  (names-in path)))))

;; EVERY DESTINATION, before any mutation: no .git component and not under the
;; git directory; every existing ancestor a real directory that is not a
;; submodule, or a file the landing removes; an added path's place empty but
;; for what the removals clear.
(define (check-destination wt git-dir tree rel added? removed case? normalisation?)
  (when (or (exists (lambda (c) (string=? (spelling-key 'both c) ".git")) (split-on rel #\/))
            (prefix? (string-append wt "/" rel) (string-append git-dir "/")))
    (refuse 'git-path rel))
  (let loop ((cs (split-on rel #\/)) (at ""))
    (let* ((dir (if (string=? at "") wt (string-append wt "/" at)))
           (name (entry-for dir (car cs) case? normalisation?))
           (here (and name (if (string=? at "") name (string-append at "/" name))))
           (type (and here (type-at (string-append wt "/" here)))))
      (cond
        ((not here) #t)
        ((pair? (cdr cs))
         (cond ((and (eq? type 'directory) (let ((e (hashtable-ref tree here #f))) (and e (string=? (car e) "160000"))))
                (refuse 'unsafe-destination rel (list 'submodule here)))
               ((eq? type 'directory) (loop (cdr cs) here))
               ((eq? type 'link) (refuse 'unsafe-destination rel (list 'link here)))
               ((in? removed here) #t)
               (else (refuse 'destination-exists rel (list 'entry here)))))
        ((not added?)
         (unless (eq? type 'regular) (refuse 'unsafe-destination rel (list type here))))
        ((in? removed here) #t)
        ((and (eq? type 'directory) (cleared? wt here removed)) #t)
        ((eq? type 'link) (refuse 'unsafe-destination rel (list 'link here)))
        (else (refuse 'destination-exists rel (list 'entry here)))))))

;; THE WORKTREE'S ENTRIES THAT STAY: every file, directory and link, but .git,
;; the scratch directory, the removed paths and what the removals clear.
(define (staying-entries wt removed skip)
  (let walk ((rel ""))
    (apply append
           (map (lambda (n)
                  (let ((r (if (string=? rel "") n (string-append rel "/" n))))
                    (cond ((or (string=? r ".git") (string=? r skip) (in? removed r)) '())
                          ((eq? 'directory (type-at (string-append wt "/" r)))
                           (if (cleared? wt r removed) '() (cons r (walk r))))
                          (else (list r)))))
                (names-in (if (string=? rel "") wt (string-append wt "/" rel)))))))

;; ---- the run -------------------------------------------------------------------------

;; THE WORKTREE BY ITS OWN SPELLING: git's top level, required to be the
;; directory named (same device and inode), so every path compared with the
;; git directory's below is in git's spelling (a "/." or a symbolic link in
;; the name given would otherwise make a prefix test miss).
(define (worktree-root given)
  (let ((top (chomp (git-text given "rev-parse" "--show-toplevel"))))
    (unless (equal? (list-head (path-stat top) 2) (list-head (path-stat given) 2)) (refuse 'not-a-worktree-root given))
    top))

(define (publish store start freeze given)
  (let* ((wt (worktree-root given))
         (start-cut (tag-cut store start))
         (freeze-cut (tag-cut store freeze))
         (freeze-state (let ((s (store-state-at-cut store #f (cut-text freeze-cut))))
                         (if (reduction? s) s (raise s)))))
    (let-values (((id message base) (landing-at freeze-state freeze)))
      ;; the worktree: no leftover scratch, clean, at the base
      (let ((left (filter (lambda (n) (prefix? n ".theourgia-landing-")) (names-in wt))))
        (when (pair? left) (refuse 'leftover-scratch (cons 'names left))))
      (unless (= 0 (bytevector-length (git-bytes wt "status" "--porcelain" "--untracked-files=all" "-z")))
        (refuse 'dirty wt))
      (let ((head (chomp (git-text wt "rev-parse" "HEAD"))))
        (unless (string=? head base) (refuse 'wrong-head (list 'head head) (list 'base base))))
      (let* ((git-dir (chomp (git-text wt "rev-parse" "--absolute-git-dir")))
             (same-volume (equal? (car (path-stat git-dir)) (car (path-stat wt))))
             (scratch-name (string-append (if same-volume "theourgia-landing-" ".theourgia-landing-")
                                          (number->string (process-id))))
             (scratch (string-append (if same-volume git-dir wt) "/" scratch-name))
             (made #f))
        ;; a run that died with its scratch in the git directory left it there
        (let ((left (filter (lambda (n) (prefix? n "theourgia-landing-")) (names-in git-dir))))
          (when (pair? left) (refuse 'leftover-scratch (cons 'names (map (lambda (n) (string-append git-dir "/" n)) left)))))
        (dynamic-wind
          (lambda () #f)
          (lambda ()
            (unless (eq? 'created (mkdir-exclusive! scratch)) (refuse 'leftover-scratch (list 'names scratch-name)))
            (set! made #t)
            (let-values (((case? normalisation?) (volume-insensitivity scratch)))
              (let* ((tree (base-tree wt base))
                     (start-dir (string-append scratch "/start"))
                     (freeze-dir (string-append scratch "/freeze"))
                     (skipped '()))
                ;; THE BASE: the start cut's raw export against the literal blobs
                (answer-ok (ask store 'export-code start-dir "--raw" "--cut" (cut-text start-cut)))
                (let ((exported (files-under start-dir)) (compared (make-hashtable string-hash string=?)) (differ '()))
                  (let-values (((paths entries) (hashtable-entries tree)))
                    (vector-for-each
                      (lambda (p e)
                        (cond ((dot-named? p) (set! skipped (cons (list p 'dot-named) skipped)))
                              ((member (car e) '("100644" "100755"))
                               (hashtable-set! compared p #t)
                               (let ((x (assoc p exported)))
                                 (unless (and x (bytevector=? (cadr x) (git-bytes wt "cat-file" "blob" (cadr e))))
                                   (set! differ (cons p differ)))))
                              (else (set! skipped (cons (list p (string->symbol (car e))) skipped)))))
                      paths entries))
                  ;; NEVER: AN EXPORTED FILE AT OR UNDER A SKIPPED ENTRY IS NOT A
                  ;; DIFFERENCE. The import follows a symbolic link, so the store
                  ;; holds a link's target bytes, or a linked directory's files,
                  ;; at the link's path; a submodule's checkout is imported the
                  ;; same way. Those paths are what the skip lists.
                  (let ((under-skipped? (lambda (p) (exists (lambda (s) (or (string=? p (car s)) (prefix? p (string-append (car s) "/"))))
                                                            skipped))))
                    (for-each (lambda (x) (unless (or (dot-named? (car x)) (in? compared (car x)) (under-skipped? (car x)))
                                            (set! differ (cons (car x) differ))))
                              exported))
                  (when (pair? differ) (refuse 'base-mismatch (cons 'paths (list-sort string<? differ)))))
                (set! skipped (list-sort (lambda (x y) (string<? (car x) (car y))) skipped))
                ;; WHAT CHANGED
                (let* ((items (cdr (cadr (answer-ok (ask store 'changed-files (cut-text start-cut) (cut-text freeze-cut))))))
                       (of (lambda (r) (map cadr (filter (lambda (i) (eq? (caddr i) r)) items))))
                       (added (of 'added)) (content (of 'content)) (removed (of 'removed))
                       (removed-set (string-set-of removed))
                       (mode-of (lambda (p) (let ((e (hashtable-ref tree p #f))) (and e (car e))))))
                  (when (null? items) (refuse 'nothing-to-publish (list 'from start) (list 'to freeze)))
                  (for-each (lambda (p) (unless (equal? (mode-of p) "100644") (refuse 'unsupported-entry p (list 'mode (mode-of p)))))
                            content)
                  (for-each (lambda (p) (unless (member (mode-of p) '("100644" "100755")) (refuse 'unsupported-entry p (list 'mode (mode-of p)))))
                            removed)
                  (for-each (lambda (p) (when (mode-of p) (refuse 'unsupported-entry p (list 'mode (mode-of p))))) added)
                  (for-each (lambda (p) (check-destination wt git-dir tree p #t removed-set case? normalisation?)) added)
                  (for-each (lambda (p) (check-destination wt git-dir tree p #f removed-set case? normalisation?)) content)
                  (let ((c (landing-collision case? normalisation? (append added content)
                                              (staying-entries wt removed-set (if same-volume "" scratch-name)))))
                    (when c (raise c)))
                  ;; THE FREEZE EXPORT, and the bytes the staged blobs must equal
                  (answer-ok (ask store 'export-code freeze-dir "--raw" "--cut" (cut-text freeze-cut)))
                  (let ((projected (raw-projection freeze-state)) (targets (append added content)))
                    ;; PASS ONE: the removals, then any directory left empty in the way
                    (set! touched '(staged))
                    (when (pair? removed) (git-run wt (append (list "rm" "-q" "--") removed)))
                    (for-each (lambda (p)
                                (for-each (lambda (a) (let ((d (string-append wt "/" a)))
                                                        (when (and (eq? 'directory (type-at d)) (cleared? wt a removed-set))
                                                          (remove-tree! d))))
                                          (append (ancestors p) (list p))))
                              added)
                    ;; PASS TWO: every added or content path, from the freeze export
                    (for-each (lambda (p)
                                ;; NEVER: A CONTENT FILE IS REPLACED, NOT TRUNCATED IN PLACE:
                                ;; another tracked path hard-linked to it would change too.
                                (let ((dest (string-append wt "/" p)))
                                  (mkdir-p! (parent-of dest))
                                  (when (eq? 'regular (type-at dest)) (unlink! dest))
                                  (overwrite-entry! dest (entry-bytes (string-append freeze-dir "/" p)))))
                              targets)
                    (remove-tree! scratch)
                    (set! made #f)
                    (when (pair? targets) (git-run wt (append (list "add" "-f" "--") targets)))
                    ;; THE STAGED SET, read back
                    (let ((want (list-sort (lambda (x y) (string<? (cadr x) (cadr y)))
                                           (append (map (lambda (p) (list "A" p)) added) (map (lambda (p) (list "M" p)) content)
                                                   (map (lambda (p) (list "D" p)) removed))))
                          (got (let loop ((fs (nul-fields (git-text wt "diff" "--cached" "--no-renames" "--name-status" "-z")))
                                          (out '()))
                                 (if (or (null? fs) (null? (cdr fs)))
                                     (list-sort (lambda (x y) (string<? (cadr x) (cadr y))) out)
                                     (loop (cddr fs) (cons (list (car fs) (cadr fs)) out))))))
                      (unless (equal? want got) (refuse 'staged-set-mismatch (list 'intended want) (list 'staged got))))
                    (for-each (lambda (p)
                                (let ((x (assoc p projected)))
                                  (unless (and x (bytevector=? (cadr x) (git-bytes wt "cat-file" "blob" (string-append ":" p))))
                                    (refuse 'staging-converted p))))
                              targets)
                    ;; NEVER: NOT "NO OUTPUT". A pathspec x also selects x/y, which
                    ;; a file becoming a directory has just staged; the index entry
                    ;; must be absent at exactly the removed path.
                    (let ((indexed (map (lambda (rec) (let loop ((i 0)) (if (char=? (string-ref rec i) #\tab) (substring rec (+ i 1) (string-length rec)) (loop (+ i 1)))))
                                        (nul-fields (git-text wt "ls-files" "--stage" "-z")))))
                      (for-each (lambda (p) (when (member p indexed) (refuse 'staged-set-mismatch (list 'still-indexed p))))
                                removed))
                    (git-run wt (list "commit" "-q" "--no-verify" "--cleanup=verbatim" "-F" "-") (string->utf8 message))
                    (set! touched '(committed))
                    (set! touched (list 'committed (chomp (git-text wt "rev-parse" "HEAD"))))
                    ;; the message read back, all of it: the commit object's bytes
                    ;; after its first blank line are exactly the block's text
                    (let* ((c (git-bytes wt "cat-file" "commit" "HEAD"))
                           (body (let loop ((i 0))
                                   (cond ((>= (+ i 1) (bytevector-length c)) #f)
                                         ((and (= 10 (bytevector-u8-ref c i)) (= 10 (bytevector-u8-ref c (+ i 1))))
                                          (let ((b (make-bytevector (- (bytevector-length c) i 2))))
                                            (bytevector-copy! c (+ i 2) b 0 (bytevector-length b)) b))
                                         (else (loop (+ i 1)))))))
                      (unless (and body (bytevector=? body (string->utf8 message)))
                        (refuse 'commit-message-changed)))
                    (list 'ok (list 'commit (chomp (git-text wt "rev-parse" "HEAD")))
                          (list 'landing id) (list 'base base)
                          (list 'added added) (list 'content content) (list 'removed removed)
                          (cons 'skipped skipped)))))))
          (lambda () (when made (remove-tree! scratch))))))))

(define (main args)
  (if (not (= 4 (length args)))
      (begin (write '(usage (publish-landing <store> <start-tag> <freeze-tag> <worktree>))) (newline) (exit 2))
      (let ((a (guard (e ((and (pair? e) (eq? (car e) 'error)) e))
                 (publish (absolute (car args)) (cadr args) (caddr args) (absolute (cadddr args))))))
        (write (if (and touched (eq? (car a) 'error)) (append a (list (cons 'state touched))) a))
        (newline)
        (exit (if (eq? (car a) 'ok) 0 1)))))

(main (cdr (command-line)))
