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

;; tools/drafts-dump.sc, tools/drafts-restore.sc and tools/landing-commit.sc,
;; each run as a child on small stores, reading the datum it prints and the
;; store after it; and the seed a writable store starts from.
;;
;; DD: two drafts with different cuts, the store's cut later still, dumped and
;; restored on a git clone of the store with the same bytes, based-on, cut and
;; version; a second restore writes nothing; a moved baseline, an occupied
;; slot and another store are refused with nothing written.
;; LC: a frozen draft and a commit on an unrelated block since the freeze: the
;; landing is refused before anything is committed; with the cut unchanged it
;; commits; after a rebase and a new freeze it commits.
;; With --start <tag>, landing-commit tags the store's applied cut after the
;; cut check and before the commit: the tag binds the pinned cut; a commit
;; since the freeze refuses with no tag made; a name already bound is
;; refused before anything is written; a commit between the check and the tag
;; (held at landing-before-tag) leaves the tag on the moved cut and refuses
;; start-tag-cut with nothing committed.
;; LF: tools/landing-freeze.sc on a store imported from files: its directory
;; holds the draft list, the working view's raw export, its manifest and the
;; changed files; a draft written while the freeze waits at its hold stage
;; (THEOURGIA_HOLD=freeze-after-dump) refuses it; a draft rewritten A to B
;; before the view is read and back to A before the final list (held at both
;; stages) is refused by the view's own drafts, the one check that sees it;
;; a draft rewritten A to B while the freeze waits before its final list
;; only (held at freeze-before-list, after the view check and the export) is
;; refused by the final list, the one check that sees it, and CHANGED.sexp is
;; not written; landing-commit commits from the freeze's directory.
;; ST: a writer's structural drafts (a file created, a file deleted) dumped
;; and restored; frozen (the tree holds the new file and not the deleted one;
;; CHANGED names both); landed, the new file's holder and children equal to
;; tools/import-git.sc's for the same bytes in a twin store; a delete whose
;; file changed since is refused at commit, and a mixed landing with it
;; commits nothing; a refused landing keeps its start tag, the same name is
;; then refused, a new name lands; the construction is one procedure (a
;; census of the sources); the completion check reads a structural item.
;; BH: a draft on a block another writer commits is listed `behind` by drafts
;; (a commit on another block, which moves only the cut, is not); landing-
;; freeze rebases it when the two changes touch different lines (frozen bytes
;; carry both, the pinned cut is the applied cut, the freeze lands), and stops
;; with drafts-behind-conflict naming it when they touch the same or adjacent
;; lines, or repeated lines put the alignment in doubt, nothing frozen and no
;; draft touched; a draft whose block did not move is moved onto the applied
;; cut, and the freeze lands; a commit during the freeze refuses it
;; (cut-moved). --check judges a freeze already made, its frozen bytes and not
;; the live drafts: current (a delete at the applied cut included), refreeze
;; naming the drafts behind and those only moved, drafts-behind-conflict,
;; frozen-damaged for bytes or fields that no longer match their record, and
;; manifest-unreadable for a manifest that is not one datum.
;; MV: a structural draft behind an unrelated commit is moved onto the applied
;; cut by the freeze (a new file, the path still free there; a delete, its
;; file unchanged there), recorded, frozen and landed; a path another writer
;; created, or a deleted file another writer edited, is named and nothing is
;; moved; --check names the first as moved and the second as a conflict and
;; writes nothing. The move rewrites the entry in place under one draft lock,
;; and only while it is still the version judged: its write faulted inside the
;; lock (the structural directory read-only at structural-move-before-write)
;; leaves the entry at its old cut with its old bytes; the directory flush
;; faulted after the write's rename leaves the whole new entry at the applied
;; cut, and a second freeze reads it; a draft written again
;; at freeze-before-move is refused structural-changed and stands; a write of
;; the same draft begun inside the lock waits for the move; --check on a freeze
;; whose draft was made again judges the frozen item and writes nothing.
;; SEED: a copy of a store holds a writer's owner.sexp; adopt takes it over,
;; and a draft is written and committed. THEOURGIA_SEED_STORE names a store to
;; copy as well (the split mirror a writable store would be seeded from);
;; without it, that row prints why it was skipped.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia store) open-and-reduce store-tags)
        (only (theourgia reduce) block-hash reduce-applied-cut state-block-ids state-read)
        (only (theourgia code-project) code-files code-field code-children lang-fields)
        (only (theourgia languages) language-for-path)
        (only (theourgia log) writer-directory)
        (only (theourgia ffi) path-stat)
        (only (theourgia digest) sha256 bytevector->hex))

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
;; a list whose elements are evaluated left to right: a row that runs a child
;; and then reads the store must read it after
(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/drafts-tools-" (number->string (get-process-id))))
(when (file-exists? root) (assertion-violation 'drafts-tools "scratch directory already exists" root))
(define (sh . xs) (system (apply string-append xs)))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(sh "mkdir -p " (quoted root))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(sh "mkdir -p " (quoted (string-append root "/home")))
(define (path-parent p)
  (let loop ((i (- (string-length p) 1))) (cond ((< i 0) ".") ((char=? (string-ref p i) #\/) (substring p 0 i)) (else (loop (- i 1))))))
(define (file-datum path) (guard (e (#t 'UNREADABLE)) (call-with-input-file path read)))
(define (sha256-hex bytes) (bytevector->hex (sha256 bytes)))
(define (ask st . req) (rpc-dispatch st req "test"))
(define (head-of a) (cond ((not (pair? a)) a) ((eq? (car a) 'error) (list 'error (cadr a))) (else (car a))))
(define (checked what a) (unless (eq? 'ok (head-of a)) (assertion-violation 'drafts-tools what a)) a)
;; A CLAUSE IS A LIST AFTER THE HEAD; an error's kind is a symbol, so the
;; lookup passes over what is not a pair.
(define (clause name datum)
  (let ((c (and (pair? datum) (list? datum)
                (find (lambda (x) (and (pair? x) (eq? (car x) name))) (cdr datum)))))
    (and c (cdr c))))
(define (clause1 name datum) (let ((c (clause name datum))) (and c (pair? c) (car c))))

;; the three tools, as children: -> the datum each prints
(define tools-dir (string-append (path-parent (current-directory)) "/tools"))
(define runs 0)
(define (tool name . args)
  (set! runs (+ runs 1))
  (let ((o (string-append root "/run-" (number->string runs) ".out")))
    (sh "scheme --script " (quoted (string-append tools-dir "/" name ".sc"))
        (apply string-append (map (lambda (a) (string-append " " (quoted a))) args))
        " > " (quoted o) " 2> " (quoted (string-append root "/run-" (number->string runs) ".err")) " < /dev/null")
    (file-datum o)))

;; ---- stores -------------------------------------------------------------------
(define n 0)
(define (store!)
  (set! n (+ n 1))
  (let ((st (string-append root "/s" (number->string n))))
    (sh "mkdir -p " (quoted st))
    (checked "init" (ask st 'init))
    st))
;; -> the new block's id, from the answer's (state ((<id> . <hash>)))
(define (insert! st title text)
  (let ((a (checked "insert" (ask st 'insert "--title" title "--text" text))))
    (car (car (clause1 'state a)))))
(define (hash-of st id) (block-hash (open-and-reduce st) id))
(define (cut-of st) (reduce-applied-cut (open-and-reduce st)))
(define W "coder-item")
;; THE TEXT GOES IN AS STANDARD INPUT, as drafts-restore writes it: a text
;; beginning `--` would otherwise read as an option.
(define (draft! st id text)
  (checked "write" (rpc-dispatch st (list 'write id "-" "--writer" W) "test" #f #f (lambda () text) #f)))
;; -> (version based-on cut text) of W's draft on ID, or what read answered
(define (draft-facts st id)
  (let ((a (ask st 'read id "--working-info" "--writer" W)))
    (if (and (eq? (head-of a) 'ok) (pair? (cadr a)) (eq? (cadr (cadr a)) 'working))
        (let ((p (cadr a))) (list (list-ref p 4) (list-ref p 5) (list-ref p 6) (list-ref p 7)))
        (list 'no-draft (head-of a)))))
(define (drafts-of st)
  (let ((a (ask st 'drafts "--writer" W)))
    (map (lambda (d) (clause1 'block d))
         (filter (lambda (d) (and (pair? d) (eq? (car d) 'draft))) (or (clause 'items a) '())))))
;; THE STORE IN GIT, cloned: the store's own .gitignore keeps the working
;; areas out, so the clone has the log and no drafts; adopt makes it this
;; machine's.
(define (git-of! st)
  (sh "git -C " (quoted st) " init -q && git -C " (quoted st) " config user.name fixture && git -C " (quoted st)
      " config user.email fixture@example.invalid && git -C " (quoted st) " config commit.gpgsign false && git -C "
      (quoted st) " add -A && git -C " (quoted st) " commit -q -m store"))
(define (clone! st)
  (set! n (+ n 1))
  (let ((c (string-append root "/c" (number->string n))))
    (sh "git clone -q " (quoted st) " " (quoted c))
    (checked "adopt" (ask c 'adopt))
    c))

;; ---- DD: dump and restore ---------------------------------------------------------
(printf "== DD: a writer's drafts dumped and restored on a git clone ==\n")
(define s1 (store!))
(define a1 (insert! s1 "A" "alpha\n"))
(define b1 (insert! s1 "B" "beta\n"))
(define c1 (insert! s1 "C" "gamma\n"))
(draft! s1 a1 "alpha, drafted\n")
(checked "set" (ask s1 'set c1 "title" "C two"))
(draft! s1 b1 "--beta, drafted\n")
(checked "set" (ask s1 'set c1 "title" "C three"))
(define facts-a (draft-facts s1 a1))
(define facts-b (draft-facts s1 b1))
(define dump1 (string-append root "/dump1"))
(define dumped (tool "drafts-dump" s1 "--writer" W dump1))
(define manifest (file-datum (string-append dump1 "/DRAFTS.sexp")))
(want "DD-1 the dump holds both drafts, their cuts differ, and the store's cut is later than both"
      (list (head-of dumped) (clause1 'dumped dumped)
            (length (filter (lambda (d) (and (pair? d) (eq? (car d) 'draft))) (cdr manifest)))
            (not (equal? (caddr facts-a) (caddr facts-b)))
            (not (equal? (cut-of s1) (caddr facts-b))))
      '(ok 2 2 #t #t))
(want "DD-1 the manifest names the store, the writer and the pinned cut, the later draft's cut"
      (list (string? (clause1 'store manifest)) (clause1 'writer manifest) (equal? (clause1 'pinned-cut manifest) (caddr facts-b)))
      (list #t W #t))
(want "DD-1 a dump into a directory that exists is refused"
      (head-of (tool "drafts-dump" s1 "--writer" W dump1))
      '(error dir-exists))
(git-of! s1)
(define cl1 (clone! s1))
(want "DD-2 the clone has the log and no drafts"
      (list (equal? (hash-of cl1 a1) (hash-of s1 a1)) (drafts-of cl1))
      '(#t ()))
(want "DD-2 restored on the clone, both drafts come back with the same version, based-on, cut and bytes"
      (in-order (tool "drafts-restore" cl1 dump1 "--writer" W)
                (list (draft-facts cl1 a1) (draft-facts cl1 b1)))
      (list '(ok (restored 2) (already 0)) (list facts-a facts-b)))
;; WRITES NOTHING, observed, not only reported: an identical write would
;; still replace the draft's file, so each file's identity is read before and
;; after.
(define (draft-file st id) (string-append (writer-directory st W) "/working/" id))
(define (identities st ids) (map (lambda (id) (path-stat (draft-file st id))) ids))
(define before-dd3 (identities cl1 (list a1 b1)))
(want "DD-3 a second restore finds both slots holding the dumped drafts and writes nothing"
      (in-order (tool "drafts-restore" cl1 dump1 "--writer" W)
                (equal? (identities cl1 (list a1 b1)) before-dd3))
      '((ok (restored 0) (already 2)) #t))
(define cl2 (clone! s1))
(checked "set" (ask cl2 'set a1 "title" "A moved"))
(want "DD-4 a block whose committed hash moved since the dump is refused by name, and nothing is written"
      (in-order (let ((a (tool "drafts-restore" cl2 dump1 "--writer" W))) (list (head-of a) (clause1 'block a)))
                (drafts-of cl2))
      (list (list '(error baseline-moved) a1) '()))
(define cl3 (clone! s1))
(checked "write" (ask cl3 'write b1 "something else\n" "--writer" W))
(define occupied-before (draft-facts cl3 b1))
(want "DD-5 a slot holding another draft is refused; that draft is unchanged, and nothing else is written"
      (in-order (let ((a (tool "drafts-restore" cl3 dump1 "--writer" W))) (list (head-of a) (clause1 'block a)))
                (equal? (draft-facts cl3 b1) occupied-before)
                (drafts-of cl3))
      (list (list '(error slot-occupied) b1) #t (list b1)))
(define other (store!))
(want "DD-6 a restore into another store is refused"
      (head-of (tool "drafts-restore" other dump1 "--writer" W))
      '(error other-store))
(want "DD-6 a restore under another writer is refused"
      (head-of (tool "drafts-restore" cl3 dump1 "--writer" "someone-else"))
      '(error other-writer))

;; ---- LC: the cut check before a landing's commit -------------------------------------
(printf "== LC: the landing commits only at the frozen cut ==\n")
(define s2 (store!))
(define a2 (insert! s2 "A" "alpha\n"))
(define b2 (insert! s2 "B" "beta\n"))
(draft! s2 a2 "alpha, landed\n")
(define freeze1 (string-append root "/freeze1"))
(checked "freeze" (tool "drafts-dump" s2 "--writer" W freeze1))
(define a2-before (hash-of s2 a2))
(checked "set" (ask s2 'set b2 "title" "B elsewhere"))
(want "LC-1 a commit on an unrelated root document since the freeze (no file changed): the drafts are committed at the cut it moved to"
      (in-order (head-of (tool "landing-commit" s2 freeze1))
                (equal? (hash-of s2 a2) a2-before)
                (drafts-of s2))
      (list 'ok #f '()))
(define s3 (store!))
(define a3 (insert! s3 "A" "alpha\n"))
(insert! s3 "B" "beta\n")
(draft! s3 a3 "alpha, landed\n")
(define freeze3 (string-append root "/freeze3"))
(checked "freeze" (tool "drafts-dump" s3 "--writer" W freeze3))
(define a3-before (hash-of s3 a3))
(want "LC-2 with the cut unchanged, the landing commits the frozen draft"
      (in-order (head-of (tool "landing-commit" s3 freeze3))
                (equal? (hash-of s3 a3) a3-before)
                (drafts-of s3))
      '(ok #f ()))
;; THE REBASE: the draft is written again onto the block's hash at the new
;; cut, frozen again, and lands.
(checked "rebase" (ask s2 'write a2 "alpha, landed\n" "--writer" W "--rebase"
                       "--based-on" (hash-of s2 a2) "--working-cut" (format "~s" (cut-of s2))))
(define freeze2 (string-append root "/freeze2"))
(checked "freeze" (tool "drafts-dump" s2 "--writer" W freeze2))
(want "LC-3 after the rebase and a new freeze, the landing commits"
      (in-order (head-of (tool "landing-commit" s2 freeze2))
                (equal? (hash-of s2 a2) a2-before)
                (drafts-of s2))
      '(ok #f ()))

;; ---- LF: the freeze ---------------------------------------------------------------
(printf "== LF: a freeze holds the draft list, the export, its manifest and the changes ==\n")
;; -> a store imported from a directory holding a.txt and b.txt
(define (imported-store!)
  (set! n (+ n 1))
  (let ((src (string-append root "/src" (number->string n))) (st (string-append root "/s" (number->string n))))
    (sh "mkdir -p " (quoted src) " " (quoted st))
    (call-with-output-file (string-append src "/a.txt") (lambda (p) (display "one\n" p)))
    (call-with-output-file (string-append src "/b.txt") (lambda (p) (display "two\n" p)))
    (checked "init" (ask st 'init))
    (checked "import" (ask st 'import-code src))
    st))
;; -> the one block of the file at PATH
(define (block-of st path)
  (let* ((state (open-and-reduce st))
         (file (find (lambda (id) (equal? (code-field state id 'path) path)) (code-files state)))
         (kids (code-children state file)))
    (unless (= 1 (length kids)) (assertion-violation 'drafts-tools "not one block" path kids))
    (car kids)))
(define (text-of path) (guard (e (#t 'ABSENT)) (call-with-input-file path get-string-all)))
;; -> ((<path> <hash matches the tree's file>) ...), MANIFEST.sha256 read line
;; by line: "<sha256>  <path>"
(define (manifest-checks dir)
  (let ((m (text-of (string-append dir "/MANIFEST.sha256"))))
    (if (not (string? m))
        'NO-MANIFEST
        (let loop ((lines (let split ((i 0) (start 0) (out '()))
                            (cond ((= i (string-length m)) (reverse (if (< start i) (cons (substring m start i) out) out)))
                                  ((char=? (string-ref m i) #\newline) (split (+ i 1) (+ i 1) (cons (substring m start i) out)))
                                  (else (split (+ i 1) start out)))))
                   (out '()))
          (if (null? lines)
              (reverse out)
              (let* ((l (car lines)) (hash (substring l 0 (min 64 (string-length l))))
                     (path (if (> (string-length l) 66) (substring l 66 (string-length l)) ""))
                     (bytes (guard (e (#t #f)) (call-with-port (open-file-input-port (string-append dir "/tree/" path)) get-bytevector-all))))
                (loop (cdr lines)
                      (cons (list path (and (bytevector? bytes)
                                            (string=? hash (sha256-hex bytes))))
                            out))))))))
(define s4 (imported-store!))
(define a4 (block-of s4 "a.txt"))
(draft! s4 a4 "one, frozen\n")
(define freeze4 (string-append root "/freeze4"))
(define frozen4 (tool "landing-freeze" s4 "--writer" W freeze4))
(want "LF-1 the freeze answers one draft and one changed file"
      frozen4
      (list 'ok '(frozen 1) '(changed 1) (list 'dir freeze4)))
(want "LF-1 its directory holds the draft list, the exported tree with the drafted bytes, the manifest and the changed files"
      (list (clause1 'writer (file-datum (string-append freeze4 "/DRAFTS.sexp")))
            (text-of (string-append freeze4 "/tree/a.txt"))
            (text-of (string-append freeze4 "/tree/b.txt"))
            (manifest-checks freeze4)
            (clause 'items (file-datum (string-append freeze4 "/CHANGED.sexp"))))
      (list W "one, frozen\n" "two\n" '(("a.txt" #t) ("b.txt" #t)) '((path "a.txt" content))))
(want "LF-3 landing-commit commits from the freeze's directory"
      (in-order (head-of (tool "landing-commit" s4 freeze4))
                (drafts-of s4))
      '(ok ()))
;; A DRAFT WRITTEN WHILE THE FREEZE WAITS after its dump: the freeze runs in
;; the background, held at freeze-after-dump; the row writes a second draft
;; once the hold is acknowledged, releases it, and reads the answer.
;; THE EXIT STATUS IS WRITTEN BESIDE ITS FILE AND MOVED INTO PLACE, here and
;; in the rows below: the rows wait for the file to exist, and a file that
;; exists before its status is written in it would read as no status.
(define s5 (imported-store!))
(draft! s5 (block-of s5 "a.txt") "one, frozen\n")
(define freeze5 (string-append root "/freeze5"))
(define release5 (string-append root "/release5"))
(define out5 (string-append root "/freeze5.out"))
(define rc5 (string-append root "/freeze5.rc"))
(sh "( THEOURGIA_INJECT=on THEOURGIA_HOLD=" (quoted (string-append "freeze-after-dump:" release5))
    " scheme --script " (quoted (string-append tools-dir "/landing-freeze.sc")) " " (quoted s5) " --writer " (quoted W)
    " " (quoted freeze5) " > " (quoted out5) " 2> " (quoted (string-append root "/freeze5.err")) " < /dev/null; echo $? > "
    (quoted (string-append rc5 ".part")) " && mv " (quoted (string-append rc5 ".part")) " " (quoted rc5) " ) &")
(define (wait-for path ms)
  (let loop ((k 0))
    (cond ((file-exists? path) #t)
          ((> k (div ms 50)) #f)
          (else (sleep (make-time 'time-duration 50000000 0)) (loop (+ k 1))))))
(define held5 (wait-for (string-append release5 ".held") 60000))
(when held5 (draft! s5 (block-of s5 "b.txt") "two, written during the freeze\n"))
(call-with-output-file release5 (lambda (p) (display "go" p)))
(define done5 (wait-for rc5 60000))
(want "LF-2 a draft written while the freeze waits after its dump refuses the freeze, which exits 1"
      (list held5 done5 (head-of (file-datum out5)) (and done5 (file-datum rc5)))
      '(#t #t (error drafts-changed) 1))
;; A TO B AND BACK TO A: held after the dump, the draft is rewritten to B, so
;; the view the tree is written from holds B; held again before the final
;; list -- reached only if the view's drafts were not checked -- it is
;; rewritten to A, its old version, so the final list matches the dump. Only
;; the view's own drafts see it, and the refusal says so: in-view, not now.
(define s6 (imported-store!))
(define a6 (block-of s6 "a.txt"))
(draft! s6 a6 "one, frozen\n")
(define freeze6 (string-append root "/freeze6"))
(define first6 (string-append root "/release6a"))
(define second6 (string-append root "/release6b"))
(define out6 (string-append root "/freeze6.out"))
(define rc6 (string-append root "/freeze6.rc"))
(sh "( THEOURGIA_INJECT=on THEOURGIA_HOLD=" (quoted (string-append "freeze-after-dump:" first6 ";freeze-before-list:" second6))
    " scheme --script " (quoted (string-append tools-dir "/landing-freeze.sc")) " " (quoted s6) " --writer " (quoted W)
    " " (quoted freeze6) " > " (quoted out6) " 2> " (quoted (string-append root "/freeze6.err")) " < /dev/null; echo $? > "
    (quoted (string-append rc6 ".part")) " && mv " (quoted (string-append rc6 ".part")) " " (quoted rc6) " ) &")
(define held6a (wait-for (string-append first6 ".held") 60000))
(when held6a (draft! s6 a6 "one, rewritten during the freeze\n"))
(call-with-output-file first6 (lambda (p) (display "go" p)))
;; the second hold is reached only when the view check let B through
(define held6b
  (let loop ((k 0))
    (cond ((file-exists? (string-append second6 ".held")) #t)
          ((file-exists? rc6) #f)
          ((> k 1200) 'NEITHER)
          (else (sleep (make-time 'time-duration 50000000 0)) (loop (+ k 1))))))
(when (eq? held6b #t) (draft! s6 a6 "one, frozen\n"))
(call-with-output-file second6 (lambda (p) (display "go" p)))
(define done6 (wait-for rc6 60000))
(want "LF-4 a draft rewritten A to B to A across the freeze is refused by the view's drafts, before the second hold"
      (let ((a (file-datum out6)))
        (list held6a held6b done6 (head-of a) (and (clause 'in-view a) #t) (and done6 (file-datum rc6))))
      '(#t #f #t (error drafts-changed) #t 1))
;; A TO B AFTER THE VIEW: held before the final list only, the draft is
;; rewritten once the view has been checked and the tree written from it, so
;; the view check has passed; the final list alone sees the change, and the
;; refusal says so (now, not in-view). Nothing is written after it.
(define s7 (imported-store!))
(define a7 (block-of s7 "a.txt"))
(draft! s7 a7 "one, frozen\n")
(define freeze7 (string-append root "/freeze7"))
(define release7 (string-append root "/release7"))
(define out7 (string-append root "/freeze7.out"))
(define rc7 (string-append root "/freeze7.rc"))
(sh "( THEOURGIA_INJECT=on THEOURGIA_HOLD=" (quoted (string-append "freeze-before-list:" release7))
    " scheme --script " (quoted (string-append tools-dir "/landing-freeze.sc")) " " (quoted s7) " --writer " (quoted W)
    " " (quoted freeze7) " > " (quoted out7) " 2> " (quoted (string-append root "/freeze7.err")) " < /dev/null; echo $? > "
    (quoted (string-append rc7 ".part")) " && mv " (quoted (string-append rc7 ".part")) " " (quoted rc7) " ) &")
(define held7 (wait-for (string-append release7 ".held") 60000))
(when held7 (draft! s7 a7 "one, rewritten before the final list\n"))
(call-with-output-file release7 (lambda (p) (display "go" p)))
(define done7 (wait-for rc7 60000))
(want "LF-5 a draft rewritten A to B after the view check is refused by the final list alone, which exits 1 and writes no CHANGED.sexp"
      (let ((a (file-datum out7)))
        (list held7 done7 (head-of a) (and (clause 'now a) #t) (and (clause 'in-view a) #t)
              (file-exists? (string-append freeze7 "/MANIFEST.sha256"))
              (file-exists? (string-append freeze7 "/CHANGED.sexp"))
              (and done7 (file-datum rc7))))
      '(#t #t (error drafts-changed) #t #f #t #f 1))

;; ---- LC with --start: the landing's start tag ---------------------------------------
(printf "== LC: --start tags the pinned cut, before the commit ==\n")
;; -> the cut a name binds, when it is settled; #f when the name is not bound
(define (tag-cut st name)
  (let ((t (find (lambda (t) (equal? (cadr t) name)) (store-tags st))))
    (and t (eq? (car t) 'settled) (caddr t))))
;; A CUT COMPARED AS A SET of (writer . seq), as the tools compare them.
(define (cut-set c)
  (list-sort (lambda (a b) (string<? (car a) (car b)))
             (filter (lambda (e) (and (pair? e) (integer? (cdr e)) (> (cdr e) 0))) c)))
;; -> (store a b dir pinned-cut): a store of two blocks, W's draft on A, the
;; draft list dumped into DIR. BEFORE, when given, runs on the store before
;; the draft is written, so what it writes is inside the pinned cut.
(define (frozen-store! name before)
  (let* ((st (store!)) (a (insert! st "A" "alpha\n")) (b (insert! st "B" "beta\n"))
         (dir (string-append root "/" name)))
    (when before (before st))
    (draft! st a "alpha, landed\n")
    (checked "freeze" (tool "drafts-dump" st "--writer" W dir))
    (list st a b dir (clause1 'pinned-cut (file-datum (string-append dir "/DRAFTS.sexp"))))))
;; THE TAG IS MADE BEFORE THE COMMIT: it binds the pinned cut, which the
;; commit's event is not in.
(define lc4 (frozen-store! "freeze-lc4" #f))
(define lc4-answer (tool "landing-commit" (car lc4) (cadddr lc4) "--start" "start-lc4"))
(want "LC-4 --start tags the pinned cut and then commits: the tag binds the pinned cut, the draft landed, and the answer names the tag and that cut"
      (in-order (head-of lc4-answer)
                (equal? (cut-set (tag-cut (car lc4) "start-lc4")) (cut-set (list-ref lc4 4)))
                (drafts-of (car lc4))
                (let ((c (clause 'start-tag lc4-answer)))
                  (and (pair? c)
                       (list (car c) (equal? (cut-set (clause1 'cut c)) (cut-set (list-ref lc4 4)))))))
      '(ok #t () ("start-lc4" #t)))
(define lc5 (frozen-store! "freeze-lc5" #f))
(checked "set" (ask (car lc5) 'set (caddr lc5) "title" "B elsewhere"))
(define lc5-moved (cut-of (car lc5)))
(want "LC-5 with --start and a commit on a root document since the freeze (no file changed): committed, the tag bound at the cut it moved to"
      (in-order (head-of (tool "landing-commit" (car lc5) (cadddr lc5) "--start" "start-lc5"))
                (let ((tc (tag-cut (car lc5) "start-lc5"))) (and tc (equal? (cut-set tc) (cut-set lc5-moved))))
                (drafts-of (car lc5)))
      (list 'ok #t '()))
;; THE NAME IS BOUND BEFORE THE FREEZE, so the pinned cut holds the tag's own
;; record and the cut check alone would pass: only the name's check refuses.
(define lc6 (frozen-store! "freeze-lc6" (lambda (st) (checked "tag" (ask st 'tag "start-lc6")))))
(define lc6-before (tag-cut (car lc6) "start-lc6"))
(want "LC-6 a start name already bound is refused before anything is written: the tag keeps its cut, nothing is committed"
      (in-order (head-of (tool "landing-commit" (car lc6) (cadddr lc6) "--start" "start-lc6"))
                (and lc6-before (equal? (tag-cut (car lc6) "start-lc6") lc6-before))
                (drafts-of (car lc6)))
      (list '(error start-tag-exists) #t (list (cadr lc6))))
;; A COMMIT BETWEEN THE CUT CHECK AND THE TAG: held at landing-before-tag, a
;; set on B moves the applied cut, so the tag binds a cut that is not the
;; pinned one; the landing is refused and the tag stays.
(define lc7 (frozen-store! "freeze-lc7" #f))
(define release-lc7 (string-append root "/release-lc7"))
(define out-lc7 (string-append root "/lc7.out"))
(define rc-lc7 (string-append root "/lc7.rc"))
(sh "( THEOURGIA_INJECT=on THEOURGIA_HOLD=" (quoted (string-append "landing-before-tag:" release-lc7))
    " scheme --script " (quoted (string-append tools-dir "/landing-commit.sc")) " " (quoted (car lc7)) " " (quoted (cadddr lc7))
    " --start start-lc7 > " (quoted out-lc7) " 2> " (quoted (string-append root "/lc7.err")) " < /dev/null; echo $? > "
    (quoted (string-append rc-lc7 ".part")) " && mv " (quoted (string-append rc-lc7 ".part")) " " (quoted rc-lc7) " ) &")
(define held-lc7 (wait-for (string-append release-lc7 ".held") 60000))
(when held-lc7 (checked "set" (ask (car lc7) 'set (caddr lc7) "title" "B during the landing")))
(call-with-output-file release-lc7 (lambda (p) (display "go" p)))
(define done-lc7 (wait-for rc-lc7 60000))
(want "LC-7 a commit between the cut check and the tag: refused start-tag-cut naming both cuts, the tag on the moved cut, nothing committed, exit 1"
      (let ((a (file-datum out-lc7)))
        (in-order held-lc7 done-lc7 (head-of a)
                  (and (equal? (cut-set (clause1 'pinned a)) (cut-set (list-ref lc7 4)))
                       (equal? (cut-set (clause1 'bound a)) (cut-set (tag-cut (car lc7) "start-lc7"))))
                  (let ((c (tag-cut (car lc7) "start-lc7")))
                    (and c (not (equal? (cut-set c) (cut-set (list-ref lc7 4))))))
                  (drafts-of (car lc7))
                  (and done-lc7 (file-datum rc-lc7))))
      (list #t #t '(error start-tag-cut) #t #t (list (cadr lc7)) 1))

;; ---- ST: structural drafts ----------------------------------------------------------
(printf "== ST: files created and deleted, dumped, frozen and landed ==\n")
;; THE NEW PROCEDURES ARE FOUND BY NAME, so the file loads on a tree without
;; them and its rows read red there rather than the file failing to load.
(define (entry lib name) (guard (e (#t #f)) (eval name (environment lib))))
(define working-new! (entry '(theourgia working) 'working-new!))
(define working-delete! (entry '(theourgia working) 'working-delete!))
(define working-structural (entry '(theourgia working) 'working-structural))
(define structural-version (entry '(theourgia reduce) 'structural-version))
(define provisional-id (entry '(theourgia reduce) 'provisional-id))
(define file-new-intents (entry '(theourgia landing-files) 'file-new-intents))
(define structural-consumes-mismatch (entry '(theourgia store) 'structural-consumes-mismatch))
(define (st-file st path)
  (let ((state (open-and-reduce st)))
    (find (lambda (id) (and (eq? 'text (code-field state id 'mode)) (equal? (code-field state id 'path) path)))
          (code-files state))))
(define (new-draft! st path text)
  (if working-new!
      (checked "working-new" (working-new! st #f W path (lang-fields (language-for-path path)) (string->utf8 text) #f))
      'no-working-new))
(define (delete-draft! st path)
  (if working-delete! (checked "working-delete" (working-delete! st #f W (st-file st path) #f)) 'no-working-delete))
(define (structural-items st)
  (filter (lambda (d) (and (pair? d) (memq (car d) '(new delete))))
          (or (clause 'items (ask st 'drafts "--writer" W)) '())))
(define (structural-key d) (if (eq? (car d) 'new) (clause1 'provisional d) (string-append "delete-" (clause1 'holder d))))
(define (version-of st kind key)
  (let ((d (find (lambda (d) (and (eq? (car d) kind) (equal? (clause1 (if (eq? kind 'new) 'path 'holder) d) key)))
                 (structural-items st))))
    (and d (clause1 'version d))))
;; -> ((kind mode path lang) ((kind mode src lang) ...)), a file as blocks, ids aside
(define (file-shape st path)
  (let* ((state (open-and-reduce st)) (h (st-file st path)))
    (and h (list (map (lambda (k) (code-field state h k)) '(kind mode path lang))
                 (map (lambda (c) (map (lambda (k) (let ((v (code-field state c k))) (if (string? v) (string->utf8 v) v)))
                                       '(kind mode src lang)))
                      (code-children state h))))))
(define (live-paths st)
  (let ((state (open-and-reduce st)))
    (list-sort string<? (map (lambda (id) (code-field state id 'path))
                             (filter (lambda (id) (eq? 'text (code-field state id 'mode))) (code-files state))))))
(define (block-count st) (length (state-block-ids (open-and-reduce st))))
(define c-sc "(define c 1)\n(define d 2)\n")

;; DUMP AND RESTORE: both kinds dumped, discarded, restored to the same versions
(define st1 (imported-store!))
(new-draft! st1 "c.sc" c-sc)
(delete-draft! st1 "b.txt")
(define st1-before (list-sort (lambda (x y) (string<? (car x) (car y)))
                              (map (lambda (d) (cons (structural-key d) (clause1 'version d))) (structural-items st1))))
(define st1-dump (string-append root "/st1-dump"))
(want "ST-DR both kinds are dumped, and after a discard each is restored at its dumped version"
      (in-order (head-of (tool "drafts-dump" st1 "--writer" W st1-dump))
                (list-sort (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
                           (map car (filter (lambda (d) (and (pair? d) (memq (car d) '(new delete))))
                                            (let ((m (file-datum (string-append st1-dump "/DRAFTS.sexp")))) (if (pair? m) (cdr m) '())))))
                (begin (for-each (lambda (p) (ask st1 'discard (car p) "--writer" W)) st1-before) (length (structural-items st1)))
                (head-of (tool "drafts-restore" st1 st1-dump "--writer" W))
                (equal? st1-before (list-sort (lambda (x y) (string<? (car x) (car y)))
                                              (map (lambda (d) (cons (structural-key d) (clause1 'version d))) (structural-items st1)))))
      '(ok (delete new) 0 ok #t))
;; THE FREEZE: the tree has the new file and not the deleted one
(define st1-freeze (string-append root "/st1-freeze"))
(define st1-frozen (tool "landing-freeze" st1 "--writer" W st1-freeze))
(want "ST-FZ the freeze's tree holds the new file and not the deleted one, and CHANGED names both"
      (list (head-of st1-frozen) (clause1 'frozen st1-frozen)
            (text-of (string-append st1-freeze "/tree/c.sc"))
            (text-of (string-append st1-freeze "/tree/b.txt"))
            (text-of (string-append st1-freeze "/tree/a.txt"))
            (let ((items (clause 'items (file-datum (string-append st1-freeze "/CHANGED.sexp")))))
              (and (list? items) (list (and (member '(path "c.sc" added) items) #t) (and (member '(path "b.txt" removed) items) #t)))))
      (list 'ok 2 c-sc 'ABSENT "one\n" '(#t #t)))
;; THE TWIN: the same bytes imported by tools/import-git.sc from a git commit
(define (twin-shape)
  (let* ((t (imported-store!)) (wt (string-append root "/twin-wt")))
    (checked "export" (ask t 'export-code wt "--raw"))
    (sh "git init -q " (quoted wt) " && git -C " (quoted wt) " config user.name fixture && git -C " (quoted wt)
        " config user.email fixture@example.invalid && git -C " (quoted wt) " config commit.gpgsign false && git -C "
        (quoted wt) " add -A && git -C " (quoted wt) " commit -q -m base")
    (call-with-output-file (string-append wt "/c.sc") (lambda (p) (display c-sc p)))
    (sh "rm " (quoted (string-append wt "/b.txt")) " && git -C " (quoted wt) " add -A && git -C " (quoted wt) " commit -q -m change")
    (let ((a (tool "import-git" t wt "HEAD~1" "HEAD" "--tag" "twin")))
      (list (head-of a) (file-shape t "c.sc") (live-paths t)))))
;; b.txt is one holder and one child, c.sc one holder and two: a landing that
;; deletes the one whole and creates the other changes the live count by one
(define st1-count-before (block-count st1))
(define st1-landed (tool "landing-commit" st1 st1-freeze "--start" "st-1"))
(define st1-twin (twin-shape))
(want "ST-LC1 the landing creates the file as import-git does for the same bytes, deletes the other, and maps the provisional id"
      (let ((shape (file-shape st1 "c.sc")))
        (list (head-of st1-landed)
              (live-paths st1)
              (and shape (length (cadr shape)))
              (and shape (equal? shape (cadr st1-twin)))
              (car st1-twin)
              (let ((c (clause 'created st1-landed)))
                (and (list? c) (= 1 (length c)) (equal? (cdr (car c)) (st-file st1 "c.sc"))))
              (structural-items st1)
              (- (block-count st1) st1-count-before)))
      (list 'ok '("a.txt" "c.sc") 2 #t 'ok #t '() 1))
;; THE SAME COMMIT AGAIN, under one request identity (--req and --cursor): a
;; replay, and its answer maps the provisional id as the first one did. A
;; commit without --req has a fresh identity each time and is never replayed:
;; its retry answers no-draft, its drafts being retired.
(define st6 (imported-store!))
(new-draft! st6 "f.sc" "(define f 1)\n")
(define st6-request
  (let ((c (car (cut-of st6))))
    (list 'commit "--writer" W
          "--working-new" (string-append (if provisional-id (provisional-id "f.sc") "none") "="
                                         (or (version-of st6 'new "f.sc") "none"))
          "--req" "st-lc6" "--cursor" (string-append (car c) ":" (number->string (cdr c))))))
(want "ST-LC6 a commit of a new file sent again under its request identity is a replay whose created clause is the first answer's"
      (let* ((first (rpc-dispatch st6 st6-request "test")) (again (rpc-dispatch st6 st6-request "test")))
        (list (head-of first) (head-of again)
              (and (clause 'created first) #t)
              (and (clause 'created first) (equal? (clause 'created again) (clause 'created first)))))
      '(ok ok #t #t))
;; THE PIECES, NAMED AS THE REVIEW CHANNEL NAMES THEM: a new file's commit names
;; each piece "<provisional id>/<K>", K from 0, as the block live under its
;; holder at that place after the landing; the replay of the same request
;; names them the same.
(define st8 (imported-store!))
(new-draft! st8 "p.sc" c-sc)
(define st8-pid (if provisional-id (provisional-id "p.sc") "none"))
(define st8-request
  (let ((c (car (cut-of st8))))
    (list 'commit "--writer" W
          "--working-new" (string-append st8-pid "=" (or (version-of st8 'new "p.sc") "none"))
          "--req" "st-lc8" "--cursor" (string-append (car c) ":" (number->string (cdr c))))))
(define st8-first (rpc-dispatch st8 st8-request "test"))
(define st8-again (rpc-dispatch st8 st8-request "test"))
(define st8-kids (let ((h (st-file st8 "p.sc"))) (if h (code-children (open-and-reduce st8) h) '())))
(want "ST-LC8 a new file's commit names its pieces, <pid>/0 and <pid>/1, the blocks live under its holder in order; its replay names them the same"
      (list (head-of st8-first) (head-of st8-again) (length st8-kids)
            (clause 'pieces st8-first) (clause 'pieces st8-again))
      (list 'ok 'ok 2
            (map (lambda (k id) (cons (string-append st8-pid "/" (number->string k)) id)) '(0 1) st8-kids)
            (map (lambda (k id) (cons (string-append st8-pid "/" (number->string k)) id)) '(0 1) st8-kids)))
;; A MIXED LANDING NAMES EVERY PIECE: an ordinary edit, two new files and a
;; deletion in one landing. The edit is the plan's first intent, so the new
;; files' inserts do not start at index 0, and each piece the commit names is
;; the block live at that place under its own holder after the landing.
(define st9 (imported-store!))
(define e-sc "(define e 1)\n(define f 2)\n(define g 3)\n")
(draft! st9 (block-of st9 "a.txt") "one, edited\n")
(new-draft! st9 "d.sc" c-sc)
(new-draft! st9 "e.sc" e-sc)
(delete-draft! st9 "b.txt")
(define st9-freeze (string-append root "/st9-freeze"))
(define st9-frozen (tool "landing-freeze" st9 "--writer" W st9-freeze))
(define st9-landed (tool "landing-commit" st9 st9-freeze))
(define (st9-kids path) (let ((h (st-file st9 path))) (if h (code-children (open-and-reduce st9) h) '())))
(define (st9-named path)
  (let ((pid (if provisional-id (provisional-id path) "none")) (kids (st9-kids path)))
    (map (lambda (id k) (cons (string-append pid "/" (number->string k)) id)) kids (iota (length kids)))))
(define (by-name xs) (list-sort (lambda (x y) (string<? (car x) (car y))) xs))
(want "ST-LC9 a landing of an edit, two new files and a deletion names every piece of both files as the block live at its place under its own holder"
      (list (head-of st9-frozen) (head-of st9-landed)
            (map (lambda (p) (length (st9-kids p))) '("d.sc" "e.sc"))
            (by-name (or (clause 'pieces st9-landed) '()))
            (by-name (or (clause 'created st9-landed) '()))
            (live-paths st9)
            (let ((v (code-field (open-and-reduce st9) (block-of st9 "a.txt") 'src))) (if (bytevector? v) (utf8->string v) v)))
      (list 'ok 'ok '(2 3)
            (by-name (append (st9-named "d.sc") (st9-named "e.sc")))
            (by-name (map (lambda (p) (cons (if provisional-id (provisional-id p) "none") (st-file st9 p))) '("d.sc" "e.sc")))
            '("a.txt" "d.sc" "e.sc")
            "one, edited\n"))
;; NO ROW FOR A REPLAY WHOSE HISTORY APPLIES THE PIECES OUT OF INDEX ORDER.
;; The replay names a new file's pieces by their intent index. The rule: the
;; members of a plan are applied by the writer that owns it, in ascending index
;; order (the order a completion writes the missing ones in); ST-LC9 pins the
;; pieces of that order. Measured when ST-LC9 was written, and not pinned by a
;; row: a member forged into another writer's segment references a plan that
;; segment does not hold and is held pending-plan (check answers verdict
;; damaged), and a forged lower index is refused consumes-version-mismatch.
;; Whether any other path can hand the replay out-of-order pieces is not
;; claimed here.
;; A DAMAGED ENVELOPE AT THE REPLAYED REQUEST'S SLOT: the replay needs no draft,
;; so the damage is not the answer
(define st7 (imported-store!))
(new-draft! st7 "g.sc" "(define g 1)\n")
(define st7-request
  (let ((c (car (cut-of st7))))
    (list 'commit "--writer" W
          "--working-new" (string-append (if provisional-id (provisional-id "g.sc") "none") "="
                                         (or (version-of st7 'new "g.sc") "none"))
          "--req" "st-lc7" "--cursor" (string-append (car c) ":" (number->string (cdr c))))))
(want "ST-LC7 a replay whose provisional slot holds a damaged envelope answers the replay and its created clause"
      (let* ((first (rpc-dispatch st7 st7-request "test"))
             (slot (string-append (writer-directory st7 W) "/structural/" (if provisional-id (provisional-id "g.sc") "none"))))
        (call-with-output-file slot (lambda (p) (display "(not an envelope" p)) 'replace)
        (let ((again (rpc-dispatch st7 st7-request "test")))
          (list (head-of first) (head-of again)
                (and (clause 'created first) (equal? (clause 'created again) (clause 'created first))))))
      '(ok ok #t))
;; A DELETE WHOSE FILE CHANGED SINCE: refused at commit, nothing written
(define (changed-under! st path)
  (let* ((state (open-and-reduce st)) (kid (car (code-children state (st-file st path)))))
    (checked "other write" (ask st 'write kid "changed\n" "--writer" "other"))
    (checked "other commit" (ask st 'commit "--writer" "other"))))
(define st2 (imported-store!))
(delete-draft! st2 "a.txt")
(changed-under! st2 "a.txt")
(want "ST-LC3 a delete whose file changed since its draft is refused stale-delete at commit, and nothing is written"
      (let ((h (st-file st2 "a.txt")) (count (block-count st2)) (cut (cut-of st2)))
        (in-order (head-of (ask st2 'commit "--writer" W "--working-delete" (string-append h "=" (or (version-of st2 'delete h) "none"))))
                  (= count (block-count st2)) (equal? (cut-set cut) (cut-set (cut-of st2)))))
      '((error stale-delete) #t #t))
;; THE MIXED LANDING: a valid new file beside that stale delete commits nothing
(define st3 (imported-store!))
(new-draft! st3 "d.sc" c-sc)
(delete-draft! st3 "a.txt")
(changed-under! st3 "a.txt")
(want "ST-LC4 a valid new file and a stale delete in one commit: refused, no record of the new file, the count and the cut unchanged"
      (let ((h (st-file st3 "a.txt")) (count (block-count st3)) (cut (cut-of st3))
            (pid (and provisional-id (provisional-id "d.sc"))))
        (in-order (head-of (ask st3 'commit "--writer" W
                                "--working-new" (string-append (or pid "none") "=" (or (version-of st3 'new "d.sc") "none"))
                                "--working-delete" (string-append h "=" (or (version-of st3 'delete h) "none"))))
                  (= count (block-count st3)) (equal? (cut-set cut) (cut-set (cut-of st3))) (st-file st3 "d.sc")))
      '((error stale-delete) #t #t #f))
;; THE START TAG OF A REFUSED LANDING: kept; the same name then refused. The
;; tag is a record, so the store's cut has moved past the drafts': the new
;; file's draft is discarded and written again on it, frozen again, and lands
;; under a new name
(define st4 (imported-store!))
(new-draft! st4 "e.sc" "(define e 1)\n")
(define st4-freeze (string-append root "/st4-freeze"))
(tool "landing-freeze" st4 "--writer" W st4-freeze)
(new-draft! st4 "e.sc" "(define e 2)\n")
(define st4-freeze2 (string-append root "/st4-freeze2"))
(want "ST-LC5 a landing refused after its tag keeps the tag; the same name is refused; a fresh freeze lands under a new name"
      (in-order (head-of (tool "landing-commit" st4 st4-freeze "--start" "st4-a"))
                (and (tag-cut st4 "st4-a") #t)
                (head-of (tool "landing-commit" st4 st4-freeze "--start" "st4-a"))
                (begin (ask st4 'discard (if provisional-id (provisional-id "e.sc") "none") "--writer" W)
                       (new-draft! st4 "e.sc" "(define e 2)\n")
                       (head-of (tool "landing-freeze" st4 "--writer" W st4-freeze2)))
                (head-of (tool "landing-commit" st4 st4-freeze2 "--start" "st4-b"))
                (live-paths st4))
      '((error working-version-changed) #t (error start-tag-exists) ok ok ("a.txt" "b.txt" "e.sc")))
;; THE CENSUS: a file's construction is written once. One other site builds a
;; file holder and is listed on purpose: import-code's own (code-project.sc).
;; The review channel's scope of a freeze (channel.sc) takes its copy of a
;; frozen new file from the same construction, the holder and its pieces as
;; the landing makes them, so it calls file-new-intents and builds no holder
;; of its own.
(define src-root (path-parent (current-directory)))
(define (source-text rel) (guard (e (#t "")) (call-with-input-file (string-append src-root "/" rel) get-string-all)))
(define (contains? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0)) (cond ((> (+ i m) n) #f) ((string=? (substring s i (+ i m)) sub) #t) (else (loop (+ i 1)))))))
(want "ST-CEN a file holder's insert is built in landing-files.sc (and import-code's own, code-project.sc), and import-git, the commit and the review channel's scope call that one construction"
      (list (list-sort string<? (filter (lambda (f) (contains? (source-text f) "'(kind . file)"))
                                        (append (filter (lambda (f) (and (> (string-length f) 3) (string=? (substring f (- (string-length f) 3) (string-length f)) ".sc")))
                                                        (directory-list src-root))
                                                (map (lambda (f) (string-append "tools/" f))
                                                     (filter (lambda (f) (and (> (string-length f) 3) (string=? (substring f (- (string-length f) 3) (string-length f)) ".sc")))
                                                             (directory-list (string-append src-root "/tools")))))))
            (map (lambda (f) (and (contains? (source-text f) "file-new-intents") (contains? (source-text f) "file-delete-intents")))
                 '("tools/import-git.sc" "working.sc"))
            ;; the review channel's copy of a file a freeze adds is built by it too
            (contains? (source-text "channel.sc") "file-new-intents"))
      '(("code-project.sc" "landing-files.sc") (#t #t) #t))
;; THE COMPLETION CHECK reads a structural item against the plan's members
(want "ST-CMP a new file's frozen members agree with their consumes item; a changed piece is named"
      (if (not (and structural-consumes-mismatch file-new-intents structural-version provisional-id))
          'no-structural-check
          (let* ((bytes (string->utf8 c-sc)) (intents (file-new-intents "c.sc" bytes 0))
                 (content (sha256-hex bytes)) (cut '(("wwwwwwww" . 1)))
                 (pid (provisional-id "c.sc"))
                 (consumes (list 'consumes "req1" (list (list pid (structural-version 'new "c.sc" content cut) content cut))))
                 (members (map cons (iota (length intents)) intents))
                 (tampered (map (lambda (m) (if (= (car m) 1)
                                                (cons 1 (list 'insert (cadr (cdr m)) (caddr (cdr m))
                                                              (map (lambda (f) (if (eq? (car f) 'src) (cons 'src (string->utf8 "(define c 9)\n")) f))
                                                                   (cadddr (cdr m)))))
                                                m))
                                members)))
            (list (structural-consumes-mismatch members consumes)
                  (equal? (structural-consumes-mismatch tampered consumes) pid))))
      '(#f #t))

;; ---- BH: a draft whose base fell behind, and the freeze that rebases it -----------
(printf "== BH: drafts says which drafts fell behind; the freeze rebases them or stops ==\n")
(define O "other-writer")
;; -> a store imported from a directory holding a.txt (TEXT) and b.txt
(define (lines-store! text)
  (set! n (+ n 1))
  (let ((src (string-append root "/src" (number->string n))) (st (string-append root "/s" (number->string n))))
    (sh "mkdir -p " (quoted src) " " (quoted st))
    (call-with-output-file (string-append src "/a.txt") (lambda (p) (display text p)))
    (call-with-output-file (string-append src "/b.txt") (lambda (p) (display "two\n" p)))
    (checked "init" (ask st 'init))
    (checked "import" (ask st 'import-code src))
    st))
;; another writer's committed change to block ID
(define (commit-other! st id text)
  (checked "other write" (rpc-dispatch st (list 'write id "-" "--writer" O) "test" #f #f (lambda () text) #f))
  (checked "other commit" (ask st 'commit id "--writer" O)))
(define (behind-clauses st)
  (filter (lambda (c) (and (pair? c) (eq? (car c) 'behind))) (cdr (ask st 'drafts "--writer" W))))
(define five "l1\nl2\nl3\nl4\nl5\n")
(define bh1 (lines-store! five))
(define bh1-a (block-of bh1 "a.txt"))
(define bh1-b (block-of bh1 "b.txt"))
(draft! bh1 bh1-a "l1 drafted\nl2\nl3\nl4\nl5\n")
(define bh1-h0 (hash-of bh1 bh1-a))
(checked "set" (ask bh1 'set bh1-b "title" "B elsewhere"))
(want "BH-1 TWIN a commit on another block moves the cut and not the drafted block's version: no behind clause"
      (behind-clauses bh1)
      '())
(draft! bh1 bh1-b "two drafted\n")
(commit-other! bh1 bh1-a "l1\nl2\nl3\nl4\nl5 committed\n")
(define bh1-h1 (hash-of bh1 bh1-a))
(want "BH-1 another writer commits the drafted block: drafts carries (behind <block> (based-on h0) (committed h1)), and the draft on the other block none"
      (list (behind-clauses bh1) (not (equal? bh1-h0 bh1-h1)))
      (list (list (list 'behind bh1-a (list 'based-on bh1-h0) (list 'committed bh1-h1))) #t))
(want "BH-1 the items are as they were: the behind draft reads fresh #f with now h1"
      (map (lambda (d) (list (clause1 'block d) (clause1 'now d) (clause1 'fresh d)))
           (filter (lambda (d) (and (pair? d) (eq? (car d) 'draft) (equal? (clause1 'block d) bh1-a)))
                   (or (clause 'items (ask bh1 'drafts "--writer" W)) '())))
      (list (list bh1-a bh1-h1 #f)))
;; THE FREEZE REBASES: the two changes touch different lines
(define bh1-freeze (string-append root "/bh1-freeze"))
(define bh1-frozen (tool "landing-freeze" bh1 "--writer" W bh1-freeze))
(define (dumped-text dir id)
  (let* ((m (file-datum (string-append dir "/DRAFTS.sexp")))
         (d (and (pair? m) (find (lambda (d) (and (pair? d) (eq? (car d) 'draft) (equal? (clause1 'block d) id))) (cdr m)))))
    (and d (text-of (string-append dir "/" (clause1 'bytes d))))))
(want "BH-2 the freeze rebases the behind draft (and moves the other onto the applied cut), and records it"
      (list (head-of bh1-frozen) (clause1 'rebased bh1-frozen) (clause1 'moved bh1-frozen)
            (let ((r (file-datum (string-append bh1-freeze "/REBASED.sexp"))))
              (and (pair? r) (filter (lambda (x) (eq? (car x) 'rebased)) (cdr r)))))
      (list 'ok 1 1 (list (list 'rebased bh1-a (list 'from bh1-h0) (list 'to bh1-h1)))))
(want "BH-2 the frozen bytes carry both changes, the tree too, and the pinned cut is the applied cut"
      (list (dumped-text bh1-freeze bh1-a)
            (text-of (string-append bh1-freeze "/tree/a.txt"))
            (equal? (clause1 'pinned-cut (file-datum (string-append bh1-freeze "/DRAFTS.sexp"))) (cut-of bh1)))
      (list "l1 drafted\nl2\nl3\nl4\nl5 committed\n" "l1 drafted\nl2\nl3\nl4\nl5 committed\n" #t))
(define (committed-text st id)
  (let* ((rec (state-read (open-and-reduce st) id))
         (src (and (list? rec) (assq 'fields rec) (assq 'src (cdr (assq 'fields rec))))))
    (cond ((not src) #f) ((bytevector? (cdr src)) (utf8->string (cdr src))) (else (cdr src)))))
(want "BH-2 the rebased freeze lands, and the block holds both changes"
      (in-order (head-of (tool "landing-commit" bh1 bh1-freeze))
                (committed-text bh1 bh1-a)
                (drafts-of bh1))
      '(ok "l1 drafted\nl2\nl3\nl4\nl5 committed\n" ()))
;; THE FREEZE STOPS: the two changes touch the same line
(define bh2 (lines-store! five))
(define bh2-a (block-of bh2 "a.txt"))
(define bh2-b (block-of bh2 "b.txt"))
(draft! bh2 bh2-a "l1\nl2\nl3 drafted\nl4\nl5\n")
(draft! bh2 bh2-b "two drafted\n")
(commit-other! bh2 bh2-a "l1\nl2\nl3 committed\nl4\nl5\n")
(define bh2-facts (list (draft-facts bh2 bh2-a) (draft-facts bh2 bh2-b)))
(define bh2-freeze (string-append root "/bh2-freeze"))
(want "BH-2 changes to the same line: drafts-behind-conflict naming the block, nothing frozen, no draft touched"
      (in-order (tool "landing-freeze" bh2 "--writer" W bh2-freeze)
                (file-exists? bh2-freeze)
                (equal? (list (draft-facts bh2 bh2-a) (draft-facts bh2 bh2-b)) bh2-facts))
      (list (list 'error 'drafts-behind-conflict (list 'blocks (list bh2-a))) #f #t))
;; ADJACENT LINES ARE A CONFLICT TOO: a doubt is not merged
(define bh3 (lines-store! five))
(define bh3-a (block-of bh3 "a.txt"))
(draft! bh3 bh3-a "l1\nl2 drafted\nl3\nl4\nl5\n")
(commit-other! bh3 bh3-a "l1\nl2\nl3 committed\nl4\nl5\n")
(want "BH-2 changes to adjacent lines: drafts-behind-conflict"
      (head-of (tool "landing-freeze" bh3 "--writer" W (string-append root "/bh3-freeze")))
      '(error drafts-behind-conflict))
;; THE CUT MOVED AND NO BLOCK DID: a commit on another block after the drafts
(define bh4 (lines-store! five))
(define bh4-a (block-of bh4 "a.txt"))
(define bh4-b (block-of bh4 "b.txt"))
(draft! bh4 bh4-a "l1 drafted\nl2\nl3\nl4\nl5\n")
(commit-other! bh4 bh4-b "two committed\n")
(define bh4-freeze (string-append root "/bh4-freeze"))
(define bh4-frozen (tool "landing-freeze" bh4 "--writer" W bh4-freeze))
(want "BH-2c a commit on another block since the drafts: the freeze moves the draft onto the applied cut, and the landing commits"
      (in-order (list (head-of bh4-frozen) (clause1 'moved bh4-frozen)
                      (let ((r (file-datum (string-append bh4-freeze "/REBASED.sexp"))))
                        (and (pair? r) (map (lambda (x) (list (car x) (cadr x))) (cdr r)))))
                (head-of (tool "landing-commit" bh4 bh4-freeze))
                (committed-text bh4 bh4-a)
                (drafts-of bh4))
      (list (list 'ok 1 (list (list 'moved bh4-a))) 'ok "l1 drafted\nl2\nl3\nl4\nl5\n" '()))
;; --check: a freeze made before another writer's commit, judged before it
;; lands. THE FROZEN BYTES ARE JUDGED: after the freeze the writer's live
;; draft also changes l5, so a check that read the live draft would call the
;; first commit below a conflict where the frozen draft merges with it.
(define bh5 (lines-store! five))
(define bh5-a (block-of bh5 "a.txt"))
(draft! bh5 bh5-a "l1 drafted\nl2\nl3\nl4\nl5\n")
(define bh5-freeze (string-append root "/bh5-freeze"))
(checked "freeze" (tool "landing-freeze" bh5 "--writer" W bh5-freeze))
(want "BH-3 --check on a freeze at the applied cut: current"
      (head-of (tool "landing-freeze" bh5 "--writer" W "--check" bh5-freeze))
      'ok)
(draft! bh5 bh5-a "l1 drafted\nl2\nl3\nl4\nl5 live\n")
(commit-other! bh5 bh5-a "l1\nl2\nl3\nl4\nl5 committed\n")
(define bh5-facts (draft-facts bh5 bh5-a))
(want "BH-3 --check after a commit that merges with the FROZEN draft (not the live one): refreeze naming the block as behind"
      (let ((a (tool "landing-freeze" bh5 "--writer" W "--check" bh5-freeze)))
        (list (head-of a) (clause1 'behind a) (clause1 'moved a)))
      (list '(error refreeze) (list bh5-a) '()))
(commit-other! bh5 bh5-a "l1 committed\nl2\nl3\nl4\nl5 committed\n")
(want "BH-3 --check after a commit to the drafted line: drafts-behind-conflict naming the block; the checks wrote no draft"
      (in-order (tool "landing-freeze" bh5 "--writer" W "--check" bh5-freeze)
                (equal? (draft-facts bh5 bh5-a) bh5-facts))
      (list (list 'error 'drafts-behind-conflict (list 'blocks (list bh5-a))) #t))
;; ONLY THE CUT MOVED: the check names the drafts it would move
(define bh6 (lines-store! five))
(define bh6-a (block-of bh6 "a.txt"))
(draft! bh6 bh6-a "l1 drafted\nl2\nl3\nl4\nl5\n")
(define bh6-freeze (string-append root "/bh6-freeze"))
(checked "freeze" (tool "landing-freeze" bh6 "--writer" W bh6-freeze))
(commit-other! bh6 (block-of bh6 "b.txt") "two committed\n")
(want "BH-3 --check after a commit on another file: refreeze naming the draft as moved, none behind, and the file that changed since the pin"
      (let ((a (tool "landing-freeze" bh6 "--writer" W "--check" bh6-freeze)))
        (list (head-of a) (clause1 'behind a) (clause1 'moved a) (clause1 'changed-since-pin a)))
      (list '(error refreeze) '() (list bh6-a) '("b.txt")))
;; A DELETE FROZEN AT THE APPLIED CUT is current, and the frozen items are
;; checked against their records
(define bh7 (lines-store! five))
(define bh7-a (block-of bh7 "a.txt"))
(draft! bh7 bh7-a "l1 drafted\nl2\nl3\nl4\nl5\n")
(delete-draft! bh7 "b.txt")
(define bh7-freeze (string-append root "/bh7-freeze"))
(checked "freeze" (tool "landing-freeze" bh7 "--writer" W bh7-freeze))
(want "BH-3 --check on a freeze holding a delete, at the applied cut: current"
      (head-of (tool "landing-freeze" bh7 "--writer" W "--check" bh7-freeze))
      'ok)
;; A FROZEN DELETE WITH ITS VERSION AND BASELINE GONE: the version cannot be
;; rebuilt and none is recorded, two failed readings that must not match
(define bh7-bare (string-append root "/bh7-bare"))
(sh "cp -R " (quoted bh7-freeze) " " (quoted bh7-bare))
(let ((m (file-datum (string-append bh7-bare "/DRAFTS.sexp"))))
  (call-with-output-file (string-append bh7-bare "/DRAFTS.sexp")
    (lambda (p)
      (write (cons (car m) (map (lambda (d) (if (and (pair? d) (eq? (car d) 'delete))
                                                (filter (lambda (c) (not (and (pair? c) (memq (car c) '(version based-on))))) d)
                                                d))
                                (cdr m)))
             p))
    'replace))
(want "BH-3 --check on a frozen delete with no version and no based-on: frozen-damaged naming it"
      (head-of (tool "landing-freeze" bh7 "--writer" W "--check" bh7-bare))
      '(error frozen-damaged))
(want "BH-3 landing-freeze with no arguments answers its usage"
      (head-of (tool "landing-freeze"))
      'usage)
(define bh7-before (draft-facts bh7 bh7-a))
(want "BH-3 a check with its directory left off answers usage, freezes nothing and moves no draft"
      (in-order (head-of (tool "landing-freeze" bh7 "--writer" W "--check"))
                (file-exists? "--check")
                (equal? (draft-facts bh7 bh7-a) bh7-before))
      '(usage #f #t))
;; THE DAMAGED FREEZES are copies of one holding a block draft only, so the
;; rows read the record checks and nothing else
(define bh10 (lines-store! five))
(define bh10-a (block-of bh10 "a.txt"))
(draft! bh10 bh10-a "l1 drafted\nl2\nl3\nl4\nl5\n")
(define bh10-freeze (string-append root "/bh10-freeze"))
(checked "freeze" (tool "landing-freeze" bh10 "--writer" W bh10-freeze))
(define bh10-bad (string-append root "/bh10-bad"))
(sh "cp -R " (quoted bh10-freeze) " " (quoted bh10-bad))
(let ((d (find (lambda (d) (and (pair? d) (eq? (car d) 'draft))) (cdr (file-datum (string-append bh10-bad "/DRAFTS.sexp"))))))
  (call-with-output-file (string-append bh10-bad "/" (clause1 'bytes d)) (lambda (p) (display "l1 tampered\n" p)) 'replace))
(want "BH-3 --check on a freeze whose draft bytes no longer match their record: frozen-damaged naming the block"
      (tool "landing-freeze" bh10 "--writer" W "--check" bh10-bad)
      (list 'error 'frozen-damaged (list 'block bh10-a)))
(define bh10-extra (string-append root "/bh10-extra"))
(sh "cp -R " (quoted bh10-freeze) " " (quoted bh10-extra) " && echo '(drafts)' >> " (quoted (string-append bh10-extra "/DRAFTS.sexp")))
(want "BH-3 --check on a manifest followed by a second datum: manifest-unreadable"
      (head-of (tool "landing-freeze" bh10 "--writer" W "--check" bh10-extra))
      '(error manifest-unreadable))
;; LINES THAT REPEAT: the draft deletes one of two equal pairs and the commit
;; changes the first line; read from the other end the deletion meets the
;; change, so the alignment is in doubt and the freeze stops
(define bh8 (lines-store! "a\nb\na\nb\n"))
(define bh8-a (block-of bh8 "a.txt"))
(draft! bh8 bh8-a "a\nb\n")
(commit-other! bh8 bh8-a "A\nb\na\nb\n")
(want "BH-2 a deletion of one of two equal runs against a change to the first: drafts-behind-conflict"
      (head-of (tool "landing-freeze" bh8 "--writer" W (string-append root "/bh8-freeze")))
      '(error drafts-behind-conflict))
;; A COMMIT DURING THE FREEZE: held after its dump, another writer commits
;; another block; the freeze is refused, its pinned cut behind the applied one
(define bh9 (lines-store! five))
(draft! bh9 (block-of bh9 "a.txt") "l1 drafted\nl2\nl3\nl4\nl5\n")
(define bh9-freeze (string-append root "/bh9-freeze"))
(define release9 (string-append root "/release9"))
(define out9 (string-append root "/bh9.out"))
(define rc9 (string-append root "/bh9.rc"))
(sh "( THEOURGIA_INJECT=on THEOURGIA_HOLD=" (quoted (string-append "freeze-after-dump:" release9))
    " scheme --script " (quoted (string-append tools-dir "/landing-freeze.sc")) " " (quoted bh9) " --writer " (quoted W)
    " " (quoted bh9-freeze) " > " (quoted out9) " 2> " (quoted (string-append root "/bh9.err")) " < /dev/null; echo $? > "
    (quoted (string-append rc9 ".part")) " && mv " (quoted (string-append rc9 ".part")) " " (quoted rc9) " ) &")
(define held9 (wait-for (string-append release9 ".held") 60000))
(when held9 (commit-other! bh9 (block-of bh9 "b.txt") "two committed\n"))
(call-with-output-file release9 (lambda (p) (display "go" p)))
(define done9 (wait-for rc9 60000))
(want "BH-4 a commit while the freeze waits after its dump refuses it, cut-moved, which exits 1"
      (list held9 done9 (head-of (file-datum out9)) (and done9 (file-datum rc9)))
      '(#t #t (error cut-moved) 1))
;; ---- MV: a structural draft off the applied cut, moved or named ----------------------
(printf "== MV: a structural draft behind the applied cut is moved when it still applies, or named ==\n")
;; a commit that moves the applied cut and no file a structural draft names
(define (unrelated-commit! st) (checked "set" (ask st 'set (block-of st "a.txt") "title" "elsewhere")))
(define (structural-of st key) (find (lambda (d) (equal? (structural-key d) key)) (structural-items st)))
(define m-text "(define m 1)\n")
(define m-pid (if provisional-id (provisional-id "m.sc") "none"))
(define q-pid (if provisional-id (provisional-id "q.sc") "none"))
;; ANOTHER WRITER CREATES PATH: its own new-file draft, committed
(define (other-creates! st path)
  (let ((a (checked "other new" (working-new! st #f O path (lang-fields (language-for-path path)) (string->utf8 "(define other 1)\n") #f))))
    (checked "other commit" (ask st 'commit "--writer" O "--working-new"
                                 (string-append (provisional-id path) "=" (clause1 'version a))))))
(define (rebased-records dir) (let ((r (file-datum (string-append dir "/REBASED.sexp")))) (and (pair? r) (cdr r))))
;; A NEW FILE BEHIND AN UNRELATED COMMIT: moved, frozen, checked, landed
(define m1 (imported-store!))
(new-draft! m1 "m.sc" m-text)
(define m1-cut0 (let ((d (structural-of m1 m-pid))) (and d (clause1 'cut d))))
(unrelated-commit! m1)
(define m1-cut1 (cut-of m1))
(define m1-freeze (string-append root "/m1-freeze"))
(define m1-frozen (tool "landing-freeze" m1 "--writer" W m1-freeze))
(want "ST-M1 a new file's draft behind an unrelated commit is moved: (moved 1), REBASED records it from its cut to the applied one, the tree holds its bytes, the pinned cut is the applied cut, --check is current, and the freeze lands"
      (in-order (head-of m1-frozen) (clause1 'moved m1-frozen)
                (rebased-records m1-freeze)
                (text-of (string-append m1-freeze "/tree/m.sc"))
                (equal? (clause1 'pinned-cut (file-datum (string-append m1-freeze "/DRAFTS.sexp"))) (cut-of m1))
                (head-of (tool "landing-freeze" m1 "--writer" W "--check" m1-freeze))
                (head-of (tool "landing-commit" m1 m1-freeze))
                (live-paths m1))
      (list 'ok 1 (list (list 'moved m-pid (list 'from-cut m1-cut0) (list 'to-cut m1-cut1))) m-text #t 'ok 'ok
            '("a.txt" "b.txt" "m.sc")))
;; THE PATH CREATED BY ANOTHER WRITER: that draft is named; a second new file
;; that still applies is not, and neither is moved
(define m2 (imported-store!))
(new-draft! m2 "m.sc" m-text)
(new-draft! m2 "q.sc" "(define q 1)\n")
(define m2-before (structural-items m2))
(other-creates! m2 "m.sc")
(define m2-freeze (string-append root "/m2-freeze"))
(want "ST-M2 a new file whose path another writer's commit created: drafts-behind-conflict naming it alone, nothing frozen, W's drafts untouched at their cut"
      (in-order (tool "landing-freeze" m2 "--writer" W m2-freeze)
                (file-exists? m2-freeze)
                (equal? (structural-items m2) m2-before))
      (list (list 'error 'drafts-behind-conflict (list 'blocks '()) (list 'structural (list m-pid))) #f #t))
;; A DELETE: moved behind an unrelated commit; named when its file changed
(define m3 (imported-store!))
(delete-draft! m3 "b.txt")
(define m3-key (string-append "delete-" (st-file m3 "b.txt")))
(define m3-cut0 (let ((d (structural-of m3 m3-key))) (and d (clause1 'cut d))))
(unrelated-commit! m3)
(define m3-cut1 (cut-of m3))
(define m3-freeze (string-append root "/m3-freeze"))
(define m3-frozen (tool "landing-freeze" m3 "--writer" W m3-freeze))
(want "ST-M3 a delete behind an unrelated commit is moved, recorded under delete-<holder>, the tree leaves the file out, and the freeze lands"
      (in-order (head-of m3-frozen) (clause1 'moved m3-frozen)
                (rebased-records m3-freeze)
                (text-of (string-append m3-freeze "/tree/b.txt"))
                (head-of (tool "landing-commit" m3 m3-freeze))
                (live-paths m3))
      (list 'ok 1 (list (list 'moved m3-key (list 'from-cut m3-cut0) (list 'to-cut m3-cut1))) 'ABSENT 'ok '("a.txt")))
(define m3b (imported-store!))
(delete-draft! m3b "b.txt")
(define m3b-key (string-append "delete-" (st-file m3b "b.txt")))
(changed-under! m3b "b.txt")
(define m3b-freeze (string-append root "/m3b-freeze"))
(want "ST-M3 a delete whose file another writer edited since: drafts-behind-conflict naming delete-<holder>, nothing frozen"
      (in-order (tool "landing-freeze" m3b "--writer" W m3b-freeze) (file-exists? m3b-freeze))
      (list (list 'error 'drafts-behind-conflict (list 'blocks '()) (list 'structural (list m3b-key))) #f))
;; --check: a freeze whose new file is behind an unrelated commit would be
;; moved; once another writer creates its path it conflicts. Nothing written.
(define m4 (imported-store!))
(new-draft! m4 "m.sc" m-text)
(define m4-freeze (string-append root "/m4-freeze"))
(checked "freeze" (tool "landing-freeze" m4 "--writer" W m4-freeze))
(unrelated-commit! m4)
(define m4-before (structural-items m4))
(want "ST-M4 --check on a freeze whose new file is now behind: refreeze naming its key as moved, none behind; the check writes nothing"
      (let ((a (tool "landing-freeze" m4 "--writer" W "--check" m4-freeze)))
        (in-order (head-of a) (clause1 'behind a) (clause1 'moved a) (equal? (structural-items m4) m4-before)))
      (list '(error refreeze) '() (list m-pid) #t))
(other-creates! m4 "m.sc")
(want "ST-M4 --check once another writer created the path: drafts-behind-conflict naming the key; nothing written"
      (in-order (tool "landing-freeze" m4 "--writer" W "--check" m4-freeze) (equal? (structural-items m4) m4-before))
      (list (list 'error 'drafts-behind-conflict (list 'blocks '()) (list 'structural (list m-pid))) #t))
;; A MOVE HELD: the freeze waits at STAGE, freeze-before-move (between the
;; judgement and the move) or structural-move-before-write (inside the move's
;; draft lock, before its one write); ON-HELD runs there with the writer's
;; structural directory and its answer is kept. The directory is made writable
;; again on every way out of the row, so no later row meets it read-only.
;; -> (held done <answer> <exit code> <on-held's answer>)
(define (held-freeze st dir tag stage on-held)
  (let ((rel (string-append root "/" tag "-release"))
        (out (string-append root "/" tag ".out")) (rc (string-append root "/" tag ".rc"))
        (sdir (string-append (writer-directory st W) "/structural")))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (sh "( THEOURGIA_INJECT=on THEOURGIA_HOLD=" (quoted (string-append stage ":" rel))
            " scheme --script " (quoted (string-append tools-dir "/landing-freeze.sc")) " " (quoted st) " --writer " (quoted W)
            " " (quoted dir) " > " (quoted out) " 2> " (quoted (string-append root "/" tag ".err")) " < /dev/null; echo $? > "
            (quoted (string-append rc ".part")) " && mv " (quoted (string-append rc ".part")) " " (quoted rc) " ) &")
        (let* ((h (wait-for (string-append rel ".held") 60000))
               (seen (and h (on-held sdir))))
          (call-with-output-file rel (lambda (p) (display "go" p)))
          (let ((done (wait-for rc 60000)))
            (list h done (file-datum out) (and done (file-datum rc)) seen))))
      (lambda () (sh "chmod u+w " (quoted sdir) " 2>/dev/null")))))
(define (read-only! d) (sh "chmod a-w " (quoted d)) #t)
;; -> (<version> <cut> <bytes>) of the live structural entry at KEY, or #f,
;; read from working-structural, whose items carry the bytes (drafts' do not)
(define (entry-of st key)
  (let* ((a (and working-structural (working-structural st #f W)))
         (d (and (pair? a) (eq? (car a) 'ok)
                 (find (lambda (i) (equal? (structural-key i) key)) (caddr a)))))
    (and d (list (clause1 'version d) (clause1 'cut d) (clause1 'bytes d)))))
;; THE WRITE FAULTED INSIDE THE LOCK: the structural directory read-only at
;; structural-move-before-write, so the move's one write fails; nothing was
;; removed before it, so the entry stands at its old cut with its old bytes
(define m5 (imported-store!))
(new-draft! m5 "m.sc" m-text)
(define m5-before (entry-of m5 m-pid))
(unrelated-commit! m5)
(define m5-freeze (string-append root "/m5-freeze"))
(define m5-run (held-freeze m5 m5-freeze "m5" "structural-move-before-write" read-only!))
(want "ST-M5 the move's write refused inside its lock: structural-move-refused naming the key and working-unavailable, the entry still at its old cut and version with its old bytes, nothing frozen"
      (let ((a (list-ref m5-run 2)))
        (list (list (car m5-run) (cadr m5-run)) (list-ref m5-run 3)
              (head-of a) (clause1 'key a) (head-of (clause1 'answer a))
              (entry-of m5 m-pid) (file-exists? m5-freeze)))
      (list '(#t #t) 1 '(error structural-move-refused) m-pid '(error working-unavailable) m5-before #f))
;; THE DIRECTORY FLUSH FAULTED AFTER THE RENAME (fsync-fail on the structural
;; directory's flush, the last step of the move's one atomic write): the move
;; is refused although the whole new entry is installed, at the applied cut
;; with its old bytes and a version recomputed there; a second freeze reads it
;; as it stands. A pin of what the write does, not a red-before row.
(define m5c (imported-store!))
(new-draft! m5c "m.sc" m-text)
(define m5c-before (entry-of m5c m-pid))
(unrelated-commit! m5c)
(define m5c-out (string-append root "/m5c.out"))
(sh "THEOURGIA_INJECT=on THEOURGIA_FAULT=fsync-fail@working:dir=structural scheme --script "
    (quoted (string-append tools-dir "/landing-freeze.sc")) " " (quoted m5c) " --writer " (quoted W) " "
    (quoted (string-append root "/m5c-freeze")) " > " (quoted m5c-out) " 2> " (quoted (string-append root "/m5c.err")) " < /dev/null")
(define m5c-answer (file-datum m5c-out))
(define m5c-after (entry-of m5c m-pid))
(want "ST-M5c the directory flush faulted after the move's rename: structural-move-refused with working-unavailable, the whole new entry at the applied cut (old bytes, version recomputed there), and a second freeze answers ok"
      (list (head-of m5c-answer) (head-of (clause1 'answer m5c-answer))
            (and m5c-after (equal? (cadr m5c-after) (cut-of m5c)))
            (and m5c-after (caddr m5c-after))
            (and m5c-after m5c-before (not (equal? (car m5c-after) (car m5c-before)))
                 (equal? (car m5c-after) (structural-version 'new "m.sc" (sha256-hex (caddr m5c-after)) (cadr m5c-after))))
            (head-of (tool "landing-freeze" m5c "--writer" W (string-append root "/m5c-freeze2"))))
      (list '(error structural-move-refused) '(error working-unavailable) #t (string->utf8 m-text) #t 'ok))
;; A DRAFT WRITTEN AGAIN AFTER THE JUDGEMENT: at freeze-before-move the coder
;; writes m.sc again (same key, its own version); the move finds another
;; version than the one judged and is refused, and the new draft stands
(define m6-text "(define m 2)\n")
(define m6 (imported-store!))
(new-draft! m6 "m.sc" m-text)
(define m6-before (entry-of m6 m-pid))
(unrelated-commit! m6)
(define m6-run (held-freeze m6 (string-append root "/m6-freeze") "m6" "freeze-before-move"
                            (lambda (d) (new-draft! m6 "m.sc" m6-text) (entry-of m6 m-pid))))
(want "ST-M6 a draft written again at freeze-before-move: the move is refused structural-changed (expected the judged version, found the new one), and the new draft stands untouched at its cut"
      (let ((a (list-ref m6-run 2)) (seen (list-ref m6-run 4)))
        (list (list (car m6-run) (cadr m6-run)) (list-ref m6-run 3) (head-of a) (clause1 'key a)
              (head-of (clause1 'answer a))
              (equal? (clause1 'expected (clause1 'answer a)) (and m6-before (car m6-before)))
              (equal? (clause1 'found (clause1 'answer a)) (and seen (car seen)))
              (and seen (caddr seen)) (equal? (entry-of m6 m-pid) seen)))
      (list '(#t #t) 1 '(error structural-move-refused) m-pid '(error structural-changed) #t #t
            (string->utf8 m6-text) #t))
;; THE STAGE IS INSIDE THE LOCK: a write of the same writer's draft begun at
;; structural-move-before-write waits for the move, and lands after it
(define m6b (imported-store!))
(new-draft! m6b "m.sc" m-text)
(unrelated-commit! m6b)
(define m6b-script (string-append root "/m6b-write.sc"))
(define m6b-rc (string-append root "/m6b-write.rc"))
(call-with-output-file m6b-script
  (lambda (p)
    (write '(import (chezscheme) (only (theourgia working) working-new!)) p) (newline p)
    (write `(let ((a (working-new! ,m6b #f ,W "m.sc" ',(lang-fields (language-for-path "m.sc")) (string->utf8 ,m6-text) #f)))
              (write a) (newline))
           p)
    (newline p)))
(define m6b-run
  (held-freeze m6b (string-append root "/m6b-freeze") "m6b" "structural-move-before-write"
               (lambda (d)
                 (sh "( scheme --script " (quoted m6b-script) " > " (quoted (string-append root "/m6b-write.out")) " 2>&1 < /dev/null; echo $? > "
                     (quoted (string-append m6b-rc ".part")) " && mv " (quoted (string-append m6b-rc ".part")) " " (quoted m6b-rc) " ) &")
                 ;; still waiting after 1.5 s while the move holds its lock
                 (not (wait-for m6b-rc 1500)))))
(define m6b-done (wait-for m6b-rc 60000))
(want "ST-M6 a write of the same draft begun while the move holds inside its lock waits for it, then lands: the draft holds the later bytes"
      (list (list (car m6b-run) (cadr m6b-run)) (list-ref m6b-run 4) m6b-done (and m6b-done (file-datum m6b-rc))
            (let ((e (entry-of m6b m-pid))) (and e (caddr e))))
      (list '(#t #t) #t #t 0 (string->utf8 m6-text)))
;; --check AFTER THE DRAFT WAS MADE AGAIN: a freeze whose new file is behind,
;; its live draft discarded and laid again; the frozen item is judged, so the
;; answer names it as moved, and nothing is written
(define m7 (imported-store!))
(new-draft! m7 "m.sc" m-text)
(define m7-freeze (string-append root "/m7-freeze"))
(checked "freeze" (tool "landing-freeze" m7 "--writer" W m7-freeze))
(unrelated-commit! m7)
(checked "discard" (ask m7 'discard m-pid "--writer" W))
(new-draft! m7 "m.sc" m6-text)
(define m7-before (entry-of m7 m-pid))
(want "ST-M7 --check on a freeze whose new file is behind and whose draft was made again: refreeze naming the key as moved; the live draft untouched"
      (let ((a (tool "landing-freeze" m7 "--writer" W "--check" m7-freeze)))
        (list (head-of a) (clause1 'behind a) (clause1 'moved a) (equal? (entry-of m7 m-pid) m7-before)))
      (list '(error refreeze) '() (list m-pid) #t))

;; ---- SEED: what a writable store starts from ---------------------------------------
(printf "== SEED: a copy of a store is adopted, written and committed ==\n")
;; -> the first block of the store that holds a src, for the seed's one
;; write, read off one reduction of the store: a block's record is
;; ((id . <id>) (fields (<name> . <value>) ...) ...), and its src is a string
;; in a section and bytes in an imported file's block
(define (text-block st)
  (let ((state (open-and-reduce st)))
    (find (lambda (id)
            (let* ((rec (state-read state id))
                   (fields (and (list? rec) (assq 'fields rec)))
                   (src (and fields (assq 'src (cdr fields)))))
              (and src (or (string? (cdr src)) (bytevector? (cdr src))))))
          (state-block-ids state))))
;; -> (owner? adopt write commit) on a cp -R copy of ST, in a scratch place
(define (seed-check st)
  (set! n (+ n 1))
  (let ((copy (string-append root "/seed" (number->string n))))
    (sh "cp -R " (quoted st) " " (quoted copy))
    (let* ((owners (string-append root "/owners-" (number->string n) ".txt"))
           (_ (sh "ls " (quoted copy) "/writers/*/owner.sexp > " (quoted owners) " 2>/dev/null"))
           (owner? (> (string-length (call-with-input-file owners get-string-all)) 0))
           (adopted (head-of (ask copy 'adopt)))
           (id (text-block copy))
           (wrote (head-of (ask copy 'write id "seeded\n" "--writer" "seed-check")))
           (committed (head-of (ask copy 'commit id "--writer" "seed-check"))))
      (list owner? adopted wrote committed))))
(want "SEED-1 a copy of a store holds a writer's owner.sexp, is adopted, and takes a draft and a commit"
      (seed-check s3)
      '(#t ok ok ok))
(let ((v (getenv "THEOURGIA_SEED_STORE")))
  (if (and (string? v) (> (string-length v) 0))
      (want "SEED-2 a copy of THEOURGIA_SEED_STORE holds owner.sexp, is adopted, and takes a draft and a commit"
            (seed-check v)
            '(#t ok ok ok))
      (printf "SKIP SEED-2: set THEOURGIA_SEED_STORE to a split mirror to check the store a writable store is seeded from (an opt-in, not a failure)\n")))

;; ---- FS: what the files are made of decides, not the cut ---------------------
;; A cut moved only by records on other blocks (a root document, a tag)
;; leaves the gated tree as it was: --check is current and landing-commit
;; commits at the cut it moved to. A change to a file's holder or to any
;; block under it, text or datum, is a refreeze naming the file.
(printf "== FS: a cut moved only by records on other blocks is not a move ==\n")
;; -> (store a-block freeze-dir pinned-cut), W's draft of a.txt frozen;
;; SETUP, when given, runs on the store before the draft
(define (fs-freeze! name . setup)
  (let* ((st (lines-store! five)) (dir (string-append root "/" name)))
    (when (pair? setup) ((car setup) st))
    (let ((a (block-of st "a.txt")))
      (draft! st a "l1 drafted\nl2\nl3\nl4\nl5\n")
      (checked "freeze" (tool "landing-freeze" st "--writer" W dir))
      (list st a dir (cut-of st)))))
;; -> the clauses inside (ok (current ...)), or #f
(define (current-of a) (and (eq? (head-of a) 'ok) (clause 'current a)))
(define (in-current name cur) (and cur (clause1 name (cons 'current cur))))
(let* ((f (fs-freeze! "fs1")) (st (car f)) (a (cadr f)) (dir (caddr f)) (pinned (cadddr f)))
  (insert! st "a review letter" "not a file")
  (let* ((moved-to (cut-of st)) (c (tool "landing-freeze" st "--writer" W "--check" dir)) (cur (current-of c)))
    (want "FS-1 --check after a root document was inserted: current, naming the pinned cut and the cut it moved to"
          (list (head-of c) (equal? (in-current 'pinned cur) pinned) (equal? (in-current 'now cur) moved-to)
                (equal? pinned moved-to))
          '(ok #t #t #f))
    (let* ((before (hash-of st a)) (l (tool "landing-commit" st dir "--start" "fs1-start"))
           (t (clause 'start-tag l)))
      (want "FS-2 landing-commit after it: committed, the start tag bound at the cut the check judged, the draft gone"
            (list (head-of l) (not (equal? (hash-of st a) before)) (drafts-of st)
                  (and t (equal? (clause1 'cut (cons 'start-tag (cdr t))) moved-to)))
            '(ok #t () #t)))))
(let* ((f (fs-freeze! "fs3")) (st (car f)) (dir (caddr f)))
  (checked "tag" (ask st 'tag "fs3-tag"))
  (want "FS-3 --check after a tag only: current"
        (head-of (tool "landing-freeze" st "--writer" W "--check" dir))
        'ok))
(let* ((f (fs-freeze! "fs4")) (st (car f)) (a (cadr f)) (dir (caddr f)))
  (commit-other! st (block-of st "b.txt") "two committed\n")
  (let* ((c (tool "landing-freeze" st "--writer" W "--check" dir)) (before (hash-of st a))
         (l (tool "landing-commit" st dir)))
    (want "FS-4 another writer's commit to another file: --check is refreeze and landing-commit cut-moved, both naming it, nothing committed"
          (list (head-of c) (clause1 'changed-since-pin c) (head-of l) (clause1 'changed-since-pin l)
                (equal? (hash-of st a) before) (drafts-of st))
          (list '(error refreeze) '("b.txt") '(error cut-moved) '("b.txt") #t (list a)))
    (want "FS-4 with --start after that commit: refused cut-moved before the tag, so no tag is made"
          (in-order (head-of (tool "landing-commit" st dir "--start" "fs4-start"))
                    (tag-cut st "fs4-start")
                    (drafts-of st))
          (list '(error cut-moved) #f (list a)))))
(let* ((f (fs-freeze! "fs5")) (st (car f)) (dir (caddr f)))
  (checked "title" (ask st 'set (block-of st "b.txt") "title" "a title only"))
  (want "FS-5 a title on a block under a file holder: a refreeze naming the file (compared by version: it errs toward a refreeze)"
        (let ((c (tool "landing-freeze" st "--writer" W "--check" dir)))
          (list (head-of c) (clause1 'changed-since-pin c)))
        (list '(error refreeze) '("b.txt"))))
;; A BLOCK NESTED UNDER A FILE'S CHILD is under the file too: the scope is
;; every block under a holder at any depth.
(define fs7-nested #f)
(let* ((f (fs-freeze! "fs7" (lambda (st)
                              (let ((a (checked "nested" (ask st 'insert "--under" (block-of st "b.txt") "--title" "nested" "--text" "under b's block"))))
                                (set! fs7-nested (car (car (clause1 'state a))))))))
       (st (car f)) (dir (caddr f)))
  (checked "nested set" (ask st 'set fs7-nested "title" "nested, changed"))
  (want "FS-7 a block nested under a file's child, changed after the freeze: a refreeze naming that file"
        (let ((c (tool "landing-freeze" st "--writer" W "--check" dir)))
          (list (head-of c) (clause1 'changed-since-pin c)))
        (list '(error refreeze) '("b.txt"))))
;; A DATUM LIBRARY is not in the raw projection, so changed-files says nothing
;; of a change to one; the version comparison sees it. Its holder is kind
;; library, which code-files does not list, so the form is found by path.
(define (datum-form-of st path)
  (let* ((state (open-and-reduce st))
         (lib (find (lambda (id) (and (eq? (code-field state id 'kind) 'library) (equal? (code-field state id 'path) path)))
                    (state-block-ids state)))
         (forms (if lib (code-children state lib) '())))
    (unless (= 1 (length forms)) (assertion-violation 'drafts-tools "not one form" path forms))
    (car forms)))
(let* ((dsrc (string-append root "/fs6-datum"))
       (f (fs-freeze! "fs6" (lambda (st)
                              (sh "mkdir -p " (quoted dsrc))
                              (call-with-output-file (string-append dsrc "/lib.sls")
                                (lambda (p) (display "(library (lib a) (export f) (import (rnrs)) (define (f) 1))\n" p)))
                              (checked "import datum" (ask st 'import-code dsrc "--datum")))))
       (st (car f)) (dir (caddr f)) (pinned (cadddr f)))
  (checked "datum set" (ask st 'set (datum-form-of st "lib.sls") "body" "(define (f) 2)"))
  (let ((c (tool "landing-freeze" st "--writer" W "--check" dir))
        (files (ask st 'changed-files (format "~s" pinned) (format "~s" (cut-of st)))))
    (want "FS-6 a datum library's form changed after the freeze: changed-files answers nothing, and --check is a refreeze naming the library"
          (list (clause 'items files) (head-of c) (clause1 'changed-since-pin c))
          (list '() '(error refreeze) '("lib.sls")))))

(sh "rm -rf " (quoted root))
(printf "rows: ~a\n~a failures\ndrafts-tools complete\n" rows bad)
(exit (if (zero? bad) 0 1))
