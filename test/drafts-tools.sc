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
;; LF: tools/landing-freeze.sc on a store imported from files: its directory
;; holds the draft list, the working view's raw export, its manifest and the
;; changed files; a draft written while the freeze waits at its hold stage
;; (THEOURGIA_HOLD=freeze-after-dump) refuses it; landing-commit commits from
;; the freeze's directory.
;; SEED: a copy of a store holds a writer's owner.sexp; adopt takes it over,
;; and a draft is written and committed. THEOURGIA_SEED_STORE names a store to
;; copy as well (the split mirror a writable store would be seeded from);
;; without it, that row prints why it was skipped.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) block-hash reduce-applied-cut state-block-ids)
        (only (theourgia code-project) code-files code-field code-children)
        (only (theourgia log) writer-directory)
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
(define (ask st . req) (rpc-dispatch st req "test"))
(define (head-of a) (cond ((not (pair? a)) a) ((eq? (car a) 'error) (list 'error (cadr a))) (else (car a))))
(define (checked what a) (unless (eq? 'ok (head-of a)) (assertion-violation 'drafts-tools what a)) a)
(define (clause name datum)
  (let ((c (and (pair? datum) (list? datum) (assq name (cdr datum))))) (and c (cdr c))))
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
(want "LC-1 a commit on an unrelated block since the freeze: refused cut-moved, and nothing is committed"
      (in-order (head-of (tool "landing-commit" s2 freeze1))
                (equal? (hash-of s2 a2) a2-before)
                (drafts-of s2))
      (list '(error cut-moved) #t (list a2)))
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
            (let ((m (text-of (string-append freeze4 "/MANIFEST.sha256")))) (and (string? m) (length (filter (lambda (c) (char=? c #\newline)) (string->list m)))))
            (clause 'items (file-datum (string-append freeze4 "/CHANGED.sexp"))))
      (list W "one, frozen\n" "two\n" 2 '((path "a.txt" content))))
(want "LF-3 landing-commit commits from the freeze's directory"
      (in-order (head-of (tool "landing-commit" s4 freeze4))
                (drafts-of s4))
      '(ok ()))
;; A DRAFT WRITTEN WHILE THE FREEZE WAITS after its dump: the freeze runs in
;; the background, held at freeze-after-dump; the row writes a second draft
;; once the hold is acknowledged, releases it, and reads the answer.
(define s5 (imported-store!))
(draft! s5 (block-of s5 "a.txt") "one, frozen\n")
(define freeze5 (string-append root "/freeze5"))
(define release5 (string-append root "/release5"))
(define out5 (string-append root "/freeze5.out"))
(define rc5 (string-append root "/freeze5.rc"))
(sh "( THEOURGIA_INJECT=on THEOURGIA_HOLD=" (quoted (string-append "freeze-after-dump:" release5))
    " scheme --script " (quoted (string-append tools-dir "/landing-freeze.sc")) " " (quoted s5) " --writer " (quoted W)
    " " (quoted freeze5) " > " (quoted out5) " 2> " (quoted (string-append root "/freeze5.err")) " < /dev/null; echo $? > "
    (quoted rc5) " ) &")
(define (wait-for path ms)
  (let loop ((k 0))
    (cond ((file-exists? path) #t)
          ((> k (div ms 50)) #f)
          (else (sleep (make-time 'time-duration 50000000 0)) (loop (+ k 1))))))
(define held5 (wait-for (string-append release5 ".held") 60000))
(when held5 (draft! s5 (block-of s5 "b.txt") "two, written during the freeze\n"))
(call-with-output-file release5 (lambda (p) (display "go" p)))
(wait-for rc5 60000)
(want "LF-2 a draft written while the freeze waits after its dump refuses the freeze"
      (list held5 (head-of (file-datum out5)))
      '(#t (error drafts-changed)))

;; ---- SEED: what a writable store starts from ---------------------------------------
(printf "== SEED: a copy of a store is adopted, written and committed ==\n")
;; -> the first block of the store whose read answers text, for the seed's
;; one write
(define (text-block st)
  (find (lambda (id) (let ((a (ask st 'read id))) (and (eq? (head-of a) 'ok) (pair? (cadr a)) (eq? (car (cadr a)) 'text))))
        (state-block-ids (open-and-reduce st))))
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

(sh "rm -rf " (quoted root))
(printf "rows: ~a\n~a failures\ndrafts-tools complete\n" rows bad)
(exit (if (zero? bad) 0 1))
