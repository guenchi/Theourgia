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

;; tools/edit-export.sc and tools/edit-sync.sc: a writer's working view
;; exported as files, edited as files, and synced back as that writer's
;; drafts. Each store is split-imported from definitions and its blocks are
;; then given known line texts, so every span is plain arithmetic; a git
;; repository holds the store's raw export as the base the modes come from.
;; Both tools run as children and the fixture reads the datum each prints,
;; then the drafts through the store. Each row uses a writer of its own, so
;; one row's drafts are never another's working view; the rows that commit
;; to the store come last.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia store) open-and-reduce)
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

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/edit-tools-" (number->string (get-process-id))))
(when (file-exists? root) (assertion-violation 'edit-tools "scratch directory already exists" root))
(define (sh . xs) (system (apply string-append xs)))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(sh "mkdir -p " (quoted root))

(define (ask st . req) (rpc-dispatch st req "test"))
(define (head-of a)
  (cond ((not (pair? a)) a)
        ((eq? (car a) 'error) (list 'error (cadr a)))
        (else (car a))))
(define (clause-of a head) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) head))) (cdr a))))
(define (value-of a head) (let ((c (clause-of a head))) (and c (pair? (cdr c)) (cadr c))))
;; a list whose elements are evaluated left to right: R6RS leaves the order
;; of a call's arguments open, and a row that syncs and then reads the
;; drafts must read them after
(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))

(define (path-parent p)
  (let loop ((i (- (string-length p) 1))) (cond ((< i 0) ".") ((char=? (string-ref p i) #\/) (substring p 0 i)) (else (loop (- i 1))))))
(define (write-bytes! path bv)
  (sh "mkdir -p " (quoted (path-parent path)))
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p bv))))
(define (write-file! path text) (write-bytes! path (string->utf8 text)))
(define (file-bytes path) (call-with-port (open-file-input-port path) (lambda (p) (let ((b (get-bytevector-all p))) (if (eof-object? b) (make-bytevector 0) b)))))
(define (file-text path) (utf8->string (file-bytes path)))
(define (string-index s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0)) (cond ((> (+ i m) n) #f) ((string=? (substring s i (+ i m)) sub) i) (else (loop (+ i 1)))))))
;; the one occurrence of FROM in the file replaced by TO
(define (edit! path from to)
  (let* ((s (file-text path)) (i (string-index s from)))
    (unless (and i (not (string-index (substring s (+ i 1) (string-length s)) from)))
      (assertion-violation 'edit-tools "not exactly one occurrence" path from))
    (write-file! path (string-append (substring s 0 i) to (substring s (+ i (string-length from)) (string-length s))))))
(define (checked what a) (unless (eq? 'ok (head-of a)) (assertion-violation 'edit-tools what a)) a)

;; ---- a store and its git base ---------------------------------------------------
;;
;; FILES: ((<path> <text> ...) ...), one block per text. Every file is
;; split from that many definitions, all imported in ONE import-code (a
;; second import of a path already held would add a second holder), and
;; each block then given its text, committed by writer w0. The repository
;; holds the raw export, committed with EXECUTABLES at mode 100755.
;; -> (dir store worktree base)
(define n 0)
(define (store-of! files executables)
  (set! n (+ n 1))
  (let* ((d (string-append root "/s" (number->string n))) (st (string-append d "/store")) (wt (string-append d "/wt")))
    (sh "mkdir -p " (quoted st) " " (quoted (string-append d "/home")) " " (quoted (string-append d "/raw"))
        " " (quoted (string-append d "/src")))
    (putenv "THEOURGIA_HOME" (string-append d "/home"))
    (ask st 'init)
    (for-each (lambda (f)
                (let ((raw (string-append d "/raw/" (car f))) (out (string-append d "/src/" (car f))))
                  (write-file! raw (apply string-append (map (lambda (k) (format "(define d~a ~a)\n" k k)) (iota (length (cdr f))))))
                  (sh "mkdir -p " (quoted (path-parent out)))
                  (checked "split" (ask st 'split-suggest raw "--output" out))))
              files)
    (checked "import" (ask st 'import-code (string-append d "/src")))
    (for-each (lambda (f)
                (let ((kids (children-at st (car f))))
                  (unless (= (length kids) (length (cdr f))) (assertion-violation 'edit-tools "not one block per definition" (car f) kids))
                  (for-each (lambda (k t) (checked "write" (ask st 'write k t "--writer" "w0"))) kids (cdr f))))
              files)
    (checked "commit" (ask st 'commit "--writer" "w0"))
    (checked "export" (ask st 'export-code wt "--raw"))
    (sh "git init -q " (quoted wt) " && git -C " (quoted wt) " config user.name fixture && git -C " (quoted wt)
        " config user.email fixture@example.invalid && git -C " (quoted wt) " config commit.gpgsign false")
    (for-each (lambda (x) (sh "chmod 755 " (quoted (string-append wt "/" x)))) executables)
    (sh "git -C " (quoted wt) " add -A && git -C " (quoted wt) " commit -q -m base")
    (list d st wt (git-head wt))))
(define (out-of cmd)
  (let-values (((to from err pid) (open-process-ports cmd (buffer-mode block) (native-transcoder))))
    (close-port to)
    (let ((s (get-string-all from))) (get-string-all err) (close-port from) (close-port err) (if (eof-object? s) "" s))))
(define (git-head wt)
  (let ((s (out-of (string-append "git -C " (quoted wt) " rev-parse HEAD 2>/dev/null"))))
    (if (> (string-length s) 0) (substring s 0 (- (string-length s) 1)) s)))
(define (dir-of s) (car s)) (define (st-of s) (cadr s)) (define (wt-of s) (caddr s)) (define (base-of s) (cadddr s))
(define (file-at st path)
  (let ((state (open-and-reduce st)))
    (find (lambda (id) (equal? (code-field state id 'path) path)) (code-files state))))
(define (children-at st path) (code-children (open-and-reduce st) (file-at st path)))
(define (block-of st path k) (list-ref (children-at st path) k))

;; ---- the two tools, as children ------------------------------------------------------

(define tools (string-append (path-parent (current-directory)) "/tools/"))
(define (tool! s name out . args)
  (sh "scheme --script " (quoted (string-append tools name)) " "
      (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
      "> " (quoted out) " 2> " (quoted (string-append out ".err")) " < /dev/null")
  (guard (e (#t (list 'UNREADABLE (file-text out)))) (call-with-input-file out read)))
(define runs 0)
(define (next-out s) (set! runs (+ runs 1)) (string-append (dir-of s) "/run" (number->string runs) ".out"))
(define (export! s dir w)
  (tool! s "edit-export.sc" (next-out s) (st-of s) dir "--writer" w "--git" (wt-of s) "--base" (base-of s)))
(define (sync! s dir w . store)
  (tool! s "edit-sync.sc" (next-out s) (if (pair? store) (car store) (st-of s)) dir "--writer" w))
(define (edit-dir s name) (string-append (dir-of s) "/" name))

;; ---- the drafts, read through the store ----------------------------------------------

;; W's drafts: ((<block> . <text>) ...), sorted by block id
(define (drafts-of s w)
  (let* ((a (ask (st-of s) 'drafts "--writer" w))
         (items (filter (lambda (x) (and (pair? x) (eq? (car x) 'draft))) (cdr (cadr a)))))
    (list-sort (lambda (x y) (string<? (car x) (car y)))
               (map (lambda (it) (let ((id (cadr (assq 'block (cdr it)))))
                                   (cons id (draft-text s w id))))
                    items))))
;; the projection of a block for W: (kind version based-on cut text)
(define (projection-of s w id)
  (let ((p (cadr (ask (st-of s) 'read id "--working-info" "--writer" w))))
    (list (cadr p) (list-ref p 4) (list-ref p 5) (list-ref p 6) (list-ref p 7))))
(define (draft-text s w id) (list-ref (projection-of s w id) 4))
(define (sorted-pairs . ps) (list-sort (lambda (x y) (string<? (car x) (car y))) ps))
(define (draft-file s w id) (string-append (writer-directory (st-of s) w) "/working/" id))
;; THE READS BELOW ANSWER RATHER THAN RAISE, so a run where a tool failed
;; (the base has no tools, a mutant may die) reaches every row and reads
;; red there, instead of stopping the fixture between rows
(define (sidecar-of dir)
  (guard (e (#t '(edit-export)))
    (let ((d (call-with-input-file (string-append dir ".edit/SIDECAR.sexp") read))) (if (pair? d) d '(edit-export)))))
(define (sidecar-file side path)
  (or (find (lambda (c) (and (pair? c) (eq? (car c) 'file) (equal? (cadr c) path))) (cdr side)) (list 'file path)))
(define (sidecar-block side path id)
  (find (lambda (c) (and (pair? c) (eq? (car c) 'block) (equal? (cadr c) id))) (cddr (sidecar-file side path))))
(define (pristine-name side path)
  (let ((c (assq 'pristine (cddr (sidecar-file side path))))) (if c (cadr c) "none")))
(define (stat-or-absent path) (guard (e (#t 'absent)) (path-stat path)))

;; ---- the store every row but the last two uses ----------------------------------------
(printf "== the store: a.sc four blocks, b.sc two, x.sc one and executable in git ==\n")
(define s1 (store-of! '(("a.sc" "a0\na1\na2\na3\n" "b0\nb1\nb2\nb3\n" "c0\nc1\nc2\nc3\n" "d0\nd1\nd2\nd3\n")
                        ("b.sc" "p0\np1\n" "q0\nq1\n")
                        ("x.sc" "x0\n"))
                      '("x.sc")))
(define A (lambda (k) (block-of (st-of s1) "a.sc" k)))
(define B (lambda (k) (block-of (st-of s1) "b.sc" k)))
(putenv "THEOURGIA_HOME" (string-append (dir-of s1) "/home"))

;; ---- ET-1..ET-4: the export ----------------------------------------------------------
(printf "== ET-1..ET-4: the export ==\n")
(let* ((dir (edit-dir s1 "e1")) (a (export! s1 dir "we1")))
  (want "ET-1 the export answers ok with three files, seven blocks and no draft"
        (list (head-of a) (clause-of a 'files) (clause-of a 'blocks) (clause-of a 'drafts))
        '(ok (files 3) (blocks 7) (drafts 0)))
  (let ((plain (edit-dir s1 "e1-plain")))
    (checked "plain export" (ask (st-of s1) 'export-code plain "--raw" "--working" "--writer" "we1"))
    (want "ET-2 its files are export-code --working --raw's, byte for byte, and the sidecar's pristine copies the same"
          (map (lambda (p) (list p (bytevector=? (file-bytes (string-append dir "/" p)) (file-bytes (string-append plain "/" p)))))
               '("a.sc" "b.sc" "x.sc"))
          '(("a.sc" #t) ("b.sc" #t) ("x.sc" #t))))
  (let* ((side (sidecar-of dir)) (fa (sidecar-file side "a.sc")) (fx (sidecar-file side "x.sc")))
    (want "ET-3 the sidecar names the store's writer, the git modes from the base, and a.sc's spans as cumulative lengths"
          (list (let ((w (assq 'writer (cdr side)))) (and w (cadr w)))
                (assq 'mode (cddr fa)) (assq 'mode (cddr fx))
                (map (lambda (b) (list (cadr b) (cdr (assq 'span (cddr b)))))
                     (filter (lambda (c) (and (pair? c) (eq? (car c) 'block))) (cddr fa))))
          (list "we1" '(mode "100644") '(mode "100755")
                (map (lambda (k) (list (A k) (list (* 12 k) (* 12 (+ k 1))))) '(0 1 2 3))))))
(want "ET-4 an export into a directory that exists refuses export-exists and writes no sidecar"
      (let* ((dir (edit-dir s1 "e4")))
        (sh "mkdir -p " (quoted dir))
        (in-order (head-of (export! s1 dir "we4")) (file-exists? (string-append dir ".edit/SIDECAR.sexp"))))
      '((error export-exists) #f))

;; ---- ET-5..ET-9: edits that sync ----------------------------------------------------------
(printf "== ET-5..ET-9: edits that sync ==\n")
(let ((dir (edit-dir s1 "e5")))
  (export! s1 dir "we5")
  (edit! (string-append dir "/a.sc") "b1\n" "B1\n")
  (let ((a (sync! s1 dir "we5")))
    (want "ET-5 a one-hunk edit drafts exactly the located block, with the block's new text"
          (in-order (head-of a) (value-of a 'writes) (map cadr (cdr (clause-of a 'written))) (drafts-of s1 "we5"))
          (list 'ok 1 (list (A 1)) (list (cons (A 1) "b0\nB1\nb2\nb3\n"))))))
(let ((dir (edit-dir s1 "e6")))
  (export! s1 dir "we6")
  (edit! (string-append dir "/a.sc") "c1\n" "C1\n")
  (edit! (string-append dir "/a.sc") "c3\n" "C3\n")
  (let ((a (sync! s1 dir "we6")))
    (want "ET-6 two hunks in one block make one write, holding both changes"
          (in-order (head-of a) (value-of a 'writes) (drafts-of s1 "we6"))
          (list 'ok 1 (list (cons (A 2) "c0\nC1\nc2\nC3\n"))))))
(let ((dir (edit-dir s1 "e7")))
  (export! s1 dir "we7")
  (edit! (string-append dir "/a.sc") "c0\n" "new\nc0\n")
  (edit! (string-append dir "/a.sc") "d3\n" "d3\nend\n")
  (let ((a (sync! s1 dir "we7")))
    (want "ET-7 an insertion at a boundary belongs to the block that starts there, an append at the end to the last block"
          (in-order (head-of a) (drafts-of s1 "we7"))
          (list 'ok (sorted-pairs (cons (A 2) "new\nc0\nc1\nc2\nc3\n") (cons (A 3) "d0\nd1\nd2\nd3\nend\n"))))))
(let ((dir (edit-dir s1 "e8")))
  (export! s1 dir "we8")
  (edit! (string-append dir "/a.sc") "a1\n" "A1\n")
  (let* ((first (sync! s1 dir "we8"))
         (before (stat-or-absent (draft-file s1 "we8" (A 0))))
         (second (sync! s1 dir "we8"))
         (after (stat-or-absent (draft-file s1 "we8" (A 0)))))
    (want "ET-8 run twice: the second run succeeds, makes no write call and names the block skipped; the draft file is untouched"
          (list (head-of first) (value-of first 'writes) (head-of second) (value-of second 'writes)
                (cdr (clause-of second 'skipped)) (and (pair? before) (equal? before after)))
          (list 'ok 1 'ok 0 (list (A 0)) #t))))
(let ((dir1 (edit-dir s1 "e9a")) (dir2 (edit-dir s1 "e9b")))
  (export! s1 dir1 "we9")
  (edit! (string-append dir1 "/a.sc") "b2\n" "b2\nmore\nlines\n")
  (sync! s1 dir1 "we9")
  (let* ((a2 (export! s1 dir2 "we9"))
         (side (sidecar-of dir2))
         (bb (sidecar-block side "a.sc" (A 1))))
    (edit! (string-append dir2 "/a.sc") "d1\n" "D1\n")
    (let ((a (sync! s1 dir2 "we9")))
      (want "ET-9 the repeated cycle: a second export records the first draft and the moved spans; an edit of a later block syncs, both drafts correct"
            (in-order (clause-of a2 'drafts) (and bb (cdr (assq 'span (cddr bb)))) (and bb (pair? (assq 'draft (cddr bb))))
                      (head-of a) (value-of a 'writes) (drafts-of s1 "we9"))
            (list '(drafts 1) '(12 35) #t 'ok 1
                  (sorted-pairs (cons (A 1) "b0\nb1\nb2\nmore\nlines\nb3\n") (cons (A 3) "d0\nD1\nd2\nd3\n")))))))

;; ---- ET-10..ET-20: refusals, each writing nothing ---------------------------------------------
(printf "== ET-10..ET-20: refusals, each writing nothing ==\n")
;; -> (head drafts-after): a refusal must leave W with no draft
(define (refused s dir w)
  (let ((a (sync! s dir w))) (in-order (head-of a) (drafts-of s w))))
(let ((dir (edit-dir s1 "e10")))
  (export! s1 dir "we10")
  (edit! (string-append dir "/a.sc") "a3\nb0\n" "A3\nB0\n")
  (want "ET-10 a hunk across a block boundary refuses crossing" (refused s1 dir "we10") '((error crossing) ())))
(let ((dir (edit-dir s1 "e11")))
  (export! s1 dir "we11")
  (write-file! (string-append dir "/new.sc") "n0\n")
  (want "ET-11 an added file refuses added-file" (refused s1 dir "we11") '((error added-file) ())))
(let ((dir (edit-dir s1 "e12")))
  (export! s1 dir "we12")
  (sh "rm " (quoted (string-append dir "/b.sc")))
  (want "ET-12 a removed file refuses removed-file" (refused s1 dir "we12") '((error removed-file) ())))
(let ((dir (edit-dir s1 "e13")))
  (export! s1 dir "we13")
  (edit! (string-append dir "/x.sc") "x0\n" "X0\n")
  (want "ET-13 an edit of a file that is 100755 in git (0644 on disk after the export) refuses file-mode"
        (refused s1 dir "we13") '((error file-mode) ())))
(let ((dir (edit-dir s1 "e14")))
  (export! s1 dir "we14")
  (edit! (string-append dir "/a.sc") "a0\n" "A0\n")
  (sh "chmod 755 " (quoted (string-append dir "/a.sc")))
  (want "ET-14 a changed file made executable on disk refuses file-mode" (refused s1 dir "we14") '((error file-mode) ())))
(let ((dir (edit-dir s1 "e15")))
  (export! s1 dir "we15")
  (write-bytes! (string-append dir "/b.sc") (bytevector 112 48 10 255 10 113 48 10 113 49 10))
  (want "ET-15 text that is not valid UTF-8 refuses invalid-utf8" (refused s1 dir "we15") '((error invalid-utf8) ())))
(let ((dir (edit-dir s1 "e16")))
  (export! s1 dir "we16")
  (edit! (string-append dir "/a.sc") "a0\n" "A0\n")
  (write-bytes! (string-append dir "/b.sc") (bytevector 112 48 10 255 10 113 48 10 113 49 10))
  (want "ET-16 nothing is written when any file refuses: a valid edit of a.sc beside an invalid b.sc leaves no draft"
        (refused s1 dir "we16") '((error invalid-utf8) ())))
(let ((dir (edit-dir s1 "e17")))
  (export! s1 dir "we17")
  (edit! (string-append dir "/a.sc") "a0\n" "A0\n")
  (sh "rm -rf " (quoted (string-append dir ".edit")))
  (want "ET-17 a missing sidecar refuses missing-sidecar" (refused s1 dir "we17") '((error missing-sidecar) ())))
(let ((dir (edit-dir s1 "e18")))
  (export! s1 dir "we18")
  (edit! (string-append dir "/a.sc") "a0\n" "A0\n")
  (want "ET-18 a sidecar of another writer refuses foreign-writer" (refused s1 dir "we18x") '((error foreign-writer) ())))
(let ((dir (edit-dir s1 "e19")))
  (export! s1 dir "we19")
  (edit! (string-append dir "/b.sc") "p1\n" "P1\n")
  (sh "rm -f " (quoted (string-append dir ".edit/pristine/" (pristine-name (sidecar-of dir) "b.sc"))))
  (want "ET-19 a pristine copy that is gone refuses pristine-damaged" (refused s1 dir "we19") '((error pristine-damaged) ())))
(let ((dir (edit-dir s1 "e20")))
  (export! s1 dir "we20")
  (ask (st-of s1) 'write (A 0) "elsewhere\n" "--writer" "we20")
  (edit! (string-append dir "/a.sc") "a1\n" "A1\n")
  (let ((a (sync! s1 dir "we20")))
    (want "ET-20 a draft changed since the export to another version refuses draft-changed, and the other write stays"
          (in-order (head-of a) (drafts-of s1 "we20"))
          (list '(error draft-changed) (list (cons (A 0) "elsewhere\n"))))))

;; ---- ET-21: another store, the same writer name ----------------------------------------------
(printf "== ET-21: a sidecar of another store ==\n")
(define s2 (store-of! '(("a.sc" "a0\na1\na2\na3\n" "b0\nb1\nb2\nb3\n" "c0\nc1\nc2\nc3\n" "d0\nd1\nd2\nd3\n")
                        ("b.sc" "p0\np1\n" "q0\nq1\n")
                        ("x.sc" "x0\n"))
                      '("x.sc")))
(let ((dir (edit-dir s1 "e21")))
  (putenv "THEOURGIA_HOME" (string-append (dir-of s1) "/home"))
  (export! s1 dir "we21")
  (edit! (string-append dir "/a.sc") "a0\n" "A0\n")
  (putenv "THEOURGIA_HOME" (string-append (dir-of s2) "/home"))
  (let ((a (sync! s1 dir "we21" (st-of s2))))
    (want "ET-21 a sidecar of another store, with the same writer name and the same files, refuses foreign-store"
          (in-order (head-of a) (drafts-of s2 "we21"))
          '((error foreign-store) ())))
  (putenv "THEOURGIA_HOME" (string-append (dir-of s1) "/home")))

;; ---- ET-22: a hunk in no block ----------------------------------------------------------
(printf "== ET-22: a file with no block ==\n")
(define s3 (store-of! '(("e.sc" "e0\n") ("f.sc" "f0\n")) '()))
(let ((only (block-of (st-of s3) "e.sc" 0)) (dir (edit-dir s3 "e22")))
  (checked "del" (ask (st-of s3) 'del only))
  (let ((ex (export! s3 dir "we22")))
    (write-file! (string-append dir "/e.sc") "e1\n")
    (want "ET-22 a file whose only block was deleted exports empty; text written into it is in no block and refuses no-block"
          (in-order (head-of ex) (file-bytes (string-append dir "/f.sc")) (refused s3 dir "we22"))
          (list 'ok (string->utf8 "f0\n") '((error no-block) ())))))
(putenv "THEOURGIA_HOME" (string-append (dir-of s1) "/home"))

;; ---- ET-23, ET-24: commits under the export: stale -----------------------------------------------
(printf "== ET-23, ET-24: a committed hash that moved ==\n")
(let ((dir (edit-dir s1 "e23")))
  (export! s1 dir "we23")
  (checked "w9 write" (ask (st-of s1) 'write (B 1) "q0\nQ1\n" "--writer" "w9"))
  (checked "w9 commit" (ask (st-of s1) 'commit "--writer" "w9"))
  (edit! (string-append dir "/b.sc") "q0\n" "QQ\n")
  (want "ET-23 a block whose committed hash moved since the export refuses stale" (refused s1 dir "we23") '((error stale) ())))
(let ((d1 (edit-dir s1 "e24a")) (d2 (edit-dir s1 "e24b")) (d3 (edit-dir s1 "e24c")))
  (export! s1 d1 "we24")
  (edit! (string-append d1 "/b.sc") "p1\n" "P1\n")
  (sync! s1 d1 "we24")
  (checked "w9 write" (ask (st-of s1) 'write (B 0) "p0\np1\nw9\n" "--writer" "w9"))
  (checked "w9 commit" (ask (st-of s1) 'commit "--writer" "w9"))
  ;; the drafts still stand on the old cut: a fresh export projects them there
  (export! s1 d2 "we24")
  (edit! (string-append d2 "/b.sc") "p0\n" "P0\n")
  (let* ((first (sync! s1 d2 "we24"))
         ;; C4': the coder reads the new committed version and moves its draft onto it, merged by hand
         (now (projection-of s1 "nobody" (B 0)))
         (rebase (ask (st-of s1) 'write (B 0) "p0\nP1\nw9\n" "--writer" "we24" "--rebase"
                      "--based-on" (list-ref now 2) "--working-cut" (format "~s" (list-ref now 3))))
         (ex (export! s1 d3 "we24")))
    (edit! (string-append d3 "/b.sc") "p0\n" "P0\n")
    (let ((a (sync! s1 d3 "we24")))
      (want "ET-24 drafts pinned to an older cut: stale, then rebased onto the new cut, exported again and re-applied"
            (in-order (head-of first) (head-of rebase) (head-of ex) (head-of a) (value-of a 'writes)
                      (drafts-of s1 "we24") (list-ref (projection-of s1 "we24" (B 0)) 2))
            (list '(error stale) 'ok 'ok 'ok 1 (list (cons (B 0) "P0\nP1\nw9\n")) (list-ref now 2))))))

(sh "rm -rf " (quoted root))
(printf "rows: ~a\n~a failures\nedit-tools complete\n" rows bad)
(exit (if (zero? bad) 0 1))
