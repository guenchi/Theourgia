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
;; the one occurrence of FROM in the file replaced by TO; a file that is
;; not there (the export failed) is left absent, for the row to read red
(define (edit! path from to)
  (when (file-exists? path) (edit-present! path from to)))
(define (edit-present! path from to)
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
;; AN EXPORT A ROW STANDS ON MUST HAVE HAPPENED: export! stops the fixture on
;; a refusal (a directory reused by an earlier row is one), so no row reads
;; an earlier row's state; export-answer! gives the answer, for the row that
;; asks for the refusal itself
(define (export-answer! s dir w)
  (tool! s "edit-export.sc" (next-out s) (st-of s) dir "--writer" w "--git" (wt-of s) "--base" (base-of s)))
(define (export! s dir w)
  (let ((a (export-answer! s dir w)))
    (unless (eq? 'ok (head-of a)) (assertion-violation 'edit-tools "export refused" dir a))
    a))
;; A PROCEDURE THE BASE MAY LACK, reached by eval so this file loads on it: #f there
(define provisional-id (guard (e (#t #f)) (eval 'provisional-id (environment '(theourgia reduce)))))
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
;; W's structural drafts, as the listing names them: ((new <path>) ...) and
;; ((delete <holder>) ...), sorted; and the version of the one at KEY
(define (structural-of s w)
  (let* ((a (ask (st-of s) 'drafts "--writer" w))
         (items (filter (lambda (x) (and (pair? x) (memq (car x) '(new delete)))) (if (and (pair? a) (pair? (cdr a))) (cdr (cadr a)) '()))))
    (list-sort (lambda (x y) (string<? (cadr x) (cadr y)))
               (map (lambda (it) (list (car it) (cadr (assq (if (eq? (car it) 'new) 'path 'holder) (cdr it))))) items))))
(define (structural-version-of s w key)
  (let* ((a (ask (st-of s) 'drafts "--writer" w))
         (it (find (lambda (x) (and (pair? x) (memq (car x) '(new delete))
                                    (equal? (cadr (assq (if (eq? (car x) 'new) 'path 'holder) (cdr x))) key)))
                   (if (and (pair? a) (pair? (cdr a))) (cdr (cadr a)) '()))))
    (and it (cadr (assq 'version (cdr it))))))
(define (sidecar-new side path)
  (find (lambda (c) (and (pair? c) (eq? (car c) 'new) (equal? (cadr c) path))) (cdr side)))
;; -> (head drafts structural): a refusal leaves W no draft of either kind
(define (refused-all s dir w)
  (let ((a (sync! s dir w))) (in-order (head-of a) (clause-of a 'reason) (drafts-of s w) (structural-of s w))))

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
          (map (lambda (p)
                 (let ((mine (file-bytes (string-append dir "/" p))))
                   (list p (bytevector=? mine (file-bytes (string-append plain "/" p)))
                         (bytevector=? mine (file-bytes (string-append dir ".edit/pristine/" (pristine-name (sidecar-of dir) p)))))))
               '("a.sc" "b.sc" "x.sc"))
          '(("a.sc" #t #t) ("b.sc" #t #t) ("x.sc" #t #t))))
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
        (in-order (head-of (export-answer! s1 dir "we4")) (file-exists? (string-append dir ".edit/SIDECAR.sexp"))))
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
;; ---- SN, SD, SR, SS, SO, SP: files added and removed are structural drafts ----
(printf "== SN..SP: files added and removed, structural drafts ==\n")
(let ((dir (edit-dir s1 "e11")) (dir2 (edit-dir s1 "e11b")))
  (export! s1 dir "we11")
  (write-file! (string-append dir "/new.sc") "n0\n")
  (let* ((a (sync! s1 dir "we11")) (again (sync! s1 dir "we11")))
    (want "SN-1 an added file becomes a working-new draft, and a second sync writes nothing"
          (in-order (head-of a) (value-of a 'files-added) (structural-of s1 "we11") (drafts-of s1 "we11")
                    (value-of again 'writes) (cdr (clause-of again 'skipped)))
          '(ok 1 ((new "new.sc")) () 0 ("new.sc"))))
  (let* ((v1 (structural-version-of s1 "we11" "new.sc")) (ex (export! s1 dir2 "we11")) (entry (sidecar-new (sidecar-of dir2) "new.sc")))
    (want "SN-2 a fresh export writes the new file from its draft, and its sidecar records it with the draft's version"
          (in-order (head-of ex) (clause-of ex 'new-files)
                    (guard (e (#t 'absent)) (file-text (string-append dir2 "/new.sc")))
                    (and entry (equal? (cadr (assq 'version (cddr entry))) v1)))
          '(ok (new-files 1) "n0\n" #t))
    (edit! (string-append dir2 "/new.sc") "n0\n" "n0\nn1\n")
    (let* ((a (sync! s1 dir2 "we11")) (v2 (structural-version-of s1 "we11" "new.sc")) (again (sync! s1 dir2 "we11")))
      (want "SN-3 the round: the exported new file edited and synced writes a new version of the same draft; a second sync writes nothing"
            (in-order (head-of a) (value-of a 'writes) (structural-of s1 "we11") (and v1 v2 (not (equal? v1 v2)))
                      (value-of again 'writes))
            '(ok 1 ((new "new.sc")) #t 0)))
    (want "LIM outline --working does not show the new file (v1: only the file trees built from the working view do)"
          (let ((a (ask (st-of s1) 'outline "--working" "--writer" "we11")))
            (let walk ((x a)) (cond ((equal? x "new.sc") #t) ((pair? x) (or (walk (car x)) (walk (cdr x)))) (else #f))))
          #f)))
(let ((dir (edit-dir s1 "e12")) (dir2 (edit-dir s1 "e12b")))
  (export! s1 dir "we12")
  (sh "rm " (quoted (string-append dir "/b.sc")))
  (let ((a (sync! s1 dir "we12")))
    (want "SD-1 a removed file becomes a working-delete draft of its holder; the next export does not write it"
          (in-order (head-of a) (value-of a 'files-removed) (structural-of s1 "we12")
                    (begin (export! s1 dir2 "we12") (file-exists? (string-append dir2 "/b.sc")))
                    (file-exists? (string-append dir2 "/a.sc")))
          (list 'ok 1 (list (list 'delete (file-at (st-of s1) "b.sc"))) #f #t))))
(let ((dir (edit-dir s1 "e12r")))
  (export! s1 dir "we12r")
  (sh "mv " (quoted (string-append dir "/b.sc")) " " (quoted (string-append dir "/bb.sc")))
  (let ((a (sync! s1 dir "we12r")))
    (want "SR a rename is a delete of the old path's holder and a new file at the new path"
          (in-order (head-of a) (structural-of s1 "we12r"))
          (list 'ok (list-sort (lambda (x y) (string<? (cadr x) (cadr y)))
                               (list (list 'new "bb.sc") (list 'delete (file-at (st-of s1) "b.sc"))))))))
(let ((dir (edit-dir s1 "e12s")))
  (export! s1 dir "we12s")
  (write-file! (string-append dir "/sub/deeper/n.sc") "n\n")
  (want "SS a file in a new subdirectory becomes a new file at its path"
        (let ((a (sync! s1 dir "we12s"))) (in-order (head-of a) (structural-of s1 "we12s")))
        '(ok ((new "sub/deeper/n.sc")))))
(let ((dir (edit-dir s1 "e12o1")))
  (export! s1 dir "we12o1")
  (write-file! (string-append dir "/.hidden.sc") "h\n")
  (want "SO-1 a dot-named added file refuses outside-domain before any write"
        (refused-all s1 dir "we12o1") '((error outside-domain) (reason dot-named) () ())))
(let ((dir (edit-dir s1 "e12o2")))
  (export! s1 dir "we12o2")
  (write-file! (string-append dir "/run.sc") "r\n")
  (sh "chmod 755 " (quoted (string-append dir "/run.sc")))
  (want "SO-2 an executable added file refuses outside-domain before any write"
        (refused-all s1 dir "we12o2") '((error outside-domain) (reason mode) () ())))
(let ((dir (edit-dir s1 "e12o3")))
  (export! s1 dir "we12o3")
  (write-bytes! (string-append dir "/blob.sc") (bytevector 0 1 2 255 0))
  (want "SO-3 an added file that is not text refuses outside-domain before any write"
        (refused-all s1 dir "we12o3") '((error outside-domain) (reason not-text) () ())))
;; A CASE COLLISION IS ASKED OF THE PREDICATE edit-sync asks: the volume these
;; rows run on folds case, so an A.sc written beside a.sc is a.sc, edited
(want "SO-4 a path a case-folding volume would merge with another is outside the store route's domain"
      (let ((domain (guard (e (#t #f)) (eval 'store-route-domain (environment '(theourgia landing-files))))))
        (and domain (list (domain "A.sc" "100644" (string->utf8 "c\n") '("a.sc" "b.sc"))
                          (domain "sub/A.sc" "100644" (string->utf8 "c\n") '("Sub/b.sc"))
                          (domain "c.sc" "100644" (string->utf8 "c\n") '("a.sc" "b.sc")))))
      '(case-collision case-collision in))
(want "SP a draft of a block named by a provisional id refuses provisional-target"
      (let ((a (ask (st-of s1) 'write (string-append "new-" (make-string 56 #\a)) "x\n" "--writer" "we12p")))
        (and (pair? a) (list-head a (min 3 (length a)))))
      '(error bad-request provisional-target))
;; A REMOVED EXECUTABLE IS OUTSIDE THE DOMAIN too, renamed as well as deleted
(let ((dir (edit-dir s1 "e12o5")) (dir2 (edit-dir s1 "e12o5b")))
  (export! s1 dir "we12o5")
  (export! s1 dir2 "we12o5b")
  (sh "rm " (quoted (string-append dir "/x.sc")))
  (sh "mv " (quoted (string-append dir2 "/x.sc")) " " (quoted (string-append dir2 "/xx.sc")) " && chmod 644 " (quoted (string-append dir2 "/xx.sc")))
  (want "SO-5 a removed file whose git mode is 100755 refuses outside-domain before any write, deleted or renamed"
        (list (refused-all s1 dir "we12o5") (refused-all s1 dir2 "we12o5b"))
        '(((error outside-domain) (reason mode) () ()) ((error outside-domain) (reason mode) () ()))))
;; A MODE-ONLY CHANGE: the bytes unchanged, the file made executable on disk,
;; beside an added file that would otherwise be written
(let ((dir (edit-dir s1 "s17")))
  (export! s1 dir "we17")
  (sh "chmod 755 " (quoted (string-append dir "/a.sc")))
  (write-file! (string-append dir "/m.sc") "m\n")
  (want "SM-1 a recorded file made executable with its bytes unchanged refuses file-mode before any write"
        (refused-all s1 dir "we17") '((error file-mode) #f () ())))
;; A CASE-ONLY RENAME: the tree the sync leaves holds A.sc alone
(let ((dir (edit-dir s1 "e12r2")))
  (export! s1 dir "we12r2")
  (sh "mv " (quoted (string-append dir "/a.sc")) " " (quoted (string-append dir "/A.sc")))
  (let ((a (sync! s1 dir "we12r2")))
    (want "SR-2 a.sc renamed A.sc is a delete of a.sc's holder and a new A.sc, not a case collision"
          (in-order (head-of a) (structural-of s1 "we12r2"))
          (list 'ok (list-sort (lambda (x y) (string<? (cadr x) (cadr y)))
                               (list (list 'new "A.sc") (list 'delete (file-at (st-of s1) "a.sc"))))))))
;; A STALE EXPORT: two exports show the new file at v1; one syncs v2; the other
;; can neither overwrite v2 nor discard it
(let ((dir (edit-dir s1 "s14")) (e1 (edit-dir s1 "s14a")) (e2 (edit-dir s1 "s14b")))
  (export! s1 dir "we14")
  (write-file! (string-append dir "/n.sc") "n0
")
  (sync! s1 dir "we14")
  (export! s1 e1 "we14")
  (export! s1 e2 "we14")
  (edit! (string-append e2 "/n.sc") "n0
" "n0
n2
")
  (sync! s1 e2 "we14")
  (let ((v2 (structural-version-of s1 "we14" "n.sc")))
    (edit! (string-append e1 "/n.sc") "n0
" "n0
n3
")
    (let ((edited (sync! s1 e1 "we14")))
      (sh "rm " (quoted (string-append e1 "/n.sc")))
      (let ((removed (sync! s1 e1 "we14")))
        (want "SN-4 a stale export's edit or removal of a new file refuses draft-changed, and the newer version stays"
              (list (head-of edited) (head-of removed) (clause-of removed 'remedy)
                    (and v2 (equal? (structural-version-of s1 "we14" "n.sc") v2)))
              '((error draft-changed) (error draft-changed) (remedy export-again) #t))))))
;; AN UNDONE ADD: the new file synced, then deleted from the same export
(let ((dir (edit-dir s1 "s15")) (dir2 (edit-dir s1 "s15b")))
  (export! s1 dir "we15")
  (write-file! (string-append dir "/u.sc") "u
")
  (sync! s1 dir "we15")
  (sh "rm " (quoted (string-append dir "/u.sc")))
  (let ((a (sync! s1 dir "we15")))
    (want "SN-5 an add undone in the same export refuses draft-changed and keeps the draft; a fresh export's removal discards it"
          (in-order (head-of a) (clause-of a 'key) (structural-of s1 "we15")
                    (begin (export! s1 dir2 "we15")
                           (sh "rm " (quoted (string-append dir2 "/u.sc")))
                           (head-of (sync! s1 dir2 "we15")))
                    (structural-of s1 "we15"))
          (list '(error draft-changed) (list 'key (if provisional-id (provisional-id "u.sc") "none"))
                '((new "u.sc")) 'ok '()))))
;; AN UNDONE DELETE: b.sc removed and synced, then its bytes put back
(let* ((dir (edit-dir s1 "s16")) (h (file-at (st-of s1) "b.sc")))
  (export! s1 dir "we16")
  (let ((bytes (file-bytes (string-append dir "/b.sc"))))
    (sh "rm " (quoted (string-append dir "/b.sc")))
    (sync! s1 dir "we16")
    (write-bytes! (string-append dir "/b.sc") bytes)
    (let ((a (sync! s1 dir "we16")))
      (want "SD-2 a delete undone in the same export refuses draft-changed naming discard delete-<holder>, which clears it"
            (in-order (head-of a) (clause-of a 'remedy) (structural-of s1 "we16")
                      (head-of (ask (st-of s1) 'discard (string-append "delete-" h) "--writer" "we16"))
                      (structural-of s1 "we16"))
            (list '(error draft-changed) (list 'remedy (list 'export-again (list 'discard (string-append "delete-" h))))
                  (list (list 'delete h)) 'ok '())))))
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

;; ---- ET-25..ET-30: a sidecar edited by hand -------------------------------------------------
(printf "== ET-25..ET-30: a sidecar edited by hand ==\n")
;; the sidecar datum rewritten by F, or text appended to it
(define (sidecar-path dir) (string-append dir ".edit/SIDECAR.sexp"))
(define (rewrite-sidecar! dir f) (write-file! (sidecar-path dir) (format "~s\n" (f (sidecar-of dir)))))
(define (with-header-cut cut) (lambda (d) (map (lambda (c) (if (and (pair? c) (eq? (car c) 'cut)) (list 'cut cut) c)) d)))
;; every draft clause's cut replaced, at any depth
(define (with-draft-cuts cut)
  (lambda (d)
    (let walk ((x d))
      (cond ((and (pair? x) (eq? (car x) 'draft))
             (map (lambda (c) (if (and (pair? c) (eq? (car c) 'cut)) (list 'cut cut) c)) x))
            ((list? x) (map walk x))
            (else x)))))
;; -> (head reason drafts-after)
(define (refused-why s dir w)
  (let ((a (sync! s dir w))) (in-order (head-of a) (clause-of a 'reason) (drafts-of s w))))
(let ((dir (edit-dir s1 "e25")))
  (export! s1 dir "we25")
  (edit! (string-append dir "/a.sc") "a1\n" "A1\n")
  (when (file-exists? (sidecar-path dir))
    (write-file! (sidecar-path dir) (string-append (file-text (sidecar-path dir)) "(another datum)\n")))
  (want "ET-25 a second datum after the sidecar's refuses bad-sidecar, trailing-data"
        (refused-why s1 dir "we25") '((error bad-sidecar) (reason trailing-data) ())))
(let ((dir (edit-dir s1 "e26")))
  (export! s1 dir "we26")
  (edit! (string-append dir "/a.sc") "a1\n" "A1\n")
  (edit! (sidecar-path dir) "(span 0 12)" "(span 0 13)")
  (want "ET-26 a span past its block's end, so the spans no longer tile the pristine copy, refuses bad-sidecar, spans"
        (refused-why s1 dir "we26") '((error bad-sidecar) (reason spans) ())))
(let ((dir (edit-dir s1 "e27")))
  (export! s1 dir "we27")
  (edit! (string-append dir "/a.sc") "a1\n" "A1\n")
  (rewrite-sidecar! dir (with-header-cut 7))
  (want "ET-27 a header cut that is not a list of (writer . count) refuses bad-sidecar, header"
        (refused-why s1 dir "we27") '((error bad-sidecar) (reason header) ())))
(let ((dir (edit-dir s1 "e28")))
  (export! s1 dir "we28")
  (edit! (string-append dir "/a.sc") "a1\n" "A1\n")
  (rewrite-sidecar! dir (with-header-cut '(("zzzzzzzz" . 1))))
  (want "ET-28 a well-formed header cut the store cannot serve refuses bad-sidecar, committed-not-at-cut, before any write"
        (refused-why s1 dir "we28") '((error bad-sidecar) (reason committed-not-at-cut) ())))
(let ((d1 (edit-dir s1 "e29a")) (d2 (edit-dir s1 "e29b")))
  (export! s1 d1 "we29")
  (edit! (string-append d1 "/a.sc") "a1\n" "A1\n")
  (sync! s1 d1 "we29")
  (export! s1 d2 "we29")
  (edit! (string-append d2 "/a.sc") "a2\n" "A2\n")
  (rewrite-sidecar! d2 (with-draft-cuts '(("zzzzzzzz" . 1))))
  (want "ET-29 a drafted block whose recorded cut no longer gives its recorded version refuses bad-sidecar, draft-inconsistent; the draft stays"
        (refused-why s1 d2 "we29")
        (list '(error bad-sidecar) '(reason draft-inconsistent) (list (cons (A 0) "a0\nA1\na2\na3\n")))))

;; a.sc's second block entry given the first block's id, spans and bytes unchanged
(define (with-second-id-as-first d)
  (map (lambda (c)
         (if (and (pair? c) (eq? (car c) 'file) (equal? (cadr c) "a.sc"))
             (let* ((blocks (filter (lambda (x) (and (pair? x) (eq? (car x) 'block))) c))
                    (first-id (and (pair? blocks) (cadr (car blocks)))))
               (let loop ((xs c) (k 0) (out '()))
                 (cond ((null? xs) (reverse out))
                       ((and (pair? (car xs)) (eq? (car (car xs)) 'block))
                        (loop (cdr xs) (+ k 1) (cons (if (= k 1) (cons 'block (cons first-id (cddr (car xs)))) (car xs)) out)))
                       (else (loop (cdr xs) k (cons (car xs) out))))))
             c))
       d))
(let ((dir (edit-dir s1 "e30")))
  (export! s1 dir "we30")
  (edit! (string-append dir "/a.sc") "a1\n" "A1\n")
  (rewrite-sidecar! dir with-second-id-as-first)
  (want "ET-30 two block entries with one id refuse bad-sidecar, duplicate-block, before any slice or write"
        (refused-why s1 dir "we30") '((error bad-sidecar) (reason duplicate-block) ())))

;; ---- TR: a freeze given the edit directory that was tested -------------------------
;; A refused sync writes nothing, so the writer's drafts stay as they were and
;; a freeze holds those; an edit after the last sync was never synced.
;; landing-freeze --tree <dir> refuses a frozen tree other than the directory.
(define (freeze! s fdir w . extra) (apply tool! s "landing-freeze.sc" (next-out s) (st-of s) "--writer" w fdir extra))
(define (check! s fdir w . extra) (apply tool! s "landing-freeze.sc" (next-out s) (st-of s) "--writer" w "--check" fdir extra))
(define (tree-clause a name) (let ((c (clause-of a name))) (and c (cadr c))))
(define s9 (store-of! '(("a.sc" "a1\n" "a2\n") ("b.sc" "b1\n")) '()))
;; a first round synced: W holds a draft of b.sc
(define tr-d1 (edit-dir s9 "tr-d1"))
(export! s9 tr-d1 "wtr")
(edit! (string-append tr-d1 "/b.sc") "b1\n" "B1\n")
(checked "first sync" (sync! s9 tr-d1 "wtr"))
;; the second round's sync is refused: another writer committed a.sc's block
(define tr-d2 (edit-dir s9 "tr-d2"))
(export! s9 tr-d2 "wtr")
(edit! (string-append tr-d2 "/a.sc") "a1\n" "A1\n")
(checked "other write" (ask (st-of s9) 'write (block-of (st-of s9) "a.sc" 0) "a1 other\n" "--writer" "w0"))
(checked "other commit" (ask (st-of s9) 'commit "--writer" "w0"))
(define tr-refused (sync! s9 tr-d2 "wtr"))
(let ((a (freeze! s9 (edit-dir s9 "tr-f1") "wtr" "--tree" tr-d2)))
  (want "TR-1 a refused sync, then a freeze given the tested directory: refused tree-differs naming the file the drafts lack, the freeze directory left"
        (list (head-of tr-refused) (head-of a) (tree-clause a 'differs) (tree-clause a 'only-in-freeze) (tree-clause a 'only-in-tree)
              (file-exists? (edit-dir s9 "tr-f1")))
        '((error stale) (error tree-differs) ("a.sc") () () #t)))
(want "TR-1 CONTROL the same freeze without --tree succeeds and freezes the older drafts"
      (let ((a (freeze! s9 (edit-dir s9 "tr-f1b") "wtr")))
        (list (head-of a) (file-text (string-append (edit-dir s9 "tr-f1b") "/tree/a.sc"))))
      '(ok "a1 other\na2\n"))
;; exported again and the edit applied again: the sync goes through
(define tr-d3 (edit-dir s9 "tr-d3"))
(export! s9 tr-d3 "wtr")
(edit! (string-append tr-d3 "/a.sc") "a1 other\n" "A1\n")
(checked "third sync" (sync! s9 tr-d3 "wtr"))
(define tr-f2 (edit-dir s9 "tr-f2"))
(want "TR-2 after a sync that went through, the freeze given that directory is ok"
      (head-of (freeze! s9 tr-f2 "wtr" "--tree" tr-d3))
      'ok)
(want "TR-5 --check given the directory on that freeze: ok"
      (head-of (check! s9 tr-f2 "wtr" "--tree" tr-d3))
      'ok)
;; an edit after the sync, never synced
(edit! (string-append tr-d3 "/b.sc") "B1\n" "B2\n")
(let ((a (freeze! s9 (edit-dir s9 "tr-f3") "wtr" "--tree" tr-d3)))
  (want "TR-3 an edit made after the last sync: the freeze given the directory is refused tree-differs naming it"
        (list (head-of a) (tree-clause a 'differs))
        '((error tree-differs) ("b.sc"))))
(let ((a (check! s9 tr-f2 "wtr" "--tree" tr-d3)))
  (want "TR-5 --check given the directory after that edit: tree-differs naming the file, and nothing written"
        (list (head-of a) (tree-clause a 'differs))
        '((error tree-differs) ("b.sc"))))
;; another writer's export
(define tr-other (edit-dir s9 "tr-other"))
(export! s9 tr-other "wother")
(let ((a (freeze! s9 (edit-dir s9 "tr-f4") "wtr" "--tree" tr-other)))
  (want "TR-4 --tree naming another writer's export: refused tree-foreign, naming the sidecar's writer, nothing written"
        (list (head-of a) (tree-clause a 'writer) (file-exists? (edit-dir s9 "tr-f4")))
        '((error tree-foreign) "wother" #f)))
;; a fresh export, unedited: the same tree as the drafts
(define tr-d5 (edit-dir s9 "tr-d5"))
(export! s9 tr-d5 "wtr")
(define tr-f5 (edit-dir s9 "tr-f5"))
(want "TR-6 --tree given with a trailing slash: the freeze and --check are both ok (the sidecar is <dir>.edit beside it)"
      (list (head-of (freeze! s9 tr-f5 "wtr" "--tree" (string-append tr-d5 "/")))
            (head-of (check! s9 tr-f5 "wtr" "--tree" (string-append tr-d5 "//"))))
      '(ok ok))
;; A BARE --tree IS USAGE, before anything runs: here a commit on another
;; block leaves W's drafts off the applied cut, so a freeze would move them
(checked "other write 2" (ask (st-of s9) 'write (block-of (st-of s9) "a.sc" 1) "a2 other\n" "--writer" "w0"))
(checked "other commit 2" (ask (st-of s9) 'commit "--writer" "w0"))
(let* ((before (ask (st-of s9) 'drafts "--writer" "wtr"))
       (a (tool! s9 "landing-freeze.sc" (next-out s9) (st-of s9) "--writer" "wtr" "--tree"))
       (b (tool! s9 "landing-freeze.sc" (next-out s9) (st-of s9) "--writer" "wtr" (edit-dir s9 "tr-f7") "--tree"))
       (after (ask (st-of s9) 'drafts "--writer" "wtr")))
  (want "TR-7 a --tree without its directory, last or in the directory's place: usage, no directory made, and the drafts a freeze would move untouched"
        (list (head-of a) (head-of b) (file-exists? "--tree") (file-exists? (edit-dir s9 "tr-f7")) (equal? before after))
        '(usage usage #f #f #t)))
;; A --tree SPELLED WITH . OR .. names the directory: it is folded to one
;; name as edit-export and edit-sync fold theirs (landing-files.sc,
;; edit-dir-path). "." is given from inside the directory.
(define (tool-in! s dir name out . args)
  (sh "cd " (quoted dir) " && scheme --script " (quoted (string-append tools name)) " "
      (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
      "> " (quoted out) " 2> " (quoted (string-append out ".err")) " < /dev/null")
  (guard (e (#t (list 'UNREADABLE (file-text out)))) (call-with-input-file out read)))
;; W's drafts are behind TR-7's commit, and an export projects them at
;; their own cut, so a freeze (which moves them onto the applied cut first)
;; would hold another writer's change the export does not: a plain freeze
;; moves them, then a fresh export is the tree a freeze writes
(checked "move the drafts" (freeze! s9 (edit-dir s9 "tr-f8-move") "wtr"))
(define tr-d8 (edit-dir s9 "tr-d8"))
(export! s9 tr-d8 "wtr")
(let* ((a (tool-in! s9 tr-d8 "landing-freeze.sc" (next-out s9) (st-of s9) "--writer" "wtr" (edit-dir s9 "tr-f8a") "--tree" "."))
       (b (freeze! s9 (edit-dir s9 "tr-f8b") "wtr" "--tree" (string-append tr-d8 "/sub/..")))
       (c (check! s9 (edit-dir s9 "tr-f8a") "wtr" "--tree" (string-append tr-d8 "/."))))
  (want "TR-8 --tree spelled . (from inside), dir/sub/.. and dir/. (freeze, freeze, --check): each names the directory and is ok"
        (list (head-of a) (head-of b) (head-of c))
        '(ok ok ok)))

;; ---- FD: one name for an edit directory, in all three tools ----------------------
;; edit-export, edit-sync and landing-freeze fold <dir> lexically (absolute,
;; "." and ".." folded, links not resolved), so every spelling of a
;; directory finds the sidecar <dir>.edit beside it.
(define s10 (store-of! '(("a.sc" "a1\n" "a2\n")) '()))
(define fd-base (dir-of s10))
(define fd-new (string-append fd-base "/new"))
(define (fd-export! dir w)
  (tool! s10 "edit-export.sc" (next-out s10) (st-of s10) dir "--writer" w "--git" (wt-of s10) "--base" (base-of s10)))
(let* ((a (fd-export! (string-append fd-base "/new/sub/..") "wfd"))
       (b (fd-export! (string-append fd-base "/new2/.") "wfd2")))
  (want "FD-1 edit-export to new/sub/.. and to new2/.: the directories new and new2, each sidecar beside it, none inside and no sub made"
        (list (head-of a) (head-of b)
              (file-exists? (string-append fd-base "/new.edit/SIDECAR.sexp")) (file-exists? (string-append fd-base "/new2.edit/SIDECAR.sexp"))
              (file-exists? (string-append fd-base "/new/..edit")) (file-exists? (string-append fd-base "/new2/..edit"))
              (file-exists? (string-append fd-base "/new/sub")) (file-exists? (string-append fd-base "/new/sub/..edit")))
        '(ok ok #t #t #f #f #f #f)))
(edit! (string-append fd-new "/a.sc") "a1\n" "A1\n")
(let* ((a (tool-in! s10 fd-new "edit-sync.sc" (next-out s10) (st-of s10) "." "--writer" "wfd"))
       (b (tool-in! s10 fd-new "edit-sync.sc" (next-out s10) (st-of s10) "./" "--writer" "wfd"))
       (c (sync! s10 (string-append fd-new "/.") "wfd"))
       (d (sync! s10 (string-append fd-new "/x/..") "wfd")))
  (want "FD-2 edit-sync of new spelled . and ./ (from inside), new/. and new/x/..: each finds new.edit; the first writes the edit, the rest write nothing"
        (list (head-of a) (clause-of a 'writes) (head-of b) (clause-of b 'writes) (head-of c) (head-of d))
        '(ok (writes 1) ok (writes 0) ok ok)))
(let* ((a (tool-in! s10 fd-new "landing-freeze.sc" (next-out s10) (st-of s10) "--writer" "wfd" (string-append fd-base "/fd-f1") "--tree" "."))
       (b (tool! s10 "landing-freeze.sc" (next-out s10) (st-of s10) "--writer" "wfd" "--check" (string-append fd-base "/fd-f1") "--tree" (string-append fd-new "/.")))
       (c (tool! s10 "landing-freeze.sc" (next-out s10) (st-of s10) "--writer" "wfd" "--check" (string-append fd-base "/fd-f1") "--tree" (string-append fd-new "/x/.."))))
  (want "FD-3 landing-freeze --tree . (from inside), and --check with new/. and new/x/..: all ok"
        (list (head-of a) (head-of b) (head-of c))
        '(ok ok ok)))
;; LINKS ARE NOT RESOLVED: a link to the edit directory has a name of its own,
;; and its sidecar would be beside it; a directory reached through a link to
;; its parent keeps that spelling, and the sidecar written there is found by it
(sh "ln -s " (quoted fd-new) " " (quoted (string-append fd-base "/lnk"))
    " && mkdir -p " (quoted (string-append fd-base "/realp")) " && ln -s " (quoted (string-append fd-base "/realp"))
    " " (quoted (string-append fd-base "/viap")))
(let* ((a (sync! s10 (string-append fd-base "/lnk") "wfd"))
       (b (fd-export! (string-append fd-base "/viap/e") "wfd3"))
       (c (sync! s10 (string-append fd-base "/viap/e") "wfd3")))
  (want "FD-4 a link to new is not new: edit-sync of lnk names lnk.edit as missing; an export through a link to its parent is synced through the same spelling"
        (list (head-of a) (clause-of a 'path) (head-of b) (head-of c))
        (list '(error missing-sidecar) (list 'path (string-append fd-base "/lnk.edit/SIDECAR.sexp")) 'ok 'ok)))
;; ".." AFTER A LINK IS LOGICAL: lnk2 points at realp/inner, so lnk2/../new is
;; new beside lnk2 lexically, and realp/new physically (which does not exist)
(sh "mkdir -p " (quoted (string-append fd-base "/realp/inner")) " && ln -s " (quoted (string-append fd-base "/realp/inner"))
    " " (quoted (string-append fd-base "/lnk2")))
(want "FD-5 edit-sync of lnk2/../new folds lexically to new beside lnk2 (a physical fold would look under realp)"
      (head-of (sync! s10 (string-append fd-base "/lnk2/../new") "wfd"))
      'ok)

(sh "rm -rf " (quoted root))
(printf "rows: ~a\n~a failures\nedit-tools complete\n" rows bad)
(exit (if (zero? bad) 0 1))
