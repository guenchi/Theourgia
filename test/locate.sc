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

;; locate <path> (--writer W | --cut C) (--bytes s:e | --lines a:b | --hunk):
;; the blocks a range of a text file's raw bytes touches, each whole with its
;; span. Files are built with known block texts (a split import, each block
;; then written), and every answer is compared with spans the fixture adds up
;; from those texts. LC-5 sets locate's empty-range rule beside
;; projection-range's opposite one on the same pieces; LC-8 TRIAL is the trial's
;; own edit: test/client-program.sc at 20b4878 imported split, and the
;; changed-line hunks of its next commit's diff (locate-data/).

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia store) open-and-reduce)
        (only (theourgia code-project) code-files code-field code-children)
        (only (theourgia derived) projection-range)
        (only (theourgia reduce) block-id)
        (only (theourgia arguments) parse-arguments argument-stdin))

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
                 "/locate-" (number->string (get-process-id))))
(when (file-exists? root) (assertion-violation 'locate "scratch directory already exists" root))
(define (sh . xs) (system (apply string-append xs)))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(sh "mkdir -p " (quoted root))

(define (ask st . req) (rpc-dispatch st req "test"))
(define (head-of a)
  (cond ((not (pair? a)) a)
        ((and (eq? (car a) 'error) (pair? (cdr a)) (pair? (cddr a)) (pair? (caddr a)) (eq? (car (caddr a)) 'reason))
         (list 'error (cadr a) (caddr a)))
        ((eq? (car a) 'error) (list 'error (cadr a)))
        (else (car a))))
(define (items-of a) (if (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a)) (pair? (cadr a)) (eq? (car (cadr a)) 'items))
                         (cdr (cadr a))
                         (list 'NOT-ITEMS a)))
(define (clause-of a head) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) head))) (cdr a))))


(define (path-parent p)
  (let loop ((i (- (string-length p) 1))) (cond ((< i 0) ".") ((char=? (string-ref p i) #\/) (substring p 0 i)) (else (loop (- i 1))))))
(define (write-file! path text)
  (sh "mkdir -p " (quoted (path-parent path)))
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p (string->utf8 text)))))
(define (file-text path) (call-with-port (open-file-input-port path) (lambda (p) (let ((b (get-bytevector-all p))) (if (eof-object? b) "" (utf8->string b))))))
(define n 0)
;; -> (store . home); one fresh store per call
(define (fresh-store!)
  (set! n (+ n 1))
  (let* ((d (string-append root "/s" (number->string n))) (st (string-append d "/store")))
    (sh "mkdir -p " (quoted st) " " (quoted (string-append d "/home")) " " (quoted (string-append d "/raw"))
        " " (quoted (string-append d "/src")))
    (putenv "THEOURGIA_HOME" (string-append d "/home"))
    (ask st 'init)
    d))
(define (store-in d) (string-append d "/store"))
(define (file-at st path)
  (let ((state (open-and-reduce st)))
    (find (lambda (id) (equal? (code-field state id 'path) path)) (code-files state))))
(define (children-at st path) (code-children (open-and-reduce st) (file-at st path)))
(define (srcs-at st path)
  (let ((state (open-and-reduce st))) (map (lambda (k) (utf8->string (code-field state k 'src))) (children-at st path))))
;; TEXT imported split at its definitions: split-suggest's review copy of it,
;; imported in its place. -> the store
(define (split-import! d path text)
  (let ((st (store-in d)))
    (write-file! (string-append d "/raw/" path) text)
    (sh "mkdir -p " (quoted (path-parent (string-append d "/src/" path))))
    (ask st 'split-suggest (string-append d "/raw/" path) "--output" (string-append d "/src/" path))
    (unless (eq? 'ok (head-of (ask st 'import-code (string-append d "/src"))))
      (assertion-violation 'locate "split import failed" path))
    st))
;; A file PATH whose blocks hold exactly TEXTS, in order: one definition per
;; block imported split, then each block written and committed. -> the store
(define (blocks-store! path texts)
  (let* ((d (fresh-store!))
         (st (split-import! d path (apply string-append
                                          (map (lambda (k) (format "(define d~a ~a)\n" k k)) (iota (length texts))))))
         (kids (children-at st path)))
    (unless (= (length kids) (length texts)) (assertion-violation 'locate "not one block per definition" kids))
    (for-each (lambda (k t) (ask st 'write k t "--writer" "w1")) kids texts)
    (let ((c (ask st 'commit "--writer" "w1")))
      (unless (eq? 'ok (head-of c)) (assertion-violation 'locate "commit refused" c)))
    st))
;; the fixture's own spans: ((block <id> <start> <end>) ...) from the texts
(define (spans st path)
  (let loop ((ks (children-at st path)) (ts (srcs-at st path)) (at 0) (out '()))
    (if (null? ks) (reverse out)
        (let ((end (+ at (bytevector-length (string->utf8 (car ts))))))
          (loop (cdr ks) (cdr ts) end (cons (list 'block (car ks) at end) out))))))
(define (loc st path . range) (apply ask st 'locate path "--cut" "t0" range))
(define (pick sp . ks) (map (lambda (k) (list-ref sp k)) ks))

;; ---- A | B, the boundary at line 4 -----------------------------------------
(printf "== LC-1..LC-5, LC-8: two blocks, three lines each ==\n")
(define ab (blocks-store! "m.sc" '("l1\nl2\nl3\n" "l4\nl5\nl6\n")))
(ask ab 'tag "t0")
(define ab-spans (spans ab "m.sc"))
(want "LC SETUP the file is two blocks, 0-9 and 9-18, with the texts written"
      (list (srcs-at ab "m.sc") (map cddr ab-spans))
      '(("l1\nl2\nl3\n" "l4\nl5\nl6\n") ((0 9) (9 18))))
(want "LC-1 a range inside one block answers that block whole, with its own span"
      (items-of (loc ab "m.sc" "--bytes" "1:3"))
      (pick ab-spans 0))
(want "LC-2 a range crossing the boundary answers both blocks, in order"
      (items-of (loc ab "m.sc" "--bytes" "7:11"))
      ab-spans)
(want "LC-3 a hunk whose old-side lines 3 and 4 straddle the boundary answers both blocks"
      (items-of (loc ab "m.sc" "--hunk" "@@ -3,2 +3,2 @@\n-l3\n-l4\n+x\n+y\n"))
      ab-spans)
(want "LC-4 --cut against the start tag equals --writer of a writer with no drafts, when nothing changed"
      (let ((at-cut (items-of (loc ab "m.sc" "--bytes" "7:11")))
            (working (items-of (ask ab 'locate "m.sc" "--writer" "w2" "--bytes" "7:11"))))
        (list (equal? at-cut working) working))
      (list #t ab-spans))
(want "LC-5 an empty range at the boundary answers the block that starts there, where projection-range on the same pieces answers the one that ends there"
      (list (items-of (loc ab "m.sc" "--bytes" "9:9"))
            (projection-range (map (lambda (s) (list 'source (cadr s) (caddr s) (cadddr s) 0 '())) ab-spans) 9 9))
      (list (pick ab-spans 1) (list 'mapped (cadr (car ab-spans)) 9 9)))
(want "LC-8 --lines 1:2 is the bytes from line 1's start to line 2's LF, and answers the first block"
      (list (items-of (loc ab "m.sc" "--lines" "1:2")) (equal? (loc ab "m.sc" "--lines" "1:2") (loc ab "m.sc" "--bytes" "0:6")))
      (list (pick ab-spans 0) #t))
(want "LC-8 a -3,0 hunk, an insertion after line 3, answers the block starting at line 4"
      (items-of (loc ab "m.sc" "--hunk" "@@ -3,0 +4 @@\n+x\n"))
      (pick ab-spans 1))
(want "LC-8 a -5,0 hunk inside the second block answers that block"
      (items-of (loc ab "m.sc" "--hunk" "@@ -5,0 +6 @@\n+x\n"))
      (pick ab-spans 1))
(want "LC-8 a hunk @@ -5 +4,0 @@ (no count: one line) answers the block holding line 5"
      (items-of (loc ab "m.sc" "--hunk" "@@ -5 +4,0 @@\n-l5\n"))
      (pick ab-spans 1))
(want "LC-8 a -6,0 hunk at the last line is an append, and answers the last block, which ends there"
      (items-of (loc ab "m.sc" "--hunk" "@@ -6,0 +7 @@\n+x\n"))
      (pick ab-spans 1))

;; ---- LC-9 a writer with no drafts reads the head ------------------------------
(let ((st (blocks-store! "n.sc" '("a\n" "b\n"))))
  (ask st 'tag "t0")
  (ask st 'write (cadr (children-at st "n.sc")) "bb\n" "--writer" "w1")
  (ask st 'commit "--writer" "w1")
  (ask st 'tag "t1")
  (want "LC-9 --writer of a writer with no drafts equals the head, the cut after a later commit, not the start tag"
        (let ((working (items-of (ask st 'locate "n.sc" "--writer" "w2" "--bytes" "0:4"))))
          (list (map cddr working)
                (equal? working (items-of (ask st 'locate "n.sc" "--cut" "t1" "--bytes" "0:4")))
                (equal? working (items-of (ask st 'locate "n.sc" "--cut" "t0" "--bytes" "0:4")))))
        '(((0 2) (2 5)) #t #f)))

;; ---- LC-6 a file with no children ------------------------------------------
(printf "== LC-6, LC-7: no children, a zero-length child ==\n")
(let* ((d (fresh-store!)) (st (store-in d)))
  (write-file! (string-append d "/src/e.txt") "x\n")
  (ask st 'import-code (string-append d "/src"))
  (for-each (lambda (k) (ask st 'del k)) (children-at st "e.txt"))
  (ask st 'tag "t0")
  (want "LC-6 a childless file answers no item for 0:0 and range-outside for 1:1"
        (list (length (children-at st "e.txt")) (items-of (loc st "e.txt" "--bytes" "0:0"))
              (loc st "e.txt" "--bytes" "1:1"))
        '(0 () (error range-outside (bytes 0) (lines 0)))))

;; ---- LC-7 a zero-length child at the boundary --------------------------------
(let* ((st (blocks-store! "z.sc" '("l1\nl2\nl3\n" "" "l4\nl5\nl6\n"))) (sp (begin (ask st 'tag "t0") (spans st "z.sc"))))
  (want "LC-7 SETUP the middle block is empty, at 9"
        (map cddr sp)
        '((0 9) (9 9) (9 18)))
  (want "LC-7 an empty range at the boundary answers the zero-length block and the block after it, in order"
        (items-of (loc st "z.sc" "--bytes" "9:9"))
        (pick sp 1 2))
  (want "LC-7 a range across the boundary answers the zero-length block strictly inside it"
        (items-of (loc st "z.sc" "--bytes" "7:11"))
        sp))

;; ---- refusals ----------------------------------------------------------------
(printf "== refusals, the view, the receipt ==\n")
(want "LC a path no file holds is unknown-path; a range outside is refused before any block rule"
      (list (loc ab "nowhere.sc" "--bytes" "0:0") (loc ab "m.sc" "--bytes" "5:19")
            (loc ab "m.sc" "--lines" "3:2") (loc ab "m.sc" "--lines" "6:7") (head-of (loc ab "m.sc" "--bytes" "5:4")))
      '((error unknown-path "nowhere.sc") (error range-outside (bytes 18) (lines 6))
        (error range-outside (bytes 18) (lines 6)) (error range-outside (bytes 18) (lines 6))
        (error range-outside)))
(want "LC a hunk with text before its header is not-a-hunk; a second header is one-hunk-only"
      (list (loc ab "m.sc" "--hunk" "diff --git a/m.sc b/m.sc\n@@ -1 +1 @@\n")
            (loc ab "m.sc" "--hunk" "@@ -1 +1 @@\n-l1\n+x\n@@ -4 +4 @@\n-l4\n+y\n")
            (loc ab "m.sc" "--hunk" "@@ -1 @@\n"))
      '((error not-a-hunk) (error one-hunk-only) (error not-a-hunk)))
;; a block made a file with a path and no mode: a file block, not text (a
;; text file's mode cannot change: set refuses mode-mismatch)
(let* ((d (fresh-store!)) (st (store-in d))
       (a (ask st 'insert "--title" "f"))
       (f (let ((ev (car (cadr (assq 'events (cdr a)))))) (block-id (car ev) (cdr ev)))))
  (ask st 'set f "kind" "file")
  (ask st 'set f "path" "f.txt")
  (ask st 'tag "t0")
  (want "LC a path held by a file that is not text mode is not-a-text-file"
        (list (and (file-at st "f.txt") #t) (loc st "f.txt" "--bytes" "0:0"))
        '(#t (error not-a-text-file "f.txt"))))
(want "LC exactly one view: --writer and --cut together are refused, neither is the usage form"
      (list (ask ab 'locate "m.sc" "--writer" "w2" "--cut" "t0" "--bytes" "0:1")
            (head-of (ask ab 'locate "m.sc" "--bytes" "0:1"))
            (head-of (ask ab 'locate "m.sc" "--cut" "t0" "--bytes" "0:1" "--lines" "1:1"))
            (head-of (ask ab 'locate "m.sc" "--cut" "t0" "--bytes" "1")))
      '((error bad-request incompatible-cut-options) usage usage usage))
(want "LC --hunk - takes the hunk from standard input, as insert's --text - does"
      (argument-stdin 'locate (parse-arguments 'locate '("m.sc" "--cut" "t0" "--hunk" "-")) (lambda () "@@ -1 +1 @@\n"))
      (parse-arguments 'locate '("m.sc" "--cut" "t0" "--hunk" "@@ -1 +1 @@\n")))
(want "LC the answer is an items answer with the cut it served and no versions; --writer adds (working #t)"
      (let ((at-cut (loc ab "m.sc" "--bytes" "0:1")) (working (ask ab 'locate "m.sc" "--writer" "w2" "--bytes" "0:1")))
        (list (car at-cut) (and (clause-of at-cut 'cut) #t) (clause-of at-cut 'versions)
              (clause-of working 'working) (clause-of working 'versions)))
      '(ok #t #f (working #t) #f))

;; ---- LC-8 TRIAL the trial's own edit -----------------------------------------------
(printf "== LC-8 TRIAL: test/client-program.sc at 20b4878 and its next commit's hunks ==\n")
(define base-text (file-text "locate-data/client-program-20b4878.txt"))
(define (hunks diff)
  ;; each "@@" line with the lines under it, up to the next
  (let loop ((ls (let split ((s diff) (out '()))
                   (let ((i (let find ((j 0)) (cond ((= j (string-length s)) #f) ((char=? (string-ref s j) #\newline) j) (else (find (+ j 1)))))))
                     (if i (split (substring s (+ i 1) (string-length s)) (cons (substring s 0 (+ i 1)) out)) (reverse out)))))
             (current #f) (out '()))
    (cond ((null? ls) (reverse (if current (cons current out) out)))
          ((and (>= (string-length (car ls)) 2) (string=? "@@" (substring (car ls) 0 2)))
           (loop (cdr ls) (car ls) (if current (cons current out) out)))
          (current (loop (cdr ls) (string-append current (car ls)) out))
          (else (loop (cdr ls) current out)))))
(let* ((d (fresh-store!)) (st (split-import! d "test/client-program.sc" base-text)))
  (ask st 'tag "t0")
  (let* ((kids (children-at st "test/client-program.sc")) (srcs (srcs-at st "test/client-program.sc"))
         (starting (lambda (prefix)
                     (let loop ((ks kids) (ss srcs))
                       (cond ((null? ks) #f)
                             ((and (>= (string-length (car ss)) (string-length prefix))
                                   (string=? prefix (substring (car ss) 0 (string-length prefix))))
                              (car ks))
                             (else (loop (cdr ks) (cdr ss)))))))
         (daemons-alive (starting "(define (daemons-alive)"))
         (kill-daemons (starting ";; NEVER: EVERY DAEMON THIS FILE CAUSED TO EXIST"))
         (answered (lambda (diff)
                     (map (lambda (h) (map cadr (items-of (loc st "test/client-program.sc" "--hunk" h))))
                          (hunks (file-text diff))))))
    (want "LC-8 TRIAL SETUP the split import is the trial's: 63 blocks, run together equal to the file, the two definitions found"
          (list (length kids) (string=? (apply string-append srcs) base-text) (and daemons-alive kill-daemons #t))
          '(63 #t #t))
    (want "LC-8 TRIAL the five changed-line hunks (-U0) answer daemons-alive's block and kill-daemons!'s, and nothing else"
          (let ((per (answered "locate-data/client-program-46f14c8-U0.diff")))
            (list (length per) (fold-left (lambda (acc l) (fold-left (lambda (a x) (if (member x a) a (append a (list x)))) acc l)) '() per)))
          (list 5 (list daemons-alive kill-daemons)))
    (want "LC-8 TRIAL CONTROL: with three lines of context the first hunk also answers the two blocks before, out-of's and err-of's"
          (let ((first (car (answered "locate-data/client-program-46f14c8-U3.diff"))))
            (list (length first) (list-tail first 2)))
          (list 4 (list daemons-alive kill-daemons)))))

(sh "rm -rf " (quoted root))
(printf "rows: ~a\n~a failures\nlocate complete\n" rows bad)
(exit (if (zero? bad) 0 1))
