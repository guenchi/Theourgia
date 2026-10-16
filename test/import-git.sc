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

;; tools/import-git.sc: a git range absorbed into the store as committed
;; blocks and a tag. Each group builds a small git repository in the
;; scratch directory and seeds a store from its first commit with
;; split-suggest, so a file has one block per definition (an unmarked
;; import makes one block per file); a definition is one line, so a block's
;; text names it. The script runs as a child and the fixture reads the datum
;; it prints, then the store. A held run (THEOURGIA_HOLD at
;; import-before-batch) lets the fixture write between the script's
;; premises and its batch; THEOURGIA_IMPORT_STOP makes the prefix an
;; interrupted run leaves.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia store) open-and-reduce store-tags)
        (only (theourgia reduce) state-read block-hash)
        (only (theourgia code-project) code-files code-field code-children)
        (only (theourgia code-suggest) suggest-boundaries)
        (only (theourgia languages) language-for-path)
        (only (theourgia markers) byte-slice)
        (only (theourgia query) make-query-session session-answer))

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
;; a list whose elements are evaluated left to right: R6RS leaves the order
;; of a call's arguments open, and a row that runs the script and then
;; reads the store must read it after
(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/import-git-" (number->string (get-process-id))))
(when (file-exists? root) (assertion-violation 'import-git "scratch directory already exists" root))
(define (sh . xs) (system (apply string-append xs)))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(sh "mkdir -p " (quoted root) " " (quoted (string-append root "/home")))
(putenv "THEOURGIA_HOME" (string-append root "/home"))

(define (ask st . req) (rpc-dispatch st req "test"))
(define (head-of a)
  (cond ((not (pair? a)) a)
        ((eq? (car a) 'error) (list 'error (cadr a)))
        (else (car a))))
(define (clause-of a head) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) head))) (cdr a))))
(define (value-of a head) (let ((c (clause-of a head))) (and c (pair? (cdr c)) (cadr c))))
(define (checked what a) (unless (eq? 'ok (head-of a)) (assertion-violation 'import-git what a)) a)
(define (file-datum path) (guard (e (#t 'UNREADABLE)) (call-with-input-file path read)))

(define (path-parent p)
  (let loop ((i (- (string-length p) 1))) (cond ((< i 0) ".") ((char=? (string-ref p i) #\/) (substring p 0 i)) (else (loop (- i 1))))))
(define (write-bytes! path bv)
  (sh "mkdir -p " (quoted (path-parent path)))
  (when (file-exists? path) (delete-file path))
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p bv))))
(define (out-of cmd)
  (let-values (((to from err pid) (open-process-ports cmd (buffer-mode block) (native-transcoder))))
    (close-port to)
    (let ((s (get-string-all from))) (get-string-all err) (close-port from) (close-port err) (if (eof-object? s) "" s))))
(define (chomp s) (if (and (> (string-length s) 0) (char=? #\newline (string-ref s (- (string-length s) 1)))) (substring s 0 (- (string-length s) 1)) s))
(define (wait-for path ms)
  (let loop ((k 0))
    (cond ((file-exists? path) #t)
          ((> k (div ms 50)) #f)
          (else (sleep (make-time 'time-duration 50000000 0)) (loop (+ k 1))))))

;; ---- the texts ---------------------------------------------------------------
;;
;; A block's text is one definition: (d 'a) is "(define a 1)\n", and an
;; edited one is given whole.
(define (d name) (format "(define ~a 1)\n" name))
(define (defs . names) (apply string-append (map (lambda (n) (if (symbol? n) (d n) n)) names)))
(define B* "(define b 22)\n")

;; ---- a repository --------------------------------------------------------------

(define n 0)
(define (fresh-dir name) (set! n (+ n 1)) (string-append root "/" name "-" (number->string n)))
(define (git wt . args) (chomp (out-of (string-append "git -C " (quoted wt) " " (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args)) " 2>/dev/null"))))
(define (repo! name)
  (let ((wt (string-append (fresh-dir name) "/wt")))
    (sh "mkdir -p " (quoted wt) " && git init -q " (quoted wt) " && git -C " (quoted wt) " config user.name fixture && git -C " (quoted wt)
        " config user.email fixture@example.invalid && git -C " (quoted wt) " config commit.gpgsign false")
    wt))
;; CHANGES, applied in order: (<path> . <text or bytes>) writes the file,
;; (<path> . #f) removes it, (chmod <path> <mode>), (link <path> <target>)
;; and (mv <old> <new>). -> the new commit's id
(define (commit! wt msg . changes)
  (for-each
    (lambda (c)
      (cond ((eq? (car c) 'chmod) (sh "chmod " (caddr c) " " (quoted (string-append wt "/" (cadr c)))))
            ((eq? (car c) 'link) (sh "cd " (quoted wt) " && rm -f " (quoted (cadr c)) " && ln -s " (quoted (caddr c)) " " (quoted (cadr c))))
            ((eq? (car c) 'mv) (sh "git -C " (quoted wt) " mv " (quoted (cadr c)) " " (quoted (caddr c))))
            ((not (cdr c)) (sh "git -C " (quoted wt) " rm -q " (quoted (car c))))
            (else (write-bytes! (string-append wt "/" (car c)) (if (string? (cdr c)) (string->utf8 (cdr c)) (cdr c))))))
    changes)
  (sh "git -C " (quoted wt) " add -A && git -C " (quoted wt) " commit -q --allow-empty -m " (quoted msg))
  (git wt "rev-parse" "HEAD"))
;; the worktree at REV, detached, so the next commit branches from it
(define (at! wt rev) (sh "git -C " (quoted wt) " checkout -q --detach " rev))

;; ---- a store seeded from a commit ----------------------------------------------
;;
;; The commit's files, from git archive; each regular, not dot-named file
;; split with split-suggest (SPLIT?) or imported as it is, all in one
;; import-code. -> the store's path
(define (store-from! wt rev name split?)
  (let* ((dir (fresh-dir name)) (st (string-append dir "/store")) (raw (string-append dir "/raw")) (src (string-append dir "/src")))
    (sh "mkdir -p " (quoted st) " " (quoted raw) " " (quoted src))
    (checked "init" (ask st 'init))
    (sh "git -C " (quoted wt) " archive " rev " | tar -x -C " (quoted raw))
    (for-each (lambda (rel)
                (let ((out (string-append src "/" rel)))
                  (sh "mkdir -p " (quoted (path-parent out)))
                  (if split?
                      (checked "split" (ask st 'split-suggest (string-append raw "/" rel) "--output" out))
                      (sh "cp " (quoted (string-append raw "/" rel)) " " (quoted out)))))
              (filter (lambda (p) (> (string-length p) 0))
                      (let ((s (out-of (string-append "cd " (quoted raw) " && find . -type f ! -path '*/.*' | sed 's|^\\./||' | sort"))))
                        (let loop ((i 0) (from 0) (out '()))
                          (cond ((= i (string-length s)) (reverse (cons (substring s from i) out)))
                                ((char=? (string-ref s i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring s from i) out)))
                                (else (loop (+ i 1) from out)))))))
    (checked "import" (ask st 'import-code src))
    st))

;; ---- the store, read ------------------------------------------------------------

(define (holders st path)
  (let ((state (open-and-reduce st)))
    (filter (lambda (id) (and (eq? 'text (code-field state id 'mode)) (equal? (code-field state id 'path) path))) (code-files state))))
(define (file-at st path) (let ((h (holders st path))) (and (pair? h) (car h))))
(define (kids st path) (let ((f (file-at st path))) (if f (code-children (open-and-reduce st) f) '())))
(define (src-text v) (if (bytevector? v) (utf8->string v) v))
(define (texts st path) (let ((state (open-and-reduce st))) (map (lambda (id) (src-text (code-field state id 'src))) (kids st path))))
(define (field-of st id name . cut)
  (let* ((state (apply open-and-reduce st cut)) (row (state-read state id)) (f (and row (assq name (cdr (assq 'fields row))))))
    (and f (if (symbol? (cdr f)) (symbol->string (cdr f)) (src-text (cdr f))))))
(define (deleted? st id) (let ((row (state-read (open-and-reduce st) id))) (and row (cdr (assq 'deleted row)))))
(define (position st) (value-of (ask st 'position) 'position))
(define (tag-cut st name)
  (let ((t (find (lambda (t) (equal? (cadr t) name)) (store-tags st))))
    (and t (eq? (car t) 'settled) (caddr t))))
(define (tag-count st name) (length (filter (lambda (t) (equal? (cadr t) name)) (store-tags st))))
;; the live import blocks: ((<id> . <status>) ...)
(define (import-blocks st)
  (let* ((state (open-and-reduce st))
         (a (session-answer (make-query-session state) '(field ?b "slot" "import"))))
    (map (lambda (row) (cons (car row) (field-of st (car row) 'status))) (cadr a))))
;; each new child named by the old child it is, or `new`: OLD is
;; ((<name> . <id>) ...)
(define (shape old new-ids)
  (map (lambda (id) (let ((x (find (lambda (o) (equal? (cdr o) id)) old))) (if x (car x) 'new))) new-ids))
(define (named st path . names) (map cons names (kids st path)))

;; THE EXPORT AT CUT (now when none) AGAINST git archive REV: #t when the two
;; trees hold the same files with the same bytes, dot-named ones aside
(define (export-is? st wt rev . cut)
  (let ((a (fresh-dir "export")) (b (fresh-dir "archive")))
    (sh "mkdir -p " (quoted b))
    (and (eq? 'ok (head-of (apply ask st 'export-code a "--raw" (if (pair? cut) (list "--cut" (format "~s" (car cut))) '()))))
         (= 0 (sh "git -C " (quoted wt) " archive " rev " | tar -x -C " (quoted b)
                  " && diff -rq -x '.*' " (quoted a) " " (quoted b) " > /dev/null 2>&1")))))

;; THE PIECES A FRESH IMPORT CUTS, from suggest-boundaries, as text
(define (pieces path text)
  (let* ((bytes (string->utf8 text)) (cuts (car (suggest-boundaries (language-for-path path) bytes))))
    (map (lambda (from to) (utf8->string (byte-slice bytes from to))) cuts (append (cdr cuts) (list (bytevector-length bytes))))))

;; ---- the script, as a child ----------------------------------------------------------

(define script (string-append (path-parent (current-directory)) "/tools/import-git.sc"))
(define (command env st wt from to tag extra out)
  (string-append env " scheme --script " (quoted script) " " (quoted st) " " (quoted wt) " " from " " to " --tag " (quoted tag) extra
                 " > " (quoted out) " 2> " (quoted (string-append out ".err")) " < /dev/null; echo $? > " (quoted (string-append out ".rc.part")) " && mv " (quoted (string-append out ".rc.part")) " " (quoted (string-append out ".rc"))))
;; -> (<datum printed> <exit status>)
(define (import! st wt from to tag . opts)
  (let ((out (string-append (fresh-dir "run") ".out")) (env (if (pair? opts) (car opts) ""))
        (extra (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) "")))
    (sh "( " (command env st wt from to tag extra out) " )")
    (list (file-datum out) (file-datum (string-append out ".rc")))))
(define (answer r) (car r))
;; A STOPPED RUN does not raise when it did not stop: the row that follows
;; reads the prefix it expected, and is red there, so the rows after it run.
(define (stopped! st wt from to tag k)
  (import! st wt from to tag (format "THEOURGIA_IMPORT_STOP=~a" k)))
;; A HELD RUN: started at import-before-batch, DURING called when it is
;; held, then released. -> (<during's value> <datum printed> <exit status>)
(define (held! st wt from to tag during)
  (let* ((out (string-append (fresh-dir "held") ".out")) (release (string-append out ".release")))
    (sh "( " (command (string-append "THEOURGIA_INJECT=on THEOURGIA_HOLD=" (quoted (string-append "import-before-batch:" release)))
                      st wt from to tag "" out) " ) &")
    (let* ((held (wait-for (string-append release ".held") 60000))
           (v (and held (during))))
      (call-with-output-file release (lambda (p) (display "go" p)))
      (wait-for (string-append out ".rc") 60000)
      (list v (file-datum out) (file-datum (string-append out ".rc"))))))

;; ---- IG-1: two files modified --------------------------------------------------------
(printf "== IG-1: two files modified ==\n")
(define wt1 (repo! "r1"))
(define c1-0 (commit! wt1 "base" (cons "a.sc" (defs 'a 'b 'c)) (cons "b.sc" (defs 'p 'q))))
(define c1-1 (commit! wt1 "edit b, append r" (cons "a.sc" (defs 'a B* 'c)) (cons "b.sc" (defs 'p 'q 'r))))
(define st1 (store-from! wt1 c1-0 "s1" #t))
(define r1 (import! st1 wt1 c1-0 c1-1 "t1"))
(define block1 (let ((b (import-blocks st1))) (and (= 1 (length b)) (car (car b)))))
(want "IG-1 the range lands: export at the tag = git archive <to>; the import block's fields; the answer's counts"
      (list (head-of (answer r1)) (export-is? st1 wt1 c1-1 (tag-cut st1 "t1"))
            (and block1 (map (lambda (f) (field-of st1 block1 f)) '(kind slot from to tag status src)))
            (value-of (answer r1) 'files) (value-of (answer r1) 'rerun))
      (list 'ok #t (list "decision" "import" c1-0 c1-1 "t1" "done" "edit b, append r\n")
            '(modified 2) #f))
(want "IG-1 the tag binds the cut of `set status done`, and is the last record: pending one record before the cut, done at it"
      (let* ((cut (tag-cut st1 "t1")) (end (position st1))
             (w (car end)) (seq (and cut (cdr (assoc w cut))))
             (before (and seq (map (lambda (e) (if (equal? (car e) w) (cons w (- (cdr e) 1)) e)) cut))))
        (list (and seq (= (+ seq 1) (cdr end)))
              (and block1 before (field-of st1 block1 'status before))
              (and block1 cut (field-of st1 block1 'status cut))))
      (list #t "pending" "done"))

;; ---- IG-2: the matcher -------------------------------------------------------------
(printf "== IG-2: the matcher ==\n")
(define wt2 (repo! "r2"))
(define c2-0 (commit! wt2 "base"
                      (cons "m1.sc" (defs 'a 'b 'c)) (cons "m2.sc" (defs 'a 'b 'c)) (cons "m3.sc" (defs 'a 'b))
                      (cons "m4.sc" (defs 'a 'b 'c)) (cons "m5.sc" (defs 'a 'b)) (cons "m6.sc" (defs 'a 'b 'c))
                      (cons "m7.sc" (defs 'a 'b)) (cons "m8.sc" (defs 'x 'x))))
(define c2-1 (commit! wt2 "the eight shapes"
                      (cons "m1.sc" (defs 'a 'x 'b 'c)) (cons "m2.sc" (defs 'a B* 'c)) (cons "m3.sc" (defs 'a 'b 'c))
                      (cons "m4.sc" (defs 'a 'c)) (cons "m5.sc" (defs 'b 'a)) (cons "m6.sc" (defs 'x 'b 'c 'a))
                      (cons "m7.sc" (defs 'x 'a 'y 'b)) (cons "m8.sc" (defs 'x))))
(define st2 (store-from! wt2 c2-0 "s2" #t))
(define o1 (named st2 "m1.sc" 'a 'b 'c)) (define o2 (named st2 "m2.sc" 'a 'b 'c)) (define o3 (named st2 "m3.sc" 'a 'b))
(define o4 (named st2 "m4.sc" 'a 'b 'c)) (define o5 (named st2 "m5.sc" 'a 'b)) (define o6 (named st2 "m6.sc" 'a 'b 'c))
(define o7 (named st2 "m7.sc" 'a 'b)) (define o8 (named st2 "m8.sc" 'x1 'x2))
(define r2 (import! st2 wt2 c2-0 c2-1 "t2"))
(define (listed r name) (let* ((b (clause-of (answer r) 'blocks)) (c (and b (assq name (cdr b))))) (if c (cadr c) '())))
(define moved2 (listed r2 'moved))
(define (row2 path old . target) (list (shape old (kids st2 path)) (equal? (texts st2 path) target)))
(want "IG-2 a middle insert [a,b,c] -> [a,x,b,c]: b and c keep their ids, x is one insert (the run itself landed)"
      (in-order (head-of (answer r2)) (row2 "m1.sc" o1 (d 'a) (d 'x) (d 'b) (d 'c)))
      (list 'ok (list '(a new b c) #t)))
(want "IG-2 a definition edited in place keeps its id"
      (row2 "m2.sc" o2 (d 'a) B* (d 'c))
      (list '(a b c) #t))
(want "IG-2 an appended definition is an insert"
      (row2 "m3.sc" o3 (d 'a) (d 'b) (d 'c))
      (list '(a b new) #t))
(want "IG-2 a deleted definition is a delete, and no block is left holding nothing"
      (list (row2 "m4.sc" o4 (d 'a) (d 'c)) (deleted? st2 (cdr (assq 'b o4))))
      (list (list '(a c) #t) #t))
(want "IG-2 a swapped pair [a,b] -> [b,a] keeps both ids, by two moves"
      (list (row2 "m5.sc" o5 (d 'b) (d 'a)) (filter (lambda (id) (member id (map cdr o5))) moved2))
      (list (list '(b a) #t) (list (cdr (assq 'b o5)) (cdr (assq 'a o5)))))
(want "IG-2 a relocation with an insert [a,b,c] -> [x,b,c,a]: a keeps its id (reserved before the pairing)"
      (row2 "m6.sc" o6 (d 'x) (d 'b) (d 'c) (d 'a))
      (list '(new b c a) #t))
(want "IG-2 a leading and a middle insert [a,b] -> [x,a,y,b]: a and b keep their ids, the order right"
      (row2 "m7.sc" o7 (d 'x) (d 'a) (d 'y) (d 'b))
      (list '(new a new b) #t))
(want "IG-2 two equal pieces [x,x] -> [x]: the earliest keeps its id; the export at the tag is <to>"
      (list (row2 "m8.sc" o8 (d 'x)) (export-is? st2 wt2 c2-1 (tag-cut st2 "t2")))
      (list (list '(x1) #t) #t))

;; ---- IG-3: added, removed, renamed --------------------------------------------------
(printf "== IG-3: added, removed, renamed ==\n")
(define wt3 (repo! "r3"))
(define c3-0 (commit! wt3 "base" (cons "k.sc" (defs 'k)) (cons "o.sc" (defs 'u 'v))
                      (cons "r.sc" "(define removed-one \"gone\")\n(define removed-two \"also gone\")\n")))
(define c3-1 (commit! wt3 "add, remove, rename" (cons "n.sc" (defs 'p 'q)) (cons "r.sc" #f) '(mv "o.sc" "o2.sc")))
(define st3 (store-from! wt3 c3-0 "s3" #t))
(define o-file (file-at st3 "o.sc")) (define o-kids (kids st3 "o.sc"))
(define r-file (file-at st3 "r.sc")) (define r-kids (kids st3 "r.sc"))
(define r3 (import! st3 wt3 c3-0 c3-1 "t3"))
(want "IG-3 an added file: a file block, then its children in order, all inserted in that order; the export is <to>"
      (in-order (head-of (answer r3)) (texts st3 "n.sc")
                (let ((ins (listed r3 'inserted))) (and (>= (length ins) 3) (equal? (list-head ins 3) (cons (file-at st3 "n.sc") (kids st3 "n.sc")))))
                (export-is? st3 wt3 c3-1 (tag-cut st3 "t3")) (value-of (answer r3) 'files))
      (list 'ok (list (d 'p) (d 'q)) #t #t '(modified 0)))
(want "IG-3 a removed file: its children, then its file block, deleted in that order; the path is gone"
      (list (equal? (listed r3 'deleted) (append o-kids (list o-file) r-kids (list r-file))) (file-at st3 "r.sc"))
      (list #t #f))
(want "IG-3 a rename is a removal and an addition: the new path's blocks are new, the old path is gone"
      (let ((new (kids st3 "o2.sc")))
        (list (texts st3 "o2.sc") (null? (filter (lambda (id) (member id o-kids)) new)) (file-at st3 "o.sc")))
      (list (list (d 'u) (d 'v)) #t #f))

;; ---- IG-4: the base ----------------------------------------------------------------
(printf "== IG-4: the base ==\n")
(define wt4 (repo! "r4"))
(define c4-0 (commit! wt4 "base" (cons "a.sc" (defs 'a 'b))))
(define c4-1 (commit! wt4 "one" (cons "a.sc" (defs 'a 'b 'c))))
(define c4-2 (commit! wt4 "two" (cons "a.sc" (defs 'a 'b 'c 'd))))
(define c4-3 (commit! wt4 "dot only" (cons ".hidden" "x\n")))
(define st4 (store-from! wt4 c4-0 "s4" #t))
(define p4 (position st4))
(define r4a (import! st4 wt4 c4-1 c4-2 "t4a"))
(want "IG-4 the store one commit behind <from>: base-mismatch naming the path, nothing written, no tag"
      (in-order (head-of (answer r4a)) (value-of (answer r4a) 'differing) (equal? (position st4) p4) (tag-cut st4 "t4a"))
      (list '(error base-mismatch) "a.sc" #t #f))
(define r4b (import! st4 wt4 c4-0 c4-1 "t4"))
(want "IG-4 the same range twice: the second run is base-mismatch"
      (in-order (head-of (answer r4b)) (head-of (answer (import! st4 wt4 c4-0 c4-1 "t4"))))
      (list 'ok '(error base-mismatch)))
(want "IG-4 the next range is accepted: imports chain"
      (in-order (head-of (answer (import! st4 wt4 c4-1 c4-2 "t4c"))) (export-is? st4 wt4 c4-2 (tag-cut st4 "t4c")))
      (list 'ok #t))
(want "IG-4 a dot-only range and an empty range: nothing-to-import"
      (in-order (head-of (answer (import! st4 wt4 c4-2 c4-3 "t4d"))) (head-of (answer (import! st4 wt4 c4-2 c4-2 "t4e"))))
      (list '(error nothing-to-import) '(error nothing-to-import)))

;; ---- IG-5: modes, letters, text, the tag --------------------------------------------
(printf "== IG-5: modes, letters, text, the tag ==\n")
(define wt5 (repo! "r5"))
(define c5-0 (commit! wt5 "base" (cons "a.sc" (defs 'a 'b)) (cons "x.sh" "echo one\n") '(chmod "x.sh" "755")))
(define c5-mode (begin (at! wt5 c5-0) (commit! wt5 "mode" '(chmod "a.sc" "755"))))
(define c5-exec (begin (at! wt5 c5-0) (commit! wt5 "exec content" (cons "x.sh" "echo two\n"))))
(define c5-type (begin (at! wt5 c5-0) (commit! wt5 "type" '(link "a.sc" "x.sh"))))
(define c5-nul (begin (at! wt5 c5-0) (commit! wt5 "nul" (cons "a.sc" (bytevector 40 100 101 102 0 41 10)))))
(define c5-utf (begin (at! wt5 c5-0) (commit! wt5 "latin-1" (cons "a.sc" (bytevector 40 100 101 102 255 41 10)))))
(define c5-add (begin (at! wt5 c5-0) (commit! wt5 "add exec" (cons "y.sh" "echo y\n") '(chmod "y.sh" "755"))))
(define st5 (store-from! wt5 c5-0 "s5" #t))
(define (refusal r) (let ((a (answer r))) (if (and (pair? a) (eq? (car a) 'error)) (cdr a) a)))
(want "IG-5 a mode change 100644 -> 100755: file-mode"
      (refusal (import! st5 wt5 c5-0 c5-mode "t5"))
      '(file-mode (path "a.sc") (from "100644") (to "100755")))
(want "IG-5 a type change: paths-unsupported"
      (refusal (import! st5 wt5 c5-0 c5-type "t5"))
      '(paths-unsupported (letter "T") (path "a.sc")))
(want "IG-5 a changed file holding a NUL byte: not-text"
      (refusal (import! st5 wt5 c5-0 c5-nul "t5"))
      '(not-text (path "a.sc")))
(want "IG-5 a changed file that is not valid UTF-8: not-text"
      (refusal (import! st5 wt5 c5-0 c5-utf "t5"))
      '(not-text (path "a.sc")))
(want "IG-5 an added file at 100755: file-mode"
      (refusal (import! st5 wt5 c5-0 c5-add "t5"))
      '(file-mode (path "y.sh") (from none) (to "100755")))
(checked "tag" (ask st5 'tag "t5x"))
(define p5 (position st5))
(want "IG-5 a tag name already bound: tag-exists, before any write"
      (in-order (refusal (import! st5 wt5 c5-0 c5-exec "t5x")) (equal? (position st5) p5) (import-blocks st5))
      (list '(tag-exists (tag "t5x")) #t '()))
(want "IG-5 a content change at 100755 at both ends is accepted"
      (in-order (head-of (answer (import! st5 wt5 c5-0 c5-exec "t5"))) (export-is? st5 wt5 c5-exec (tag-cut st5 "t5")))
      (list 'ok #t))

;; ---- IG-6: premises ------------------------------------------------------------------
(printf "== IG-6: premises ==\n")
(define wt6 (repo! "r6"))
(define c6-0 (commit! wt6 "base" (cons "a.sc" (defs 'a 'b 'c)) (cons "o.sc" (defs 'o))))
(define c6-1 (commit! wt6 "edit b" (cons "a.sc" (defs 'a B* 'c))))
(define st6 (store-from! wt6 c6-0 "s6" #t))
(define o6a (named st6 "a.sc" 'a 'b 'c))
;; DURING: a set of a title on BLOCK, then the log's end after it
(define (set-title! st block) (lambda () (checked "concurrent set" (ask st 'set block "title" "concurrent")) (position st)))
(define (premise-row h st)
  (list (head-of (cadr h)) (and (car h) (equal? (position st) (car h))) (tag-cut st "t6") (caddr h)))
(want "IG-6 a concurrent set on a block the import sets: premise-changed, nothing written after it, no tag"
      (premise-row (held! st6 wt6 c6-0 c6-1 "t6" (set-title! st6 (cdr (assq 'b o6a)))) st6)
      (list '(error premise-changed) #t #f 1))
(want "IG-6 a concurrent set on a block the import keeps: premise-changed likewise"
      (premise-row (held! st6 wt6 c6-0 c6-1 "t6" (set-title! st6 (cdr (assq 'a o6a)))) st6)
      (list '(error premise-changed) #t #f 1))
(want "IG-6 on a rerun, a concurrent set on the pending import block: premise-changed likewise"
      (let ((pending (map car (import-blocks st6))))
        (list (length pending)
              (premise-row (held! st6 wt6 c6-0 c6-1 "t6" (set-title! st6 (car pending))) st6)))
      (list 1 (list '(error premise-changed) #t #f 1)))

;; ---- IG-7: the convergent rerun ---------------------------------------------------------
;;
;; The batch of c7-0 -> c7-1, items in path order: d.sc's three children and
;; its file block deleted (0-3); m.sc [a,b,c] -> [x,a,B,c,y]: x inserted
;; (4), a moved after it (5), b set (6) and moved (7), c moved (8), y
;; inserted (9); n.sc's file block (10) and its three children (11-13);
;; done (14); the tag (15). Each variant checks the prefix it stopped at.
(printf "== IG-7: the convergent rerun ==\n")
(define wt7 (repo! "r7"))
(define c7-0 (commit! wt7 "base" (cons "d.sc" (defs 's 't 'u)) (cons "m.sc" (defs 'a 'b 'c)) (cons "o.sc" (defs 'o))))
(define c7-1 (commit! wt7 "the range" (cons "d.sc" #f) (cons "m.sc" (defs 'x 'a B* 'c 'y)) (cons "n.sc" (defs 'p 'q 'r))))
(define c7-2 (commit! wt7 "another range" (cons "n.sc" (defs 'p))))
(define (store7! name) (store-from! wt7 c7-0 name #t))
(define (rerun-row st old r)
  (list (head-of (answer r)) (value-of (answer r) 'rerun) (export-is? st wt7 c7-1 (tag-cut st "t7"))
        (shape old (kids st "m.sc")) (import-blocks st) (tag-count st "t7")))
(define (done-blocks st) (map (lambda (b) (cdr b)) (import-blocks st)))

(define st7a (store7! "s7a"))
(define o7a (named st7a "m.sc" 'a 'b 'c))
(stopped! st7a wt7 c7-0 c7-1 "t7" 7)
(define half7a (texts st7a "m.sc"))
(define r7a (import! st7a wt7 c7-0 c7-1 "t7"))
(want "IG-7 stopped inside a file's items: the rerun converges, every LCS and relocation id kept, the same block done, one tag; a third run is base-mismatch"
      (in-order half7a (rerun-row st7a o7a r7a) (head-of (answer (import! st7a wt7 c7-0 c7-1 "t7"))))
      (list (list B* (d 'c) (d 'x) (d 'a))
            (list 'ok #t #t '(new a b c new) (list (cons (car (car (import-blocks st7a))) "done")) 1)
            '(error base-mismatch)))

(define st7b (store7! "s7b"))
(define o7b (named st7b "m.sc" 'a 'b 'c))
(stopped! st7b wt7 c7-0 c7-1 "t7" 0)
(define mid7b (list (export-is? st7b wt7 c7-0) (done-blocks st7b)))
(want "IG-7 stopped after the import block alone: the export is still <from>, and the rerun is accepted as pending and converges"
      (in-order mid7b (rerun-row st7b o7b (import! st7b wt7 c7-0 c7-1 "t7")))
      (list (list #t '("pending"))
            (list 'ok #t #t '(new a b c new) (list (cons (car (car (import-blocks st7b))) "done")) 1)))

(define st7c (store7! "s7c"))
(stopped! st7c wt7 c7-0 c7-1 "t7" 11)
(define mid7c (list (length (holders st7c "n.sc")) (kids st7c "n.sc")))
(define r7c (import! st7c wt7 c7-0 c7-1 "t7"))
(want "IG-7 stopped after an added file's file block, before its children: one holder, its children added"
      (in-order mid7c (head-of (answer r7c)) (length (holders st7c "n.sc")) (texts st7c "n.sc") (export-is? st7c wt7 c7-1 (tag-cut st7c "t7")))
      (list (list 1 '()) 'ok 1 (list (d 'p) (d 'q) (d 'r)) #t))

(define st7d (store7! "s7d"))
(define d-file (file-at st7d "d.sc"))
(stopped! st7d wt7 c7-0 c7-1 "t7" 2)
(define mid7d (texts st7d "d.sc"))
(define r7d (import! st7d wt7 c7-0 c7-1 "t7"))
(want "IG-7 stopped after some of a removed file's children: the rest and the file block deleted"
      (in-order mid7d (head-of (answer r7d)) (file-at st7d "d.sc") (deleted? st7d d-file) (export-is? st7d wt7 c7-1 (tag-cut st7d "t7")))
      (list (list (d 'u)) 'ok #f #t #t))

(define st7e (store7! "s7e"))
(stopped! st7e wt7 c7-0 c7-1 "t7" 14)
(define mid7e (list (export-is? st7e wt7 c7-1) (done-blocks st7e)))
(define r7e (import! st7e wt7 c7-0 c7-1 "t7"))
(want "IG-7 stopped after every content item, before done: the rerun finds no work, sets done and tags"
      (in-order mid7e (head-of (answer r7e))
                (map (lambda (k) (listed r7e k)) '(set inserted deleted moved))
                (done-blocks st7e) (tag-count st7e "t7"))
      (list (list #t '("pending")) 'ok '(() () () ()) '("done") 1))

(define st7f (store7! "s7f"))
(stopped! st7f wt7 c7-0 c7-1 "t7" 15)
(define mid7f (list (done-blocks st7f) (tag-count st7f "t7")))
(define r7f (import! st7f wt7 c7-0 c7-1 "t7"))
(want "IG-7 stopped after done, before the tag: done without tag, the rerun only tags, once"
      (in-order mid7f (head-of (answer r7f)) (value-of (answer r7f) 'rerun)
                (map (lambda (k) (listed r7f k)) '(set inserted deleted moved))
                (tag-count st7f "t7") (export-is? st7f wt7 c7-1 (tag-cut st7f "t7")))
      (list (list '("done") 0) 'ok #t '(() () () ()) 1 #t))

(define st7g (store7! "s7g"))
(stopped! st7g wt7 c7-0 c7-1 "t7" 0)
(define p7g (position st7g))
(want "IG-7 a pending import block of another range: pending-import, nothing written"
      (in-order (head-of (answer (import! st7g wt7 c7-0 c7-2 "t7g"))) (equal? (position st7g) p7g))
      (list '(error pending-import) #t))

(define st7h (store7! "s7h"))
(stopped! st7h wt7 c7-0 c7-1 "t7" 3)
(define pending7h (let ((b (import-blocks st7h))) (and (pair? b) (car (car b)))))
(define (version st id) (and id (block-hash (open-and-reduce st) id)))
(define version7h (version st7h pending7h))
(checked "outside drift" (ask st7h 'set (car (kids st7h "o.sc")) "src" "(define o 2)\n"))
(define p7h (position st7h))
(define r7h (import! st7h wt7 c7-0 c7-1 "t7"))
(want "IG-7 a pending prefix and a commit outside the range: base-mismatch naming that path, nothing written, the pending block untouched"
      (in-order (head-of (answer r7h)) (value-of (answer r7h) 'differing) (equal? (position st7h) p7h)
                (and version7h (equal? (version st7h pending7h) version7h)) (done-blocks st7h))
      (list '(error base-mismatch) "o.sc" #t #t '("pending")))

;; ---- IG-8: the split equals a fresh import's ----------------------------------------
(printf "== IG-8: the split equals a fresh import's ==\n")
(define wt8 (repo! "r8"))
(define w-text "(define w 1\n(define v 2)\n")
(define w-text2 "(define w 11\n(define v 2)\n")
(define c8-0 (commit! wt8 "base" (cons "f.sc" (defs 'a 'b 'c)) (cons "w.sc" w-text)))
(define c8-1 (commit! wt8 "edits" (cons "f.sc" (defs 'a B* 'c 'd)) (cons "w.sc" w-text2)))
(define st8 (store-from! wt8 c8-0 "s8" #f))
(define seed8 (list (length (kids st8 "f.sc")) (length (kids st8 "w.sc"))))
(define r8 (import! st8 wt8 c8-0 c8-1 "t8"))
(want "IG-8 a store seeded one block per file: the export is <to>, and each child is a piece suggest-boundaries cuts"
      (in-order seed8 (head-of (answer r8)) (export-is? st8 wt8 c8-1 (tag-cut st8 "t8"))
                (equal? (texts st8 "f.sc") (pieces "f.sc" (defs 'a B* 'c 'd))))
      (list '(1 1) 'ok #t #t))
(want "IG-8 a file the splitter cannot prove imports to one block holding it whole"
      (list (pieces "w.sc" w-text2) (texts st8 "w.sc"))
      (list (list w-text2) (list w-text2)))

;; ---- IG-9: a coder's draft under the import ------------------------------------------
(printf "== IG-9: a coder's draft under the import ==\n")
(define wt9 (repo! "r9"))
(define c9-0 (commit! wt9 "base" (cons "a.sc" (defs 'a 'b 'c))))
(define c9-1 (commit! wt9 "edit b" (cons "a.sc" (defs 'a B* 'c))))
(define st9 (store-from! wt9 c9-0 "s9" #t))
(define b9 (cadr (kids st9 "a.sc")))
(define edit9 (fresh-dir "edit9"))
(define tools-dir (string-append (path-parent (current-directory)) "/tools/"))
(define (tool! name . args)
  (let ((out (string-append (fresh-dir "tool") ".out")))
    (sh "scheme --script " (quoted (string-append tools-dir name)) " "
        (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
        "> " (quoted out) " 2> " (quoted (string-append out ".err")) " < /dev/null")
    (file-datum out)))
(define exported9 (tool! "edit-export.sc" st9 edit9 "--writer" "w1" "--git" wt9 "--base" c9-0))
(write-bytes! (string-append edit9 "/a.sc") (string->utf8 (defs 'a "(define b 3)\n" 'c)))
(define r9 (import! st9 wt9 c9-0 c9-1 "t9"))
(want "IG-9 a coder's edit of a block the import sets, made after the export: stale at edit-sync"
      (in-order (head-of exported9) (head-of (answer r9)) (tool! "edit-sync.sc" st9 edit9 "--writer" "w1"))
      (list 'ok 'ok (list 'error 'stale (list 'block b9) '(reason committed-moved))))

;; ---- IG-10: post-verify --------------------------------------------------------------
(printf "== IG-10: post-verify ==\n")
(define wt10 (repo! "r10"))
(define c10-0 (commit! wt10 "base" (cons "a.sc" (defs 'a 'b 'c))))
(define c10-1 (commit! wt10 "edit b" (cons "a.sc" (defs 'a B* 'c))))
(define st10 (store-from! wt10 c10-0 "s10" #t))
(define h10 (held! st10 wt10 c10-0 c10-1 "t10"
                   (lambda ()
                     (checked "sibling" (ask st10 'batch (format "((insert ~s #f ((kind . code) (mode . text) (src . ~s))))"
                                                                 (file-at st10 "a.sc") (string->utf8 "(define z 1)\n")))))))
(want "IG-10 an unpremised sibling written during the hold: the batch runs, the tag binds it, import-diverged naming the path, the block done"
      (list (head-of (cadr h10)) (value-of (cadr h10) 'differing) (and (tag-cut st10 "t10") #t) (done-blocks st10) (caddr h10))
      (list '(error import-diverged) "a.sc" #t '("done") 1))

;; ---- IG-11: the dry run ---------------------------------------------------------------
(printf "== IG-11: the dry run ==\n")
(define wt11 (repo! "r11"))
(define c11-0 (commit! wt11 "base" (cons "a.sc" (defs 'a 'b 'c))))
(define c11-1 (commit! wt11 "edit, append" (cons "a.sc" (defs 'a B* 'c 'd))))
(define st11 (store-from! wt11 c11-0 "s11" #t))
(define p11 (position st11))
(define r11 (import! st11 wt11 c11-0 c11-1 "t11" "" " --dry-run"))
(want "IG-11 --dry-run: the plan and its counts, nothing written"
      (in-order (head-of (answer r11)) (value-of (answer r11) 'state) (clause-of (answer r11) 'blocks) (value-of (answer r11) 'items)
                (equal? (position st11) p11) (import-blocks st11) (cadr r11))
      (list 'ok 'fresh '(blocks (kept 2) (set 1) (inserted 1) (deleted 0) (moved 0)) 4 #t '() 0))

(sh "rm -rf " (quoted root))
(printf "rows: ~a\n~a failures\nimport-git complete\n" rows bad)
(exit (if (zero? bad) 0 1))
