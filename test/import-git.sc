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
        (only (theourgia reduce) state-read block-hash reduce-applied-cut)
        (only (theourgia code-project) code-files code-field code-children)
        (only (theourgia code-suggest) suggest-boundaries)
        (only (theourgia languages) language-for-path)
        (only (theourgia markers) byte-slice import-text?)
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
;; a batch answers (batch (<item answer> ...) (done <n>)), not (ok ...)
(define (batch-checked what a)
  (unless (and (pair? a) (eq? (car a) 'batch) (pair? (cdr a)) (list? (cadr a))
               (for-all (lambda (x) (and (pair? x) (eq? (car x) 'ok))) (cadr a)))
    (assertion-violation 'import-git what a))
  a)
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
                  (if (and split? (import-text? (call-with-port (open-file-input-port (string-append raw "/" rel)) get-bytevector-all)))
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
;; NAMES PAIRED WITH THE CHILDREN UP TO THE SHORTER OF THE TWO: a store in
;; another shape (no tool, a mutant) reads red in its row instead of raising
;; here, outside every row
(define (named st path . names)
  (let loop ((ns names) (ks (kids st path)) (out '()))
    (if (or (null? ns) (null? ks)) (reverse out) (loop (cdr ns) (cdr ks) (cons (cons (car ns) (car ks)) out)))))

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
;; the refusal's kind and clauses, or (NOT-A-REFUSAL <what was printed>)
;; A RUN THAT SUCCEEDED: (ok 0), its answer's head and its exit status
(define (ran r) (list (head-of (answer r)) (cadr r)))
(define (refusal r) (let ((a (answer r))) (if (and (pair? a) (eq? (car a) 'error)) (cdr a) (list 'NOT-A-REFUSAL a))))
(define (applied-cut st) (reduce-applied-cut (open-and-reduce st)))
(define (version st id) (and id (block-hash (open-and-reduce st) id)))
;; A REFUSED RUN: (<refusal> <log end unchanged> <exit status>)
(define (refused-run st . args)
  (let* ((p (position st)) (r (apply import! st args)))
    (list (refusal r) (equal? (position st) p) (cadr r))))
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
           ;; THE RELEASE IS WRITTEN WHATEVER DURING DOES: a raise is kept as
           ;; the value, which the row reads red, and the child goes on
           (v (and held (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e))))
                          (during)))))
      (call-with-output-file release (lambda (p) (display "go" p)))
      (wait-for (string-append out ".rc") 60000)
      (list v (file-datum out) (file-datum (string-append out ".rc"))))))

;; ---- IG-1: two files modified --------------------------------------------------------
(printf "== IG-1: two files modified ==\n")
(define wt1 (repo! "r1"))
(define c1-0 (commit! wt1 "base" (cons "a.sc" (defs 'a 'b 'c)) (cons "b.sc" (defs 'p 'q))))
(define c1-h (commit! wt1 "edit b" (cons "a.sc" (defs 'a B* 'c))))
(define c1-1 (commit! wt1 "append r" (cons "b.sc" (defs 'p 'q 'r))))
(define st1 (store-from! wt1 c1-0 "s1" #t))
(define b1 (cadr (kids st1 "a.sc")))
(define r1 (import! st1 wt1 c1-0 c1-1 "t1"))
(define block1 (let ((b (import-blocks st1))) (and (= 1 (length b)) (car (car b)))))
(want "IG-1 the range lands: export at the tag = git archive <to>; the import block's fields; the answer's tag, files and blocks whole"
      (list (ran r1) (export-is? st1 wt1 c1-1 (tag-cut st1 "t1"))
            (and block1 (map (lambda (f) (field-of st1 block1 f)) '(kind slot from to tag status src)))
            (equal? (clause-of (answer r1) 'tag) (list 'tag "t1" (list 'cut (tag-cut st1 "t1"))))
            (clause-of (answer r1) 'files) (clause-of (answer r1) 'rerun) (clause-of (answer r1) 'blocks))
      (list '(ok 0) #t (list "decision" "import" c1-0 c1-1 "t1" "done" "edit b\nappend r\n")
            #t '(files (modified 2) (added 0) (removed 0)) '(rerun #f)
            (list 'blocks '(kept 4) (list 'set (list b1)) (list 'inserted (list (caddr (kids st1 "b.sc")))) '(deleted ()) '(moved ()))))
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
                      (cons "m7.sc" (defs 'a 'b)) (cons "m8.sc" (defs 'x 'x)) (cons "m9.sc" (defs 'a 'b 'c))
                      (cons "m10.sc" (defs 'a 'b 'a))))
;; m9.sc's target does not balance, so the splitter answers one piece
(define m9-text "(define a 1\n(define b 1)\n")
(define c2-1 (commit! wt2 "the eight shapes"
                      (cons "m1.sc" (defs 'a 'x 'b 'c)) (cons "m2.sc" (defs 'a B* 'c)) (cons "m3.sc" (defs 'a 'b 'c))
                      (cons "m4.sc" (defs 'a 'c)) (cons "m5.sc" (defs 'b 'a)) (cons "m6.sc" (defs 'x 'b 'c 'a))
                      (cons "m7.sc" (defs 'x 'a 'y 'b)) (cons "m8.sc" (defs 'x)) (cons "m9.sc" m9-text)
                      (cons "m10.sc" (defs 'b 'a 'a))))
(define st2 (store-from! wt2 c2-0 "s2" #t))
(define o1 (named st2 "m1.sc" 'a 'b 'c)) (define o2 (named st2 "m2.sc" 'a 'b 'c)) (define o3 (named st2 "m3.sc" 'a 'b))
(define o4 (named st2 "m4.sc" 'a 'b 'c)) (define o5 (named st2 "m5.sc" 'a 'b)) (define o6 (named st2 "m6.sc" 'a 'b 'c))
(define o7 (named st2 "m7.sc" 'a 'b)) (define o8 (named st2 "m8.sc" 'x1 'x2))
(define o9 (named st2 "m9.sc" 'a 'b 'c)) (define o10 (named st2 "m10.sc" 'a1 'b 'a2))
(define r2 (import! st2 wt2 c2-0 c2-1 "t2"))
(define (listed r name) (let* ((b (clause-of (answer r) 'blocks)) (c (and b (assq name (cdr b))))) (if c (cadr c) 'ABSENT)))
(define moved2 (listed r2 'moved))
(define (ids-of old . names) (map (lambda (n) (cdr (assq n old))) names))
(define (row2 path old . target) (list (shape old (kids st2 path)) (equal? (texts st2 path) target)))
(want "IG-2 a middle insert [a,b,c] -> [a,x,b,c]: b and c keep their ids, x is one insert (the run itself landed)"
      (in-order (ran r2) (row2 "m1.sc" o1 (d 'a) (d 'x) (d 'b) (d 'c)))
      (list '(ok 0) (list '(a new b c) #t)))
(want "IG-2 a definition edited in place keeps its id; the answer's set, inserted and deleted lists are every file's, in item order"
      (list (row2 "m2.sc" o2 (d 'a) B* (d 'c))
            (equal? (listed r2 'set) (append (ids-of o2 'b) (ids-of o9 'a)))
            (equal? (listed r2 'inserted) (list (cadr (kids st2 "m1.sc")) (caddr (kids st2 "m3.sc")) (car (kids st2 "m6.sc"))
                                                (car (kids st2 "m7.sc")) (caddr (kids st2 "m7.sc"))))
            (equal? (listed r2 'deleted) (append (ids-of o4 'b) (ids-of o8 'x2) (ids-of o9 'b 'c))))
      (list (list '(a b c) #t) #t #t #t))
(want "IG-2 an appended definition is an insert"
      (row2 "m3.sc" o3 (d 'a) (d 'b) (d 'c))
      (list '(a b new) #t))
(want "IG-2 a deleted definition is a delete, and no block is left holding nothing"
      (list (row2 "m4.sc" o4 (d 'a) (d 'c)) (deleted? st2 (cdr (assq 'b o4))))
      (list (list '(a c) #t) #t))
(want "IG-2 a swapped pair [a,b] -> [b,a] keeps both ids, by two moves; the answer's moves are every file's, in item order"
      (list (row2 "m5.sc" o5 (d 'b) (d 'a))
            (equal? moved2 (append (ids-of o10 'b 'a1 'a2) (ids-of o5 'b 'a) (ids-of o6 'b 'c 'a) (ids-of o7 'a 'b))))
      (list (list '(b a) #t) #t))
(want "IG-2 a relocation with an insert [a,b,c] -> [x,b,c,a]: a keeps its id (reserved before the pairing)"
      (row2 "m6.sc" o6 (d 'x) (d 'b) (d 'c) (d 'a))
      (list '(new b c a) #t))
(want "IG-2 a leading and a middle insert [a,b] -> [x,a,y,b]: a and b keep their ids, the order right"
      (row2 "m7.sc" o7 (d 'x) (d 'a) (d 'y) (d 'b))
      (list '(new a new b) #t))
(want "IG-2 two equal pieces [x,x] -> [x]: the earliest keeps its id; on a tie [a,b,a] -> [b,a,a] the a's keep their order; the export at the tag is <to>"
      (list (row2 "m8.sc" o8 (d 'x)) (row2 "m10.sc" o10 (d 'b) (d 'a) (d 'a)) (export-is? st2 wt2 c2-1 (tag-cut st2 "t2")))
      (list (list '(x1) #t) (list '(b a1 a2) #t) #t))

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
      (in-order (ran r3) (texts st3 "n.sc")
                (equal? (listed r3 'inserted) (append (cons (file-at st3 "n.sc") (kids st3 "n.sc")) (cons (file-at st3 "o2.sc") (kids st3 "o2.sc"))))
                (export-is? st3 wt3 c3-1 (tag-cut st3 "t3")) (clause-of (answer r3) 'files))
      (list '(ok 0) (list (d 'p) (d 'q)) #t #t '(files (modified 0) (added 2) (removed 2))))
(want "IG-3 a removed file: its children, then its file block, deleted in that order; the path is gone"
      (list (equal? (listed r3 'deleted) (append o-kids (list o-file) r-kids (list r-file))) (file-at st3 "r.sc")
            (map (lambda (id) (deleted? st3 id)) (append r-kids (list r-file))))
      (list #t #f '(#t #t #t)))
(want "IG-3 a rename is a removal and an addition: the new path's blocks are new, the old path is gone"
      (let ((new (kids st3 "o2.sc")))
        (list (texts st3 "o2.sc") (null? (filter (lambda (id) (member id o-kids)) new)) (file-at st3 "o.sc")
              (and (file-at st3 "o2.sc") (not (equal? (file-at st3 "o2.sc") o-file))) (deleted? st3 o-file)))
      (list (list (d 'u) (d 'v)) #t #f #t #t))

;; THE LANGUAGE OF WHAT IS INSERTED: an added file's file block and each of
;; its children, and a child newly inserted into an existing file, carry the
;; language a fresh import gives them; an added file of no known language
;; carries none.
(define wt3l (repo! "r3l"))
(define c3l-0 (commit! wt3l "base" (cons "k.sc" (defs 'k))))
(define c3l-1 (commit! wt3l "add" (cons "k.sc" (defs 'k 'm)) (cons "n.sc" (defs 'p 'q)) (cons "notes.xyz" "plain words\n")))
(define st3l (store-from! wt3l c3l-0 "s3l" #t))
(define k3l-old (kids st3l "k.sc"))
(define r3l (import! st3l wt3l c3l-0 c3l-1 "t3l"))
(define (lang-of st id) (field-of st id 'lang))
(want "IG-3 inserted blocks carry their language: an added file's file block and children, a new child in an existing file; an added file of no known language carries none, on its file block or any child"
      (in-order (ran r3l)
                (lang-of st3l (file-at st3l "n.sc"))
                (map (lambda (id) (lang-of st3l id)) (kids st3l "n.sc"))
                (map (lambda (id) (lang-of st3l id)) (filter (lambda (id) (not (member id k3l-old))) (kids st3l "k.sc")))
                (and (file-at st3l "notes.xyz") (lang-of st3l (file-at st3l "notes.xyz")))
                (let ((ks (kids st3l "notes.xyz"))) (and (pair? ks) (for-all (lambda (id) (not (lang-of st3l id))) ks))))
      (list '(ok 0) "scheme" '("scheme" "scheme") '("scheme") #f #t))

;; ---- IG-4: the base ----------------------------------------------------------------
(printf "== IG-4: the base ==\n")
(define wt4 (repo! "r4"))
(define c4-0 (commit! wt4 "base" (cons "a.sc" (defs 'a 'b))))
(define c4-1 (commit! wt4 "one" (cons "a.sc" (defs 'a 'b 'c))))
(define c4-2 (commit! wt4 "two" (cons "a.sc" (defs 'a 'b 'c 'd))))
(define c4-3 (commit! wt4 "dot only" (cons ".hidden" "x\n")))
(define c4-4 (commit! wt4 "a dot path's type changes" '(link ".hidden" "a.sc")))
(define st4 (store-from! wt4 c4-0 "s4" #t))
(define p4 (position st4))
(define cut4 (applied-cut st4))
(define r4a (import! st4 wt4 c4-1 c4-2 "t4a"))
(want "IG-4 the store one commit behind <from>: base-mismatch with the store's cut, the rev and the paths, nothing written, no tag"
      (in-order (refusal r4a) (equal? (position st4) p4) (tag-cut st4 "t4a"))
      (list (list 'base-mismatch (list 'store-cut cut4) (list 'rev c4-1) '(differing "a.sc")) #t #f))
(define r4b (import! st4 wt4 c4-0 c4-1 "t4"))
(want "IG-4 the same range twice: the second run is base-mismatch"
      (in-order (ran r4b) (refused-run st4 wt4 c4-0 c4-1 "t4"))
      (list '(ok 0) (list (list 'base-mismatch (list 'store-cut (applied-cut st4)) (list 'rev c4-0) '(differing "a.sc")) #t 1)))
(want "IG-4 the next range is accepted: imports chain"
      (in-order (ran (import! st4 wt4 c4-1 c4-2 "t4c")) (export-is? st4 wt4 c4-2 (tag-cut st4 "t4c")))
      (list '(ok 0) #t))
(want "IG-4 a dot-only range, a dot path's type change and an empty range: nothing-to-import, nothing written"
      (in-order (refused-run st4 wt4 c4-2 c4-3 "t4d") (refused-run st4 wt4 c4-3 c4-4 "t4d") (refused-run st4 wt4 c4-2 c4-2 "t4e"))
      (list (list (list 'nothing-to-import (list 'from c4-2) (list 'to c4-3)) #t 1)
            (list (list 'nothing-to-import (list 'from c4-3) (list 'to c4-4)) #t 1)
            (list (list 'nothing-to-import (list 'from c4-2) (list 'to c4-2)) #t 1)))
(want "IG-4 a rev the repository does not have: repo-unreadable, nothing written, exit 1"
      (refused-run st4 wt4 "no-such-rev" c4-2 "t4f")
      (list (list 'repo-unreadable (list 'repo wt4) '(rev "no-such-rev")) #t 1))

;; A LINKED DIRECTORY: git holds `link` as a symbolic link to `lib`; the
;; store is seeded by import-code of the extracted tree itself, which
;; follows the link and holds lib/a.sc and link/a.sc. git's `link` is outside
;; the domain, so the export's link/a.sc is not a difference (publish-landing's
;; rule): the precondition passes and the range imports.
(define wt4l (repo! "r4l"))
(define c4l-0 (commit! wt4l "base" (cons "lib/a.sc" (defs 'a 'b)) (cons "m.sc" (defs 'a)) '(link "link" "lib")))
(define c4l-1 (commit! wt4l "append" (cons "m.sc" (defs 'a 'b))))
(define seed4l-import #f)
(define st4l
  (let* ((dir (fresh-dir "s4l")) (st (string-append dir "/store")) (raw (string-append dir "/raw")))
    (sh "mkdir -p " (quoted st) " " (quoted raw))
    (checked "init" (ask st 'init))
    (sh "git -C " (quoted wt4l) " archive " c4l-0 " | tar -x -C " (quoted raw))
    ;; read by the row, not checked here: an importer that did not follow the
    ;; link would refuse, and the row reads that red
    (set! seed4l-import (head-of (ask st 'import-code raw)))
    st))
(define seed4l (list seed4l-import (and (file-at st4l "link/a.sc") #t) (and (file-at st4l "lib/a.sc") #t)))
(define r4l (import! st4l wt4l c4l-0 c4l-1 "t4l"))
(want "IG-4 a linked directory: the store holds the link's files, the precondition passes, the range imports"
      (in-order seed4l (ran r4l) (texts st4l "m.sc") (and (file-at st4l "link/a.sc") #t))
      (list '(ok #t #t) '(ok 0) (list (d 'a) (d 'b)) #t))

;; A FILE GIT HOLDS AS A BINARY AND THE STORE AS TEXT is a difference: a
;; regular file that is not text is outside the domain but not skipped (only
;; links and submodules are), so the export's text file at that path differs
(define wt4b (repo! "r4b"))
(define c4b-0 (commit! wt4b "base" (cons "asset.dat" "text\n") (cons "m.sc" (defs 'a))))
(define c4b-1 (commit! wt4b "binary" (cons "asset.dat" (bytevector 98 0 98 10))))
(define c4b-2 (commit! wt4b "append" (cons "m.sc" (defs 'a 'b))))
(define st4b (store-from! wt4b c4b-0 "s4b" #t))
(want "IG-4 a path git holds as a binary file and the store as text: base-mismatch naming it, nothing written"
      (refused-run st4b wt4b c4b-1 c4b-2 "t4b")
      (list (list 'base-mismatch (list 'store-cut (applied-cut st4b)) (list 'rev c4b-1) '(differing "asset.dat")) #t 1))

;; ---- IG-5: modes, letters, text, the tag --------------------------------------------
(printf "== IG-5: modes, letters, text, the tag ==\n")
(define wt5 (repo! "r5"))
(define c5-0 (commit! wt5 "base" (cons "a.sc" (defs 'a 'b)) (cons "x.sh" "echo one\n") '(chmod "x.sh" "755")))
(define c5-mode (begin (at! wt5 c5-0) (commit! wt5 "mode" '(chmod "a.sc" "755"))))
(define c5-exec (begin (at! wt5 c5-0) (commit! wt5 "exec content" (cons "x.sh" "echo two\n") '(chmod "x.sh" "755"))))
(define c5-type (begin (at! wt5 c5-0) (commit! wt5 "type" '(link "a.sc" "x.sh"))))
(define c5-nul (begin (at! wt5 c5-0) (commit! wt5 "nul" (cons "a.sc" (bytevector 40 100 101 102 0 41 10)))))
(define c5-utf (begin (at! wt5 c5-0) (commit! wt5 "latin-1" (cons "a.sc" (bytevector 40 100 101 102 255 41 10)))))
(define c5-add (begin (at! wt5 c5-0) (commit! wt5 "add exec" (cons "y.sh" "echo y\n") '(chmod "y.sh" "755"))))
(define c5-unsafe (begin (at! wt5 c5-0) (commit! wt5 "add a backslash" (cons "back\\slash.sc" (defs 'z)))))
(define st5 (store-from! wt5 c5-0 "s5" #t))
(want "IG-5 a mode change 100644 -> 100755: file-mode"
      (refused-run st5 wt5 c5-0 c5-mode "t5")
      (list '(file-mode (path "a.sc") (from "100644") (to "100755")) #t 1))
(want "IG-5 a type change: paths-unsupported"
      (refused-run st5 wt5 c5-0 c5-type "t5")
      (list '(paths-unsupported (letter "T") (path "a.sc")) #t 1))
;; THE EXPORT AT CUT AGAINST git archive REV OVER THE TEXT DOMAIN: as
;; export-is?, with every archived link and every archived file whose bytes
;; are not text removed first, since the importer's domain holds regular
;; text files only.
(define (export-text-is? st wt rev cut)
  (let ((a (fresh-dir "export")) (b (fresh-dir "archive")))
    (sh "mkdir -p " (quoted b))
    (and cut
         (eq? 'ok (head-of (ask st 'export-code a "--raw" "--cut" (format "~s" cut))))
         (= 0 (sh "git -C " (quoted wt) " archive " rev " | tar -x -C " (quoted b)))
         (= 0 (sh "find " (quoted b) " -type l -delete"))
         (begin
           (for-each (lambda (rel)
                       (let* ((p (string-append b "/" rel))
                              (bv (call-with-port (open-file-input-port p) get-bytevector-all)))
                         (unless (import-text? (if (eof-object? bv) (bytevector) bv)) (delete-file p))))
                     (filter (lambda (s) (> (string-length s) 0))
                             (let ((s (out-of (string-append "cd " (quoted b) " && find . -type f | sed 's|^\\./||'"))))
                               (let loop ((i 0) (from 0) (out '()))
                                 (cond ((= i (string-length s)) (reverse (cons (substring s from i) out)))
                                       ((char=? (string-ref s i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring s from i) out)))
                                       (else (loop (+ i 1) from out)))))))
           (= 0 (sh "diff -rq -x '.*' " (quoted a) " " (quoted b) " > /dev/null 2>&1"))))))
;; A FILE THAT STOPS BEING TEXT IS REMOVED: its bytes are text at the old end
;; only, so the range has it as a removal and its blocks go, as for a file git
;; deleted. Each on a store of its own, since the run lands.
(define st5n (store-from! wt5 c5-0 "s5n" #t))
(define r5n (import! st5n wt5 c5-0 c5-nul "t5n"))
(define st5u (store-from! wt5 c5-0 "s5u" #t))
(define r5u (import! st5u wt5 c5-0 c5-utf "t5u"))
(want "IG-5 a changed file that comes to hold a NUL byte, and one that comes to be invalid UTF-8: each removed, no file block left, the export at the tag is <to>'s text"
      (map (lambda (st r rev tag)
             (list (ran r) (clause-of (answer r) 'files) (file-at st "a.sc") (export-text-is? st wt5 rev (tag-cut st tag))))
           (list st5n st5u) (list r5n r5u) (list c5-nul c5-utf) (list "t5n" "t5u"))
      (list (list '(ok 0) '(files (modified 0) (added 0) (removed 1)) #f #t)
            (list '(ok 0) '(files (modified 0) (added 0) (removed 1)) #f #t)))
(want "IG-5 an added file at 100755: file-mode"
      (refused-run st5 wt5 c5-0 c5-add "t5")
      (list '(file-mode (path "y.sh") (from none) (to "100755")) #t 1))
(want "IG-5 an added path the store cannot hold (a backslash): unsafe-path, nothing written, exit 1"
      (refused-run st5 wt5 c5-0 c5-unsafe "t5")
      (list '(unsafe-path (path "back\\slash.sc")) #t 1))
(checked "tag" (ask st5 'tag "t5x"))
(define p5 (position st5))
(want "IG-5 a tag name already bound: tag-exists, before any write, exit 1"
      (in-order (refused-run st5 wt5 c5-0 c5-exec "t5x") (equal? (position st5) p5) (import-blocks st5))
      (list '((tag-exists (tag "t5x")) #t 1) #t '()))
(want "IG-5 a content change at 100755 at both ends is accepted"
      (in-order (ran (import! st5 wt5 c5-0 c5-exec "t5")) (export-is? st5 wt5 c5-exec (tag-cut st5 "t5")))
      (list '(ok 0) #t))

;; THE OTHER ENDS OF THE TEXT AND MODE CHECKS: an added binary and a removed
;; binary (the store never held either) are skipped, a removed link (git
;; mode 120000) is file-mode, and a binary that becomes text is added
(define wt5b (repo! "r5b"))
(define c5b-0 (commit! wt5b "base" (cons "a.sc" (defs 'a)) (cons "bin.dat" (bytevector 98 0 98 10)) '(link "l.sc" "a.sc")))
(define c5b-add (begin (at! wt5b c5b-0) (commit! wt5b "add a binary" (cons "new.dat" (bytevector 110 0 10)))))
(define c5b-rmbin (begin (at! wt5b c5b-0) (commit! wt5b "remove the binary" (cons "bin.dat" #f))))
(define c5b-rmlink (begin (at! wt5b c5b-0) (commit! wt5b "remove the link" (cons "l.sc" #f))))
(define c5b-totext (begin (at! wt5b c5b-0) (commit! wt5b "the binary becomes text" (cons "bin.dat" "now text\n"))))
(define st5b (store-from! wt5b c5b-0 "s5b" #t))
(want "IG-5 an added binary and a removed binary, each alone in its range: nothing-to-import naming it; a removed link: file-mode; nothing written"
      (in-order (refused-run st5b wt5b c5b-0 c5b-add "t5b") (refused-run st5b wt5b c5b-0 c5b-rmbin "t5b")
                (refused-run st5b wt5b c5b-0 c5b-rmlink "t5b"))
      (list (list (list 'nothing-to-import (list 'from c5b-0) (list 'to c5b-add) '(not-text "new.dat")) #t 1)
            (list (list 'nothing-to-import (list 'from c5b-0) (list 'to c5b-rmbin) '(not-text "bin.dat")) #t 1)
            (list '(file-mode (path "l.sc") (from "120000") (to none)) #t 1)))
(define st5t (store-from! wt5b c5b-0 "s5t" #t))
(define r5t (import! st5t wt5b c5b-0 c5b-totext "t5t"))
(want "IG-5 a binary that becomes text is added: a file block holding the new text, one added file and no not-text clause, the export at the tag is <to>'s text"
      (list (ran r5t) (clause-of (answer r5t) 'files) (and (file-at st5t "bin.dat") #t) (texts st5t "bin.dat")
            (export-text-is? st5t wt5b c5b-totext (tag-cut st5t "t5t")))
      (list '(ok 0) '(files (modified 0) (added 1) (removed 0)) #t (pieces "bin.dat" "now text\n") #t))

;; ---- IG-5b: a range with binary files in it ---------------------------------------------
;; A CHANGED BINARY IS OUTSIDE THE DOMAIN AT BOTH ENDS, so the range skips
;; it and the answer names it: alone, the range has nothing to import and
;; the store is left as it was; beside a text change, the text change lands
;; (the dry run and the run name the binary alike) and the export at the tag
;; is <to>'s text. Red on an importer that refuses not-text for a binary the
;; domain never held.
(printf "== IG-5b: a range with binary files in it ==\n")
(define wt5c (repo! "r5c"))
(define c5c-0 (commit! wt5c "base" (cons "a.sc" (defs 'a)) (cons "bin.dat" (bytevector 98 0 98 10))))
(define c5c-bin (begin (at! wt5c c5c-0) (commit! wt5c "the binary changes" (cons "bin.dat" (bytevector 99 0 99 10)))))
(define c5c-both (begin (at! wt5c c5c-0) (commit! wt5c "text and binary" (cons "a.sc" (defs 'a 'b)) (cons "bin.dat" (bytevector 99 0 99 10)))))
(define st5c (store-from! wt5c c5c-0 "s5c" #t))
(want "IG-5b a range that changes only a binary: nothing-to-import naming it, nothing written, no import block, exit 1"
      (in-order (refused-run st5c wt5c c5c-0 c5c-bin "t5c") (import-blocks st5c) (tag-count st5c "t5c"))
      (list (list (list 'nothing-to-import (list 'from c5c-0) (list 'to c5c-bin) '(not-text "bin.dat")) #t 1) '() 0))
(define d5c (import! st5c wt5c c5c-0 c5c-both "t5c" "" " --dry-run"))
(define r5c (import! st5c wt5c c5c-0 c5c-both "t5c"))
(want "IG-5b a binary and a text file changed: the text change lands, the binary is named in files (dry run and run alike), the export at the tag is <to>'s text"
      (list (ran d5c) (clause-of (answer d5c) 'files) (ran r5c) (clause-of (answer r5c) 'files)
            (texts st5c "a.sc") (file-at st5c "bin.dat") (export-text-is? st5c wt5c c5c-both (tag-cut st5c "t5c")))
      (list '(ok 0) '(files (modified 1) (added 0) (removed 0) (not-text "bin.dat"))
            '(ok 0) '(files (modified 1) (added 0) (removed 0) (not-text "bin.dat"))
            (pieces "a.sc" (defs 'a 'b)) #f #t))

;; ---- IG-6: premises ------------------------------------------------------------------
(printf "== IG-6: premises ==\n")
(define wt6 (repo! "r6"))
(define c6-0 (commit! wt6 "base" (cons "a.sc" (defs 'a 'b 'c)) (cons "o.sc" (defs 'o))))
(define c6-1 (commit! wt6 "edit b" (cons "a.sc" (defs 'a B* 'c))))
(define st6 (store-from! wt6 c6-0 "s6" #t))
(define o6a (named st6 "a.sc" 'a 'b 'c))
;; DURING: a set of a title on BLOCK, then the log's end after it
;; DURING: a set of a title on BLOCK. -> (<log end after it> <block>
;; <version before> <version after>)
(define (set-title! st block)
  (lambda ()
    (let ((before (version st block)))
      (checked "concurrent set" (ask st 'set block "title" "concurrent"))
      (list (position st) block before (version st block)))))
;; a premise refusal, and whether its first clause is exactly the set block
;; with the version the run premised (the one before the set) and the one
;; it found: (block <id> (expected <v>) (current <v>) ...)
(define (premise-head a during)
  (let ((c (and (pair? a) (eq? (car a) 'error) (pair? (cdr a)) (pair? (cddr a)) (caddr a))))
    (list (head-of a)
          (and (pair? during) (list? c) (>= (length c) 4)
               (equal? (list-head c 4) (list 'block (cadr during) (list 'expected (caddr during)) (list 'current (cadddr during))))))))
(define (premise-row h st)
  (list (premise-head (cadr h) (car h)) (and (pair? (car h)) (equal? (position st) (car (car h)))) (tag-cut st "t6") (caddr h)))
(want "IG-6 a concurrent set on a block the import sets: premise-changed, nothing written after it, no tag"
      (premise-row (held! st6 wt6 c6-0 c6-1 "t6" (set-title! st6 (cdr (assq 'b o6a)))) st6)
      (list '((error premise-changed) #t) #t #f 1))
(want "IG-6 a concurrent set on a block the import keeps: premise-changed likewise"
      (premise-row (held! st6 wt6 c6-0 c6-1 "t6" (set-title! st6 (cdr (assq 'a o6a)))) st6)
      (list '((error premise-changed) #t) #t #f 1))
(define st6d (store-from! wt6 c6-0 "s6d" #t))
(want "IG-6 a concurrent set on a touched file's own file block: premise-changed likewise"
      (premise-row (held! st6d wt6 c6-0 c6-1 "t6" (set-title! st6d (file-at st6d "a.sc"))) st6d)
      (list '((error premise-changed) #t) #t #f 1))
;; ITS OWN STORE: the pending block comes from a run stopped after it, not
;; from another row's refused run
(define st6c (store-from! wt6 c6-0 "s6c" #t))
(stopped! st6c wt6 c6-0 c6-1 "t6" 0)
(define pending6c (map car (import-blocks st6c)))
(want "IG-6 on a rerun, a concurrent set on the pending import block: premise-changed likewise"
      (list (length pending6c)
            (premise-row (held! st6c wt6 c6-0 c6-1 "t6" (set-title! st6c (and (pair? pending6c) (car pending6c)))) st6c))
      (list 1 (list '((error premise-changed) #t) #t #f 1)))

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
;; A VARIANT: a store from c7-0, m.sc's blocks named, a run stopped after K
;; items. -> (store names stop-reading pending-id position-after-the-stop),
;; the stop reading being (<head> <items sent> <exit status>)
(define (variant! name k)
  (let* ((st (store-from! wt7 c7-0 name #t)) (old (named st "m.sc" 'a 'b 'c))
         (r (stopped! st wt7 c7-0 c7-1 "t7" k))
         (d (car r))
         (sent (and (pair? d) (value-of d 'answer))))
    (list st old
          (list (and (pair? d) (car d)) (and (pair? d) (value-of d 'items)) (cadr r)
                (and (pair? sent) (eq? (car sent) 'batch) (list? (cadr sent)) (= k (length (cadr sent)))
                     (for-all (lambda (x) (and (pair? x) (eq? (car x) 'ok))) (cadr sent))))
          (let ((b (import-blocks st))) (and (pair? b) (car (car b))))
          (position st)
          (kids st "m.sc"))))
(define (v-st v) (car v)) (define (v-old v) (cadr v)) (define (v-stop v) (caddr v)) (define (v-pending v) (cadddr v))
(define (v-position v) (list-ref v 4)) (define (v-after v) (list-ref v 5))
;; CONVERGED: the rerun answered ok as a rerun, the export at the tag is
;; <to>, m.sc's kept ids are the first run's and every child m.sc held after
;; the stop is still there (all of them are in the target), the one import
;; block is the pending one and is done, and the tag is bound once
(define (converged v r)
  (let ((st (v-st v)))
    (list (ran r) (clause-of (answer r) 'rerun) (export-is? st wt7 c7-1 (tag-cut st "t7"))
          (shape (v-old v) (kids st "m.sc")) (for-all (lambda (id) (and (member id (kids st "m.sc")) #t)) (v-after v))
          (equal? (import-blocks st) (list (cons (v-pending v) "done"))) (tag-count st "t7"))))
(define converged-ok '((ok 0) (rerun #t) #t (new a b c new) #t #t 1))
(define (records-since v) (- (cdr (position (v-st v))) (cdr (v-position v))))
(define (done-blocks st) (map (lambda (b) (cdr b)) (import-blocks st)))

(define v7a (variant! "s7a" 7))
(define half7a (texts (v-st v7a) "m.sc"))
;; THE PENDING PLAN from [B,c,x,a]: LCS B,c (a tie passes over the new
;; piece), x and a relocated (4 kept),
;; x appended and a, B, c moved after it (4 moves), y inserted, n.sc's file
;; block and three children inserted (5 inserts); d.sc is gone: 9 items, then
;; done and the tag
(define before7a (position (v-st v7a)))
(define cut7a (applied-cut (v-st v7a)))
(define after7a (named (v-st v7a) "m.sc" 'B 'c 'x 'a))
(define dry7a-run (import! (v-st v7a) wt7 c7-0 c7-1 "t7" "" " --dry-run"))
(define dry7a (answer dry7a-run))
(define dry7a-wrote (not (equal? (position (v-st v7a)) before7a)))
(define r7a (import! (v-st v7a) wt7 c7-0 c7-1 "t7"))
(want "IG-7 stopped inside a file's items: the rerun converges, every LCS and relocation id kept, the same block done, one tag; a third run is base-mismatch"
      (in-order (v-stop v7a) half7a (list dry7a dry7a-wrote (cadr dry7a-run)) (converged v7a r7a)
                (list (listed r7a 'kept) (listed r7a 'set) (equal? (listed r7a 'moved) (ids-of after7a 'x 'a 'B 'c))
                      (equal? (listed r7a 'inserted) (cons (list-ref (kids (v-st v7a) "m.sc") 4) (cons (file-at (v-st v7a) "n.sc") (kids (v-st v7a) "n.sc"))))
                      (listed r7a 'deleted))
                (head-of (answer (import! (v-st v7a) wt7 c7-0 c7-1 "t7"))))
      (list '(stopped 7 3 #t) (list B* (d 'c) (d 'x) (d 'a))
            (list (list 'ok '(dry-run #t) '(state pending) (list 'read-cut cut7a) '(files (modified 1) (added 1) (removed 1))
                        '(blocks (kept 4) (set 0) (inserted 5) (deleted 0) (moved 4)) '(items 11))
                  #f 0)
            converged-ok (list 4 '() #t #t '()) '(error base-mismatch)))

(define v7b (variant! "s7b" 0))
(define mid7b (list (export-is? (v-st v7b) wt7 c7-0) (done-blocks (v-st v7b))))
(want "IG-7 stopped after the import block alone: the export is still <from>, and the rerun is accepted as pending and converges"
      (in-order (v-stop v7b) mid7b (converged v7b (import! (v-st v7b) wt7 c7-0 c7-1 "t7")))
      (list '(stopped 0 3 #t) (list #t '("pending")) converged-ok))

(define v7c (variant! "s7c" 11))
(define holder7c (holders (v-st v7c) "n.sc"))
(define mid7c (list (length holder7c) (kids (v-st v7c) "n.sc") (file-at (v-st v7c) "d.sc") (texts (v-st v7c) "m.sc")))
(define r7c (import! (v-st v7c) wt7 c7-0 c7-1 "t7"))
(want "IG-7 stopped after an added file's file block, before its children: the same one holder, its children added"
      (in-order (v-stop v7c) mid7c (converged v7c r7c) (equal? (holders (v-st v7c) "n.sc") holder7c) (texts (v-st v7c) "n.sc"))
      (list '(stopped 11 3 #t) (list 1 '() #f (list (d 'x) (d 'a) B* (d 'c) (d 'y))) converged-ok #t (list (d 'p) (d 'q) (d 'r))))

(define v7d (variant! "s7d" 2))
(define d-file (file-at (v-st v7d) "d.sc"))
(define mid7d (texts (v-st v7d) "d.sc"))
(define r7d (import! (v-st v7d) wt7 c7-0 c7-1 "t7"))
(want "IG-7 stopped after some of a removed file's children: the rest and the file block deleted"
      (in-order (v-stop v7d) mid7d (converged v7d r7d) (file-at (v-st v7d) "d.sc") (and d-file (deleted? (v-st v7d) d-file)))
      (list '(stopped 2 3 #t) (list (d 'u)) converged-ok #f #t))

(define v7e (variant! "s7e" 14))
(define mid7e (list (export-is? (v-st v7e) wt7 c7-1) (done-blocks (v-st v7e))))
(define r7e (import! (v-st v7e) wt7 c7-0 c7-1 "t7"))
(want "IG-7 stopped after every content item, before done: the rerun finds no work and writes two records, done and the tag"
      (in-order (v-stop v7e) mid7e (converged v7e r7e)
                (map (lambda (k) (listed r7e k)) '(kept set inserted deleted moved)) (records-since v7e))
      (list '(stopped 14 3 #t) (list #t '("pending")) converged-ok '(8 () () () ()) 2))

(define v7f (variant! "s7f" 15))
(define mid7f (list (done-blocks (v-st v7f)) (tag-count (v-st v7f) "t7")))
(define dry7f-run (import! (v-st v7f) wt7 c7-0 c7-1 "t7" "" " --dry-run"))
(define dry7f (answer dry7f-run))
(define r7f (import! (v-st v7f) wt7 c7-0 c7-1 "t7"))
(want "IG-7 stopped after done, before the tag: done without tag (its dry run plans the tag alone), the rerun only tags: one record"
      (in-order (v-stop v7f) mid7f (list (value-of dry7f 'state) (clause-of dry7f 'blocks) (value-of dry7f 'items) (cadr dry7f-run))
                (converged v7f r7f) (map (lambda (k) (listed r7f k)) '(kept set inserted deleted moved)) (records-since v7f))
      (list '(stopped 15 3 #t) (list '("done") 0)
            '(done-without-tag (blocks (kept 0) (set 0) (inserted 0) (deleted 0) (moved 0)) 1 0)
            converged-ok '(0 () () () ()) 1))

;; DONE WITHOUT TAG NEEDS THE EXPORT AT <to>: a block's src edited after the
;; stop, the rerun is base-mismatch and writes nothing
(define v7l (variant! "s7l" 15))
(checked "edit after the stop" (ask (v-st v7l) 'set (car (kids (v-st v7l) "m.sc")) "src" "(define x 2)\n"))
(define p7l (position (v-st v7l)))
(define cut7l (applied-cut (v-st v7l)))
(want "IG-7 a done block, its tag unbound, but the export no longer <to>: base-mismatch naming the path, nothing written, no tag"
      (in-order (v-stop v7l) (refused-run (v-st v7l) wt7 c7-0 c7-1 "t7") (equal? (position (v-st v7l)) p7l) (tag-count (v-st v7l) "t7"))
      (list '(stopped 15 3 #t) (list (list 'base-mismatch (list 'store-cut cut7l) (list 'rev c7-0) '(differing "d.sc" "m.sc" "n.sc")) #t 1) #t 0))

;; A DONE BLOCK NAMES ITS TAG: a rerun asking another tag is not done
;; without tag, so it is base-mismatch and binds neither name
(define v7m (variant! "s7m" 15))
(define cut7m (applied-cut (v-st v7m)))
(want "IG-7 a done block whose tag is another name: base-mismatch, nothing written, neither tag bound"
      (in-order (v-stop v7m) (refused-run (v-st v7m) wt7 c7-0 c7-1 "t7x") (tag-count (v-st v7m) "t7x") (tag-count (v-st v7m) "t7"))
      (list '(stopped 15 3 #t) (list (list 'base-mismatch (list 'store-cut cut7m) (list 'rev c7-0) '(differing "d.sc" "m.sc" "n.sc")) #t 1) 0 0))

(define v7g (variant! "s7g" 0))
(want "IG-7 a pending import block of another range: pending-import naming it, nothing written, exit 1"
      (in-order (refused-run (v-st v7g) wt7 c7-0 c7-2 "t7g") (records-since v7g))
      (list (list (list 'pending-import (list 'from c7-0) (list 'to c7-1) (list 'block (v-pending v7g))) #t 1) 0))

;; A DONE BLOCK FOR THE RANGE STAYS WHEN THE EXPORT IS BACK AT <from>: the
;; range landed (tag t7x-fwd), then the reverse range (tag t7x-rev) brought
;; the export back to <from>. The range again under a third name is not
;; pending, not done without tag (the name differs), and nothing differs from
;; <from>: only the done block of the range refuses it. Without that the
;; range would land twice. The two tags are read after it, so the refusal is
;; this one and not a tag name in use.
(define wt7x (repo! "r7x"))
(define c7x-0 (commit! wt7x "base" (cons "a.sc" (defs 'a 'b))))
(define c7x-1 (commit! wt7x "grow" (cons "a.sc" (defs 'a 'b 'c))))
(define st7x (store-from! wt7x c7x-0 "s7x" #t))
(define fwd7x (ran (import! st7x wt7x c7x-0 c7x-1 "t7x-fwd")))
(define rev7x (ran (import! st7x wt7x c7x-1 c7x-0 "t7x-rev")))
(define cut7x (applied-cut st7x))
(want "IG-7 the range again after its reverse: the export is <from>, a done block of the range refuses it, base-mismatch with nothing differing, nothing written, exit 1, the reverse's tag bound and the new one not"
      (in-order fwd7x rev7x (export-is? st7x wt7x c7x-0)
                (refused-run st7x wt7x c7x-0 c7x-1 "t7x-again")
                (tag-count st7x "t7x-rev") (tag-count st7x "t7x-again"))
      (list '(ok 0) '(ok 0) #t
            (list (list 'base-mismatch (list 'store-cut cut7x) (list 'rev c7x-0) '(differing)) #t 1)
            1 0))

(define v7h (variant! "s7h" 3))
(define version7h (version (v-st v7h) (v-pending v7h)))
(define mid7h (list (and (file-at (v-st v7h) "d.sc") #t) (kids (v-st v7h) "d.sc")))
(checked "outside drift" (ask (v-st v7h) 'set (car (kids (v-st v7h) "o.sc")) "src" "(define o 2)\n"))
(define p7h (position (v-st v7h)))
(define cut7h (applied-cut (v-st v7h)))
(define r7h (import! (v-st v7h) wt7 c7-0 c7-1 "t7"))
(want "IG-7 a pending prefix and a commit outside the range: base-mismatch naming only that path, nothing written, the pending block untouched"
      (in-order (v-stop v7h) mid7h (refusal r7h) (equal? (position (v-st v7h)) p7h)
                (and version7h (equal? (version (v-st v7h) (v-pending v7h)) version7h)) (done-blocks (v-st v7h)))
      (list '(stopped 3 3 #t) '(#t ()) (list 'base-mismatch (list 'store-cut cut7h) (list 'rev c7-0) '(differing "o.sc")) #t #t '("pending")))

;; A REMOVED FILE'S BLOCKS ARE PREMISED: a fresh run held, a set on d.sc's
;; first child, which the batch deletes
(define st7k (store-from! wt7 c7-0 "s7k" #t))
(want "IG-7 a concurrent set on a block of a file the range removes: premise-changed, nothing written after it, no tag"
      (let ((h (held! st7k wt7 c7-0 c7-1 "t7" (set-title! st7k (car (kids st7k "d.sc"))))))
        (list (premise-head (cadr h) (car h)) (and (pair? (car h)) (equal? (position st7k) (car (car h)))) (tag-cut st7k "t7") (caddr h)))
      (list '((error premise-changed) #t) #t #f 1))
;; THE PREMISES OF A RERUN: a block the prefix inserted (x, at item 4) is
;; premised; and the tag-only run of done-without-tag is premised
(define (held-refused st during)
  (let ((h (held! st wt7 c7-0 c7-1 "t7" during)))
    (list (premise-head (cadr h) (car h)) (and (pair? (car h)) (equal? (position st) (car (car h)))) (tag-cut st "t7") (caddr h))))
(define v7i (variant! "s7i" 9))
(define mid7i (texts (v-st v7i) "m.sc"))
(define x7i (let ((st (v-st v7i))) (find (lambda (id) (equal? (src-text (code-field (open-and-reduce st) id 'src)) (d 'x))) (kids st "m.sc"))))
(want "IG-7 on a rerun, a concurrent set on a block the interrupted run inserted: premise-changed, nothing written after it, no tag"
      (in-order (v-stop v7i) mid7i (held-refused (v-st v7i) (set-title! (v-st v7i) x7i)))
      (list '(stopped 9 3 #t) (list (d 'x) (d 'a) B* (d 'c)) (list '((error premise-changed) #t) #t #f 1)))
(define v7j (variant! "s7j" 15))
(define m7j (car (kids (v-st v7j) "m.sc")))
(want "IG-7 done without tag: a concurrent set on a range block during the tag-only run is premise-changed, no tag"
      (in-order (v-stop v7j) (held-refused (v-st v7j) (set-title! (v-st v7j) m7j)))
      (list '(stopped 15 3 #t) (list '((error premise-changed) #t) #t #f 1)))

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
      (in-order seed8 (ran r8) (export-is? st8 wt8 c8-1 (tag-cut st8 "t8"))
                (equal? (texts st8 "f.sc") (pieces "f.sc" (defs 'a B* 'c 'd))))
      (list '(1 1) '(ok 0) #t #t))
(want "IG-8 a file the splitter cannot prove imports to one block holding it whole; from three blocks, the first keeps its id and the others are deleted"
      (list (pieces "w.sc" w-text2) (texts st8 "w.sc")
            (pieces "m9.sc" m9-text) (row2 "m9.sc" o9 m9-text) (map (lambda (n) (deleted? st2 (cdr (assq n o9)))) '(b c)))
      (list (list w-text2) (list w-text2) (list m9-text) (list '(a) #t) '(#t #t)))

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
      (in-order (head-of exported9) (ran r9) (tool! "edit-sync.sc" st9 edit9 "--writer" "w1"))
      (list 'ok '(ok 0) (list 'error 'stale (list 'block b9) '(reason committed-moved))))

;; ---- IG-10: post-verify --------------------------------------------------------------
(printf "== IG-10: post-verify ==\n")
(define wt10 (repo! "r10"))
(define c10-0 (commit! wt10 "base" (cons "a.sc" (defs 'a 'b 'c))))
(define c10-1 (commit! wt10 "edit b" (cons "a.sc" (defs 'a B* 'c))))
(define st10 (store-from! wt10 c10-0 "s10" #t))
(define h10 (held! st10 wt10 c10-0 c10-1 "t10"
                   (lambda ()
                     (batch-checked "sibling" (ask st10 'batch (format "((insert ~s #f ((kind . code) (mode . text) (src . ~s))))"
                                                                 (file-at st10 "a.sc") (string->utf8 "(define z 1)\n"))))
                     #t)))
(want "IG-10 an unpremised sibling written during the hold: the batch runs, the tag binds it, import-diverged naming the path, the block done"
      (list (car h10) (cadr h10) (and (tag-cut st10 "t10") #t) (done-blocks st10) (caddr h10))
      (list #t (list 'error 'import-diverged '(differing "a.sc") (list 'tag "t10" (list 'cut (tag-cut st10 "t10")))) #t '("done") 1))

;; ---- IG-12: what a base is compared over ------------------------------------------
;;
;; ONE JUDGEMENT OF A BASE ENTRY, landing-files.sc's, which publish-landing
;; asks too, so the importer and the publisher compare a store's export over
;; the same paths. Read through eval, so a tree without it reads RAISED here.
(printf "== IG-12: the base entry's judgement, shared with publish-landing ==\n")
(want "IG-12 the judgement of a base entry: in for a text file at 100644 or 100755; not-text, link, submodule, mode and dot-named otherwise"
      (let ((judge (eval 'base-entry-judgement (environment '(theourgia landing-files)))))
        (map (lambda (c) (apply judge c))
             (list (list "a.sc" "100644" (string->utf8 "(define a 1)\n"))
                   (list "run.sh" "100755" (string->utf8 "#!/bin/sh\n"))
                   (list "bin.dat" "100644" (bytevector 0 1 2 255))
                   (list "link" "120000" #f)
                   (list "sub" "160000" #f)
                   (list "odd" "100664" #f)
                   (list "dir/.hidden" "100644" (string->utf8 "x\n")))))
      '(in in not-text link submodule mode dot-named))
;; A BINARY FILE UNCHANGED IN THE BASE: the store does not hold it (the
;; importer imports text), and it is in neither the domain nor the exempt
;; paths, so its absence from the export is no difference: the range imports.
(define wt12 (repo! "r12"))
(define c12-0 (commit! wt12 "base" (cons "a.sc" (defs 'a)) (cons "bin.dat" (bytevector 0 1 2 255))))
(define c12-1 (commit! wt12 "change" (cons "a.sc" (defs 'a 'b))))
(define st12 (store-from! wt12 c12-0 "s12" #t))
(want "IG-12 a binary file unchanged in the base: the store does not hold it, and the range imports"
      (in-order (file-at st12 "bin.dat") (ran (import! st12 wt12 c12-0 c12-1 "t12")) (texts st12 "a.sc"))
      (list #f '(ok 0) (list (d 'a) (d 'b))))

;; ---- IG-11: the dry run ---------------------------------------------------------------
(printf "== IG-11: the dry run ==\n")
(define wt11 (repo! "r11"))
;; [a,b,c,d] -> [c,a,B,e]: c relocated and a kept by the LCS (2 kept), b set
;; to B, e inserted, d deleted; placement moves c (appended), a after c and
;; b after a (3 moves): 6 items, then done and the tag
(define c11-0 (commit! wt11 "base" (cons "a.sc" (defs 'a 'b 'c 'd))))
(define c11-1 (commit! wt11 "reorder, edit, append, delete" (cons "a.sc" (defs 'c 'a B* 'e))))
(define st11 (store-from! wt11 c11-0 "s11" #t))
(define p11 (position st11))
(define cut11 (applied-cut st11))
(define r11 (import! st11 wt11 c11-0 c11-1 "t11" "" " --dry-run"))
(want "IG-11 --dry-run: the state, the read cut, the files and every count of the plan, nothing written"
      (in-order (answer r11) (equal? (position st11) p11) (import-blocks st11) (cadr r11))
      (list (list 'ok '(dry-run #t) '(state fresh) (list 'read-cut cut11) '(files (modified 1) (added 0) (removed 0))
                  '(blocks (kept 2) (set 1) (inserted 1) (deleted 1) (moved 3)) '(items 8))
            #t '() 0))

(sh "rm -rf " (quoted root))
(printf "rows: ~a\n~a failures\nimport-git complete\n" rows bad)
(exit (if (zero? bad) 0 1))
