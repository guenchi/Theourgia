#!r6rs
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

;; THE REVIEW CHANNEL: scope, review-results, collect-into, collect, and the
;; mail the reader is told of (the hooks' command and the doorbell).
;;
;; The rows run in process first -- every store on its local route -- and
;; then again with daemons serving both stores, through the command line.
;; A second writer is forged where a row needs what this store's own write
;; path refuses to make: a contested field, a title on two lines, a
;; reserved relation.

(import (chezscheme)
        (theourgia rpc)
        (only (theourgia reduce) block-id state-read state-outline outline-subtree
              reduce-applied-cut)
        (only (theourgia store) open-and-reduce)
        (only (theourgia wire) encode-record storable-encode)
        (only (theourgia log) log-publish! segment-sha)
        (only (theourgia client) call! request-frame socket-path answer-field)
        (only (theourgia arguments) parse-arguments argument-positionals)
        (only (theourgia extensions) extension-verbs))

(define bad 0)
(define rows 0)
(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ name got expected)
     (with-expected name expected (x) (want-1 name (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                             (condition-message e) e)
                                 (if (and (condition? e) (irritants-condition? e)) (condition-irritants e) '()))))
               got) x)))))

(define (string-contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (file-text path)
  (if (file-exists? path)
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))
      ""))
(define (first-datum text)
  (guard (e (#t (list 'UNREADABLE text)))
    (let ((d (read (open-string-input-port text)))) (if (eof-object? d) (list 'EMPTY) d))))
(define (sleep-ms n) (sleep (make-time 'time-duration (* (mod n 1000) 1000000) (div n 1000))))

(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
(define here (current-directory))
(define tree (string-append here "/.."))
(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/channel-" (number->string (get-process-id))))
(define sock-root (string-append (or (getenv "THEOURGIA_TEST_SOCK") "/tmp") "/ch" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "' '" sock-root "'; mkdir -p '" root "/home' '" root "/bin' '" sock-root "/run'"))
;; THE TEARDOWN RUNS ON EVERY EXIT PATH. Daemons, the doorbell and the
;; subscribers this file starts all name a store under `root` on their
;; command lines, so one match stops them; it is written with a bracket
;; class so the shell running pkill does not match its own command line. An
;; uncaught raise reaches the base exception handler, which this file wraps
;; to tear down first; the normal end calls it too. Twice, a second apart:
;; the doorbell subscribes again after a stream ends.
(define root-pattern
  (let ((name (let loop ((i (- (string-length root) 1)))
                (if (char=? (string-ref root i) #\/) (substring root (+ i 1) (string-length root)) (loop (- i 1))))))
    (string-append "[" (substring name 0 1) "]" (substring name 1 (string-length name)) "/")))
(define (teardown!)
  (system (string-append "pkill -f '" root-pattern "' 2>/dev/null; sleep 1; pkill -f '" root-pattern "' 2>/dev/null")))
;; A warning reaches the same handler and returns; only what ends the run
;; tears down.
(let ((handler (base-exception-handler)))
  (base-exception-handler
    (lambda (c)
      (unless (and (condition? c) (not (serious-condition? c))) (teardown!))
      (handler c))))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(putenv "THEOURGIA_RUN" (string-append sock-root "/run"))
(define (env extra)
  (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_HOME=" root "/home "
                 "THEOURGIA_RUN=" sock-root "/run THEOURGIA_TRACE=1 " extra " "))
(define (sh-out name command)
  (let ((out (string-append root "/" name ".out")))
;; A command that names its own input keeps it: a second `<` would win.
    (system (string-append command " > '" out "' 2> '" root "/" name ".err'"
                           (if (string-contains? command " < ") "" " < /dev/null")))
    (file-text out)))
(define (cli name actor . args)
  (sh-out name (string-append (env "") "scheme --script ../theourgia.sc"
                              (apply string-append (map (lambda (a) (string-append " '" a "'")) args))
                              " --actor " actor " --wire")))

;; THE ACCESSOR THIS CHANGE ADDS IS LOOKED UP WHEN IT IS CALLED, not imported:
;; on a tree without it the rows still run and say what is missing, instead
;; of the whole file failing to load.
(define (block-creation r id) ((eval 'block-creation (environment '(theourgia reduce))) r id))
(define (event-actor r e) ((eval 'event-actor (environment '(theourgia reduce))) r e))
(define (link-events r from rel to) ((eval 'state-link-events (environment '(theourgia reduce))) r from rel to))
;; `batch` on the command line reads its intents from standard input.
(define (cli-batch name actor store intents)
  (let ((in (string-append root "/" name ".in")))
    (call-with-output-file in (lambda (p) (put-string p intents)) 'replace)
    (sh-out name (string-append (env "") "scheme --script ../theourgia.sc batch --store '" store "' --actor " actor
                                " --wire < '" in "'"))))

;; `query` is registered from outside the core table, as core.sc registers it.
(register-verbs! extension-verbs)

(define M (string-append root "/main"))
(define X (string-append root "/scoped"))
(define (run store actor . args) (rpc-dispatch store args actor))
;; scope and collect are core.sc's programs, not dispatcher verbs; in
;; process they are run through their library, as core.sc runs them. A
;; request of the wrong shape answers the bare word `usage` there, and
;; core.sc answers the catalogue's form for it (a row below).
(define (chan name) (eval name (environment '(theourgia channel))))
(define (scope-run store actor . args)
  (let ((nodes (parse-arguments 'scope args)))
    ((chan 'scope-verb) store actor (argument-positionals nodes) nodes #f)))
(define (collect-run store actor dir) ((chan 'collect-verb) store actor (list dir) #f))
(define (new-id answer)
  (let* ((ev (and (pair? answer) (eq? (car answer) 'ok) (assq 'events (cdr answer))))
         (e (and ev (pair? (cadr ev)) (car (cadr ev)))))
    (and (pair? e) (block-id (car e) (cdr e)))))
(define (clause a key)
  (let ((c (and (pair? a) (list? a) (find (lambda (x) (and (pair? x) (eq? (car x) key))) (cdr a)))))
    (and c (cdr c))))
(define (one a key) (let ((c (clause a key))) (and (pair? c) (car c))))
(define (state store) (open-and-reduce store))
(define (fields-of store id)
  (let ((b (state-read (state store) id))) (and b (cdr (assq 'fields b)))))
(define (field-of store id name)
  (let ((f (assq name (or (fields-of store id) '())))) (and f (cdr f))))
(define (edges-of store id)
  (let ((b (state-read (state store) id))) (and b (cdr (assq 'edges b)))))
(define (parent-of store id)
  (let ((b (state-read (state store) id))) (and b (car (cdr (assq 'position b))))))
(define (events-applied store) (apply + (map cdr (reduce-applied-cut (state store)))))
(define (outline-order store)
  (let ((rs (state-outline (state store))))
    (apply append (map (lambda (r) (outline-subtree rs (caddr r)))
                       (filter (lambda (r) (eq? (car r) 'root)) rs)))))
(define (without keys fs) (filter (lambda (f) (not (memq (car f) keys))) fs))

;; ---- the main store -----------------------------------------------------------
;;
;;   R1 "Root one"            a root
;;     A (src "a")            A cites B, A cites O (O is outside: dropped), B cites R1
;;       B                    its title contested: the author's set and a second writer's
;;   O "Outside"
;;   R2, R3                   R2 calls R3, a reserved relation, written by the second writer
;;   U                        the second writer's block, its title on two lines
;; then t1; after it, A's src changes and R1 gains a child.

(run M "author" 'init)
(define R1 (new-id (run M "author" 'insert "--title" "Root one" "--text" "r1")))
(define A (new-id (run M "author" 'insert "--under" R1 "--title" "A" "--text" "a")))
(define B (new-id (run M "author" 'insert "--under" A "--title" "B")))
(define O (new-id (run M "author" 'insert "--title" "Outside")))
(define R2 (new-id (run M "author" 'insert "--title" "R2")))
(define R3 (new-id (run M "author" 'insert "--title" "R3")))
(run M "author" 'link A "cites" B)
(run M "author" 'link A "cites" O)
(run M "author" 'link B "cites" R1)
(define author-writer (car (car (reduce-applied-cut (state M)))))
(define past-of-B (cdr (assoc author-writer (reduce-applied-cut (state M)))))
(run M "author" 'set B "title" "mine")
(define (bytevector-append-all . bvs)
  (call-with-bytevector-output-port (lambda (p) (for-each (lambda (b) (put-bytevector p b)) bvs))))
(define rival
  (let ((deps (list (cons author-writer past-of-B))))
    (apply bytevector-append-all
           (list (encode-record 1 1789000000001 "peer" deps (storable-encode (list 'set B 'title "rival")))
                 (encode-record 2 1789000000002 "peer" deps
                                (storable-encode '(put ((kind . section) (title . "two\nlines") (parent . root) (ord . 100)))))
                 (encode-record 3 1789000000003 "peer" deps (storable-encode (list 'link R2 'calls R3)))))))
(define published (log-publish! M "rivalzzz" 1 rival (segment-sha rival)))
(define U (block-id "rivalzzz" 2))
;; A TAG BINDS THE CUT BEFORE ITS OWN RECORD, so the literal is read first.
(define cut-t1 (format "~s" (reduce-applied-cut (state M))))
(run M "author" 'tag "t1")
(run M "author" 'set A "src" "changed after the cut")
(run M "author" 'insert "--under" R1 "--title" "Late")

(want "SETUP the seed: B's title contested, U and R2's edge from the second writer, t1 tagged"
      (in-order (car (field-of M B 'title)) (field-of M U 'title)
                (and (member '(calls . "x") (map (lambda (e) (cons (car e) "x")) (edges-of M R2))) #t))
      (list 'conflict "two\nlines" #t))

;; ---- P4: scope ---------------------------------------------------------------------

(define main-before (events-applied M))
(define t1-before (run M "author" 'read R1 "--cut" "t1" "--recursive"))
;; Guarded, so a tree without scope reaches the row below and says so there.
(define S (guard (e (#t (list 'RAISED (if (message-condition? e) (condition-message e) e))))
            (scope-run M "author" X "--cut" "t1" "--roots" R1 "--for" "codex")))
(define L (one S 'letter))
(define LC (one S 'letter-copy))
(define BL (one S 'baseline))

(want "P4 scope answers ok with the counts, the dropped edge and the contested field"
      (in-order (car S) (one S 'scope) (one S 'cut) (string? L) (string? LC) (string? BL)
                (one S 'blocks) (one S 'edges) (one S 'dropped-edges) (one S 'contested-fields)
                (filter (lambda (c) (and (pair? c) (memq (car c) '(dropped-edge contested)))) (cdr S)))
      (list 'ok X cut-t1 #t #t #t 3 2 1 1 (list (list 'dropped-edge A 'cites O) (list 'contested B 'title))))

(define (origin id) (field-of X id 'origin))
(want "P4 the scoped store: the letter copy, then the copies in outline order, a root at the top level"
      (in-order (map (lambda (id) (if (equal? id LC) 'letter (origin id))) (outline-order X))
                (parent-of X LC)
                (let ((r1c (find (lambda (id) (equal? (origin id) R1)) (outline-order X)))) (parent-of X r1c)))
      (list (list 'letter R1 A B) 'root 'root))

(define t1-items (cdr (assq 'items (cdr t1-before))))
(define (t1-fields id) (cdr (assq 'fields (find (lambda (r) (equal? (cdr (assq 'id r)) id)) t1-items))))
(define (copy-of id) (find (lambda (c) (equal? (origin c) id)) (outline-order X)))
(want "P4 every copy carries the block's settled fields at the cut, origin and origin-cut, and no contested field"
      (map (lambda (id)
             (let ((c (copy-of id)))
               (in-order (equal? (without '(origin origin-cut) (fields-of X c))
                                 (filter (lambda (f) (not (and (pair? (cdr f)) (eq? (cadr f) 'conflict)))) (t1-fields id)))
                         (field-of X c 'origin-cut))))
           (list R1 A B))
      (list (list #t cut-t1) (list #t cut-t1) (list #t cut-t1)))
(want "P4 A's copy holds the text at the cut, not the later one; the block written after the cut is not there"
      (in-order (field-of X (copy-of A) 'src) (length (outline-order X)))
      '("a" 4))
(want "P4 the copies' edges are exactly the retained edges, between the copies"
      (in-order (edges-of X (copy-of A)) (edges-of X (copy-of B)) (edges-of X (copy-of R1)))
      (list (list (cons 'cites (copy-of B))) (list (cons 'cites (copy-of R1))) '()))
(want "P4 the letter copy carries to, status, cut, roots, scope, baseline and letter, as strings"
      (map (lambda (k) (field-of X LC k)) '(kind to status cut roots scope baseline letter))
      (list 'doc "codex" "unread" cut-t1 (list R1) X BL L))
(want "P4 the main store gained exactly one record, the top-level letter; its state at t1 is unchanged"
      (in-order (- (events-applied M) main-before) (parent-of M L)
                (map (lambda (k) (field-of M L k)) '(kind to status cut roots scope))
                (equal? (run M "author" 'read R1 "--cut" "t1" "--recursive") t1-before))
      (list 1 'root (list 'doc "codex" "unread" cut-t1 (list R1) X) #t))

;; the reader edits a copy; the baseline still serves what was written
(run X "codex" 'set (copy-of A) "src" "edited by the reader")
(want "P4 read --cut <baseline> in the scoped store serves the copy as written after the reader edits it"
      (in-order (field-of X (copy-of A) 'src)
                (let ((a (run X "codex" 'read (copy-of A) "--cut" BL)))
                  (cdr (assq 'src (cdr (assq 'fields (cadr a)))))))
      '("edited by the reader" "a"))

(define (dir-exists? d) (file-exists? d))
(want "P4 an existing directory is refused before anything is written"
      (in-order (scope-run M "author" X "--cut" "t1" "--roots" R1 "--for" "codex") (- (events-applied M) main-before))
      (list (list 'error 'scope-exists (list 'dir X)) 1))
(want "P4 a cut the store cannot serve is its refusal, and nothing is written in either place"
      (let ((a (scope-run M "author" (string-append root "/x2") "--cut" "((\"nosuchwr\" . 5))" "--roots" R1 "--for" "codex")))
        (in-order (car a) (dir-exists? (string-append root "/x2")) (- (events-applied M) main-before)))
      (list 'error #f 1))
(want "P4 a title the write refuses refuses the whole scope, and nothing is written in either place"
      (let ((a (scope-run M "author" (string-append root "/x3") "--cut" "t1" "--roots" U "--for" "codex")))
        (in-order (list-head a 2) (dir-exists? (string-append root "/x3")) (- (events-applied M) main-before)))
      (list '(error scope-unwritable) #f 1))
(want "P4 a reserved relation between retained blocks refuses the whole scope"
      (let ((a (scope-run M "author" (string-append root "/x4") "--cut" "t1" "--roots" R2 "--roots" R3 "--for" "codex")))
        (in-order a (dir-exists? (string-append root "/x4"))))
      (list (list 'error 'scope-unwritable (list 'items (list (list R2 'calls 'relation-name-reserved)))) #f))
(want "P4 roots that overlap are refused"
      (car (cdr (scope-run M "author" (string-append root "/x5") "--cut" "t1" "--roots" R1 "--roots" A "--for" "codex")))
      'bad-request)
(want "P4 a scope missing --cut, --roots or --for answers usage"
      (map (lambda (args) (apply scope-run M "author" (string-append root "/x6") args))
           '(("--roots" "x" "--for" "codex") ("--cut" "t1" "--for" "codex") ("--cut" "t1" "--roots" "x")))
      '(usage usage usage))

;; ---- P4: what a scope refuses or keeps beyond the seed ------------------------------
;;
;; A root retired at the cut; a document a second writer placed under a block
;; (replay keeps it, the write path would refuse to place it); a block whose
;; src is a bytevector (read through a daemon below); a field whose one value
;; merely starts with the word `conflict`.
(define (now-literal) (format "~s" (reduce-applied-cut (state M))))
(define Z (new-id (run M "author" 'insert "--title" "Z")))
(run M "author" 'del Z)
(define author-now (cdr (assoc author-writer (reduce-applied-cut (state M)))))
(define rival2
  (let ((deps (list (cons author-writer author-now))))
    (bytevector-append-all
      (encode-record 4 1789000000004 "peer" deps
                     (storable-encode (list 'put (list '(kind . doc) '(title . "nested doc") (cons 'parent R2) '(ord . 300)))))
      (encode-record 5 1789000000005 "peer" deps
                     (storable-encode '(put ((kind . section) (title . "bytes") (src . #vu8(65 66)) (glyph . #\a) (pair . #(1 2)) (parent . root) (ord . 400))))))))
(log-publish! M "rivalzzz" 2 rival2 (segment-sha rival2))
(define NESTED (block-id "rivalzzz" 4))
(define BV (block-id "rivalzzz" 5))
(run M "author" 'batch (format "~s" (list (list 'set R3 'note '(conflict "x")))))
(want "P4 a root retired at the cut is refused, and nothing is made"
      (in-order (scope-run M "author" (string-append root "/x11") "--cut" (now-literal) "--roots" Z "--for" "codex")
                (dir-exists? (string-append root "/x11")))
      (list (list 'error 'bad-request '(reason root-deleted) (list 'block Z)) #f))
(want "P4 a document the write would not place under its parent's copy refuses the scope before anything is written"
      (let ((n (events-applied M)))
        (in-order (scope-run M "author" (string-append root "/x12") "--cut" (now-literal) "--roots" R2 "--for" "codex")
                  (dir-exists? (string-append root "/x12")) (- (events-applied M) n)))
      (list (list 'error 'scope-unwritable (list 'items (list (list NESTED 'kind 'doc-must-be-top-level)))) #f 0))
;; THE MAIN LETTER IS WRITTEN LAST. A one-shot fault (ffi.sc, THEOURGIA_FAULT)
;; fails the first flush of a segment under the new store's writers -- the
;; import's -- in a scope run as the command line runs it: the answer names
;; the step, the new store's directory is left, and the main store has no
;; new record. A letter written before the copies would be on the main log.
(define X14 (string-append root "/x14"))
(define faulted
  (let ((n (events-applied M)))
    (let ((a (first-datum (sh-out "scope-faulted"
                                  (string-append (env (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=fsync-fail@commit:file=" X14 "/writers/"))
                                                 "scheme --script ../core.sc scope '" X14 "' --cut t1 --roots " R1
                                                 " --for codex --store '" M "' --actor author --wire")))))
      (list a (- (events-applied M) n)))))
(want "P4 a scope whose import fails answers scope-failed at the import, leaves the new store, and writes nothing in the main store"
      (in-order (and (pair? (car faulted)) (list? (car faulted)) (>= (length (car faulted)) 4) (list-head (car faulted) 4))
                (dir-exists? X14) (cadr faulted))
      (list (list 'error 'scope-failed (list 'dir X14) '(step import)) #t 0))
(define S13 (scope-run M "author" (string-append root "/x13") "--cut" (now-literal) "--roots" R3 "--for" "codex"))
(want "P4 a field whose one value starts with `conflict` is copied, and is not listed as contested"
      (in-order (car S13) (one S13 'contested-fields)
                (let ((c (find (lambda (id) (equal? (field-of (string-append root "/x13") id 'origin) R3))
                               (outline-order (string-append root "/x13")))))
                  (and c (field-of (string-append root "/x13") c 'note))))
      '(ok 0 (conflict "x")))

;; ---- P4: the reader's results ----------------------------------------------------------
;;
;;   V  a verdict under the letter copy, with the four condition fields
;;     F1  about A's copy; the author also sets a field on it
;;     F2  about F1, a reader block
;;     F3  with no about
;;   N  a block the author inserts under the letter copy: not a result

(define (insert-as store actor parent fields)
  (let ((a (run store actor 'batch (format "~s" (list (list 'insert parent #f fields))))))
    (new-id (car (cadr a)))))
(define V (insert-as X "codex" LC '((kind . section) (title . "Verdict") (model . "m") (approval . "never")
                                     (sandbox . "read-only") (effort . "high"))))
(define F1 (insert-as X "codex" V '((kind . section) (title . "F1") (src . "one"))))
(define F2 (insert-as X "codex" V '((kind . section) (title . "F2"))))
(define F3 (insert-as X "codex" V '((kind . section) (title . "F3"))))
(run X "codex" 'link F1 "about" (copy-of A))
(run X "codex" 'link F2 "about" F1)
(define N (insert-as X "author" LC '((kind . section) (title . "author note"))))
(run X "author" 'set F1 "keywords" "set by the author")

(define RR (run X "codex" 'review-results))
(define (result id) (find (lambda (b) (equal? (cadr b) id)) (cdr (assq 'blocks (cdr (one-results RR))))))
(define (one-results a) (find (lambda (c) (and (pair? c) (eq? (car c) 'results))) (cdr a)))
(want "R review-results: the reader's four blocks, in outline order; not the copies, the letter copy or the author's"
      (in-order (car RR) (map cadr (cdr (assq 'blocks (cdr (one-results RR)))))
                (cadr (assq 'letter (cdr (one-results RR)))) (cadr (assq 'to (cdr (one-results RR)))))
      (list 'ok (list V F1 F2 F3) L "codex"))
(want "R each result's parent, abouts, and every field with its actor"
      (in-order (cadr (assq 'parent (cddr (result V)))) (cadr (assq 'parent (cddr (result F2))))
                (cadr (assq 'about (cddr (result F1)))) (cadr (assq 'about (cddr (result F2))))
                (assq 'keywords (cadr (assq 'fields (cddr (result F1)))))
                (assq 'title (cadr (assq 'fields (cddr (result F1))))))
      (list 'letter V (list (list 'origin A)) (list (list 'scoped F1))
            (list 'keywords "set by the author" "author") (list 'title "F1" "codex")))
;; THE BARE VERDICT IS MADE BEFORE THE ROW: an argument list has no order, so
;; an expectation that reads it cannot sit beside the call that writes it.
(define RR-before (filter (lambda (c) (and (pair? c) (memq (car c) '(no-about missing)))) (cdr RR)))
(define V2 (insert-as X "codex" LC '((kind . section) (title . "Bare verdict"))))
(want "R a finding with no about and a verdict missing a condition are listed, and nothing is refused"
      (in-order RR-before
                (filter (lambda (c) (and (pair? c) (eq? (car c) 'missing) (equal? (cadr c) V2)))
                        (cdr (run X "codex" 'review-results))))
      (list (list (list 'no-about F3))
            (map (lambda (k) (list 'missing V2 k)) '(model approval sandbox effort))))
(want "R a store scope did not make is refused"
      (run M "author" 'review-results)
      '(error bad-request (reason not-a-scoped-store)))

;; ---- P4: collect -----------------------------------------------------------------------

(define (collected sid)
  (let ((st (state M)))
    (filter (lambda (id) (let ((c (block-creation st id)))
                           (and c (let ((p (assq 'scoped-id (cadr (cdr c))))) (and p (equal? (cdr p) sid))))))
            (outline-order M))))
(define C1 (collect-run M "author" X))
(define V* (car (collected V)))
(define F1* (car (collected F1)))
(define F2* (car (collected F2)))
(want "C collect: five inserted, two linked, none already, the author's field named"
      (in-order (list-head C1 4) (filter (lambda (c) (and (pair? c) (eq? (car c) 'foreign-field))) (cdr C1)))
      (list '(ok (inserted 5) (linked 2) (already 0)) (list (list 'foreign-field F1 'keywords "author"))))
(want "C the verdict under the letter, the findings under it, with their fields and scoped-id, the reader's actor"
      (in-order (parent-of M V*) (parent-of M F1*) (field-of M F1* 'src) (field-of M F1* 'scoped-id)
                (car (block-creation (state M) F1*)) (field-of M V* 'effort))
      (list L V* "one" F1 "codex" "high"))
(want "C about edges point at the origin id, and at the copy of a reader block"
      (in-order (edges-of M F1*) (edges-of M F2*))
      (list (list (cons 'about A)) (list (cons 'about F1*))))
(want "C the links collect writes are the reader's records too"
      (let ((st (state M)))
        (in-order (map (lambda (e) (event-actor st e)) (link-events st F1* 'about A))
                  (map (lambda (e) (event-actor st e)) (link-events st F2* 'about F1*))))
      '(("codex") ("codex")))
(define before-second (events-applied M))
(want "C a second collect inserts and links nothing"
      (in-order (list-head (collect-run M "author" X) 4) (- (events-applied M) before-second))
      (list '(ok (inserted 0) (linked 0) (already 2)) 0))

;; a stop between the two writes: the first write's block is there, its edge is not
(define F4 (insert-as X "codex" V '((kind . section) (title . "F4"))))
(run X "codex" 'link F4 "about" (copy-of B))
(insert-as M "codex" V* (list '(kind . section) '(title . "F4") (cons 'scoped-id F4)))
(run M "author" 'del B)
(want "C a collect after a stop between its writes inserts nothing and links the missing edge, to a block deleted since"
      (in-order (list-head (collect-run M "author" X) 4) (edges-of M (car (collected F4))))
      (list '(ok (inserted 0) (linked 1) (already 2)) (list (cons 'about B))))
(want "C and the edge to the deleted block is linked once and never again"
      (list-head (collect-run M "author" X) 4)
      '(ok (inserted 0) (linked 0) (already 3)))
(run M "author" 'move V* "root")
(run M "author" 'set F1* "title" "renamed here")
(want "C a moved copy, and one whose field changed, are still the same copies"
      (list-head (collect-run M "author" X) 4)
      '(ok (inserted 0) (linked 0) (already 3)))
(run M "author" 'set F1* "scoped-id" "changed here")
(want "C a copy whose scoped-id field was changed is still the copy: identity is its creation record's"
      (list-head (collect-run M "author" X) 4)
      '(ok (inserted 0) (linked 0) (already 3)))
(define dup (insert-as M "codex" L (list '(kind . section) '(title . "dup") (cons 'scoped-id F2))))
(want "C two copies of one result refuse collect-ambiguous, and nothing is written"
      (let* ((n (events-applied M)) (a (collect-run M "author" X)))
        (in-order (list-head a 3) (- (events-applied M) n)))
      (list (list 'error 'collect-ambiguous (list 'scoped-id F2)) 0))
(run M "author" 'del dup)
(want "C collect-into refuses a malformed results datum, and an unknown letter"
      (in-order (run M "author" 'collect-into L "--results" "(results (to 3))")
                (car (cdr (run M "author" 'collect-into "nosuch.1" "--results" "(results (letter \"x\") (to \"codex\") (blocks))"))))
      (list '(error bad-request (reason results-malformed)) 'unknown-id))
(want "C a datum naming one result twice is malformed; one about target written twice is linked once"
      (in-order (run M "author" 'collect-into L "--results"
                     "(results (letter \"x\") (to \"codex\") (blocks (block \"zz.1\" (parent letter) (fields ()) (about ())) (block \"zz.1\" (parent letter) (fields ()) (about ()))))")
                (list-head (run M "author" 'collect-into L "--results"
                                (format "~s" (list 'results '(letter "x") '(to "codex")
                                                   (list 'blocks (list 'block "zz.2" '(parent letter) '(fields ((title "twice" "codex")))
                                                                       (list 'about (list (list 'origin R1) (list 'origin R1))))))))
                           4))
      (list '(error bad-request (reason results-malformed)) '(ok (inserted 1) (linked 1) (already 0))))

;; ---- adoption: a result made before it is collected after it -----------------
;;
;; A clone of a scoped store has no instance.sexp and is adopted before it is
;; written to: the writer changes, the reader's actor does not. Results are
;; chosen by the actor of the record that created them, so the verdict
;; written before the adoption and the one written after are both collected.
(define X5 (string-append root "/scoped5"))
(define X5c (string-append root "/scoped5-clone"))
(define S5 (scope-run M "author" X5 "--cut" "t1" "--roots" R1 "--for" "codex"))
(define V5 (insert-as X5 "codex" (one S5 'letter-copy) '((kind . section) (title . "before adoption"))))
(system (string-append "cp -R '" X5 "' '" X5c "' && rm -f '" X5c "/instance.sexp'"))
(define adopted (run X5c "codex" 'adopt))
(define V6 (insert-as X5c "codex" (one S5 'letter-copy) '((kind . section) (title . "after adoption"))))
(want "A the clone is adopted under a new writer, and both verdicts are collected, the reader's"
      (in-order (car adopted) (not (equal? (one adopted 'from) (one adopted 'to)))
                (list-head (collect-run M "author" X5c) 4)
                (map (lambda (sid) (let ((c (collected sid))) (and (pair? c) (car (block-creation (state M) (car c)))))) (list V5 V6)))
      (list 'ok #t '(ok (inserted 2) (linked 0) (already 0)) '("codex" "codex")))

;; ---- the letter copy is the causally first put, not the first delivered ----------
;;
;; A store's history holds records in the order they are delivered, and a
;; record waiting on another writer's is delivered before it is applied. A
;; writer whose name sorts first, holding a reader's block that depends on the
;; scope's own records, is delivered first: a letter copy read off the front
;; of the history would be that block, and the store would read as not
;; scoped.
(define X7 (string-append root "/scoped7"))
(define S7 (scope-run M "author" X7 "--cut" "t1" "--roots" R1 "--for" "codex"))
(define x7-cut (reduce-applied-cut (state X7)))
(define forged
  (encode-record 1 1789000000009 "codex" x7-cut
                 (storable-encode (list 'put (list '(kind . section) '(title . "delivered first")
                                                   (cons 'parent (one S7 'letter-copy)) '(ord . 50))))))
(log-publish! X7 "0aaaaaaa" 1 forged (segment-sha forged))
(want "L with a writer delivered before the scope's own, the letter copy is still the scope's, and its block is a result"
      (let* ((a (run X7 "codex" 'review-results)) (r (and (pair? a) (eq? (car a) 'ok) (one-results a))))
        (in-order (car a) (and r (cadr (assq 'letter (cdr r))))
                  (and r (map cadr (cdr (assq 'blocks (cdr r)))))))
      (list 'ok (one S7 'letter) (list (block-id "0aaaaaaa" 1))))

;; Deletion does not cascade: a finding under a verdict the reader deleted is
;; alive and still a result, placed under the letter. A block whose `origin`
;; field holds #f has one, and is not a result. A field whose one value
;; starts with `conflict` is a value, not a contest.
(define V7 (insert-as X7 "codex" (one S7 'letter-copy) '((kind . section) (title . "V7"))))
(define F7 (insert-as X7 "codex" V7 '((kind . section) (title . "F7") (note . (conflict "kept")))))
(define O7 (insert-as X7 "codex" (one S7 'letter-copy) '((kind . section) (title . "O7") (origin . #f))))
(run X7 "codex" 'del V7)
(want "L a finding under a deleted verdict is a result under the letter, with a value shaped like a contest; an origin of #f is still an origin"
      (let* ((a (run X7 "codex" 'review-results)) (r (and (pair? a) (eq? (car a) 'ok) (one-results a)))
             (bs (if r (cdr (assq 'blocks (cdr r))) '()))
             (f7 (find (lambda (b) (equal? (cadr b) F7)) bs)))
        (in-order (list-sort string<? (map cadr bs))
                  (and f7 (cadr (assq 'parent (cddr f7))))
                  (and f7 (assq 'note (cadr (assq 'fields (cddr f7)))))))
      (list (list-sort string<? (list (block-id "0aaaaaaa" 1) F7)) 'letter (list 'note '(conflict "kept") "codex")))

;; COLLECTING X7. A result holding a bytevector travels in --results; a
;; finding about a reader block deleted before it was ever collected names
;; that target as unresolved; and two results whose common parent was
;; deleted, the second moved under the first, are listed child first by the
;; scoped store and still collected parent first.
(define LC7 (one S7 'letter-copy))
(define BB7 (insert-as X7 "codex" LC7 '((kind . section) (title . "bytes7") (src . #vu8(1 2)))))
(define T7 (insert-as X7 "codex" LC7 '((kind . section) (title . "T7"))))
(define U7 (insert-as X7 "codex" LC7 '((kind . section) (title . "U7"))))
(run X7 "codex" 'link U7 "about" T7)
(run X7 "codex" 'del T7)
(define D7 (insert-as X7 "codex" LC7 '((kind . section) (title . "D7"))))
(define B7 (insert-as X7 "codex" D7 '((kind . section) (title . "B7"))))
(define A7 (insert-as X7 "codex" D7 '((kind . section) (title . "A7"))))
(run X7 "codex" 'move B7 A7)
(run X7 "codex" 'del D7)
(define C7 (collect-run M "author" X7))
(want "L collect of X7: six inserted, the deleted target named unresolved, the moved child under its parent's copy, the bytes intact"
      (in-order (and (pair? C7) (list? C7) (>= (length C7) 4) (list-head C7 4))
                (and (member (list 'unresolved U7 (list 'scoped T7)) C7) #t)
                (let ((a (collected A7)) (b (collected B7)))
                  (and (pair? a) (pair? b) (equal? (parent-of M (car b)) (car a))))
                (let ((c (collected BB7))) (and (pair? c) (field-of M (car c) 'src))))
      (list '(ok (inserted 6) (linked 0) (already 0)) #t #t #vu8(1 2)))

;; ---- a code block's derived fields are not copied --------------------------------
;;
;; A datum code block's view adds `name` and `names` from its body. The copy
;; carries neither as a stored field, and the scoped store's view derives the
;; same ones again.
(define M2 (string-append root "/main2"))
(run M2 "author" 'init)
(system (string-append "mkdir -p '" root "/lib-in' && printf '(library (a) (export gnarlwick) (import (rnrs)) (define gnarlwick 1))\\n' > '" root "/lib-in/a.sc'"))
(run M2 "author" 'import-code (string-append root "/lib-in") "--datum")
(define G
  (find (lambda (id) (let ((fs (fields-of M2 id)))
                       (and (eq? (cdr (or (assq 'kind fs) '(k . #f))) 'code) (eq? (cdr (or (assq 'mode fs) '(m . #f))) 'datum)
                            (equal? (cdr (or (assq 'body fs) '(b . #f))) '(define gnarlwick 1)))))
        (outline-order M2)))
(define (read-fields store id) (cdr (assq 'fields (cadr (run store "author" 'read id)))))
(define X6 (string-append root "/scoped6"))
(define S6 (scope-run M2 "author" X6 "--cut" (format "~s" (reduce-applied-cut (state M2))) "--roots" G "--for" "codex"))
(define G* (find (lambda (id) (equal? (field-of X6 id 'origin) G)) (outline-order X6)))
(want "F a datum code block's copy stores neither name nor names, and its view derives the same ones"
      (in-order (car S6) (string? G*)
                (and G* (list (assq 'name (fields-of X6 G*)) (assq 'names (fields-of X6 G*))))
                (and (assq 'names (read-fields M2 G)) #t)
                (and G* (equal? (list (assq 'name (read-fields M2 G)) (assq 'names (read-fields M2 G)))
                                (list (assq 'name (read-fields X6 G*)) (assq 'names (read-fields X6 G*))))))
      '(ok #t (#f #f) #t #t))

;; ---- the routes ------------------------------------------------------------------------

(define X3 (string-append root "/scoped3"))
(define (served-count)
  (let ((f (string-append root "/served.txt")))
    (system (string-append "cat '" sock-root "'/run/*/serve.log 2>/dev/null | grep -c 'daemon-dispatch' > '" f "' || true"))
    (let ((t (file-text f))) (if (> (string-length t) 0) (string->number (substring t 0 (- (string-length t) 1))) 0))))
(cli "start-main" "author" "outline" "--store" M)
(define served-0 (served-count))
(define S3 (first-datum (cli "scope3" "author" "scope" X3 "--cut" "t1" "--roots" R1 "--for" "codex" "--store" M)))
(define served-1 (served-count))
(want "D scope from the command line with a daemon serving the main store: ok, and the daemon carried its requests"
      (in-order (car S3) (one S3 'blocks) (> served-1 served-0) (and (string? (one S3 'letter)) (field-of M (one S3 'letter) 'to)))
      '(ok 3 #t "codex"))
(define X9 (string-append root "/scoped9"))
(define S9 (first-datum (cli "scope9" "author" "scope" X9 "--cut" (now-literal) "--roots" BV "--for" "codex" "--store" M)))
(want "D a block holding a bytevector, a character and a vector is copied through the main store's daemon"
      (in-order (car S9)
                (let ((c (find (lambda (id) (equal? (field-of X9 id 'origin) BV)) (outline-order X9))))
                  (and c (map (lambda (k) (field-of X9 c k)) '(src glyph pair)))))
      '(ok (#vu8(65 66) #\a #(1 2))))
(define frame-answer
  (let ((o (call! (socket-path M)
                  (request-frame M 'scope (list (string-append root "/x7") "--cut" "t1" "--roots" R1 "--for" "codex")
                                 '((actor . "author") (mode . wire)))
                  30000)))
    (and (eq? (car o) 'answer) (first-datum (answer-field (first-datum (utf8->string (cadr o))) 'stdout string?)))))
(want "D a daemon asked to carry out scope does not have the verb, and makes nothing"
      (in-order (and (pair? frame-answer) (list-head frame-answer 2)) (dir-exists? (string-append root "/x7")))
      (list '(error unknown-verb) #f))
(want "D core.sc answers a scope of the wrong shape with the catalogue's usage form"
      (first-datum (sh-out "scope-usage" (string-append (env "") "scheme --script ../core.sc scope '" root "/x8' --store '" M "' --wire")))
      (list 'usage (cadr (assq 'scope (verb-catalogue)))))
(cli "start-scoped" "codex" "outline" "--store" X3)
(define LC3 (one S3 'letter-copy))
(define V3 (new-id (car (cadr (first-datum (cli-batch "v3" "codex" X3
                                                (format "~s" (list (list 'insert LC3 #f '((kind . section) (title . "V3") (model . "m")
                                                                                          (approval . "a") (sandbox . "s") (effort . "e")))))))))))
(want "D review-results on the scoped store's daemon, and collect with daemons serving both stores"
      (in-order (car (first-datum (cli "rr3" "author" "review-results" "--store" X3)))
                (list-head (first-datum (cli "collect3" "author" "collect" X3 "--store" M)) 4)
                (parent-of M (car (collected V3))))
      (list 'ok '(ok (inserted 1) (linked 0) (already 0)) (one S3 'letter)))
(want "D the scoped store is subscribable"
      (let ((t (sh-out "sub3" (string-append (env "") "perl -e 'alarm 3; exec @ARGV' scheme --script ../theourgia.sc subscribe changes 0 --store '" X3 "' --wire"))))
        (string-contains? t "(ok (subscribed"))
      #t)

;; ---- the hooks' commands and the doorbell ----------------------------------------------
;;
;; `theourgia` on the PATH is the program run from this tree; `tmux` is a
;; stand-in that writes what it was asked to type.
(call-with-output-file (string-append root "/bin/theourgia")
  (lambda (p) (put-string p (string-append "#!/bin/sh\nexec env " (env "") "scheme --script '" tree "/theourgia.sc' \"$@\"\n"))))
(call-with-output-file (string-append root "/bin/tmux")
  (lambda (p) (put-string p (string-append "#!/bin/sh\necho \"$@\" >> '" root "/tmux.log'\n"))))
(system (string-append "chmod +x '" root "/bin/theourgia' '" root "/bin/tmux'"))
(define hooks-text (file-text "../contrib/codex-hooks.json"))
(define (hook-command-in text event)
  (let* ((at (let find ((i 0))
               (cond ((> (+ i (string-length event) 2) (string-length text)) #f)
                     ((string=? (substring text i (+ i (string-length event) 2)) (string-append "\"" event "\"")) i)
                     (else (find (+ i 1))))))
         (key "\"command\": \"")
         (k (and at (let find ((i at))
                      (cond ((> (+ i (string-length key)) (string-length text)) #f)
                            ((string=? (substring text i (+ i (string-length key))) key) (+ i (string-length key)))
                            (else (find (+ i 1)))))))
         (end (and k (let find ((i k)) (if (char=? (string-ref text i) #\") i (find (+ i 1)))))))
    (and end (let* ((c (substring text k end)) (p "/path/to/theourgia") (n (string-length p)))
               (let loop ((i 0))
                 (cond ((> (+ i n) (string-length c)) c)
                       ((string=? (substring c i (+ i n)) p) (string-append (substring c 0 i) tree (substring c (+ i n) (string-length c))))
                       (else (loop (+ i 1)))))))))
(define (hook-command event) (hook-command-in hooks-text event))
(define (hook-run name event)
  (sh-out name (string-append "PATH='" root "/bin':\"$PATH\" THEOURGIA_STORE='" M "' THEOURGIA_ACTOR=codex sh -c '"
                              (hook-command event) "'")))
(want "H the shipped hooks.json has a command for PostToolUse and for UserPromptSubmit"
      (map (lambda (e) (and (hook-command e) #t)) '("PostToolUse" "UserPromptSubmit"))
      '(#t #t))
(define post (hook-run "hook-post" "PostToolUse"))
(define prompt (hook-run "hook-prompt" "UserPromptSubmit"))
(want "H with a letter to codex unread, both commands answer the hook's JSON naming the letter"
      (in-order (string-contains? post L) (string-contains? post "\"hookEventName\":\"PostToolUse\"")
                (string-contains? prompt L) (string-contains? prompt "\"hookEventName\":\"UserPromptSubmit\""))
      '(#t #t #t #t))
(for-each (lambda (id) (run M "codex" 'set id "status" "read"))
          (filter (lambda (id) (equal? (field-of M id 'status) "unread")) (outline-order M)))
(want "H with nothing unread for codex, both commands print nothing"
      (list (hook-run "hook-post-2" "PostToolUse") (hook-run "hook-prompt-2" "UserPromptSubmit"))
      '("" ""))
;; CLAUDE CODE'S HOOKS RUN THE SAME HANDLER, with THEOURGIA_ACTOR=claude in
;; each command: a letter to claude is named by them, and by codex's not.
(define claude-hooks-text
  (guard (e (#t "")) (file-text "../contrib/claude-hooks.json")))
(define (claude-hook-run name event)
  (let ((c (hook-command-in claude-hooks-text event)))
    (and c (sh-out name (string-append "PATH='" root "/bin':\"$PATH\" THEOURGIA_STORE='" M "' THEOURGIA_ACTOR=codex sh -c '"
                                       c "'")))))
(want "H the shipped claude-hooks.json has a command for PostToolUse and for UserPromptSubmit, each for actor claude"
      (map (lambda (e) (let ((c (hook-command-in claude-hooks-text e)))
                         (and c (string-contains? c "THEOURGIA_ACTOR=claude") (string-contains? c "/contrib/mail-hook.sh"))))
           '("PostToolUse" "UserPromptSubmit"))
      '(#t #t))
(define LC
  (let ((a (first-datum (cli-batch "msg-claude" "author" M
                          (format "~s" (list (list 'insert 'root #f '((kind . doc) (title . "to claude")
                                                                    (to . "claude") (status . "unread")))))))))
    (new-id (car (cadr a)))))
(define claude-post (claude-hook-run "hook-claude-post" "PostToolUse"))
(define claude-prompt (claude-hook-run "hook-claude-prompt" "UserPromptSubmit"))
;; The answer is read at both ends: the object, its two keys in the order
;; both vendors read and the actor named at its head; the string and both
;; objects closed at its end, one line; the letter's id between.
(define (hook-answer-for? answer event id)
  (let* ((head (string-append "{\"hookSpecificOutput\":{\"hookEventName\":\"" event
                              "\",\"additionalContext\":\"theourgia: unread mail for claude: "))
         (tail "\"}}\n")
         (n (if (string? answer) (string-length answer) 0)))
    (and (string? answer)
         (>= n (+ (string-length head) (string-length tail)))
         (string=? (substring answer 0 (string-length head)) head)
         (string=? (substring answer (- n (string-length tail)) n) tail)
         (string-contains? (substring answer (string-length head) (- n (string-length tail))) id))))
(want "H with a letter to claude unread, both of claude's commands answer the hook's JSON naming it, and codex's name nothing"
      (in-order (hook-answer-for? claude-post "PostToolUse" LC)
                (hook-answer-for? claude-prompt "UserPromptSubmit" LC)
                (hook-run "hook-post-3" "PostToolUse"))
      '(#t #t ""))
(want "H the handler's old name, codex-mail.sh, gives the same answer as mail-hook.sh"
      (let ((run-as (lambda (name script)
                      (sh-out name (string-append "PATH='" root "/bin':\"$PATH\" THEOURGIA_STORE='" M "' THEOURGIA_ACTOR=claude sh "
                                                  tree "/contrib/" script " UserPromptSubmit")))))
        (let ((old (run-as "hook-old-name" "codex-mail.sh")) (new (run-as "hook-new-name" "mail-hook.sh")))
          (list (hook-answer-for? new "UserPromptSubmit" LC) (string=? old new))))
      '(#t #t))
(want "H the old name, reached through a link in another directory, still runs the handler beside its target"
      (begin
        (system (string-append "mkdir -p '" root "/elsewhere' && ln -sf '" tree "/contrib/codex-mail.sh' '" root "/elsewhere/codex-mail.sh'"))
        (hook-answer-for? (sh-out "hook-linked" (string-append "PATH='" root "/bin':\"$PATH\" THEOURGIA_STORE='" M "' THEOURGIA_ACTOR=claude sh '"
                                                               root "/elsewhere/codex-mail.sh' UserPromptSubmit"))
                          "UserPromptSubmit" LC))
      #t)
(run M "claude" 'set LC "status" "read")

(system (string-append "PATH='" root "/bin':\"$PATH\" sh ../contrib/doorbell.sh '" M "' codex pane > '" root "/doorbell.out' 2>&1 &"))
(define (ring-for actor)
  (let ((a (first-datum (cli-batch "msg" "author" M
                             (format "~s" (list (list 'insert 'root #f (list '(kind . doc) (cons 'title (string-append "to " actor))
                                                                         (cons 'to actor) '(status . "unread")))))))))
    (new-id (car (cadr a)))))
(define rang
  (let try ((k 0))
    (if (= k 8)
        #f
        (let ((id (ring-for "codex")))
          (let wait ((w 0))
            (cond ((string-contains? (file-text (string-append root "/tmux.log")) (string-append "mail: block " id " at ")) id)
                  ((< w 20) (sleep-ms 250) (wait (+ w 1)))
                  (else (try (+ k 1)))))))))
(define other (ring-for "claude"))
(sleep-ms 2000)
(want "B the doorbell types one line naming a block addressed to its reader, and none for another reader's"
      (in-order (string? rang) (string-contains? (file-text (string-append root "/tmux.log")) other)
                (string-contains? (file-text (string-append root "/tmux.log")) "-t pane "))
      '(#t #f #t))

;; ---- 3.5: three routes to one message ---------------------------------------
;;
;; Each message is addressed to codex and unread. A route is REMOVED by not
;; being run; the rows read what the routes left standing say. The doorbell
;; is still subscribed from the row above.
(define unread-goal "(and (field ?m \"to\" \"codex\") (field ?m \"status\" \"unread\"))")
(define (pulled? id)
  (let ((a (run M "codex" 'query unread-goal)))
    (and (pair? a) (eq? (car a) 'ok) (member (list 'row id) (cdr (assq 'items (cdr a)))) #t)))
(define (hooked? id) (string-contains? (hook-run "hook-35" "PostToolUse") id))
(define (rang? id)
  (let wait ((w 0))
    (cond ((string-contains? (file-text (string-append root "/tmux.log")) (string-append "mail: block " id " at ")) #t)
          ((< w 20) (sleep-ms 250) (wait (+ w 1)))
          (else #f))))
(define m1 (ring-for "codex"))
(want "3.5 with the hook removed, the doorbell and the pull still bring the message"
      (in-order (rang? m1) (pulled? m1))
      '(#t #t))
(define m2 (ring-for "codex"))
(want "3.5 with the pull removed, the hook and the doorbell still bring the message"
      (in-order (hooked? m2) (rang? m2))
      '(#t #t))
;; This run's doorbell and its subscriber only, by the store they name.
(system (string-append "pkill -f '[c]ontrib/doorbell.sh " M "' 2>/dev/null"))
(system (string-append "pkill -f '[s]ubscribe changes 0 --wire --store " M "' 2>/dev/null"))
(define m3 (ring-for "codex"))
(sleep-ms 2000)
(want "3.5 with the doorbell removed, the hook and the pull still bring the message, and no bell rang"
      (in-order (hooked? m3) (pulled? m3) (string-contains? (file-text (string-append root "/tmux.log")) m3))
      '(#t #t #f))
(define m4 (ring-for "codex"))
(sleep-ms 1000)
(want "3.5 with all three removed, the message is still unread and the next pull reads it"
      (in-order (field-of M m4 'status) (pulled? m4))
      '("unread" #t))

;; ---- 3.3: a block the reader writes reaches a subscriber within the bound ------
;;
;; The bound: a write the daemon carries is published when it is made; this
;; row allows five seconds for the frame to be read back from a subscriber's
;; output, and records how long it took.
(define sub-out (string-append root "/sub33.out"))
(system (string-append (env "") "perl -e 'alarm 30; exec @ARGV' scheme --script ../theourgia.sc subscribe changes 0 --store '" M "' --wire > '" sub-out "' 2>/dev/null < /dev/null &"))
(define subscribed
  (let wait ((w 0))
    (cond ((string-contains? (file-text sub-out) "(ok (subscribed") #t)
          ((< w 40) (sleep-ms 250) (wait (+ w 1)))
          (else #f))))
(define t0 (current-time))
(define c1 (let ((a (first-datum (cli-batch "c33" "codex" M
                                      (format "~s" (list (list 'insert 'root #f '((kind . section) (title . "from codex")))))))))
             (new-id (car (cadr a)))))
(define framed-ms
  (let wait ((w 0))
    (cond ((string-contains? (file-text sub-out) (format "(added ~s)" c1))
           (let ((d (time-difference (current-time) t0)))
             (+ (* 1000 (time-second d)) (quotient (time-nanosecond d) 1000000))))
          ((< w 50) (sleep-ms 100) (wait (+ w 1)))
          (else #f))))
(printf "3.3 the frame naming the reader's block was read after ~a ms\n" framed-ms)
(want "3.3 a block written by codex reaches a subscribed session in a frame, within five seconds"
      (in-order subscribed (and framed-ms (<= framed-ms 5000)))
      '(#t #t))
(system (string-append "pkill -f '[s]ubscribe changes 0 --store " M "' 2>/dev/null"))

;; ---- the MCP shell offers scope, and runs it as its child ------------------------
(define mcp-out
  (let ((in (string-append root "/mcp-in.jsonl")))
    (call-with-output-file in
      (lambda (p)
        (put-string p (string-append
                        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":"
                        "{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"clientInfo\":{\"name\":\"p\",\"version\":\"1\"}}}\n"
                        "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}\n"
                        "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":"
                        "{\"name\":\"theourgia_scope\",\"arguments\":{\"argv\":[\"" root "/mcp-scoped\",\"--cut\",\"t1\",\"--roots\",\"" R1 "\",\"--for\",\"codex\"]}}}\n"))))
;; The shell is started with --actor, and its environment names another
    ;; actor: the child's records are the shell's actor's.
    (sh-out "mcp" (string-append (env "THEOURGIA_ACTOR=someone-else") "scheme --script ../mcp/server.sc --store '" M "' --actor author < '" in "'"))))
(unless (string-contains? mcp-out "(ok (scope ")
  (printf "M the shell's output, for the row below: ~a\n~a\n"
          (substring mcp-out 0 (min 1500 (string-length mcp-out)))
          (file-text (string-append root "/mcp.err"))))
(want "M the MCP shell's theourgia_scope runs scope as its child: the scoped store is made, the letter written as the shell's actor"
      (in-order (string-contains? mcp-out "(ok (scope ") (dir-exists? (string-append root "/mcp-scoped"))
                (let ((l (find (lambda (id) (equal? (field-of M id 'scope) (string-append root "/mcp-scoped"))) (outline-order M))))
                  (and l (car (block-creation (state M) l)))))
      '(#t #t "author"))

;; The directories are kept when a row failed: they are what the row read.
(teardown!)
(when (= bad 0) (system (string-append "rm -rf '" root "' '" sock-root "'")))
(printf "\n~a failures\nrows: ~a\nreview-channel complete\n" bad rows)
(exit (if (= bad 0) 0 1))
