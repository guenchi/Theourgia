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
        (only (theourgia extensions) extension-verbs)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (theourgia landing-files) file-pieces file-new-intents))

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
;; THE DAEMONS ARE ASKED FOR: a plain command starts none, so each store a
;; row reaches "through its daemon" is served first, with the client's own
;; environment (the trace the dispatch counts read).
(include "serve-for.ss")
(define (serve! store) (serve-for! root store #f (env "")))
(serve! M)
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
;; A TEXT-MODE CODE BLOCK THROUGH A DAEMON: its src is printed as the string
;; its bytes encode (render.sc), so the copy scope reads has the string; the
;; copy stores the bytes again (channel.sc), and the scoped store exports the
;; file byte for byte, its leading byte-order mark included.
(define MT (string-append root "/main-text"))
(define MT-dir (string-append root "/text-src"))
(define MT-bytes (string->utf8 (string-append (string (integer->char #xFEFF)) "function scoped() { return 1 }\n")))
(system (string-append "mkdir -p '" MT-dir "'"))
(call-with-port (open-file-output-port (string-append MT-dir "/scoped.js") (file-options no-fail))
  (lambda (p) (put-bytevector p MT-bytes)))
(cli "init-mt" "author" "init" "--store" MT)
(serve! MT)
(cli "import-mt" "author" "import-code" MT-dir "--store" MT)
(define MT-code (find (lambda (id) (equal? (field-of MT id 'src) MT-bytes)) (outline-order MT)))
;; THE FILE BLOCK IS SCOPED: it holds the path, and export-code writes a file
;; for a file block only; the code block is its child.
(define MT-file (and MT-code (parent-of MT MT-code)))
;; READS THE DAEMONS DISPATCHED: scope's copies are read with `read`, and
;; its letter is written with a batch, so a count of every dispatch would
;; be met by the letter alone.
(define (served-reads)
  (let ((f (string-append root "/served-reads.txt")))
    (system (string-append "cat '" sock-root "'/run/*/serve.log 2>/dev/null | grep -c 'daemon-dispatch read' > '" f "' || true"))
    (let ((t (file-text f))) (if (> (string-length t) 0) (string->number (substring t 0 (- (string-length t) 1))) 0))))
(define XT (string-append root "/scoped-text"))
(define served-t0 (served-reads))
(define ST (and MT-file
                (first-datum (cli "scope-mt" "author" "scope" XT "--cut" (format "~s" (reduce-applied-cut (state MT)))
                                  "--roots" MT-file "--for" "codex" "--store" MT))))
(define served-t1 (served-reads))
(define XT-copy (and ST (find (lambda (id) (equal? (field-of XT id 'origin) MT-code)) (outline-order XT))))
(define XT-out (string-append root "/scoped-text-export"))
(define XT-export (and XT-copy (run XT "author" 'export-code XT-out "--raw")))
(want "D a text-mode code block scoped through the main store's daemon, which read it: the copy holds the source's bytes, and the scoped store exports the file byte for byte"
      (in-order (and ST (car ST)) (> served-t1 served-t0) (and XT-copy (field-of XT XT-copy 'src))
                (and (pair? XT-export) (car XT-export))
                (let ((f (string-append XT-out "/scoped.js")))
                  (and (file-exists? f) (equal? (call-with-port (open-file-input-port f) get-bytevector-all) MT-bytes))))
      (list 'ok #t MT-bytes 'ok #t))
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
(serve! X3)
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
  (lambda (p) (put-string p (string-append "#!/bin/sh\necho \"$@\" >> '" root "/tmux.log'\n"
                                           "perl -MTime::HiRes=time -e 'printf \"%.3f\\n\", time' >> '" root "/tmux.times'\n"))))
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
;; THE WHOLE ANSWER, for the one letter claude has unread: the object, its
;; two keys in the order both vendors read, the actor, the id and the
;; instruction, closed, one line. Exactly mail-hook.sh's printf.
(define (hook-answer-for? answer event id)
  (and (string? answer)
       (string=? answer
                 (string-append "{\"hookSpecificOutput\":{\"hookEventName\":\"" event
                                "\",\"additionalContext\":\"theourgia: unread mail for claude: " id
                                ". Read each block in the store (read <id>), act on it, then set its status to read.\"}}\n"))))
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
;; THE ENTER IS ITS OWN KEY PRESS: a terminal UI reads fast text followed by
;; Enter as one paste and leaves the line unsubmitted, so the doorbell sends
;; the line, pauses, then sends Enter alone. The fake tmux logs each call and
;; its time on a line of its own (tmux.log, tmux.times): every mail line is
;; followed at once by a lone Enter call, at least 0.25 s later, and no Enter
;; stands anywhere else.
(define (lines-of text)
  (let loop ((cs (string->list text)) (cur '()) (out '()))
    (cond ((null? cs) (reverse (if (null? cur) out (cons (list->string (reverse cur)) out))))
          ((char=? (car cs) #\newline) (loop (cdr cs) '() (cons (list->string (reverse cur)) out)))
          (else (loop (cdr cs) (cons (car cs) cur) out)))))
(define (mail-line? l) (let ((m "mail: block "))
                         (let find ((j 0))
                           (cond ((> (+ j (string-length m)) (string-length l)) #f)
                                 ((string=? (substring l j (+ j (string-length m))) m) #t)
                                 (else (find (+ j 1)))))))
(want "B the doorbell sends each mail line, then after a pause Enter alone, and no Enter elsewhere"
      (let* ((ls (lines-of (file-text (string-append root "/tmux.log"))))
             (ts (map string->number (lines-of (file-text (string-append root "/tmux.times")))))
             (enter "send-keys -t pane Enter"))
        (let loop ((ls ls) (ts ts) (mails 0) (ok (= (length ls) (length ts))))
          (cond ((null? ls) (list (> mails 0) ok))
                ((mail-line? (car ls))
                 (let ((paired (and (pair? (cdr ls)) (string=? (cadr ls) enter)
                                    (pair? (cdr ts)) (car ts) (cadr ts)
                                    (>= (- (cadr ts) (car ts)) 1/4))))
                   (if paired
                       (loop (cddr ls) (cddr ts) (+ mails 1) ok)
                       (loop (cdr ls) (cdr ts) (+ mails 1) #f))))
                ((string=? (car ls) enter) (loop (cdr ls) (cdr ts) mails #f))
                (else (loop (cdr ls) (cdr ts) mails ok)))))
      '(#t #t))

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

;; ---- a review of a frozen candidate ------------------------------------------------
;;
;; A store imported from files, a.txt and b.txt; the coder W drafts a.txt,
;; and tools/landing-freeze.sc freezes it. The scope of that freeze holds the
;; candidate; a stand-in reviewer, a process of its own, reads its mail
;; through the shipped hook, edits b.txt's copy under its own writer, writes
;; a complete verdict and a finding, and answers. Then the landing, then the
;; collect with --letter new and --to-writer W.
(define (sh-rc name command)
  (system (string-append command " > '" root "/" name ".out' 2> '" root "/" name ".err' < /dev/null; echo $? > '" root "/" name ".rc'"))
  (let ((t (file-text (string-append root "/" name ".rc")))) (and (> (string-length t) 0) (string->number (substring t 0 (- (string-length t) 1))))))
(define (tool-run name script . args)
  (first-datum (sh-out name (string-append (env "") "scheme --script ../tools/" script
                                          (apply string-append (map (lambda (a) (string-append " '" a "'")) args))))))
(define MF (string-append root "/main-freeze"))
(define MF-src (string-append root "/freeze-src"))
(define W "coder")
(system (string-append "mkdir -p '" MF-src "' && printf 'one\\n' > '" MF-src "/a.txt' && printf 'two\\n' > '" MF-src "/b.txt'"))
(run MF "author" 'init)
(run MF "author" 'import-code MF-src)
(define (holder-of store path) (find (lambda (id) (equal? (field-of store id 'path) path)) (outline-order store)))
(define (child-of store id) (find (lambda (c) (equal? (parent-of store c) id)) (outline-order store)))
(define HA (holder-of MF "a.txt"))
(define HB (holder-of MF "b.txt"))
(define CA (and HA (child-of MF HA)))
(define CB (and HB (child-of MF HB)))
(run MF "author" 'write CA "one, as the change has it\n" "--writer" W)
(define FZ (string-append root "/freeze"))
(define frozen (tool-run "freeze" "landing-freeze.sc" MF "--writer" W FZ))
(define PK (string-append root "/packet.sexp"))
(define PKB (string-append root "/packet-bad.sexp"))
(call-with-output-file PKB (lambda (p) (write '(packet (doc (title "not a section"))) p)))
(define FZB (string-append root "/freeze-nobytes"))
(system (string-append "mkdir -p '" FZB "'"))
(call-with-output-file (string-append FZB "/DRAFTS.sexp")
  (lambda (p) (write (list 'drafts (list 'pinned-cut (reduce-applied-cut (state MF))) (list 'draft (list 'block CA) '(bytes "9.bytes"))) p)))
(call-with-output-file PK
  (lambda (p) (write '(packet (section (title "What changed") (text "a.txt's first line")
                                       (children (section (title "Why") (text "the reader asked")))))
                     p)))
(define cut-before (reduce-applied-cut (state MF)))
;; THE REVIEW DIRECTORY, as the launch makes it: the scoped store and its
;; own home side by side.
(system (string-append "mkdir -p '" root "/review'"))
(define XF (string-append root "/review/scope"))
(define HF (string-append root "/review/home"))
(define SF (scope-run MF "author" XF "--freeze" FZ "--roots" HA "--roots" HB "--for" "codex" "--packet" PK "--home" HF))
(define LCF (one SF 'letter-copy))
(define (copy-in store origin) (find (lambda (id) (equal? (field-of store id 'origin) origin)) (outline-order store)))
(define (children-in store id) (filter (lambda (c) (equal? (parent-of store c) id)) (outline-order store)))
(want "CR-1 a scope of a freeze: ok, no letter yet, one before; the drafted copy holds the candidate, its before the committed src; the other copy has none"
      (in-order (car frozen) (car SF) (one SF 'letter) (one SF 'before) (one SF 'packet)
                (field-of XF (copy-in XF CA) 'src)
                (map (lambda (b) (list (field-of XF b 'title) (field-of XF b 'before-of) (field-of XF b 'src)))
                     (children-in XF (copy-in XF CA)))
                (children-in XF (copy-in XF CB))
                (field-of XF LCF 'freeze) (field-of XF LCF 'letter) (field-of XF LCF 'home))
      (list 'ok 'ok 'new 1 2 (string->utf8 "one, as the change has it\n")
            (list (list "before" CA (string->utf8 "one\n"))) '() FZ #f HF))
(want "CR-2 the packet's sections are under the letter copy, the second under the first, and in the baseline"
      (let* ((secs (filter (lambda (id) (and (equal? (field-of XF id 'kind) 'section) (not (field-of XF id 'before-of))))
                           (outline-order XF)))
             (base (field-of XF LCF 'baseline)))
        (in-order (map (lambda (id) (field-of XF id 'title)) secs)
                  (and (pair? secs) (parent-of XF (car secs)))
                  (and (pair? secs) (pair? (cdr secs)) (equal? (parent-of XF (cadr secs)) (car secs)))
                  (map (lambda (id) (car (run XF "codex" 'read id "--cut" base))) secs)
                  (map (lambda (id) (field-of XF id 'text)) secs) (map (lambda (id) (field-of XF id 'packet)) secs)))
      (list '("What changed" "Why") LCF #t '(ok ok) '("a.txt's first line" "the reader asked") '(#t #t)))
;; A SECTION WITH SEVERAL TEXTS keeps every one: the section block's text is
;; their concatenation in packet order, one empty line between two; one text
;; is as before, and no text is still no text. A (text) or (text #f) entry
;; carries no text, as before, wherever it stands; an empty string is a text.
;; Each is read back through the scoped store at the letter copy's baseline.
(define PKT (string-append root "/packet-texts.sexp"))
(call-with-output-file PKT
  (lambda (p) (write '(packet (section (title "Two") (text "a") (text "b"))
                              (section (title "Three") (text "x") (text "y") (text "z"))
                              (section (title "One") (text "only"))
                              (section (title "None"))
                              (section (title "Mixed") (text) (text "a") (text #f) (text "") (text "b") (text #f)
                                       (children (section (title "Child") (text #f) (text "c") (text "d"))))
                              (section (title "Absent") (text) (text #f)))
                     p)))
(define XPT (string-append root "/scoped-texts"))
(define SPT (scope-run MF "author" XPT "--freeze" FZ "--roots" HA "--for" "codex" "--packet" PKT))
(define (section-titled store t)
  (find (lambda (id) (and (equal? (field-of store id 'kind) 'section) (equal? (field-of store id 'title) t))) (outline-order store)))
(define (text-read store id cut)
  (let* ((a (run store "codex" 'read id "--cut" cut))
         (rec (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a)) (cadr a)))
         (fs (and (list? rec) (assq 'fields rec)))
         (t (and fs (assq 'text (cdr fs)))))
    (and t (cdr t))))
(define (texts-at title)
  (let ((id (section-titled XPT title)) (base (field-of XPT (one SPT 'letter-copy) 'baseline)))
    (and id base (text-read XPT id base))))
(want "CR-2b a section with two texts arrives whole: its block's text is the first, an empty line, the second, byte for byte"
      (in-order (car SPT) (one SPT 'packet) (texts-at "Two")
                (let ((t (texts-at "Two"))) (and (string? t) (equal? (string->utf8 t) (string->utf8 "a\n\nb")))))
      (list 'ok 7 "a\n\nb" #t))
(want "CR-2c a section with three texts keeps their packet order"
      (texts-at "Three")
      "x\n\ny\n\nz")
(want "CR-2d a section with one text holds it as before, and a section with none has no text"
      (in-order (texts-at "One") (texts-at "None") (and (section-titled XPT "None") #t))
      (list "only" #f #t))
(want "CR-2f entries carrying no text drop out before, between and after the texts, an empty string stays, a section of such entries only has no text, and a child section joins its own"
      (in-order (texts-at "Mixed") (texts-at "Absent") (and (section-titled XPT "Absent") #t) (texts-at "Child")
                (let ((c (section-titled XPT "Child"))) (and c (equal? (parent-of XPT c) (section-titled XPT "Mixed")))))
      (list "a\n\n\n\nb" #f #t "c\n\nd" #t))
;; A text that is not a string refuses the packet, wherever it stands in its
;; section, before anything is made.
(define PKX (string-append root "/packet-text-bad.sexp"))
(call-with-output-file PKX (lambda (p) (write '(packet (section (title "Bad") (text "a") (text 5))) p)))
(define PKY (string-append root "/packet-text-bad-child.sexp"))
(call-with-output-file PKY
  (lambda (p) (write '(packet (section (title "Top") (text "a")
                                       (children (section (title "Deep") (text "x") (text "y") (text 5)))))
                     p)))
(want "CR-2e a section whose second text, or a child's third, is not a string refuses packet-malformed naming the file, and makes nothing"
      (in-order (scope-run MF "author" (string-append root "/x-badtext") "--freeze" FZ "--roots" HA "--for" "codex" "--packet" PKX)
                (dir-exists? (string-append root "/x-badtext"))
                (scope-run MF "author" (string-append root "/x-badtext2") "--freeze" FZ "--roots" HA "--for" "codex" "--packet" PKY)
                (dir-exists? (string-append root "/x-badtext2")))
      (list (list 'error 'bad-request '(reason packet-malformed) (list 'file PKX)) #f
            (list 'error 'bad-request '(reason packet-malformed) (list 'file PKY)) #f))
(want "CR-3 the main store is not written by the scope: its applied cut is the one before, and no letter names the scope"
      (in-order (equal? (reduce-applied-cut (state MF)) cut-before)
                (find (lambda (id) (equal? (field-of MF id 'scope) XF)) (outline-order MF)))
      '(#t #f))
(define XW (string-append root "/scoped-whole"))
(define SW (scope-run MF "author" XW "--freeze" FZ "--roots" "all" "--for" "codex"))
(want "CR-4 --roots all copies every top-level block, each at the top level"
      (in-order (car SW)
                (list-sort string<? (map (lambda (id) (field-of XW id 'origin))
                                         (filter (lambda (id) (and (eq? (parent-of XW id) 'root) (not (equal? id (one SW 'letter-copy)))))
                                                 (outline-order XW)))))
      (list 'ok (list-sort string<? (list HA HB))))
(want "CR-5 a candidate's review-results with no verdict is pending, and a collect of it answers that and writes nothing"
      (in-order (run XF "codex" 'review-results)
                (first-datum (cli "collect-pending" "author" "collect" XF "--home" HF "--store" MF))
                (equal? (reduce-applied-cut (state MF)) cut-before))
      (list (list 'pending (list 'letter-copy LCF) '(status "unread")) (list 'pending (list 'letter-copy LCF) '(status "unread")) #t))
(want "CR-6 a --cut other than the freeze's pinned cut is refused, and a freeze that cannot be read is refused, both before anything is made"
      (in-order (caddr (scope-run MF "author" (string-append root "/x-mismatch") "--freeze" FZ "--cut" "()" "--roots" HA "--for" "codex"))
                (caddr (scope-run MF "author" (string-append root "/x-nofreeze") "--freeze" (string-append root "/nosuch") "--roots" HA "--for" "codex"))
                (dir-exists? (string-append root "/x-mismatch")) (dir-exists? (string-append root "/x-nofreeze"))
                (caddr (scope-run MF "author" (string-append root "/x-badpacket") "--freeze" FZ "--roots" HA "--for" "codex" "--packet" PKB))
                (caddr (scope-run MF "author" (string-append root "/x-nobytes") "--freeze" FZB "--roots" HA "--for" "codex"))
                (dir-exists? (string-append root "/x-badpacket")) (dir-exists? (string-append root "/x-nobytes"))
                (caddr (scope-run MF "author" (string-append root "/x-home") "--freeze" FZ "--roots" HA "--for" "codex"
                                  "--home" (string-append root "/x-home/home")))
                (dir-exists? (string-append root "/x-home")))
      '((reason freeze-cut-mismatch) (reason freeze-unreadable) #f #f (reason packet-malformed) (reason freeze-unreadable) #f #f
        (reason home-inside-scope) #f))
;; THE SCOPE'S OWN BLOCKS ARE NOT RESULTS when the scope's actor is the
;; reader: a packet section made by `author` for `author` is not a verdict.
(define XS (string-append root "/scoped-self"))
(define SS (scope-run MF "author" XS "--freeze" FZ "--roots" HB "--for" "author" "--packet" PK))
(want "CR-6b a scope whose actor is its reader, with a packet: its review-results is pending, the packet is not a verdict"
      (in-order (car SS) (car (run XS "author" 'review-results)))
      '(ok pending))

;; THE STAND-IN REVIEWER, a process: the mail its hook names, read and then
;; answered; a.txt's copy set to something else and back, so its src is the
;; reviewer's and unchanged; an edit of b.txt's copy committed under its own
;; writer; a verdict with sources and the four conditions, and a finding
;; about the copy it edited. It prints what it did.
(define reviewer (string-append root "/reviewer.ss"))
(call-with-output-file reviewer
  (lambda (p)
    (write '(import (chezscheme) (theourgia rpc) (only (theourgia store) open-and-reduce)
                    (only (theourgia reduce) state-read state-block-ids block-id))
           p)
    (put-string p (string-append "
(define store (getenv \"THEOURGIA_STORE\"))
(define me (getenv \"THEOURGIA_ACTOR\"))
(define (run . args) (rpc-dispatch store args me))
(define (field-of id k) (let* ((b (state-read (open-and-reduce store) id)) (f (and b (assq k (cdr (assq 'fields b)))))) (and f (cdr f))))
(define (new-id a) (let* ((ev (assq 'events (cdr (car (cadr a))))) (e (car (cadr ev)))) (block-id (car e) (cdr e))))
(define (insert parent fields) (new-id (run 'batch (format \"~s\" (list (list 'insert parent #f fields))))))
(system \"sh '" tree "/contrib/mail-hook.sh' PostToolUse > '" root "/reviewer-hook.out'\")
(define hook (call-with-input-file \"" root "/reviewer-hook.out\" get-string-all))
(define (index-of text needle)
  (let find ((i 0)) (cond ((> (+ i (string-length needle)) (string-length text)) #f)
                          ((string=? (substring text i (+ i (string-length needle))) needle) i)
                          (else (find (+ i 1))))))
(define lc (let* ((head (string-append \"unread mail for \" me \": \"))
                  (from (+ (index-of hook head) (string-length head))))
             (substring hook from (+ from (index-of (substring hook from (string-length hook)) \". Read\")))))
(define read-set (car (run 'set lc \"status\" \"read\")))
(define (copy-of origin) (find (lambda (id) (equal? (field-of id 'origin) origin)) (state-block-ids (open-and-reduce store))))
(define edit (copy-of (getenv \"REVIEW_EDIT\")))
(define read-too (copy-of (getenv \"REVIEW_READ\")))
(run 'set read-too \"src\" \"tried something else\\n\")
(run 'set read-too \"src\" \"one, as the change has it\\n\")
(run 'write edit \"two, as the reviewer proposes\\n\" \"--writer\" me)
(define committed (run 'commit \"--writer\" me))
(define v (insert lc (list '(kind . section) '(title . \"Verdict\") '(verdict . \"findings\")
                           (cons 'sources (list read-too edit)) '(model . \"m\") '(approval . \"never\")
                           '(sandbox . \"workspace-write\") '(effort . \"high\"))))
(define f (insert v '((kind . section) (title . \"b.txt\") (claim . \"two should say so\") (evidence . \"line 1\") (confidence . \"high\"))))
(run 'link f \"about\" edit)
(run 'set lc \"status\" \"answered\")
(define read-at-baseline (car (run 'read edit \"--cut\" (field-of lc 'baseline))))
(write (list 'reviewed lc v f (car committed) read-set read-at-baseline))
(newline)
"))))
(define reviewed
  (first-datum (sh-out "reviewer" (string-append "PATH='" root "/bin':\"$PATH\" "
                                                 (env (string-append "THEOURGIA_ACTOR=codex THEOURGIA_HOME='" HF "' THEOURGIA_STORE='" XF "' REVIEW_EDIT='" CB
                                                                     "' REVIEW_READ='" CA "'"))
                                                 "scheme --script '" reviewer "'"))))
(define VF (and (pair? reviewed) (eq? (car reviewed) 'reviewed) (caddr reviewed)))
(define FF (and VF (cadddr reviewed)))
(want "CR-7 the reviewer process found its letter through the hook, committed its edit, and set the letter answered; the status is its"
      (in-order (and (pair? reviewed) (car reviewed)) (and (pair? reviewed) (cadr reviewed)) (and (pair? reviewed) (list-ref reviewed 4))
                (and (pair? reviewed) (list-ref reviewed 5)) (and (pair? reviewed) (list-ref reviewed 6))
                (field-of XF LCF 'status)
                (let ((r (run XF "codex" 'review-results)))
                  (and (pair? r) (eq? (car r) 'ok)
                       (cadr (assq 'status (cdr (cadr r)))))))
      (list 'reviewed LCF 'ok 'ok 'ok "answered" "answered"))
;; THE HOME IS THE STORE'S: the reader's writes registered under the review
;; directory's home and not under this process's; a write here, under this
;; process's home, is refused.
(define (nonce-of store)
  (let ((d (first-datum (file-text (string-append store "/instance.sexp")))))
    (and (list? d) (let ((n (assq 'nonce d))) (and n (pair? (cdr n)) (format "~a" (cadr n)))))))
(want "CR-7b the scoped store's writes register under its own home, not this process's; a write under another home is refused"
      (let ((n (nonce-of XF)))
        (in-order (and n #t)
                  (and n (string-contains? (file-text (string-append HF "/instances.sexp")) n))
                  (and n (string-contains? (file-text (string-append root "/home/instances.sexp")) n))
                  (let ((a (run XF "codex" 'set LCF "title" "from another home"))) (and (pair? a) (list-head a (min 3 (length a)))))))
      '(#t #t #f (error refused (instance machine))))
(define RF (run XF "codex" 'review-results))
(define RF-results (and (pair? RF) (eq? (car RF) 'ok) (cadr RF)))
(define (rpart key) (and RF-results (let ((p (assq key (cdr RF-results)))) (and p (if (memq key '(blocks proposals)) (cdr p) (cadr p))))))
(want "CR-8 complete results: ok, with the letter's fields, the verdict and the finding"
      (in-order (and (pair? RF) (car RF)) (rpart 'letter) (rpart 'to) (rpart 'letter-copy) (rpart 'scope)
                (equal? (rpart 'roots) (list HA HB)) (string? (rpart 'cut)) (map cadr (or (rpart 'blocks) '())))
      (list 'ok 'new "codex" LCF XF #t #t (list VF FF)))
(want "CR-9 the copy the reviewer edited is a proposal, on its finding; the copy it did not is not"
      (map (lambda (p) (list (cadr (assq 'origin (cdr p))) (cadr (assq 'src (cdr p))) (cadr (assq 'on (cdr p)))))
           (or (rpart 'proposals) '()))
      (list (list CB (string->utf8 "two, as the reviewer proposes\n") FF)))

;; THE LANDING, then the collect: the main store's cut is the freeze's, so
;; landing-commit commits; the collect makes the letter and drafts the
;; proposal for W, from the command line, so the program's options are read.
(define landed (tool-run "landing" "landing-commit.sc" MF FZ))
(want "CR-10 landing-commit commits the frozen drafts: the scope moved nothing"
      (car landed)
      'ok)
(define CF (first-datum (cli "collect-freeze" "author" "collect" XF "--to-writer" W "--home" HF "--store" MF)))
(define LF (and (pair? CF) (one CF 'letter)))
(define (collected-in store sid)
  (let ((st (state store)))
    (filter (lambda (id) (let ((c (block-creation st id)))
                           (and c (let ((p (assq 'scoped-id (cadr (cdr c))))) (and p (equal? (cdr p) sid))))))
            (outline-order store))))
(define VF* (and VF (let ((c (collected-in MF VF))) (and (pair? c) (car c)))))
(define FF* (and FF (let ((c (collected-in MF FF))) (and (pair? c) (car c)))))
(want "CR-11 collect --letter new: the letter made with the letter's fields and the letter copy's id, the results under it, the finding's edge on the committed block"
      (in-order (and (pair? CF) (list-head CF 4)) (string? LF)
                (and LF (map (lambda (k) (field-of MF LF k)) '(kind to status scope scoped-id freeze)))
                (and VF* (parent-of MF VF*)) (and FF* (parent-of MF FF*)) (and FF* (edges-of MF FF*))
                (and LF (equal? (field-of MF LF 'cut) (field-of XF LCF 'cut))) (and LF (field-of MF LF 'roots))
                (and VF* (map (lambda (k) (field-of MF VF* k)) '(verdict sources model effort)))
                (and FF* (map (lambda (k) (field-of MF FF* k)) '(claim evidence confidence))))
      (list '(ok (inserted 2) (linked 1) (already 0)) #t (list 'doc "codex" "answered" XF LCF FZ) LF VF*
            (list (cons 'about CB)) #t (list HA HB)
            (list "findings" (list (copy-in XF CA) (copy-in XF CB)) "m" "high") '("two should say so" "line 1" "high")))
(define (w-drafts) (let ((a (run MF "author" 'drafts "--writer" W))) (and (pair? a) (eq? (car a) 'ok) a)))
(want "CR-12 the proposal is a draft for W on b.txt's block, named on the finding's copy; a.txt's block has none; the store's b.txt is unchanged"
      (in-order (and (pair? CF) (map car (or (one CF 'drafted) '())))
                (and (pair? CF) (map cadr (or (one CF 'drafted) '())))
                (and FF* (map car (or (field-of MF FF* 'proposed) '())))
                (field-of MF CB 'src))
      (list '(draft) (list CB) (list CB) (string->utf8 "two\n")))
(define (w-live)
  (let ((a (w-drafts)))
    (map (lambda (d) (list (cadr (assq 'block (cdr d))) (cadr (assq 'version (cdr d))) (cadr (assq 'fresh (cdr d)))))
         (filter (lambda (x) (and (pair? x) (eq? (car x) 'draft))) (if a (cdr (assq 'items (cdr a))) '())))))
(want "CR-12b W holds exactly one draft, on b.txt's block, based on the landed block, its version the one named on the finding's copy"
      (in-order (map car (w-live)) (map caddr (w-live))
                (and FF* (equal? (map cadr (w-live)) (map cadr (or (field-of MF FF* 'proposed) '())))))
      (list (list CB) '(#t) #t))
(define WX (string-append root "/w-export"))
(run MF "author" 'export-code WX "--working" "--writer" W "--raw")
(want "CR-13 W's working export shows the proposal, and a.txt as landed"
      (in-order (file-text (string-append WX "/b.txt")) (file-text (string-append WX "/a.txt")))
      '("two, as the reviewer proposes\n" "one, as the change has it\n"))
(define before-again (events-applied MF))
(want "CR-14 the same collect again: nothing inserted or linked, one letter, the provenance not written again"
      (let ((a (first-datum (cli "collect-freeze-2" "author" "collect" XF "--to-writer" W "--home" HF "--store" MF))))
        (in-order (and (pair? a) (list-head a 4)) (length (collected-in MF LCF))
                  (equal? (and (pair? a) (one a 'letter)) LF) (- (events-applied MF) before-again)))
      (list '(ok (inserted 0) (linked 0) (already 1)) 1 #t 0))
(run MF "author" 'write CB "two, as W has it now\n" "--writer" W)
(define again-1 (first-datum (cli "collect-freeze-3" "author" "collect" XF "--to-writer" W "--home" HF "--store" MF)))
(define W-after-rewrite (begin (run MF "author" 'export-code (string-append WX "-2") "--working" "--writer" W "--raw")
                               (file-text (string-append WX "-2/b.txt"))))
(run MF "author" 'discard CB "--writer" W)
(define again-2 (first-datum (cli "collect-freeze-4" "author" "collect" XF "--to-writer" W "--home" HF "--store" MF)))
(want "CR-14b a collect after the receiver rewrote its draft keeps the receiver's bytes; after a discard it drafts nothing: the proposal was handed over"
      (in-order (and (pair? again-1) (one again-1 'drafted)) W-after-rewrite
                (and (pair? again-2) (one again-2 'drafted)) (map car (w-live)))
      (list (list (list 'already CB)) "two, as W has it now\n" (list (list 'already CB)) '()))
(want "CR-4b --roots all once the store has moved past the freeze's cut is refused, and nothing is made"
      (in-order (caddr (scope-run MF "author" (string-append root "/x-whole-moved") "--freeze" FZ "--roots" "all" "--for" "codex"))
                (dir-exists? (string-append root "/x-whole-moved")))
      '((reason roots-all-at-another-cut) #f))

;; ---- a candidate with a file it adds and a holder it deletes -----------------------
;;
;; A freeze written by hand in part 1's format: a new file, c.sc, of two
;; definitions, under its provisional id; a deletion of a.txt's holder; a draft of a.txt's block
;; equal to its committed src (no `before`), and one of b.txt's that is not.
;; The scope has a packet too. A landing's answer, also by hand, maps the
;; provisional id to the holder it made, made here by the landing's own
;; construction, so it has the pieces the landing gives the file.
;;
;; ON A TREE WITHOUT THE CANDIDATE'S SCOPE the scoped store below is never
;; made, and what reads it raises; such a statement answers #f instead, so
;; the rows report red one by one rather than ending the file.
(define-syntax safely (syntax-rules () ((_ e) (guard (x (#t #f)) e))))
(define (provisional-of path) (string-append "new-" (substring (bytevector->hex (sha256 (string->utf8 path))) 0 56)))
(define PID (provisional-of "c.sc"))
(define C-SC "(define c1 1)\n(define c2 2)\n")
(define C-PIECES (vector->list (file-pieces "c.sc" (string->utf8 C-SC))))
(define (piece-of k) (string-append PID "/" (number->string k)))
(define FZ2 (string-append root "/freeze2"))
(system (string-append "mkdir -p '" FZ2 "' && printf 'one, as the change has it\\n' > '"
                       FZ2 "/2.bytes' && printf 'two, frozen\\n' > '" FZ2 "/3.bytes'"))
(call-with-port (open-file-output-port (string-append FZ2 "/1.bytes") (file-options no-fail)) (lambda (p) (put-bytevector p (string->utf8 C-SC))))
;; A SECOND WRITER'S RECORD, so the store's cut names two writers; the
;; freeze below lists them in the other order, as drafts-dump may.
(define peer-record
  (encode-record 1 1789000000011 "peer" (reduce-applied-cut (state MF))
                 (storable-encode '(put ((kind . section) (title . "a second writer's block") (parent . root) (ord . 900))))))
(log-publish! MF "peerzzzz" 1 peer-record (segment-sha peer-record))
(define pinned2 (reduce-applied-cut (state MF)))
(define pinned2-other-order (reverse pinned2))
(call-with-output-file (string-append FZ2 "/DRAFTS.sexp")
  (lambda (p)
    (write (list 'drafts '(store "s") (list 'writer W) (list 'pinned-cut pinned2-other-order)
                 (list 'draft (list 'block CA) (list 'version (make-string 64 #\3)) '(bytes "2.bytes"))
                 (list 'draft (list 'block CB) (list 'version (make-string 64 #\4)) '(bytes "3.bytes"))
                 (list 'new (list 'provisional PID) '(path "c.sc") (list 'version (make-string 64 #\0)) (list 'cut pinned2)
                       '(bytes "1.bytes") (list 'sha256 (bytevector->hex (sha256 (string->utf8 C-SC)))))
                 (list 'delete (list 'holder HA) (list 'version (make-string 64 #\1)) (list 'based-on (make-string 64 #\2)) (list 'cut pinned2)))
           p)))
(define X2 (string-append root "/scoped-new"))
(define S2 (scope-run MF "author" X2 "--freeze" FZ2 "--roots" PID "--roots" HA "--roots" HB "--for" "codex" "--packet" PK))
(define LC2 (one S2 'letter-copy))
(define NH (safely (copy-in X2 PID)))
(define NC (safely (car (children-in X2 NH))))
(define NC1 (safely (cadr (children-in X2 NH))))
(define CA2 (safely (copy-in X2 CA)))
(define CB2 (safely (copy-in X2 CB)))
(define BB2 (safely (car (children-in X2 CB2))))
(define PS2 (safely (find (lambda (id) (field-of X2 id 'packet)) (outline-order X2))))
(want "CR-15 a file the freeze adds is a holder under its provisional id with the landing's pieces, each <id>/<K>; a deleted holder's copy says proposed and keeps its block; a draft equal to its src has no before, one that differs has"
      (in-order (car S2) (safely (field-of X2 NH 'path)) (safely (field-of X2 NH 'kind)) (>= (length C-PIECES) 2)
                (safely (map (lambda (c) (list (field-of X2 c 'origin) (field-of X2 c 'src))) (children-in X2 NH)))
                (safely (field-of X2 (copy-in X2 HA) 'deleted))
                (safely (list (equal? (parent-of X2 CA2) (copy-in X2 HA)) (field-of X2 CA2 'src) (children-in X2 CA2)))
                (safely (list (field-of X2 CB2 'src) (field-of X2 BB2 'before-of) (field-of X2 BB2 'src))))
      (list 'ok "c.sc" 'file #t (map (lambda (piece k) (list (piece-of k) piece)) C-PIECES (iota (length C-PIECES))) "proposed"
            (list #t (string->utf8 "one, as the change has it\n") '())
            (list (string->utf8 "two, frozen\n") CB (string->utf8 "two\n"))))
(define XW3 (string-append root "/scoped-whole-new"))
(define SW3 (scope-run MF "author" XW3 "--freeze" FZ2 "--roots" "all" "--for" "codex"))
(define X2c (string-append root "/scoped-new-cut"))
(define S2c (scope-run MF "author" X2c "--freeze" FZ2 "--cut" (format "~s" pinned2) "--roots" HB "--for" "codex"))
(want "CR-16a the store's cut names two writers, the freeze lists them in the other order; a --cut in the store's order is the pinned cut"
      (in-order (length pinned2) (equal? pinned2 pinned2-other-order) (car S2c))
      '(2 #f ok))
(want "CR-16 --roots all of a freeze copies the files it adds as well as every top-level block, the freeze's cut in the other writer order"
      (in-order (car SW3)
                (safely (let ((os (map (lambda (id) (field-of XW3 id 'origin))
                                       (filter (lambda (id) (eq? (parent-of XW3 id) 'root)) (outline-order XW3)))))
                          (list (and (member PID os) #t) (and (member HA os) #t) (and (member HB os) #t)))))
      '(ok (#t #t #t)))
;; THE RESULTS, MADE COMPLETE A PIECE AT A TIME: each step reads the first gap.
;; The author changes b.txt's copy (not a proposal: not the reader's), and the
;; reader writes a note at the root about a.txt's copy (a result, not a verdict).
(safely (run X2 "author" 'set CB2 "src" "the author's change\n"))
(define RN (safely (insert-as X2 "codex" 'root '((kind . section) (title . "a note at the root")))))
(safely (run X2 "codex" 'link RN "about" CA2))
(define (gap) (let ((a (safely (run X2 "codex" 'review-results)))) (if (and (pair? a) (eq? (car a) 'error)) (cddr a) (and (pair? a) (car a)))))
(define g-none (gap))
(define V2 (safely (insert-as X2 "codex" LC2 '((kind . section) (title . "Verdict")))))
(define F2 (safely (insert-as X2 "codex" V2 '((kind . section) (title . "c.sc's first piece")))))
(define (set2 id k v) (safely (run X2 "codex" 'set id k v)))
(define gaps '())
(define (step! . sets) (for-each (lambda (s) (apply set2 s)) sets) (set! gaps (append gaps (list (gap)))))
(step!)
(step! (list V2 "verdict" "approved"))
(step! (list V2 "verdict" "findings"))
(step! (list V2 "sources" ""))
(step! (list V2 "sources" LC2))
(step! (list V2 "sources" (or PS2 "")))
(step! (list V2 "sources" (or BB2 "")))
(step! (list V2 "sources" (string-append (or NC "") " zz.404")))
(step! (list V2 "sources" (string-append (or PS2 "") " " (or NC ""))))
(step! (list V2 "sources" (string-append LC2 " " (or NC ""))))
(step! (list V2 "sources" (string-append (or BB2 "") " " (or NC ""))))
(step! (list V2 "model" "m"))
(step! (list V2 "approval" "a"))
(step! (list V2 "sandbox" "s"))
(step! (list V2 "effort" "e"))
(step! (list F2 "claim" "c"))
(step! (list F2 "evidence" "e"))
(step! (list F2 "confidence" "low"))
(safely (run X2 "codex" 'link F2 "about" NC))
(step!)
(step! (list V2 "verdict" "no-objection"))
(step! (list V2 "verdict" "could-not-review"))
(step! (list V2 "verdict" "findings"))
(want "CR-17 pending while only a note at the root is the reader's"
      g-none
      'pending)
(want "CR-18 results-incomplete names each gap in turn: the verdict's value, sources absent, empty, no copy among them (the letter copy, a packet section, a before alone), an absent id; a packet section, the letter copy or a before beside a copy counts as the copy; each condition, each finding field, the about; then ok for each verdict value"
      gaps
      (append (map (lambda (k) (list (list 'block V2) (list 'field k))) '(verdict verdict sources sources sources sources sources sources model model model approval sandbox effort))
              (map (lambda (k) (list (list 'block F2) (list 'field k))) '(claim evidence confidence about))
              '(ok ok ok ok)))
;; THE READER'S EDITS: c.sc's second piece and a.txt's block; neither has a
;; finding about it (the finding is about the first piece), so both
;; proposals are the verdict's.
(set2 NC1 "src" "(define c2 22)\n")
(set2 CA2 "src" "one, proposed again\n")
(define RR2 (safely (run X2 "codex" 'review-results)))
(define complete2 (and (pair? RR2) (eq? (car RR2) 'ok) (cadr RR2)))
(define (part2 key) (and complete2 (let ((p (assq key (cdr complete2)))) (and p (if (memq key '(blocks proposals candidates)) (cdr p) (cadr p))))))
(want "CR-19 the proposals: c.sc's second piece and a.txt's block, each on the verdict; b.txt's, the author's change, is none; the note at the root has the parent root"
      (in-order (map (lambda (p) (list (cadr (assq 'origin (cdr p))) (cadr (assq 'scoped (cdr p))) (cadr (assq 'on (cdr p)))))
                     (or (part2 'proposals) '()))
                (let ((b (find (lambda (b) (equal? (cadr b) RN)) (or (part2 'blocks) '())))) (and b (cadr (assq 'parent (cddr b))))))
      (list (list (list (piece-of 1) NC1 V2) (list CA CA2 V2)) 'root))
(define (set-field d id k v)
  (cons 'results
        (map (lambda (part)
               (if (eq? (car part) 'blocks)
                   (cons 'blocks (map (lambda (b) (if (equal? (cadr b) id)
                                                      (cons 'block (cons (cadr b) (map (lambda (x) (if (eq? (car x) 'fields)
                                                                                                         (list 'fields (append (filter (lambda (f) (not (eq? (car f) k))) (cadr x))
                                                                                                                               (if v (list (list k v "codex")) '())))
                                                                                                         x))
                                                                                       (cddr b))))
                                                      b))
                                      (cdr part)))
                   (if (eq? (car part) 'proposals)
                       (cons 'proposals (map (lambda (p) (map (lambda (x) (if (and (pair? x) (eq? (car x) 'on) (eq? k 'on)) (list 'on v) x)) p)) (cdr part)))
                       part)))
             (cdr d))))
(define (refused-into name d . more)
  (let* ((n (events-applied MF))
         (a (safely (apply run MF "author" 'collect-into "--letter" "new" "--results" (format "~s" d) more))))
    (list (and (pair? a) (if (eq? (car a) 'error) (list-head a (min 3 (length a))) (car a))) (- (events-applied MF) n))))
(want "CR-20 collect-into refuses a candidate's datum, writing nothing: a verdict's field missing, a source that is not a copy, a proposal on no result, an unreadable map"
      (list (safely (refused-into "x1" (set-field complete2 V2 'verdict #f)))
            (safely (refused-into "x2" (set-field complete2 V2 'sources LC2)))
            (safely (refused-into "x3" (set-field complete2 'none 'on "zz.404")))
            (safely (refused-into "x4" complete2 "--map" (string-append root "/no-such-map.sexp"))))
      (list (list (list 'error 'results-incomplete (list 'block V2)) 0)
            (list (list 'error 'results-incomplete (list 'block V2)) 0)
            (list (list 'error 'bad-request '(reason results-malformed)) 0)
            (list (list 'error 'bad-request '(reason map-unreadable)) 0)))
(want "CR-21 the letter and the results are one write: a result the write refuses leaves no letter"
      (let* ((n (events-applied MF)) (a (safely (run MF "author" 'collect-into "--letter" "new" "--results"
                                                    (format "~s" (set-field complete2 V2 'ord 1))))))
        (in-order (and (pair? a) (not (eq? (car a) 'ok))) (length (collected-in MF LC2)) (- (events-applied MF) n)))
      '(#t 0 0))
;; THE LANDING'S ANSWER, as landing-commit prints it: the provisional id to
;; the holder it made. The holder and its pieces are made here by the
;; landing's own construction. W holds a draft of a.txt's block of its own.
(define landed-new
  (let ((a (run MF "author" 'batch (format "~s" (file-new-intents "c.sc" (string->utf8 C-SC) 0)))))
    (map (lambda (item) (new-id item)) (cadr a))))
(define NH* (car landed-new))
(define NC*0 (cadr landed-new))
(define NC*1 (caddr landed-new))
(define LANDED (string-append root "/LANDED.sexp"))
(call-with-output-file LANDED
  (lambda (p) (write (list 'ok '(committed 1) (list 'created (cons PID NH*)) (list 'pieces (cons (piece-of 0) NC*0) (cons (piece-of 1) NC*1))) p)
              (newline p)))
(run MF "author" 'write CA "a, as W has it\n" "--writer" W)
(define C2a (safely (run MF "author" 'collect-into "--letter" "new" "--results" (format "~s" complete2))))
(define drafts-after-a (map car (w-live)))
(define C2b (first-datum (cli "collect-new" "author" "collect" X2 "--map" LANDED "--to-writer" W "--store" MF)))
(define F2* (let ((c (collected-in MF F2))) (and (pair? c) (car c))))
(define V2* (let ((c (collected-in MF V2))) (and (pair? c) (car c))))
(define RN* (let ((c (collected-in MF RN))) (and (pair? c) (car c))))
(want "CR-22 without the map: the provisional origin named unresolved and not linked, the note's edge linked, the proposals counted and nothing drafted"
      (in-order (and (pair? C2a) (list-head C2a 4)) (and (pair? C2a) (member (list 'unresolved F2 (list 'origin (piece-of 0))) C2a) #t)
                (and (pair? C2a) (one C2a 'proposals)) (and (pair? C2a) (one C2a 'drafted)) drafts-after-a
                (and RN* (edges-of MF RN*)))
      (list '(ok (inserted 3) (linked 1) (already 0)) #t 2 '() (list CA) (list (cons 'about CA))))
(want "CR-23 the collect with the landing's map, from the command line: the finding linked to the landed file's first piece; the second piece's proposal drafted on its second block; a.txt's held, W's own draft kept"
      (in-order (and (pair? C2b) (list-head C2b 4)) (and F2* (edges-of MF F2*))
                (and (pair? C2b) (map (lambda (x) (list (car x) (cadr x))) (or (one C2b 'drafted) '())))
                (and V2* (map car (or (field-of MF V2* 'proposed) '())))
                (begin (run MF "author" 'export-code (string-append root "/w-export-new") "--working" "--writer" W "--raw")
                       (list (file-text (string-append root "/w-export-new/c.sc")) (file-text (string-append root "/w-export-new/a.txt")))))
      (list '(ok (inserted 0) (linked 1) (already 1)) (list (cons 'about NC*0))
            (list (list 'draft NC*1) (list 'held CA)) (list NC*1)
            (list (string-append (utf8->string (car C-PIECES)) "(define c2 22)\n") "a, as W has it\n")))
;; A PIECE THE LANDED FILE DOES NOT HAVE: an about and a proposal on c.sc's
;; tenth piece, through the map, are named and nothing is written.
(define (with-piece-nine d)
  (cons 'results
        (map (lambda (part)
               (case (car part)
                 ((blocks) (cons 'blocks (map (lambda (b) (if (equal? (cadr b) F2)
                                                              (cons 'block (cons (cadr b) (map (lambda (x) (if (eq? (car x) 'about) (list 'about (append (cadr x) (list (list 'origin (piece-of 9))))) x)) (cddr b))))
                                                              b))
                                              (cdr part))))
                 ((proposals) (cons 'proposals (map (lambda (p) (map (lambda (x) (if (and (pair? x) (eq? (car x) 'origin) (equal? (cadr x) (piece-of 1))) (list 'origin (piece-of 9)) x)) p))
                                                    (cdr part))))
                 (else part)))
             (cdr d))))
(define C2c (let* ((n (events-applied MF))
                   (a (safely (run MF "author" 'collect-into "--letter" "new" "--results" (format "~s" (with-piece-nine complete2))
                                   "--map" LANDED "--to-writer" W))))
              (list a (- (events-applied MF) n))))
(want "CR-23b a piece the landing's clause does not name: the about and the proposal named unresolved, nothing written"
      (let ((a (car C2c)))
        (in-order (and (pair? a) (list-head a 4)) (and (pair? a) (member (list 'unresolved F2 (list 'origin (piece-of 9))) a) #t)
                  (and (pair? a) (member (list 'unresolved NC1 (list 'origin (piece-of 9))) (or (one a 'drafted) '())) #t) (cadr C2c)))
      (list '(ok (inserted 0) (linked 0) (already 2)) #t #t 0))
;; THE LANDED FILE CHANGES SHAPE before the next collect: a block inserted
;; under its holder, its first piece moved after that block, its second moved
;; to the top level. A piece is the block the landing named, wherever it is
;; now: the edge is found already there and the proposal already handed over.
(define NEW* (insert-as MF "author" NH* '((kind . code) (mode . text) (src . #vu8(40 100 101 102 105 110 101 32 122 32 48 41 10)))))
(run MF "author" 'move NC*0 NH* "--after" NEW*)
(run MF "author" 'move NC*1 "root")
(define C2d (first-datum (cli "collect-new-2" "author" "collect" X2 "--map" LANDED "--to-writer" W "--store" MF)))
(want "CR-23c after the landed file changed shape, a collect with the map adds no edge, links nothing new and drafts nothing on the inserted block"
      (in-order (and (pair? C2d) (list-head C2d 4)) (and F2* (edges-of MF F2*))
                (and (pair? C2d) (map (lambda (x) (list (car x) (cadr x))) (or (one C2d 'drafted) '()))))
      (list '(ok (inserted 0) (linked 0) (already 2)) (list (cons 'about NC*0)) (list (list 'already NC*1) (list 'held CA))))
(want "CR-24 an old-style results datum naming an origin shaped like a provisional id is collected as before: not named unresolved"
      (let ((a (run M "author" 'collect-into L "--results"
                    (format "~s" (list 'results '(letter "x") '(to "codex")
                                       (list 'blocks (list 'block "zz.7" '(parent letter) '(fields ((title "legacy" "codex")))
                                                           (list 'about (list (list 'origin PID))))))))))
        (and (pair? a) (exists (lambda (c) (and (pair? c) (eq? (car c) 'unresolved))) (cdr a))))
      #f)

;; ---- the hook: a failed query is not an empty mailbox -----------------------------
;;
;; A `theourgia` that fails is put first on the PATH: it prints two lines,
;; the first on stdout, and exits 7.
(system (string-append "mkdir -p '" root "/bin-fail' && printf '#!/bin/sh\\necho first line\\necho second line >&2\\nexit 7\\n' > '"
                       root "/bin-fail/theourgia' && chmod +x '" root "/bin-fail/theourgia'"))
(define (hook-rc name store . bin)
  (sh-rc name (string-append "PATH='" (if (pair? bin) (car bin) (string-append root "/bin")) "':\"$PATH\" THEOURGIA_STORE='" store
                             "' THEOURGIA_ACTOR=nobody sh ../contrib/mail-hook.sh PostToolUse")))
(want "CR-25 the hook exits 1 with one line naming a failed query, its exit status and its first line; 0 with nothing when there is no mail"
      (in-order (hook-rc "hook-fail" (string-append root "/no-store-here"))
                (string-contains? (file-text (string-append root "/hook-fail.err")) "theourgia: mail query failed for nobody")
                (file-text (string-append root "/hook-fail.out"))
                (hook-rc "hook-fail-7" M (string-append root "/bin-fail"))
                (file-text (string-append root "/hook-fail-7.err"))
                (hook-rc "hook-none" M) (file-text (string-append root "/hook-none.out")) (file-text (string-append root "/hook-none.err")))
      '(1 #t "" 1 "theourgia: mail query failed for nobody (exit 7): first line\n" 0 "" ""))

;; The directories are kept when a row failed: they are what the row read.
(teardown!)
(when (= bad 0) (system (string-append "rm -rf '" root "' '" sock-root "'")))
(printf "\n~a failures\nrows: ~a\nreview-channel complete\n" bad rows)
(exit (if (= bad 0) 0 1))
