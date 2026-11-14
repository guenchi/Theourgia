;; Copyright 2018 - 2026 guenchi
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;     http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; THE EVIDENCE INDEX READS ITS FORK AT ITS OWN INSTANT (evidence-index.sc's
;; scan-store, log.sc's quarantine-of, store.sc's index-fork check).
;;
;;   IF-1a  quarantine.sexp ADDED without the lock while a write session holds
;;          after its discovery, two tracked records at or above the fork (one
;;          at the delivered cut, one below it): the holding session answers
;;          quarantined, as a later session with its own cut does and a cold
;;          one (the control); under only:index-fork the check fires for each,
;;          and for the first asked as a batch
;;   IF-1b  the file REMOVED at the same hold: the holding session answers
;;          undelivered, a later one replay, a cold one replay (the control)
;;   IF-3   the fork is each writer's own: two writers, each enumeration order
;;          met in one of two stores
;;   IF-4   asked of indexed-records directly, cold: an unreadable owner.sexp
;;          beside a readable quarantine file, an unreadable quarantine file
;;          (the inventory raises first, both trees), one that is a directory,
;;          an incomplete publication with and without one, a retired writer
;;   IF-5   forks that are not integers, pinned as they are
;;   IF-6   an old derivation's checkpoint is rebuilt once and keeps its
;;          missing entries; the new code accepts its own checkpoint
;;   IF-7   store-evidence as before; a plain verdict handed an earlier cut is
;;          not checked, under only:index-fork
;;   IF-8   a real write on a store with an in-contract fork, under
;;          only:index-fork: silent, quarantined; and the batch arm
;;   IF-9   a cold rebuild reports progress, and yields, once per file it scans
;;          (segment files, and the files of damaged/ and incoming/)
;;
;; THE CHILDREN PIN THE SWITCH THEMSELVES: unset for the legs as written, and
;; only:index-fork where a row says so, on a tree whose switch knows the name
;; (a tree that does not refuses the name at expansion, which is a start-up
;; failure and not a reading; such a row says so and is red).
(import (chezscheme) (theourgia rpc) (theourgia log) (theourgia trace)
        (only (theourgia store) open-and-reduce make-write-request request-verdict request-verdict/provided
              store-evidence store-snapshot!)
        (only (theourgia reduce) reduce-applied-cut reduce-gates)
        (only (theourgia request) ev-placement ev-event)
        (only (theourgia degeneracy) degeneracy-known-checks)
        (only (theourgia evidence-index) indexed-records index-checkpoint! index-forget-memory!)
        (only (theourgia wire) decode-line encode-record storable-encode storable-decode
              sexpr->string-extended string->sexpr-extended)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (theourgia ffi) io-yield-set! io-yield-tally progress-sink-set!))

(define bad 0)
(define rows-run 0)
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) (caught x)))))))

(define root
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (string-append (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")
                   "/index-fork-" (number->string (get-process-id)))))
(system (string-append "rm -rf '" root "' && mkdir -p '" root "'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))

(define (traced thunk)
  (let ((p (open-output-string)))
    (parameterize ((current-error-port p))
      (trace-enable! #t)
      (guard (e (#t (trace-enable! #f) (raise e))) (thunk))
      (trace-enable! #f))
    (get-output-string p)))
(define (count-lines text prefix)
  (let ((n (string-length prefix)))
    (let loop ((i 0) (start #t) (k 0))
      (cond ((>= i (string-length text)) k)
            ((and start (<= (+ i n) (string-length text)) (string=? (substring text i (+ i n)) prefix))
             (loop (+ i 1) #f (+ k 1)))
            (else (loop (+ i 1) (char=? (string-ref text i) #\newline) k))))))
(define (file-text p) (if (file-exists? p) (call-with-input-file p get-string-all) ""))
(define (read-or-false text)
  (guard (e (#t #f)) (let ((x (read (open-string-input-port text)))) (if (eof-object? x) #f x))))
(define (sym<? a b) (string<? (format "~s" a) (format "~s" b)))

;; ---- stores ----------------------------------------------------------------
(define (fresh-store name)
  (let* ((st (string-append root "/" name))
         (init (rpc-dispatch st '(init) "test"))
         (w (cadr (assq 'writer (cdr init)))))
    (let loop ((k 0))
      (when (< k 4)
        (rpc-dispatch st (list 'insert "--title" (format "b~a" k)) "test")
        (loop (+ k 1))))
    (cons st w)))
(define (end-seq st w) (discovery-end-seq (discover-prefix st w 'shared)))
;; A TRACKED INSERT at the writer's end: -> (req-id after-seq title), the
;; record then at after-seq + 1
(define (tracked! st w req-id title)
  (let ((after (end-seq st w)))
    (rpc-dispatch st (list 'insert "--title" title "--req" req-id "--cursor" (format "~a:~a" w after)) "test")
    (list req-id after title)))
(define (writer-dir st w) (string-append st "/writers/" w))
(define (quarantine! st w fork)
  (call-with-output-file (string-append (writer-dir st w) "/quarantine.sexp")
    (lambda (p) (display (format "((format 1) (fork ~s))\n" fork) p)) 'replace))
(define (unquarantine! st w)
  (let ((q (string-append (writer-dir st w) "/quarantine.sexp"))) (when (file-exists? q) (delete-file q))))
(define (cold-index! st)
  (index-forget-memory!)
  (let ((cp (string-append st "/request-index.sexp"))) (when (file-exists? cp) (delete-file cp))))
;; a verdict's shape: (replay) (execute) or (unknown <reason>)
(define (shape v)
  (cond ((not (pair? v)) (list 'not-a-verdict v))
        ((and (eq? (car v) 'unknown) (pair? (cdr v)) (pair? (cadr v))) (list 'unknown (car (cadr v))))
        (else (list (car v)))))
;; the placements indexed-records answers for IDENTITY, cold, sorted; or the raise
(define (placements st identity)
  (cold-index! st)
  (caught (list-sort sym<? (map (lambda (r) (list-ref r 4)) (indexed-records st identity)))))

;; ---- children ----------------------------------------------------------------
(define (child-paths tag)
  (map (lambda (x) (string-append root "/" tag x)) '(".ss" ".out" ".err" ".done" ".pid")))
(define (start-child! tag setting program args)
  (let ((ps (child-paths tag)))
    (call-with-output-file (car ps) (lambda (p) (for-each (lambda (f) (write f p) (newline p)) program)) 'replace)
    (system (string-append "sh -c \"" setting " scheme --script '" (car ps) "' "
                           (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
                           "> '" (cadr ps) "' 2> '" (caddr ps) "' < /dev/null & echo \\$! > '" (list-ref ps 4)
                           "'; wait \\$!; touch '" (cadddr ps) "'\" &"))
    ps))
(define (await-file path . limit)
  (let ((ticks (if (pair? limit) (car limit) 3000)))
    (let loop ((k 0))
      (cond ((file-exists? path) #t)
            ((> k ticks) #f)
            (else (sleep (make-time 'time-duration 20000000 0)) (loop (+ k 1)))))))
(define (child-result ps . limit)
  (if (apply await-file (cadddr ps) limit)
      (cons (read-or-false (file-text (cadr ps))) (file-text (caddr ps)))
      (let ((pid (read-or-false (file-text (list-ref ps 4)))))
        (when (and (integer? pid) (> pid 0))
          (system (string-append "kill -KILL " (number->string pid) " 2>/dev/null")))
        (printf "the child ~a outlived its wait (pid ~s): killed\n" (car ps) pid)
        (cons 'outlived-its-wait (file-text (caddr ps))))))
;; the cut and the gates of ST and its tracked requests, for a child whose
;; session holds before it could compute them
(define (given-file! st reqs tag)
  (let ((state (open-and-reduce st)) (path (string-append root "/" tag ".given")))
    (call-with-output-file path
      (lambda (p) (write (list (reduce-applied-cut state) (reduce-gates state) reqs) p)) 'replace)
    path))
(define plain-switch "env -u THEOURGIA_DEGENERACY")
(define witness-switch "THEOURGIA_DEGENERACY=only:index-fork")
(define witness-known? (and (memq 'index-fork (degeneracy-known-checks)) #t))
(define index-fork-line "degeneracy: a record filed quarantined is within the delivered cut")

;; THE CHILD: a write session that holds after its discovery (the parent
;; changes the quarantine file meanwhile), then, per tracked request, the
;; session's verdict with its provider; then a later session with its own cut;
;; then a cold plain verdict. In "witness" mode only the holding session's
;; verdicts, a marker on the error port after each. -> (holding later cold)
(define fork-child
  '((import (chezscheme) (theourgia log)
            (only (theourgia store) open-and-reduce make-write-request request-verdict request-verdict/provided)
            (only (theourgia reduce) reduce-applied-cut reduce-gates)
            (only (theourgia degeneracy) degeneracy-checks-on)
            (only (theourgia evidence-index) index-forget-memory!))
    (define args (command-line-arguments))
    (define st (car args))
    (define w (cadr args))
    (define given (call-with-input-file (caddr args) read))
    (define witness? (string=? (cadddr args) "witness"))
    (define (req-of x) (make-write-request "test" 'insert (list "--title" (caddr x)) (car x) (cons w (cadr x))))
    (define (shape v)
      (cond ((not (pair? v)) (list 'not-a-verdict v))
            ((and (eq? (car v) 'unknown) (pair? (cdr v)) (pair? (cadr v))) (list 'unknown (car (cadr v))))
            (else (list (car v)))))
    (define (marked thunk tag)
      (let ((v (thunk))) (display (string-append "MARK " tag "\n") (current-error-port)) v))
    (define s (log-begin st (lambda a 'applied)))
    (define p (session-discovery-provider s))
    ;; IN ORDER, one verdict after the other, so each marker follows its own
    ;; verdict's lines on the error port
    (define holding
      (let loop ((xs (caddr given)) (out '()))
        (if (null? xs)
            (reverse out)
            (let ((v (marked (lambda () (shape (request-verdict/provided p st (req-of (car xs)) (car given) #f (cadr given))))
                             (car (car xs)))))
              (loop (cdr xs) (cons v out))))))
    ;; THE BATCH ARM, in witness mode: the first request asked as a batch
    (define batch
      (and witness?
           (marked (lambda () (shape (request-verdict/provided p st (req-of (car (caddr given))) (car given) 2 (cadr given))))
                   "batch")))
    (log-end! s)
    (define (cold!) (index-forget-memory!)
      (let ((cp (string-append st "/request-index.sexp"))) (when (file-exists? cp) (delete-file cp))))
    (if witness?
        (write (list holding batch (degeneracy-checks-on)))
        (let* ((state (open-and-reduce st)) (cut (reduce-applied-cut state)) (gates (reduce-gates state))
               (s2 (log-begin st (lambda a 'applied)))
               (p2 (session-discovery-provider s2))
               (later (map (lambda (x) (shape (request-verdict/provided p2 st (req-of x) cut #f gates))) (caddr given))))
          (log-end! s2)
          (cold!)
          (write (list holding later
                       (map (lambda (x) (shape (request-verdict st (req-of x) cut #f gates))) (caddr given))))))))
;; -> (<child datum> . <stderr>); CHANGE! is run while the child holds
(define (held-run tag setting st w reqs mode change!)
  (let* ((release (string-append root "/" tag "-release"))
         (given (given-file! st reqs tag))
         (ps (start-child! tag (string-append setting " THEOURGIA_INJECT=on THEOURGIA_HOLD=after-discovery:" release)
                           fork-child (list st w given mode))))
    (if (await-file (string-append release ".held"))
        (begin (change!)
               (call-with-output-file release (lambda (p) (display "go" p)) 'replace)
               (child-result ps))
        (cons (list 'never-held) (cdr (child-result ps))))))
;; the fixed line's count on stderr between the start and each marker
(define (lines-per-mark text tags)
  (let loop ((tags tags) (text text) (out '()))
    (if (null? tags)
        (reverse out)
        (let* ((m (string-append "MARK " (car tags) "\n"))
               (n (string-length m))
               (at (let find ((i 0)) (cond ((> (+ i n) (string-length text)) #f)
                                           ((string=? (substring text i (+ i n)) m) i)
                                           (else (find (+ i 1)))))))
          (if at
              (loop (cdr tags) (substring text (+ at n) (string-length text))
                    (cons (count-lines (substring text 0 at) index-fork-line) out))
              (reverse (cons 'no-mark out)))))))

;; ---- IF-1a: a fork ADDED at the hold -------------------------------------------
(printf "== IF-1a ==\n")
(define a (fresh-store "a"))
(define a-r1 (tracked! (car a) (cdr a) "QA1" "one"))
(define a-r2 (tracked! (car a) (cdr a) "QA2" "two"))
(define a-fork (+ 1 (cadr a-r1)))
(define if1a (held-run "if1a" plain-switch (car a) (cdr a) (list a-r1 a-r2) "plain"
                       (lambda () (quarantine! (car a) (cdr a) a-fork))))
(printf "IF-1a ~s\n" (car if1a))
(want "IF-1a the fork added at the hold: the holding session answers quarantined for both records"
      (and (pair? (car if1a)) (car (car if1a))) '((unknown quarantined) (unknown quarantined)))
(want "IF-1a a later session in the same process, with its own cut: quarantined"
      (and (pair? (car if1a)) (cadr (car if1a))) '((unknown quarantined) (unknown quarantined)))
(want "IF-1a CONTROL: cold, quarantined"
      (and (pair? (car if1a)) (caddr (car if1a))) '((unknown quarantined) (unknown quarantined)))
;; the same under only:index-fork, on a fresh copy of the store as it was
(define a2 (fresh-store "a2"))
(define a2-r1 (tracked! (car a2) (cdr a2) "QB1" "one"))
(define a2-r2 (tracked! (car a2) (cdr a2) "QB2" "two"))
(define if1a-w
  (if witness-known?
      (held-run "if1aw" witness-switch (car a2) (cdr a2) (list a2-r1 a2-r2) "witness"
                (lambda () (quarantine! (car a2) (cdr a2) (+ 1 (cadr a2-r1)))))
      (cons 'index-fork-unknown-on-this-tree "")))
(printf "IF-1a witness ~s; lines per verdict ~s\n" (car if1a-w) (lines-per-mark (cdr if1a-w) '("QB1" "QB2" "batch")))
(want "IF-1a under only:index-fork: the check fires for the record below the cut, for the one at it, and on the batch arm, one fixed line each, and each verdict answers evidence-unavailable"
      (list (car if1a-w) (lines-per-mark (cdr if1a-w) '("QB1" "QB2" "batch")))
      '((((unknown evidence-unavailable) (unknown evidence-unavailable)) (unknown evidence-unavailable) (index-fork))
        (1 1 1)))

;; ---- IF-1b: the fork REMOVED at the hold -----------------------------------------
(printf "== IF-1b ==\n")
(define b (fresh-store "b"))
(define b-r1 (tracked! (car b) (cdr b) "QC1" "one"))
(quarantine! (car b) (cdr b) (+ 1 (cadr b-r1)))
(define if1b (held-run "if1b" plain-switch (car b) (cdr b) (list b-r1) "plain"
                       (lambda () (unquarantine! (car b) (cdr b)))))
(printf "IF-1b ~s\n" (car if1b))
(want "IF-1b the fork removed at the hold: the holding session answers undelivered (its cut stopped below the fork)"
      (and (pair? (car if1b)) (car (car if1b))) '((unknown undelivered)))
(want "IF-1b a later session with its own cut: replay"
      (and (pair? (car if1b)) (cadr (car if1b))) '((replay)))
(want "IF-1b CONTROL: cold, replay"
      (and (pair? (car if1b)) (caddr (car if1b))) '((replay)))

;; ---- IF-3: each writer's own fork, both enumeration orders ------------------------
(printf "== IF-3 ==\n")
;; A SECOND WRITER is a copy of the first's directory under another name: its
;; frames are the same bytes, filed under its own name by the scan.
(define (two-writer-store name other)
  (let* ((s (fresh-store name)) (r (tracked! (car s) (cdr s) "QD" "tracked")))
    (system (string-append "cp -R '" (writer-dir (car s) (cdr s)) "' '" (writer-dir (car s) other) "'"))
    (quarantine! (car s) (cdr s) (+ 1 (cadr r)))
    (list s r other)))
;; -> ((<forked writer's placement>) (<the other's>)) for the tracked record
(define (per-writer x)
  (let* ((s (car x)) (st (car s)) (w (cdr s)))
    (cold-index! st)
    (let ((recs (caught (indexed-records st (cons w "QD")))))
      (if (and (pair? recs) (eq? (car recs) 'RAISED))
          recs
          (list (map (lambda (r) (list-ref r 4)) (filter (lambda (r) (equal? (car (car r)) w)) recs))
                (map (lambda (r) (list-ref r 4)) (filter (lambda (r) (equal? (car (car r)) (caddr x))) recs)))))))
;; "00000000" sorts before any writer id and "zzzzzzzz" after one, and each
;; row reads which writer the store lists first
(define c-first (two-writer-store "c1" "00000000"))
(define c-last (two-writer-store "c2" "zzzzzzzz"))
(define (other-first? x) (equal? (car (store-writers (car (car x)))) (caddr x)))
(printf "IF-3 writers ~s ~s\n" (store-writers (car (car c-first))) (store-writers (car (car c-last))))
(want "IF-3 the other writer listed before the forked one: the forked writer's record at its fork quarantined, the other's valid-history"
      (list (other-first? c-first) (per-writer c-first)) '(#t ((quarantined) (valid-history))))
(want "IF-3 the other writer listed after the forked one: the same"
      (list (other-first? c-last) (per-writer c-last)) '(#f ((quarantined) (valid-history))))

;; ---- IF-4: asked of indexed-records directly, cold ----------------------------------
(printf "== IF-4 ==\n")
(define d (fresh-store "d"))
(define d-r (tracked! (car d) (cdr d) "QE" "tracked"))
(define d-id (cons (cdr d) "QE"))
(quarantine! (car d) (cdr d) (+ 1 (cadr d-r)))
(define (chmod! mode path) (system (string-append "chmod " mode " '" path "'")))
(define d-owner (string-append (writer-dir (car d) (cdr d)) "/owner.sexp"))
(chmod! "000" d-owner)
(define if4-owner (placements (car d) d-id))
(chmod! "644" d-owner)
;; THE INDEX'S OWN STAMP STATS EVERY LISTED FILE, owner.sexp among them, and
;; an unreadable one raises there (path-version): so this case is not reached
;; through indexed-records on either tree, and is pinned as that raise.
(want "IF-4 owner.sexp unreadable beside a readable quarantine file: the index's inventory raises (both trees)"
      if4-owner '(RAISED "the entry cannot be read"))
(define d-q (string-append (writer-dir (car d) (cdr d)) "/quarantine.sexp"))
(chmod! "000" d-q)
(define if4-q (placements (car d) d-id))
(chmod! "644" d-q)
(want "IF-4 quarantine.sexp unreadable (mode 000): the inventory raises first (both trees)"
      (and (pair? if4-q) (eq? (car if4-q) 'RAISED)) #t)
;; A QUARANTINE FILE THAT STATS AND WILL NOT READ: a directory in its place.
;; The inventory's stat passes; quarantine-of's own read raises, so the
;; rebuild does.
(delete-file d-q)
(mkdir d-q)
(define if4-qdir (placements (car d) d-id))
(delete-directory d-q)
(want "IF-4 quarantine.sexp a directory: the inventory passes it and quarantine-of's read raises out of the rebuild"
      (and (pair? if4-qdir) (eq? (car if4-qdir) 'RAISED)) #t)
;; AN INCOMPLETE PUBLICATION: a writer directory with no owner.sexp and no
;; published.sexp; its frames are a copy of a local writer's
(define e (fresh-store "e"))
(define e-r (tracked! (car e) (cdr e) "QF" "tracked"))
(define e-other "mmmm5555")
(define (incomplete-copy! st w other)
  (system (string-append "cp -R '" (writer-dir st w) "' '" (writer-dir st other) "' && rm -f '"
                         (writer-dir st other) "/owner.sexp' '" (writer-dir st other) "/published.sexp'")))
(incomplete-copy! (car e) (cdr e) e-other)
(define (incomplete-writer-placements st w other req)
  (cold-index! st)
  (caught (map (lambda (r) (list-ref r 4))
               (filter (lambda (r) (equal? (car (car r)) other)) (indexed-records st (cons w req))))))
(define if4-incomplete-plain (incomplete-writer-placements (car e) (cdr e) e-other "QF"))
(quarantine! (car e) e-other (+ 1 (cadr e-r)))
(define if4-incomplete-forked (incomplete-writer-placements (car e) (cdr e) e-other "QF"))
(printf "IF-4 incomplete publication: no quarantine file ~s, with one ~s\n" if4-incomplete-plain if4-incomplete-forked)
(want "IF-4 an incomplete publication without a quarantine file: its copy of the record valid-history"
      if4-incomplete-plain '(valid-history))
(want "IF-4 an incomplete publication with a quarantine file: its fork is honoured, quarantined"
      if4-incomplete-forked '(quarantined))
;; a retired writer with its successor: a copy, adopted
(define f (fresh-store "f"))
(define f-r (tracked! (car f) (cdr f) "QG" "ran"))
(define f-copy (string-append root "/f-copy"))
(system (string-append "cp -R '" (car f) "' '" f-copy "'"))
(define f-adopted (rpc-dispatch f-copy '(adopt) "test"))
(want "IF-4 CONTROL: a retired writer with its successor: the record valid-history"
      (list (car f-adopted) (placements f-copy (cons (cdr f) "QG"))) '(ok (valid-history)))

;; ---- IF-5: forks that are not integers, pinned as they are ------------------------------
(printf "== IF-5 ==\n")
(define g (fresh-store "g"))
(define g-r (tracked! (car g) (cdr g) "QH" "tracked"))
(define g-id (cons (cdr g) "QH"))
;; a second writer with no frames: its owner only
(define g-empty "nnnn6666")
(system (string-append "mkdir -p '" (writer-dir (car g) g-empty) "' && cp '" (writer-dir (car g) (cdr g)) "/owner.sexp' '"
                       (writer-dir (car g) g-empty) "/'"))
(define (fork-reading fork)
  (quarantine! (car g) (cdr g) fork)
  (unquarantine! (car g) g-empty)
  (let ((with-frames (placements (car g) g-id)))
    (unquarantine! (car g) (cdr g))
    (quarantine! (car g) g-empty fork)
    (let ((without (placements (car g) g-id)))
      (unquarantine! (car g) g-empty)
      (list with-frames without))))
(define (raised-or x) (if (and (pair? x) (eq? (car x) 'RAISED)) 'raised x))
(define if5 (map (lambda (fork) (map raised-or (fork-reading fork))) (list -1 2.5 'x)))
(printf "IF-5 forks -1, 2.5, x: ~s\n" if5)
(want "IF-5 a fork of -1: every frame of that writer quarantined; on a writer without frames, nothing changes"
      (car if5) '((quarantined) (valid-history)))
;; "2.5" IS NOT READ by the store's reader (string->sexpr-extended refuses it),
;; so quarantine-of's guarded parse answers no fork at all
(want "IF-5 a fork of 2.5: the store's reader does not read it, so there is no fork and nothing is quarantined"
      (cadr if5) '((valid-history) (valid-history)))
(want "IF-5 a symbol fork: the index's comparison raises on the writer with frames; on a writer without frames, nothing"
      (caddr if5) '(raised (valid-history)))
;; B-3: an incomplete publication holding a quarantine file with a symbol fork and a frame
(define h (fresh-store "h"))
(define h-r (tracked! (car h) (cdr h) "QI" "tracked"))
(define h-other "pppp7777")
(incomplete-copy! (car h) (cdr h) h-other)
(quarantine! (car h) h-other 'x)
(define if5-incomplete (raised-or (placements (car h) (cons (cdr h) "QI"))))
(want "IF-5 an incomplete publication with a symbol fork and a frame: the index's comparison raises (store-wide, conservative)"
      if5-incomplete 'raised)

;; ---- IF-6: checkpoints -------------------------------------------------------------------
(printf "== IF-6 ==\n")
(define k (fresh-store "k"))
(define k-r (tracked! (car k) (cdr k) "QJ" "tracked"))
(define k-id (cons (cdr k) "QJ"))
(define k-cp (string-append (car k) "/request-index.sexp"))
;; A CHECKPOINT THAT CANNOT BE WRITTEN raises here and is caught, as the
;; snapshot swallows it: the rows below then read no checkpoint, which is red.
(cold-index! (car k))
(caught (index-checkpoint! (car k)))
(define (read-checkpoint) (storable-decode (string->sexpr-extended (utf8->string (call-with-port (open-file-input-port k-cp) get-bytevector-all)))))
(define (write-checkpoint! x)
  (call-with-port (open-file-output-port k-cp (file-options no-fail))
    (lambda (p) (put-bytevector p (string->utf8 (sexpr->string-extended (storable-encode x)))))))
(define (digest-of body) (bytevector->hex (sha256 (string->utf8 (sexpr->string-extended (storable-encode body))))))
;; AS THE LANDED TREE WRITES IT: the stamp without its derivation element (a
;; stamp that does not begin with one is left as it is), the tracked record's
;; entry filed `damaged` (a placement the scan would not give), and one entry
;; for a frame the store does not hold (a forged copy of the record's frame at
;; another sequence), the digest recomputed
(define forged
  (and (file-exists? k-cp)
  (let* ((x (read-checkpoint)) (body (list-ref x 4)) (stamp (car body)) (entries (cadr body))
         (old-stamp (if (and (pair? stamp) (pair? (car stamp)) (eq? (car (car stamp)) 'derivation)) (cdr stamp) stamp))
         (hit (find (lambda (e)
                      (let ((r (decode-line (cadddr e))))
                        (and (pair? r) (eq? (car r) 'ok) (pair? (cadddr r)) (equal? (cadr r) (+ 1 (cadr k-r))))))
                    entries))
         (decoded (and hit (decode-line (cadddr hit))))
         (fake (and decoded (encode-record (+ 1000 (cadr decoded)) (caddr decoded) (cadddr decoded)
                                           (list-ref decoded 4) (list-ref decoded 5))))
         (new-entries (append (map (lambda (e) (if (eq? e hit) (list (car e) (cadr e) (caddr e) (cadddr e) 'damaged) e)) entries)
                              (if fake (list (list (car hit) (cadr hit) 999999 fake 'valid-history)) '())))
         (new-body (list old-stamp new-entries)))
    (and hit (list 'request-index 1 (caddr x) (digest-of new-body) new-body)))))
(when forged (write-checkpoint! forged))
(index-forget-memory!)
(define if6-old (caught (list-sort sym<? (map (lambda (r) (list-ref r 4)) (indexed-records (car k) k-id)))))
(want "IF-6 an old derivation's checkpoint is rebuilt once: the misfiled entry replaced by the scan's, the missing one kept"
      (list (and forged #t) if6-old) '(#t (index-evidence-missing valid-history)))
;; the new code accepts its own: a checkpoint written now, the memory
;; forgotten, a traced query decodes its hits only
(cold-index! (car k))
(caught (index-checkpoint! (car k)))
(index-forget-memory!)
(define if6-own
  (let* ((n 0)
         (text (traced (lambda () (set! n (length (indexed-records (car k) k-id)))))))
    (list n (count-lines text "(trace identity-source-decode "))))
(want "IF-6 the new code accepts its own checkpoint: a traced query decodes its one hit only, no scan"
      if6-own '(1 1))

;; ---- IF-7: store-evidence, and the plain verdict handed an earlier cut ---------------------
(printf "== IF-7 ==\n")
(define m (fresh-store "m"))
(define m-r (tracked! (car m) (cdr m) "QK" "tracked"))
(want "IF-7 CONTROL: store-evidence on a store with no fork answers the record valid-history and delivered"
      (map (lambda (e) (list (ev-placement e) (ev-event e))) (store-evidence (car m) (cons (cdr m) "QK")))
      (list (list 'valid-history (cons (cdr m) (+ 1 (cadr m-r))))))
;; THE EARLIER CUT: taken, then a fork written below the record by "another
;; process", then the plain verdict asked with that cut under only:index-fork
;; in a child. The plain verdict runs no check, so nothing is printed.
(define plain-child
  '((import (chezscheme) (theourgia log)
            (only (theourgia store) make-write-request request-verdict)
            (only (theourgia degeneracy) degeneracy-checks-on))
    (define args (command-line-arguments))
    (define st (car args))
    (define w (cadr args))
    (define given (call-with-input-file (caddr args) read))
    (define x (car (caddr given)))
    (define checks-on (degeneracy-checks-on))
    (define v (request-verdict st (make-write-request "test" 'insert (list "--title" (caddr x)) (car x) (cons w (cadr x)))
                               (car given) #f (cadr given)))
    (write (list (if (and (eq? (car v) 'unknown) (pair? (cadr v))) (list 'unknown (car (cadr v))) (list (car v)))
                 checks-on))))
(define if7
  (if witness-known?
      (let ((given (given-file! (car m) (list m-r) "if7")))
        (quarantine! (car m) (cdr m) (+ 1 (cadr m-r)))
        (cold-index! (car m))
        (child-result (start-child! "if7" witness-switch plain-child (list (car m) (cdr m) given))))
      (cons 'index-fork-unknown-on-this-tree "")))
(printf "IF-7 plain verdict with an earlier cut: ~s\n" (car if7))
(want "IF-7 a plain verdict handed a cut taken before a fork was written, under only:index-fork (the child says the check is on): no check, no line, quarantined"
      (list (car if7) (count-lines (cdr if7) index-fork-line)) '(((unknown quarantined) (index-fork)) 0))

;; ---- IF-8: a real write on a store with an in-contract fork ---------------------------------
(printf "== IF-8 ==\n")
(define write-child
  '((import (chezscheme) (theourgia rpc) (theourgia log)
            (only (theourgia store) open-and-reduce make-write-request request-verdict/provided)
            (only (theourgia reduce) reduce-applied-cut reduce-gates)
            (only (theourgia degeneracy) degeneracy-checks-on))
    (define args (command-line-arguments))
    (define st (car args))
    (define w (cadr args))
    (define req-id (caddr args))
    (define after (string->number (cadddr args)))
    (define (mentions? x sym) (cond ((eq? x sym) #t) ((pair? x) (or (mentions? (car x) sym) (mentions? (cdr x) sym))) (else #f)))
    ;; the single arm: the request asked again through a real write
    (define single (rpc-dispatch st (list 'insert "--title" "tracked" "--req" req-id "--cursor" (format "~a:~a" w after)) "test"))
    ;; the batch arm: a batch's verdict inside a write session
    (define state (open-and-reduce st))
    (define s (log-begin st (lambda a 'applied)))
    (define batch (request-verdict/provided (session-discovery-provider s) st
                                            (make-write-request "test" 'insert (list "--title" "tracked") req-id (cons w after))
                                            (reduce-applied-cut state) 2 (reduce-gates state)))
    (log-end! s)
    (write (list (car single) (mentions? single 'quarantined)
                 (if (and (eq? (car batch) 'unknown) (pair? (cadr batch))) (list 'unknown (car (cadr batch))) (list (car batch)))
                 (degeneracy-checks-on)))))
(define n (fresh-store "n"))
(define n-r (tracked! (car n) (cdr n) "QL" "tracked"))
(quarantine! (car n) (cdr n) (+ 1 (cadr n-r)))
(define if8
  (if witness-known?
      (child-result (start-child! "if8" witness-switch write-child (list (car n) (cdr n) "QL" (number->string (cadr n-r)))))
      (cons 'index-fork-unknown-on-this-tree "")))
(printf "IF-8 ~s\n" (car if8))
(want "IF-8 a real write on a store whose fork is in contract, under only:index-fork: the check silent, the request refused naming the quarantine; the batch arm unknown and silent too"
      (list (car if8) (count-lines (cdr if8) index-fork-line))
      '((error #t (unknown quarantined) (index-fork)) 0))

;; ---- IF-9: the rebuild reports progress once per file it scans ---------------------------
(printf "== IF-9 ==\n")
;; PROGRESS IS COUNTED AT ITS SINK (progress-sink-set!, a counter in this
;; process), and the yields by io-yield-tally, as test/io-yield.sc counts them
;; (an attempt reaches the clock only with a sleeper installed). The store has
;; two writers -- one with two segment files, a file in incoming/ and one in
;; damaged/, built by hand as store-memory builds its store, then adopted, so
;; its successor writes the second writer's segment. A write session's verdict
;; on a warm index and then on a cold one: the cold one's extra progress calls
;; and yield attempts are the rebuild's, one per file scanned.
(io-yield-set! (lambda args #t))
(define sink-calls 0)
(progress-sink-set! (lambda () (set! sink-calls (+ sink-calls 1))))
(define (put-bytes! path bv) (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p bv))))
(define (cat-bytes . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o
          (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                 (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (built-record seq title)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" '()
                 (storable-encode (list 'put (list (cons 'kind 'section) (cons 'title title))))))
(define p9-w "wwww1111")
(define p9-base (string-append root "/p9-base"))
(define p9 (string-append root "/p9"))
(define p9-dir (string-append p9 "/writers/" p9-w))
(system (string-append "mkdir -p '" p9-base "/writers/" p9-w "' '" p9-base "/snap'"))
(put-bytes! (string-append p9-base "/meta.sexp") (string->utf8 "((format 1) (store-id \"t\"))\n"))
(put-bytes! (string-append p9-base "/lock") (make-bytevector 0))
(put-bytes! (string-append p9-base "/writers/" p9-w "/owner.sexp") (string->utf8 "((machine \"m\"))\n"))
(let ((k (instance-install! p9-base))) (owner-install! p9-base p9-w k))
(put-bytes! (string-append p9-base "/writers/" p9-w "/" (segment-file-name 1)) (cat-bytes (built-record 1 "a") (built-record 2 "b")))
(put-bytes! (string-append p9-base "/writers/" p9-w "/" (segment-file-name 2)) (cat-bytes (built-record 3 "c") (built-record 4 "d")))
(system (string-append "cp -R '" p9-base "' '" p9 "'"))
(define p9-adopt (car (rpc-dispatch p9 '(adopt) "test")))
(define p9-insert (car (rpc-dispatch p9 (list 'insert "--title" "e") "test")))
(system (string-append "mkdir -p '" p9-dir "/incoming' '" p9-dir "/damaged'"))
(put-bytes! (string-append p9-dir "/incoming/candidate") (built-record 5 "f"))
(put-bytes! (string-append p9-dir "/damaged/broken") (built-record 6 "g"))
(define (files-scanned st)
  (apply + (map (lambda (w)
                  (let ((d (string-append st "/writers/" w)))
                    (+ (length (enumerate-segment-files st w))
                       (if (file-directory? (string-append d "/incoming")) (length (directory-list (string-append d "/incoming"))) 0)
                       (if (file-directory? (string-append d "/damaged")) (length (directory-list (string-append d "/damaged"))) 0))))
                (store-writers st))))
(define if9
  (let* ((state (open-and-reduce p9)) (cut (reduce-applied-cut state)) (gates (reduce-gates state))
         (successor (car (filter (lambda (w) (not (equal? w p9-w))) (store-writers p9))))
         (req (make-write-request "test" 'insert (list "--title" "z") "QZ" (cons successor 1)))
         (s (log-begin p9 (lambda a 'applied)))
         (pv (session-discovery-provider s))
         (counted (lambda (thunk)
                    (let ((c sink-calls) (a (car (io-yield-tally))))
                      (thunk)
                      (list (- sink-calls c) (- (car (io-yield-tally)) a)))))
         (warm (begin (request-verdict/provided pv p9 req cut #f gates)
                      (counted (lambda () (request-verdict/provided pv p9 req cut #f gates)))))
         (cold (counted (lambda () (cold-index! p9) (request-verdict/provided pv p9 req cut #f gates)))))
    (log-end! s)
    (list (map - cold warm) (files-scanned p9)
          (map (lambda (w) (length (enumerate-segment-files p9 w))) (store-writers p9)))))
(printf "IF-9 adopt ~s insert ~s; the cold verdict's extra (progress yields), the files scanned, segments per writer: ~s\n"
        p9-adopt p9-insert if9)
(want "IF-9 a cold rebuild inside a write session's verdict reports progress, and yields, once per file it scans: two writers, two segment files in one, a file in incoming/ and one in damaged/"
      (list (car if9) (cadr if9) (list-sort < (caddr if9)))
      '((5 5) 5 (1 2)))

(printf "\n~a failures\nrows: ~a\nindex-fork complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
