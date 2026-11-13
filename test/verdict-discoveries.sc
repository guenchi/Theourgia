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

;; A WRITE'S VERDICT TAKES ITS WRITER DISCOVERIES FROM THE SESSION'S OWN LOAD
;; (F437 D6-7).
;;
;;   VD-1  inside a write session, the verdict with the session's provider
;;         answers what the verdict without one answers: a request that never
;;         ran, one that ran, a writer with a torn tail, an uncertain cache
;;         stale and one unreadable
;;   VD-3  a verdict with the session's provider parses only what the evidence
;;         index decodes (each such frame traced once as identity-source-decode):
;;         its discoveries decode nothing, for a single request and for a batch;
;;         the plain verdict, the control, parses more (the guard against a
;;         provider that falls back in silence)
;;   VD-4  the provider answers only while the session stands: after an append
;;         it answers #f for every writer, and after the session ends too, and
;;         the trace says so
;;   VD-2  the premise's limit: a torn tail written WITHOUT the lock while the
;;         session holds at after-discovery is unseen by the session's
;;         discoveries and seen by a fresh one (in a child, THEOURGIA_HOLD)
;;   VD-5  the witness fires: a provider answering from another store, under
;;         verdict-discoveries; its fixed line reaches the error port through
;;         both of the verdict's guards
;;   VD-6  a child that outlives its wait is killed by its pid and its row fails
;;         by name (outlived-its-wait); none is left running
;;
;;   VD-7  the stores VD-1 leaves to the switch's witness, each asked through a
;;         provider on a COLD index (index-forget-memory! before each verdict, so
;;         the provided and the plain one each build their own): (a) a
;;         quarantined fork with a tracked record above it answers unknown,
;;         quarantined; (b) a retired writer with its successor (an adopted
;;         copy) and an unreadable writer under incomplete-accepted answer as
;;         the plain verdict; (c) a request asked again with its own cursor
;;         answers replay through the provider
;;   VD-8  THE MISS IS REACHABLE: working.sc's ensure-area! makes
;;         writers/<w>/ under the draft lock only, so a writer directory made
;;         while a session holds at after-discovery is listed and was not
;;         discovered; the provider misses it, traces verdict-provider miss,
;;         and the verdict answers as the plain one (in a child, THEOURGIA_HOLD)
;;
;; VD-5 counts the witness's line per call: the derivation's guard alone once,
;; then the verdict's at least once (which of uncertain-load's two let-bound
;; readings raises first is the implementation's order, and decides whether
;; the derivation's guard also prints inside the verdict).
(import (chezscheme) (theourgia rpc) (theourgia log) (theourgia trace)
        (only (theourgia store) open-and-reduce obtain-state make-write-request request-verdict request-verdict/provided)
        (only (theourgia reduce) reduce-applied-cut reduce-gates)
        (only (theourgia degeneracy) degeneracy-active)
        (only (theourgia evidence-index) index-forget-memory!)
        (only (theourgia incomplete) incomplete-accepted))

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
                   "/verdict-discoveries-" (number->string (get-process-id)))))
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

;; ---- a store and its writer --------------------------------------------------
(define (fresh-store name)
  (let* ((st (string-append root "/" name))
         (init (rpc-dispatch st '(init) "test"))
         (w (cadr (assq 'writer (cdr init)))))
    (let loop ((k 0))
      (when (< k 6)
        (rpc-dispatch st (list 'insert "--title" (format "b~a" k)) "test")
        (loop (+ k 1))))
    (cons st w)))
(define (end-seq st w) (discovery-end-seq (discover-prefix st w 'shared)))
(define (cursor st w) (format "~a:~a" w (end-seq st w)))

;; -> (<verdict without a provider> <verdict with the session's>), both asked
;; inside one write session of ST for request REQ-ID at the writer's end
(define (both-verdicts st w req-id)
  (let* ((state (open-and-reduce st))
         (cut (reduce-applied-cut state))
         (gates (reduce-gates state))
         (req (make-write-request "test" 'insert (list "root" "x") req-id (cons w (end-seq st w))))
         (s (log-begin st (lambda args 'applied)))
         (provider (session-discovery-provider s))
         ;; the provided verdict first: it meets the index cold, so its scan asks
         ;; the provider too
         (provided (request-verdict/provided provider st req cut #f gates))
         (plain (request-verdict st req cut #f gates)))
    (log-end! s)
    (list plain provided)))
(define (same? pair) (equal? (car pair) (cadr pair)))

(printf "== VD-1: the verdict with the session's discoveries answers as without ==\n")
(define a (fresh-store "a"))
(want "VD-1 a request that never ran: both verdicts equal, and they say execute"
      (let ((v (both-verdicts (car a) (cdr a) "NEVER")))
        (list (same? v) (car (car v))))
      '(#t execute))
(rpc-dispatch (car a) (list 'insert "--title" "ran" "--req" "RAN" "--cursor" (cursor (car a) (cdr a))) "test")
(want "VD-1 a request that ran: both verdicts equal, and neither says execute"
      (let ((v (both-verdicts (car a) (cdr a) "RAN")))
        (list (same? v) (eq? 'execute (car (car v)))))
      '(#t #f))
;; a torn tail: bytes past the last newline of the current segment
(define (current-segment-path st w)
  (let* ((p (discover-prefix st w 'shared)) (seg (car (discovery-physical-current p))))
    (string-append (writer-directory st w) "/" (segment-file-name seg))))
(define b (fresh-store "b"))
(call-with-port (open-file-output-port (current-segment-path (car b) (cdr b))
                                       (file-options no-fail no-truncate append))
  (lambda (p) (put-bytevector p (string->utf8 "00000000 (7 1 \"torn"))))
(want "VD-1 a writer with a torn tail: both verdicts equal"
      (same? (both-verdicts (car b) (cdr b) "TORN"))
      #t)
(call-with-output-file (uncertain-path (car b) (cdr b)) (lambda (p) (write '() p)) 'replace)
(want "VD-1 an uncertain cache stale (it says nothing where the torn tail says something): both verdicts equal"
      (same? (both-verdicts (car b) (cdr b) "STALE"))
      #t)
;; on a store of its own, so the refusal is the cache's and not the torn tail's
(define b2 (fresh-store "b2"))
(call-with-output-file (uncertain-path (car b2) (cdr b2)) (lambda (p) (display "not a list" p)) 'replace)
(want "VD-1 an uncertain cache unreadable (and nothing else wrong): both verdicts equal, and neither says execute"
      (let ((v (both-verdicts (car b2) (cdr b2) "UNREADABLE")))
        (list (same? v) (eq? 'execute (car (car v)))))
      '(#t #f))

(printf "== VD-3: the verdict's discoveries decode nothing ==\n")
;; Inside one write session, a first verdict untraced, so the evidence index is
;; built (its rebuild decodes every frame twice, in its scan and in
;; buckets-for, and this design leaves it); then each verdict traced against
;; that index: each hit it verifies is one `parse` and one
;; `identity-source-decode`, so a verdict whose discoveries decode nothing holds
;; as many of the one as of the other. A provider that fell back would add the
;; writer's records as parse lines alone. The plain verdict is the control.
(define c (fresh-store "c"))
(rpc-dispatch (car c) (list 'insert "--title" "ran" "--req" "RAN" "--cursor" (cursor (car c) (cdr c))) "test")
;; -> (<parse lines> <identity-source-decode lines>) of THUNK's trace
(define (counts thunk)
  (let ((text (traced thunk)))
    (list (count-lines text "(trace parse ") (count-lines text "(trace identity-source-decode "))))
(define vd3
  (let* ((st (car c)) (w (cdr c))
         (state (open-and-reduce st)) (cut (reduce-applied-cut state)) (gates (reduce-gates state))
         ;; the requests are made before any trace: their cursor is a discovery
         (at (cons w (end-seq st w)))
         (ran (make-write-request "test" 'insert (list "root" "x") "RAN" at))
         (batch-req (make-write-request "test" 'insert (list "root" "x") "B" at))
         (s (log-begin st (lambda args 'applied)))
         (session-provider (session-discovery-provider s))
         ;; THE PROVIDER'S ANSWERS COUNTED, per verdict: what the parse counts
         ;; cannot tell under the switch (the witness parses as a fallback would)
         (answered 0)
         (provider (lambda (writer)
                     (let ((d (session-provider writer))) (when d (set! answered (+ answered 1))) d)))
         (taken (lambda () (let ((n answered)) (set! answered 0) n)))
         (warm (request-verdict st ran cut #f gates))
         (single (counts (lambda () (request-verdict/provided provider st ran cut #f gates))))
         (single-answers (taken))
         (batch (counts (lambda () (request-verdict/provided provider st batch-req cut 2 gates))))
         (batch-answers (taken))
         (plain (counts (lambda () (request-verdict st ran cut #f gates))))
         (plain-batch (counts (lambda () (request-verdict st batch-req cut 2 gates))))
         ;; COLD: the index forgotten, so the batch's evidence rebuilds it and its
         ;; scan asks each writer's discovery; the rebuild decodes every frame
         ;; twice (the scan, traced as identity-source-decode, and buckets-for),
         ;; and with the provider nothing else
         (cold-batch (begin (index-forget-memory!) (taken)
                            (counts (lambda () (request-verdict/provided provider st batch-req cut 2 gates)))))
         (cold-answers (taken))
         (cold-plain-batch (begin (index-forget-memory!)
                                  (counts (lambda () (request-verdict st batch-req cut 2 gates))))))
    (log-end! s)
    (list single batch plain plain-batch cold-batch cold-plain-batch
          (list single-answers batch-answers cold-answers))))
(printf "VD-3 (parse identity-source-decode): single ~s, batch ~s, plain ~s, plain batch ~s\n"
        (car vd3) (cadr vd3) (caddr vd3) (cadddr vd3))
;; UNDER THE SWITCH the verdict-discoveries check discovers afresh each discovery
;; the provider answers, to compare them, so the provided verdict then parses
;; exactly what the plain one does: that is what the rows read there.
(define witness-on? (degeneracy-active verdict-discoveries))
(want "VD-3 a request's verdict parses only what the evidence index decodes, and decodes something (the hit of a request that ran); under the switch, what the plain verdict parses"
      (let ((x (car vd3)))
        (list (> (cadr x) 0) (if witness-on? (equal? x (caddr vd3)) (= (car x) (cadr x)))))
      '(#t #t))
(want "VD-3 a batch's verdict parses only what the evidence index decodes; under the switch, what the plain batch verdict parses"
      (let ((x (cadr vd3))) (if witness-on? (equal? x (cadddr vd3)) (= (car x) (cadr x))))
      #t)
(printf "VD-3 cold (parse identity-source-decode): batch ~s, plain batch ~s\n" (list-ref vd3 4) (list-ref vd3 5))
(want "VD-3 a batch's verdict that rebuilds the index parses its rebuild only, every frame twice; under the switch, what the plain one parses"
      (let ((x (list-ref vd3 4)))
        (list (> (cadr x) 0) (if witness-on? (equal? x (list-ref vd3 5)) (= (car x) (* 2 (cadr x))))))
      '(#t #t))
(printf "VD-3 the provider's answers: single ~a, batch ~a, cold batch ~a\n"
        (car (list-ref vd3 6)) (cadr (list-ref vd3 6)) (caddr (list-ref vd3 6)))
(want "VD-3 the provider is asked and answers each of the verdict's discoveries: the cache's and the derivation's per verdict, and the index scan's when cold"
      (list-ref vd3 6)
      '(2 2 3))
(want "VD-3 CONTROL: the verdict without the provider parses beyond what the index decodes"
      (let ((x (caddr vd3))) (> (car x) (cadr x)))
      #t)

(printf "== VD-4: the provider answers only while the session stands ==\n")
(define d (fresh-store "d"))
(define after-end
  (let* ((s (log-begin (car d) (lambda args 'applied)))
         (provider (session-discovery-provider s))
         (before (and (provider (cdr d)) #t)))
    (log-end! s)
    (list before (provider (cdr d)))))
(want "VD-4 the provider answers the writer's discovery while the session stands, and #f once it has ended"
      after-end
      '(#t #f))
(define after-append
  (let* ((s (log-begin (car d) (lambda args 'applied)))
         (provider (session-discovery-provider s))
         (v (session-view s))
         (res (session-append! s (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                             (view-expect-seq v) "agent:claude" '()
                                             (list 'put (list '(title . "appended") '(parent . root) '(ord . 1))))))
         (text (traced (lambda () (set! v (provider (cdr d)))))))
    (guard (e (#t (if #f #f))) (session-commit! s))
    (log-end! s)
    (list (and (pair? res) (car res)) v (> (count-lines text "(trace verdict-provider ") 0))))
(want "VD-4 after an append the provider answers #f and the trace says so"
      after-append
      '(committed #f #t))

(printf "== VD-2 and VD-5: in a child process ==\n")
(define (file-text p) (if (file-exists? p) (call-with-input-file p get-string-all) ""))
(define (read-or-false text)
  (guard (e (#t #f)) (let ((x (read (open-string-input-port text)))) (if (eof-object? x) #f x))))
;; -> (<the child's stdout datum or #f> . <its stderr>); SETTING is the
;; environment it runs under, BACKGROUND? leaves it running and answers the
;; paths it writes instead
(define (child-paths tag)
  (map (lambda (x) (string-append root "/" tag x)) '(".ss" ".out" ".err" ".done" ".pid")))
;; THE CHILD'S PID IS WRITTEN BY THE SHELL THAT STARTS IT ($!), so a child that
;; outlives the wait below can be stopped by pid, not left behind
(define (start-child! tag setting program args)
  (let ((ps (child-paths tag)))
    (call-with-output-file (car ps) (lambda (p) (for-each (lambda (f) (write f p) (newline p)) program)) 'replace)
    (system (string-append "sh -c \"" setting " scheme --script '" (car ps) "' "
                           (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
                           "> '" (cadr ps) "' 2> '" (caddr ps) "' < /dev/null & echo \\$! > '" (list-ref ps 4)
                           "'; wait \\$!; touch '" (cadddr ps) "'\" &"))
    ps))
;; LIMIT is a count of 20 ms ticks, about 30 s by default
(define (await-file path . limit)
  (let ((ticks (if (pair? limit) (car limit) 1500)))
    (let loop ((k 0))
      (cond ((file-exists? path) #t)
            ((> k ticks) #f)
            (else (sleep (make-time 'time-duration 20000000 0)) (loop (+ k 1)))))))
;; A CHILD THAT OUTLIVES ITS WAIT is killed by its pid and answers
;; outlived-its-wait in place of its datum, so its row fails by name and
;; nothing of it runs past the fixture
(define (child-result ps . limit)
  (if (apply await-file (cadddr ps) limit)
      (cons (read-or-false (file-text (cadr ps))) (file-text (caddr ps)))
      (let ((pid (read-or-false (file-text (list-ref ps 4)))))
        (when (and (integer? pid) (> pid 0))
          (system (string-append "kill -KILL " (number->string pid) " 2>/dev/null")))
        (printf "the child ~a outlived its wait (pid ~s): killed\n" (car ps) pid)
        (cons 'outlived-its-wait (file-text (caddr ps))))))
;; the cut, the gates and the writer's end of ST, for a child that cannot load
;; ST before its session (every load meets the hold)
(define (given-file! st w tag)
  (let ((state (open-and-reduce st)) (path (string-append root "/" tag ".given")))
    (call-with-output-file path
      (lambda (p) (write (list (reduce-applied-cut state) (reduce-gates state) (end-seq st w)) p)) 'replace)
    path))

;; VD-2, THE PREMISE'S LIMIT: the session's discoveries are taken at its open,
;; before the delivery; a fresh one at the verdict sees what changed between.
;; A process that writes into the store without the lock can make the two
;; differ. Here it appends a torn tail to the writer's current segment while
;; the child's session holds at after-discovery (THEOURGIA_HOLD); the child then
;; asks the verdict both ways.
(define vd2-child
  '((import (chezscheme) (theourgia log)
            (only (theourgia store) make-write-request request-verdict request-verdict/provided))
    (define args (command-line-arguments))
    (define st (car args))
    (define w (cadr args))
    (define given (call-with-input-file (caddr args) read))
    (define req (make-write-request "test" 'insert (list "root" "x") "LOCKLESS" (cons w (caddr given))))
    (define s (log-begin st (lambda a 'applied)))
    (define provider (session-discovery-provider s))
    (define provided (request-verdict/provided provider st req (car given) #f (cadr given)))
    (define plain (request-verdict st req (car given) #f (cadr given)))
    (log-end! s)
    (write (list (car provided) (car plain)))))
(define e (fresh-store "e"))
(define vd2-release (string-append root "/vd2-release"))
(define vd2
  (let* ((given (given-file! (car e) (cdr e) "vd2"))
         (ps (start-child! "vd2" (string-append "THEOURGIA_INJECT=on THEOURGIA_HOLD=after-discovery:" vd2-release) vd2-child
                           (list (car e) (cdr e) given))))
    (if (await-file (string-append vd2-release ".held"))
        (begin
          (call-with-port (open-file-output-port (current-segment-path (car e) (cdr e))
                                                 (file-options no-fail no-truncate append))
            (lambda (p) (put-bytevector p (string->utf8 "00000000 (9 1 \"torn"))))
          (call-with-output-file vd2-release (lambda (p) (display "go" p)) 'replace)
          (child-result ps))
        (cons (list 'never-held) (cdr (child-result ps))))))
(printf "VD-2 the verdict with the session's discoveries and the plain one, after a lockless torn tail: ~s; the witness's line ~a times\n"
        (car vd2) (count-lines (cdr vd2) "degeneracy: a provided discovery differs from a fresh one"))
;; UNDER THE SWITCH the child (which inherits it) compares the session's
;; discovery with a fresh one, sees the torn tail the session missed, prints its
;; line and refuses: the witness pins the premise's limit where it is on.
(want "VD-2 a torn tail written without the lock at after-discovery: the session's discoveries (taken before it) say execute and a fresh discovery does not; under the switch the witness sees the difference, says so on the error port, and the verdict is unknown"
      (let ((v (car vd2)) (lines (count-lines (cdr vd2) "degeneracy: a provided discovery differs from a fresh one")))
        (and (pair? v) (list (car v) (eq? 'execute (cadr v)) (> lines 0))))
      (if witness-on? '(unknown #f #t) '(execute #f #f)))

;; VD-5, THE WITNESS FIRES: a provider answering every writer with another
;; store's writer's discovery, under verdict-discoveries alone. The fixed line
;; reaches the error port through uncertain-derived's guard (which answers the
;; conservative interval), asked here directly, and through request-verdict's
;; (which answers unknown): within the verdict the first raise ends the
;; intervals' reading, and which of the two readings comes first is the
;; implementation's order of a let.
(define vd5-child
  '((import (chezscheme) (theourgia log)
            (only (theourgia store) make-write-request request-verdict/provided))
    (define args (command-line-arguments))
    (define st (car args))
    (define w (cadr args))
    (define given (call-with-input-file (caddr args) read))
    (define wrong (discover-prefix (cadddr args) (car (cddddr args)) 'shared))
    (define req (make-write-request "test" 'insert (list "root" "x") "WRONG" (cons w (caddr given))))
    (define s (log-begin st (lambda a 'applied)))
    (define derived (uncertain-derived/provided (lambda (writer) wrong) st w))
    (let ((p (console-error-port))) (display "VD-5 between the two calls\n" p) (flush-output-port p))
    (define v (request-verdict/provided (lambda (writer) wrong) st req (car given) #f (cadr given)))
    (log-end! s)
    (write (list derived (car v)))))
(define f1 (fresh-store "f1"))
(define f2 (fresh-store "f2"))
(rpc-dispatch (car f2) (list 'insert "--title" "longer") "test")
(define vd5
  (child-result (start-child! "vd5" "THEOURGIA_DEGENERACY=only:verdict-discoveries" vd5-child
                              (list (car f1) (cdr f1) (given-file! (car f1) (cdr f1) "vd5") (car f2) (cdr f2)))))
(printf "VD-5 the verdict ~s, the fixed line ~a times on the error port\n"
        (car vd5) (count-lines (cdr vd5) "degeneracy: a provided discovery differs from a fresh one"))
;; -> (<lines before the marker> <lines after it>) of the witness's fixed line
(define (lines-around text marker line)
  (let loop ((i 0))
    (cond ((> (+ i (string-length marker)) (string-length text)) (list (count-lines text line) 'no-marker))
          ((string=? (substring text i (+ i (string-length marker))) marker)
           (list (count-lines (substring text 0 i) line)
                 (count-lines (substring text i (string-length text)) line)))
          (else (loop (+ i 1))))))
(define vd5-lines (lines-around (cdr vd5) "VD-5 between the two calls" "degeneracy: a provided discovery differs from a fresh one"))
(want "VD-5 a provider answering from another store: the derivation answers the conservative interval and its guard prints the witness's line once; the verdict answers unknown and its call prints the line at least once"
      (list (car vd5) (car vd5-lines) (and (integer? (cadr vd5-lines)) (>= (cadr vd5-lines) 1)))
      (list (list (list (list (cdr f1) 0 #f)) 'unknown) 1 #t))

(printf "== VD-7: the stores VD-1 left to the witness, on a cold index ==\n")
;; -> (<plain verdict> <provided verdict>), each on a COLD index: the index's
;; memory forgotten and its checkpoint removed before each, so each builds its
;; own by a scan; the load and the session declared DECLARATION (#f or
;; incomplete-accepted); ARGS the request's arguments as the dispatcher makes
;; them (the options but --req, --cursor and --premises)
(define (cold-index! st)
  (index-forget-memory!)
  (let ((cp (string-append st "/request-index.sexp"))) (when (file-exists? cp) (delete-file cp))))
(define (cold-verdicts st req-id after declaration . args)
  (let* ((state (obtain-state st #f declaration))
         (cut (reduce-applied-cut state))
         (gates (reduce-gates state))
         (req (make-write-request "test" 'insert (if (pair? args) (car args) (list "root" "x")) req-id after))
         (s (if declaration (log-begin st (lambda args 'applied) declaration) (log-begin st (lambda args 'applied))))
         (provider (session-discovery-provider s))
         (provided (begin (cold-index! st) (request-verdict/provided provider st req cut #f gates)))
         (plain (begin (cold-index! st) (request-verdict st req cut #f gates))))
    (log-end! s)
    (list plain provided)))
(define (quarantine! st w fork)
  (call-with-output-file (string-append st "/writers/" w "/quarantine.sexp")
    (lambda (p) (display (format "((format 1) (fork ~a))\n" fork) p)) 'replace))
(define (mentions? x sym) (cond ((eq? x sym) #t) ((pair? x) (or (mentions? (car x) sym) (mentions? (cdr x) sym))) (else #f)))

;; (a) a tracked record at the fork: quarantined, the verdict cannot say it ran
(define g (fresh-store "g"))
(define g-before (end-seq (car g) (cdr g)))
(rpc-dispatch (car g) (list 'insert "--title" "tracked" "--req" "QA" "--cursor" (cursor (car g) (cdr g))) "test")
(quarantine! (car g) (cdr g) (+ 1 g-before))
(define vd7a (cold-verdicts (car g) "QA" (cons (cdr g) g-before) #f))
(printf "VD-7a plain ~s provided ~s\n" (car vd7a) (cadr vd7a))
(want "VD-7a a quarantined fork with a tracked record at it, through a provider on a cold index: unknown, naming the quarantine, as the plain verdict"
      (list (car (cadr vd7a)) (mentions? (cadr vd7a) 'quarantined) (same? vd7a))
      '(unknown #t #t))

;; (b) a retired writer with its successor: a copy, adopted, retires the copy's
;; writer and makes a successor
(define h (fresh-store "h"))
(rpc-dispatch (car h) (list 'insert "--title" "ran" "--req" "RH" "--cursor" (cursor (car h) (cdr h))) "test")
(define h-copy (string-append root "/h-copy"))
(system (string-append "cp -R '" (car h) "' '" h-copy "'"))
(define h-adopted (rpc-dispatch h-copy '(adopt) "test"))
(define h-successor (let* ((ls (log-open h-copy)) (w (local-writer-of h-copy ls))) (load-commit! ls) w))
(define vd7b-retired (cold-verdicts h-copy "RH" (cons (cdr h) (end-seq h-copy (cdr h))) #f))
(define vd7b-fresh (cold-verdicts h-copy "NEW" (cons h-successor (end-seq h-copy h-successor)) #f))
(printf "VD-7b adopt ~s successor ~s; retired ~s; fresh ~s\n" (car h-adopted) h-successor vd7b-retired vd7b-fresh)
(want "VD-7b a retired writer with its successor (an adopted copy): a request that ran on the retired writer and a new one on the successor, each provided as plain"
      (list (car h-adopted) (and (string? h-successor) (not (equal? h-successor (cdr h)))) (same? vd7b-retired) (same? vd7b-fresh))
      '(ok #t #t #t))

;; (b) an unreadable writer under a permissive declaration: a second writer
;; whose segment cannot be read, the session and the load declaring they
;; accept an incomplete store
(define k (fresh-store "k"))
(define k-other "zzzz9999")
(system (string-append "mkdir -p '" (car k) "/writers/" k-other "' && cp '" (car k) "/writers/" (cdr k) "/owner.sexp' '"
                       (car k) "/writers/" k-other "/owner.sexp' && printf 'x' > '" (car k) "/writers/" k-other "/000001.sexp' && chmod 000 '"
                       (car k) "/writers/" k-other "/000001.sexp'"))
(define vd7b-unreadable (caught (cold-verdicts (car k) "UK" (cons (cdr k) (end-seq (car k) (cdr k))) incomplete-accepted)))
(system (string-append "chmod 644 '" (car k) "/writers/" k-other "/000001.sexp'"))
(printf "VD-7b unreadable writer: ~s\n" vd7b-unreadable)
(want "VD-7b an unreadable writer under incomplete-accepted: the provided verdict as the plain one"
      (and (pair? vd7b-unreadable) (not (eq? (car vd7b-unreadable) 'RAISED)) (same? vd7b-unreadable))
      #t)

;; (c) a request asked again with its own cursor: replay, through the provider
(define m (fresh-store "m"))
(define m-cursor (cons (cdr m) (end-seq (car m) (cdr m))))
(rpc-dispatch (car m) (list 'insert "--title" "once" "--req" "RM" "--cursor" (format "~a:~a" (car m-cursor) (cdr m-cursor))) "test")
(define vd7c (cold-verdicts (car m) "RM" m-cursor #f '("--title" "once")))
(printf "VD-7c ~s\n" vd7c)
(want "VD-7c a request asked again with the cursor it ran with: replay, through the provider as plain"
      (list (car (cadr vd7c)) (same? vd7c))
      '(replay #t))

;; ---- VD-8, THE MISS, in a child holding at after-discovery -----------------------
(define vd8-child
  '((import (chezscheme) (theourgia log) (theourgia trace)
            (only (theourgia store) make-write-request request-verdict request-verdict/provided)
            (only (theourgia evidence-index) index-forget-memory!))
    (define args (command-line-arguments))
    (define st (car args))
    (define w (cadr args))
    (define given (call-with-input-file (caddr args) read))
    (define req (make-write-request "test" 'insert (list "root" "x") "MISS" (cons w (caddr given))))
    (define s (log-begin st (lambda a 'applied)))
    (trace-enable! #t)
    (define (cold!) (index-forget-memory!)
      (let ((cp (string-append st "/request-index.sexp"))) (when (file-exists? cp) (delete-file cp))))
    (define provided (begin (cold!) (request-verdict/provided (session-discovery-provider s) st req (car given) #f (cadr given))))
    (trace-enable! #f)
    (define plain (begin (cold!) (request-verdict st req (car given) #f (cadr given))))
    (log-end! s)
    (write (list (car provided) (car plain) (equal? provided plain)))))
(define n8 (fresh-store "n8"))
(define vd8-release (string-append root "/vd8-release"))
(define vd8-writer "abcd1234")
(define vd8
  (let* ((given (given-file! (car n8) (cdr n8) "vd8"))
         (ps (start-child! "vd8" (string-append "THEOURGIA_INJECT=on THEOURGIA_HOLD=after-discovery:" vd8-release) vd8-child
                           (list (car n8) (cdr n8) given))))
    (if (await-file (string-append vd8-release ".held"))
        (begin
          ;; what ensure-area! does under the draft lock only: the writer's directory
          (system (string-append "mkdir -p '" (car n8) "/writers/" vd8-writer "'"))
          (call-with-output-file vd8-release (lambda (p) (display "go" p)) 'replace)
          (child-result ps))
        (cons (list 'never-held) (cdr (child-result ps))))))
(printf "VD-8 ~s; misses ~a\n" (car vd8) (count-lines (cdr vd8) (string-append "(trace verdict-provider miss " vd8-writer ")")))
(want "VD-8 a writer directory made while the session held after discovery: the provider misses it (traced), and the verdict answers as the plain one"
      (list (and (pair? (car vd8)) (caddr (car vd8)))
            (> (count-lines (cdr vd8) (string-append "(trace verdict-provider miss " vd8-writer ")")) 0))
      '(#t #t))

;; VD-6, THE WAIT IS A BOUND: a child that sleeps past a wait of about a second
;; is killed by its pid and answers outlived-its-wait, and is gone after
(define vd6-ps (start-child! "vd6" "" '((import (chezscheme)) (sleep (make-time 'time-duration 0 60))) '()))
(define vd6 (child-result vd6-ps 50))
(define vd6-pid (read-or-false (file-text (list-ref vd6-ps 4))))
(sleep (make-time 'time-duration 200000000 0))
(define (vd6-gone?)
  (and (integer? vd6-pid)
       (not (zero? (system (string-append "kill -0 " (number->string vd6-pid) " 2>/dev/null"))))))
(want "VD-6 a child that outlives its wait is killed by its pid, answers outlived-its-wait, and is gone"
      (list (car vd6) (integer? vd6-pid) (vd6-gone?))
      '(outlived-its-wait #t #t))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\nverdict-discoveries complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
