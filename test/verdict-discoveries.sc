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
;; The quarantined fork, the retired writer with its successor and the
;; unreadable writer are judged by the verdict-discoveries check (degeneracy.sc)
;; wherever a fixture that builds them commits a request under the switch.
(import (chezscheme) (theourgia rpc) (theourgia log) (theourgia trace)
        (only (theourgia store) open-and-reduce make-write-request request-verdict request-verdict/provided)
        (only (theourgia reduce) reduce-applied-cut reduce-gates)
        (only (theourgia degeneracy) degeneracy-active)
        (only (theourgia evidence-index) index-forget-memory!))

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
(want "VD-5 a provider answering from another store: the derivation answers the conservative interval, the verdict unknown, and the witness's line is on the error port once for each"
      (list (car vd5) (count-lines (cdr vd5) "degeneracy: a provided discovery differs from a fresh one"))
      (list (list (list (list (cdr f1) 0 #f)) 'unknown) 2))

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
