#!r6rs
;; Copyright 2026 guenchi
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

;; `query` and `context` through the daemon: published reads, answered by the
;; store process from the publication made current first (a read at a cut is
;; served the same way), without replaying the store while the disk has not
;; moved. A REPLAY OPENS THE LOG, and the daemon's trace says so: `(trace
;; log-open <store> #f)`; a published read over a current publication leaves
;; none.
;;
;; KEY: THE DAEMON IS ANOTHER OS PROCESS, asked over its socket exactly as a
;; client asks it; its trace (THEOURGIA_TRACE) says which process answered.
;; THE LOCAL ROUTE is rpc-dispatch in this process over the same store: an
;; answer read from the socket is compared with it as a datum, and since a
;; query's rows are canonical and its digest is theirs, an equal datum is the
;; same bytes. Every comparison is made after the daemon has published what
;; it was compared against.

(import (chezscheme) (theourgia sched) (theourgia net) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce search-keyword-hook)
        (only (theourgia reduce) block-id reduce-applied-cut reduce-empty reduce-apply! reduce-clone)
        (only (theourgia wire) encode-record storable-encode)
        (only (theourgia log) log-publish! segment-sha)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (theourgia render) render-wire))

;; LOOKED UP, NOT IMPORTED: a base without step B lacks them, and its rows
;; read red one by one (an absent procedure answers the symbol absent)
(define (late name) (guard (e (#t #f)) (eval name (environment '(theourgia query)))))
(define (q-proc name) (or (late name) (lambda args 'absent)))
(define make-session-provider (q-proc 'make-session-provider))
(define make-query-session (q-proc 'make-query-session))
(define session-answer (q-proc 'session-answer))
(define session-budget-set! (q-proc 'session-budget-set!))
(define session-spent (q-proc 'session-spent))
(define session-synthetic-count (q-proc 'session-synthetic-count))
(define session-advance-calls (q-proc 'session-advance-calls))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(register-verbs! extension-verbs)

;; ---- helpers copied from test/daemon-e1.sc ----------------------------------------------------
;; (pid-text, the two scratch roots, line-complete?, file-text, contains?,
;; answer-text and wait-for-publication as daemon-e1 has them;
;; start-session-daemon! is its start-tagged-daemon-with! with no fault, no
;; barrier and no spelling, which these rows do not use)

(define pid-text (number->string (get-process-id)))
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define socket-base
  (let ((v (getenv "THEOURGIA_TEST_SOCK")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

(define (line-complete? bv)
  (let loop ((i 0))
    (cond ((>= i (bytevector-length bv)) #f)
          ((= (bytevector-u8-ref bv i) 10) #t)
          (else (loop (+ i 1))))))

(define (file-text path)
  (if (not (file-exists? path))
      ""
      (let ((text (call-with-input-file path get-string-all)))
        (if (string? text) text ""))))

(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

(define (answer-text bv)
  (let* ((text (if (string? bv) bv (utf8->string bv)))
         (datum (guard (e (#t #f)) (read (open-string-input-port text)))))
    (if (and (pair? datum) (eq? 'answer (car datum)))
        (let ((hit (assq 'stdout (cdr datum))))
          (if (and (pair? hit) (pair? (cdr hit)) (string? (cadr hit)))
              (cadr hit)
              text))
        text)))

;; -> (store socket runner log); the store initialised by the runner, the
;; extension verbs registered, the trace on, every run with
;; THEOURGIA_INJECT=on as daemon-e1's are
(define (start-session-daemon! suffix . options)
  (let* ((size (and (pair? options) (car options)))
         (fault (and (pair? options) (pair? (cdr options)) (cadr options)))
         ;; more environment for the daemon, as `NAME=value ` text, or #f
         (env (and (pair? options) (pair? (cdr options)) (pair? (cddr options)) (caddr options)))
         (tag (string-append pid-text "-" suffix))
         (st (string-append scratch-base "/dss-" tag))
         (sk (string-append socket-base "/dss-" tag ".sock"))
         (rn (string-append scratch-base "/dss-" tag ".sc"))
         (lg (string-append scratch-base "/dss-" tag ".log")))
    (system (string-append "rm -rf " st " " sk "; mkdir -p " st))
    (call-with-output-file rn
      (lambda (port)
        (for-each (lambda (l) (display l port) (newline port))
          (list "(import (chezscheme) (theourgia daemon) (theourgia rpc) (only (theourgia extensions) extension-verbs))"
                ;; the extension verbs registered as theourgiad.sc registers them:
                ;; query and context are among them
                "(register-verbs! extension-verbs)"
                (string-append "(rpc-dispatch \"" st "\" '(init) \"tester\")")
                (if size
                    (string-append "(serve \"" st "\" \"" sk "\" #f " (number->string size) ")")
                    (string-append "(serve \"" st "\" \"" sk "\")")))))
      'truncate)
    (system (string-append "THEOURGIA_TRACE=1 THEOURGIA_INJECT=on "
                           (if fault (string-append "THEOURGIA_FAULT=" fault " ") "")
                           (or env "")
                           "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                           " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                           " sh -c 'scheme --script " rn " > " lg " 2>&1' &"))
    (let up ((k 0))
      (cond ((file-exists? sk) (list st sk rn lg))
            ((> k 120) (error 'start-session-daemon! "the daemon never created its socket" tag))
            (else (sleep-ms 50) (up (+ k 1)))))))
(define (daemon-store d) (car d))
(define (daemon-socket d) (cadr d))
(define (daemon-log d) (file-text (cadddr d)))
(define (stop-session-daemon! d)
  (system (string-append "pkill -f " (caddr d) " 2>/dev/null")))

(define (wait-for-publication d revision ms)
  (let ((marker (string-append "(trace published " (number->string revision))))
    (let wait ((k 0))
      (cond ((contains? (daemon-log d) marker) 'published)
            ((> k (div ms 50)) (list 'never-published revision))
            (else (sleep-ms 50) (wait (+ k 1)))))))

;; ---- this fixture's own helpers ------------------------------------------------------------

;; a request through the socket: VERB and its argument strings -> the answer
;; read as a datum, or the text when it does not read
(define (ask d verb . args)
  (ask-again d verb args 20))
;; THE MACHINE'S INSTANCE REGISTRY LOCK (~/.theourgia/lock) is shared by every
;; store on the machine: a first write that meets another process holding it
;; answers store-busy, and is asked again (up to N times, 100 ms apart)
(define (registry-busy? a)
  (and (pair? a) (list? a) (eq? (car a) 'error) (pair? (cdr a)) (eq? (cadr a) 'store-busy)
       (let ((p (assq 'path (cddr a))))
         (and p (string? (cadr p))
              (let ((t (cadr p)) (tail "/.theourgia/lock"))
                (and (>= (string-length t) (string-length tail))
                     (string=? (substring t (- (string-length t) (string-length tail)) (string-length t)) tail)))))))
(define (ask-again d verb args n)
  (let ((a (ask-once d verb args)))
    (if (and (> n 0) (registry-busy? a))
        (begin (sleep-ms 100) (ask-again d verb args (- n 1)))
        a)))
(define (ask-once d verb args)
  (let* ((text (apply string-append (symbol->string verb) (map (lambda (a) (string-append " " (format "~s" a))) args)))
         (frame (string-append "(request 1 \"" (daemon-store d) "\" \"tester\" #f wire #f #f " text ")\n"))
         (r (exchange (daemon-socket d) (string->utf8 frame) line-complete? 20000)))
    (if (and (pair? r) (eq? 'answer (car r)) (> (bytevector-length (cadr r)) 0))
        (let ((out (answer-text (cadr r))))
          (guard (e (#t out)) (read (open-string-input-port out))))
        'no-answer)))
;; THE LOCAL ROUTE'S ANSWER AS THE SOCKET CARRIES IT: rendered by render-wire,
;; as the daemon renders it (a text field the store keeps as bytes is printed
;; as text there), and read back
(define (local d verb . args)
  (let ((a (rpc-dispatch (daemon-store d) (cons verb args) "tester")))
    (guard (e (#t a)) (read (open-string-input-port (render-wire a))))))
;; THE ANSWER'S TEXT AS THE SOCKET CARRIES IT, not read back: the bytes a
;; --wire client prints. The local route's text is render-wire of its answer,
;; which is how the daemon writes one.
(define (ask-text d verb . args)
  (let* ((text (apply string-append (symbol->string verb) (map (lambda (a) (string-append " " (format "~s" a))) args)))
         (frame (string-append "(request 1 \"" (daemon-store d) "\" \"tester\" #f wire #f #f " text ")\n"))
         (r (exchange (daemon-socket d) (string->utf8 frame) line-complete? 20000)))
    (if (and (pair? r) (eq? 'answer (car r)) (> (bytevector-length (cadr r)) 0))
        (answer-text (cadr r))
        'no-answer)))
(define (local-text d verb . args)
  (render-wire (rpc-dispatch (daemon-store d) (cons verb args) "tester")))
;; a search answer's hit ids, in order, and a named clause's value
(define (hit-ids a)
  (if (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a)) (pair? (cadr a)) (eq? (car (cadr a)) 'items))
      (map cadr (filter (lambda (i) (and (pair? i) (eq? (car i) 'hit))) (cdr (cadr a))))
      'NOT-A-SEARCH))
(define (clause-of a name)
  (and (pair? a) (list? a) (let ((c (find (lambda (c) (and (pair? c) (eq? (car c) name))) (cdr a)))) (and c (cdr c)))))
;; WRITES FROM ANOTHER PROCESS, as an outside writer makes them: each request
;; a list of strings after the verb, dispatched in one process in order
(define (outside-requests! store requests tag)
  (let ((rn (string-append scratch-base "/dss-req-" tag ".sc")) (lg (string-append scratch-base "/dss-req-" tag ".log")))
    (system (string-append "rm -f " rn " " lg))
    (call-with-output-file rn
      (lambda (port)
        (for-each (lambda (l) (display l port) (newline port))
          (list "(import (chezscheme) (theourgia rpc))"
                (format "(for-each (lambda (r) (write (rpc-dispatch ~s r \"outsider\")) (newline)) '~s)" store requests)))))
    (system (string-append "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                           " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                           " scheme --script " rn " > " lg " 2>&1"))
    (file-text lg)))
(define (wait-for-file path ms)
  (let wait ((k 0))
    (cond ((file-exists? path) #t)
          ((> k (div ms 50)) #f)
          (else (sleep-ms 50) (wait (+ k 1))))))
(define (new-id a)
  (let* ((ev (and (pair? a) (eq? (car a) 'ok) (assq 'events (cdr a))))
         (e (and ev (pair? (cadr ev)) (car (cadr ev)))))
    (and (pair? e) (block-id (car e) (cdr e)))))
(define (count-of text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0) (c 0))
      (cond ((> (+ i n) m) c)
            ((string=? (substring text i (+ i n)) needle) (loop (+ i 1) (+ c 1)))
            (else (loop (+ i 1) c))))))
;; how many times the trace says VERB was routed to ROUTE
(define (routed-count d verb route)
  (count-of (daemon-log d) (string-append "(trace routed " (symbol->string verb) " " (symbol->string route) ")")))
(define (published-count d) (count-of (daemon-log d) "(trace published "))
;; how many times the daemon has opened the log: once per replay
(define (log-opens d) (count-of (daemon-log d) "(trace log-open "))
(define (wait-for-more-publications d before ms)
  (let wait ((k 0))
    (cond ((> (published-count d) before) 'published)
          ((> k (div ms 50)) (list 'no-new-publication before))
          (else (sleep-ms 50) (wait (+ k 1))))))

;; THE CENSUS: the two handlers reach the store only through reduction-for and
;; the keyword hook. Both files are read as data and every symbol is walked,
;; quoted ones included (comments and strings are not data). search-state is
;; not on the list: it searches a state it is handed, reaching the derived
;; tables through that same hook.
(define store-reaching '(open-and-reduce with-store-write log-open log-publish! with-table-lock obtain-state))
(define (symbols-of file)
  (let ((seen '()))
    (call-with-input-file file
      (lambda (port)
        (let next ((d (read port)))
          (unless (eof-object? d)
            (let walk ((x d))
              (cond ((symbol? x) (set! seen (cons x seen)))
                    ((pair? x) (walk (car x)) (walk (cdr x)))
                    ((vector? x) (vector-for-each walk x))))
            (next (read port))))))
    seen))
(define (contains-text? name part) (contains? (symbol->string name) part))
;; THE NOTE: a long log after the daemon's snapshot, written by an outside
;; writer in one process; a query starts the reload, the row waits for the
;; publication, then times five queries and five outlines through the socket.
;; On a tree where the store process is handed no state each query replays the
;; store; here each reads the publication, already current. Printed, not
;; asserted.
(define (outside-inserts! store n tag)
  (let ((rn (string-append scratch-base "/dss-out-" tag ".sc")) (lg (string-append scratch-base "/dss-out-" tag ".log")))
    (system (string-append "rm -f " rn " " lg))
    (call-with-output-file rn
      (lambda (port)
        (for-each (lambda (l) (display l port) (newline port))
          (list "(import (chezscheme) (theourgia rpc))"
                (format "(let loop ((i 0)) (when (< i ~a) (rpc-dispatch ~s (list 'insert \"--title\" (format \"o~~a\" i)) \"outsider\") (loop (+ i 1))))" n store)))))
    (system (string-append "CHEZSCHEMELIBDIRS=" (getenv "CHEZSCHEMELIBDIRS")
                           " CHEZSCHEMELIBEXTS='" (getenv "CHEZSCHEMELIBEXTS") "'"
                           " scheme --script " rn " > " lg " 2>&1"))
    (file-text lg)))
(define (median-ms thunk n)
  (let ((ts (let loop ((i 0) (out '()))
              (if (= i n) out
                  (let ((t0 (real-time))) (thunk) (loop (+ i 1) (cons (- (real-time) t0) out)))))))
    (list-ref (list-sort < ts) (div n 2))))
;; ---- step B: the session the daemon keeps --------------------------------------------------

;; THE SESSION TRACE, in the order written: each (trace session <word> <rest>)
;; line read as (<word> <rest>)
(define (session-events d)
  (let ((port (open-string-input-port (daemon-log d))))
    (let next ((out '()))
      (let ((x (guard (e (#t 'skip)) (read port))))
        (cond ((eof-object? x) (reverse out))
              ((and (pair? x) (eq? (car x) 'trace) (pair? (cdr x)) (eq? (cadr x) 'session) (= (length x) 4))
               (next (cons (cddr x) out)))
              (else (next out)))))))
;; THUNK run, with the session lines it caused: (answer decisions served
;; local), decisions the (<word> <rest>) lines but `served`, served its
;; (<verb> <expanded> <new> <size>) or #f, LOCAL the local route's answer to
;; the same request taken right after it (#f when not asked for)
(define (traced d thunk . local-thunk)
  (let* ((n (length (session-events d)))
         (a (thunk))
         (l (and (pair? local-thunk) ((car local-thunk))))
         (evs (list-tail (session-events d) n))
         (served (find (lambda (e) (eq? (car e) 'served)) evs)))
    (list a (filter (lambda (e) (not (eq? (car e) 'served))) evs) (and served (cadr served)) l)))
;; the socket's answer, with the local route's answer at the same state
(define (traced-ask d verb . args)
  (traced d (lambda () (apply ask d verb args)) (lambda () (apply local d verb args))))
(define (answer-of t) (car t))
(define (decisions-of t) (cadr t))
(define (expanded-of t) (let ((s (caddr t))) (and (pair? s) (cadr s))))
(define (size-of t) (let ((s (caddr t))) (and (pair? s) (cadddr s))))
;; the decision words alone, the numbers left out: (advance (1 0 12)) -> advance
(define (words-of t) (map (lambda (e) (if (memq (car e) '(advance)) (list (car e)) e)) (decisions-of t)))
(define (same-as-local? d t . ignored) (equal? (answer-of t) (cadddr t)))

;; A RECORD FROM ANOTHER WRITER, PEER, whose premise is every other writer's
;; applied history: appended to PEER's own segment, as a record from
;; elsewhere arrives. A peer named 00000000 sorts before any local writer.
(define (forge! st peer seq payload)
  (let* ((past (filter (lambda (e) (not (equal? (car e) peer))) (reduce-applied-cut (open-and-reduce st))))
         (bytes (encode-record seq 1789000000001 "peer" past (storable-encode payload))))
    (log-publish! st peer seq bytes (segment-sha bytes))))
;; an outline through the socket starts the reload the probe asks for; this
;; waits for its publication
(define (reloaded! d)
  (let ((before (published-count d)))
    (ask d 'outline)
    (wait-for-more-publications d before 30000)))
(define (reach-value name)
  (format "~s" (list (list 'head (list name 'ID 'ID))
                     (list 'clause (list name '?a '?b) '(edge ?a cites ?b))
                     (list 'clause (list name '?a '?b) '(edge ?a cites ?m) (list name '?m '?b)))))
(define (rows-of-answer a)
  (if (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a)) (pair? (cadr a)))
      (map cdr (filter (lambda (i) (and (pair? i) (eq? (car i) 'row))) (cdr (cadr a))))
      a))
(define (daemon-pid d)
  (let ((f (string-append scratch-base "/dss-pid-" pid-text)))
    (system (string-append "pgrep -f '^scheme --script " (caddr d) "' | head -1 > " f))
    (let ((t (file-text f))) (and (> (string-length t) 1) (string->number (substring t 0 (- (string-length t) 1)))))))
(define (rss-kb pid)
  (let ((f (string-append scratch-base "/dss-rss-" pid-text)))
    (system (string-append "ps -o rss= -p " (number->string pid) " | tr -d ' ' > " f))
    (let ((t (file-text f))) (and (> (string-length t) 1) (string->number (substring t 0 (- (string-length t) 1)))))))

;; IN PROCESS: a provider over thunks this fixture controls, its trace
;; collected; the published state is the box's
(define (in-process-provider box bound)
  (let ((seen '()) (generation 0))
    (let ((p (make-session-provider (lambda () (vector-ref box 0)) (lambda () generation) bound
                                    (lambda words (set! seen (append seen (list words)))))))
      (list p (lambda () (let ((s seen)) (set! seen '()) s)) (lambda () (set! generation (+ generation 1)))))))
(define (local-store! name)
  (let ((st (string-append scratch-base "/dss-local-" pid-text "-" name)))
    (system (string-append "rm -rf " st))
    (rpc-dispatch st '(init) "tester")
    st))
(define (on st . args)
  (let again ((n 20))
    (let ((a (rpc-dispatch st args "tester")))
      (if (and (> n 0) (registry-busy? a))
          (begin (sleep-ms 100) (again (- n 1)))
          a))))
(define (fresh-rows st goal)
  (let ((a (session-answer (make-query-session (open-and-reduce st)) goal))) (if (and (pair? a) (eq? (car a) 'ok)) (cadr a) a)))
(define (session-rows-of S goal) (let ((a (session-answer S goal))) (if (and (pair? a) (eq? (car a) 'ok)) (cadr a) a)))

;; ---- the rows of the kept session over a daemon (SESS-1, 2, 2b, 3, 4a-b, 8a, two NOTEs) --------
(define (kept-session-rows)
  (let* ((E (start-session-daemon! "b"))
         (st (daemon-store E))
         (b1 (new-id (ask E 'insert "--title" "b1")))
         (b2 (new-id (ask E 'insert "--title" "b2")))
         (b3 (new-id (ask E 'insert "--title" "b3")))
         (b4 (new-id (ask E 'insert "--title" "b4")))
         (setup (list (ask E 'link b1 "cites" b2) (ask E 'link b2 "cites" b3) (ask E 'derive "rch" (reach-value 'rch))))
         (q (format "(rch ~s ?x)" b1))
         (ctx (lambda () (traced-ask E 'context "--for" b1 "--budget" "4000")))
         (ctx-same? (lambda (t) (same-as-local? E t 'context "--for" b1 "--budget" "4000")))
         ;; (a) a first ask, then context
         (t1 (traced-ask E 'query q)) (c1 (ctx))
         ;; (b) the same again
         (t2 (traced-ask E 'query q)) (c2 (ctx))
         ;; a request that takes no session (query --relations) after one that did
         (tr (traced-ask E 'query "--relations"))
         ;; (c) a link added through the daemon
         (w3 (ask E 'link b3 "cites" b4))
         (t3-start (real-time)) (t3 (traced-ask E 'query q)) (t3-ms (- (real-time) t3-start))
         (fresh3 (let ((S (make-query-session (open-and-reduce st)))) (session-answer S (read (open-string-input-port q))) (session-spent S)))
         (c3 (ctx))
         (t3r-start (real-time)) (t3r (traced-ask E 'query q)) (t3r-ms (- (real-time) t3r-start))
         ;; (d) a link removed
         (w4 (ask E 'unlink b2 "cites" b3))
         (t4 (traced-ask E 'query q)) (c4 (ctx))
         ;; (e) an outside writer whose name sorts first appends; the reload
         (f5 (forge! st "00000000" 1 (list 'set b4 'title "renamed")))
         (r5 (reloaded! E))
         (t5 (traced-ask E 'query q)) (c5 (ctx))
         ;; the snapshot-equal twin: a snapshot of the publication, the same
         ;; writer appends again, the reload
         (t6a (traced-ask E 'query q))
         (snap (ask E 'snapshot))
         (f6 (forge! st "00000000" 2 (list 'set b4 'title "renamed again")))
         (r6 (reloaded! E))
         (t6 (traced-ask E 'query q))
         ;; a goal with a form, asked four times
         (fq (format "(and (kind ?b section) (not (edge ?b cites ~s)))" b3))
         (f-sizes (let loop ((i 0) (out '())) (if (= i 4) (reverse out) (loop (+ i 1) (cons (size-of (traced-ask E 'query fq)) out))))))
    (want "SESS-1 the same query twice: both the local route's answer; the first a fresh session kept, the second reused, expanding no named key; a request that takes no session after them traces no decision and no served line"
          (list (same-as-local? E t1 'query q) (words-of t1) (same-as-local? E t2 'query q) (words-of t2) (expanded-of t2)
                (decisions-of tr) (caddr tr))
          '(#t ((fresh none)) #t ((reuse #f)) 0 () #f))
    (want "SESS-2 the socket's query and context answers equal the local route's after each of: a first ask, a repeat, a link added, a link removed, an outside earlier-sorting append and its reload"
          (list (map (lambda (t) (same-as-local? E t 'query q)) (list t1 t2 t3 t4 t5))
                (map ctx-same? (list c1 c2 c3 c4 c5)) r5)
          '((#t #t #t #t #t) (#t #t #t #t #t) published))
    (want "SESS-2b a repeated context request is handed the kept session (its decision is reuse) and expands no named key, where the first expanded some"
          (list (and (number? (expanded-of c1)) (> (expanded-of c1) 0)) (words-of c2) (expanded-of c2))
          '(#t ((reuse #f)) 0))
    (want "SESS-3 a write adds an edge: the next ask advances the session (a key carried), expands nothing and holds the new pair; a write removes one: advanced again, the pairs through it gone"
          (list (words-of t3) (let ((e (assq 'advance (decisions-of t3)))) (and e (>= (car (cadr e)) 1))) (expanded-of t3)
                (sort-rows (rows-of-answer (answer-of t3))) (words-of t4) (expanded-of t4) (rows-of-answer (answer-of t4)) (same-as-local? E t4 'query q))
          (list '((advance)) #t 0 (sort-rows (list (list b2) (list b3) (list b4))) '((advance)) 0 (list (list b2)) #t))
    (want "SESS-4 the daemon's own records after the snapshot's cut, then an earlier-sorting outside append and the reload: not forward, a fresh session; the snapshot-equal twin: the append extends the history and the session advances"
          (list (words-of t5) (words-of t6a) (and (pair? snap) (car snap)) r6 (words-of t6) (same-as-local? E t6 'query q))
          '(((fresh not-forward)) ((reuse #f)) ok published ((advance)) #t))
    (want "SESS-8 a goal with a form asked four times through the socket: the session's size after each the same (the query's and the form's keys removed each time)"
          (let ((s (car f-sizes))) (list (number? s) (map (lambda (x) (eqv? x s)) f-sizes)))
          '(#t (#t #t #t #t)))
    (let* ((e (assq 'advance (decisions-of t3))))
      (printf "NOTE SESS-3 the advance after one edge spent ~a (carried ~a, dropped ~a: the query's and the context's keys); a fresh session asked the same query at that state spends ~a (its keys only)\n"
              (and e (caddr (cadr e))) (and e (car (cadr e))) (and e (cadr (cadr e))) fresh3)
      (printf "NOTE SESS-3 wall: the first ask after the write (the advance) ~a ms, the next (reuse) ~a ms\n" t3-ms t3r-ms))
    (let* ((pid (daemon-pid E)) (before (and pid (rss-kb pid))))
      (let loop ((i 0)) (when (< i 50) (ask E 'query q) (ask E 'query fq) (loop (+ i 1))))
      (printf "NOTE the daemon's resident set before and after 100 more asks: ~a kB, ~a kB\n" before (and pid (rss-kb pid))))
    (stop-session-daemon! E)))
(define (sort-rows rows) (list-sort (lambda (x y) (string<? (format "~s" x) (format "~s" y))) rows))

;; ---- name use and the derived tables over a daemon with code (SESS-7, SESS-12, SESS-2f) -------
(define (find-headed x head)
  (cond ((and (pair? x) (eq? (car x) head)) x)
        ((pair? x) (or (find-headed (car x) head) (find-headed (cdr x) head)))
        (else #f)))
(define (write-text! path text)
  (call-with-output-file path (lambda (p) (display text p)) 'truncate))
(define (sha-of path) (bytevector->hex (sha256 (call-with-port (open-file-input-port path) get-bytevector-all))))
(define (name-use? a) (and (pair? a) (list? a) (member '(name-use syntactic) (cdr a)) #t))
;; ---- a working view through the daemon does not touch the kept session ----
;; THE KEPT SESSION IS KEYED ON THE VIEW'S IDENTITY (query.sc,
;; make-session-provider, rule 1): a view that is not the publication gets a
;; fresh session over itself, and the kept cell is neither read nor written.
;; A working view is a new reduction every call, so every context --working
;; is fresh, and the committed context after them is the kept session reused
;; with nothing expanded: not evicted and rebuilt.
(define (ask-as d writer verb . args)
  (let* ((text (apply string-append (symbol->string verb) (map (lambda (a) (string-append " " (format "~s" a))) args)))
         (frame (string-append "(request 1 \"" (daemon-store d) "\" \"tester\" " (format "~s" writer) " wire #f #f " text ")\n"))
         (r (exchange (daemon-socket d) (string->utf8 frame) line-complete? 20000)))
    (if (and (pair? r) (eq? 'answer (car r)) (> (bytevector-length (cadr r)) 0))
        (let ((out (answer-text (cadr r))))
          (guard (e (#t out)) (read (open-string-input-port out))))
        'no-answer)))
(define (working-session-rows)
  (let* ((E (start-session-daemon! "w"))
         (u (new-id (ask E 'insert "--title" "u use")))
         (setup (list (ask E 'set u "kind" "code") (ask E 'set u "lang" "javascript") (ask E 'set u "src" "k()")))
         (draft (ask E 'write u "k(); // zanzibar" "--writer" "sesswriter"))
         (ctx (lambda () (traced-ask E 'context "--for" u "--budget" "4000")))
         (c1 (ctx)) (c2 (ctx))
         (w1 (traced E (lambda () (ask-as E "sesswriter" 'context "--for" u "--budget" "4000" "--working"))))
         (w2 (traced E (lambda () (ask-as E "sesswriter" 'context "--for" u "--budget" "4000" "--working"))))
         (c3 (ctx)))
    (stop-session-daemon! E)
    (want "SESS-W committed context, then two context --working calls, then committed again through the socket: the working calls are answered with the draft and the marker, each from a fresh session (fresh cut); the committed call after them reuses the kept session, expanding nothing, and answers what it answered before them"
          (list (and (pair? draft) (car draft)) (words-of c1) (words-of c2)
                (contains? (format "~s" (answer-of w1)) "zanzibar") (clause-of (answer-of w1) 'working)
                (words-of w1) (words-of w2) (contains? (format "~s" (answer-of w2)) "zanzibar")
                (words-of c3) (expanded-of c3) (equal? (answer-of c3) (answer-of c2)) (same-as-local? E c3)
                (contains? (format "~s" (answer-of c3)) "zanzibar"))
          '(ok ((fresh none)) ((reuse #f)) #t ("sesswriter") ((fresh cut)) ((fresh cut)) #t ((reuse #f)) 0 #t #t #f))))
(define (code-rows)
  (let* ((F (start-session-daemon! "c"))
         (src (string-append scratch-base "/dss-src-" pid-text))
         (out (string-append scratch-base "/dss-out-" pid-text))
         (mk (begin (system (string-append "rm -rf " src " " out "; mkdir -p " src " " out))
                    (write-text! (string-append src "/a.js") "function alpha() {\n  return h();\n}\n")
                    (write-text! (string-append src "/b.js") "function h() {\n  return 1;\n}\n")))
         (imported (ask F 'import-code src))
         (alpha (let ((m (find-headed (ask F 'grep "function alpha") 'match))) (and m (cadr m))))
         ;; def-for, a library rule, reads uses-name (a stored relation may not)
         (q1 "(uses-name ?b ?n)") (q2 "(def-for ?b ?n ?d)") (q3 "(and (def-for ?b ?n ?d) (kind ?b code))") (q4 "(kind ?b ?k)")
         (q5 "(and (kind ?b code) (not (def-for ?b _ _)))")
         (a1 (ask F 'query q1)) (a1b (ask F 'query q1)) (a2 (ask F 'query q2)) (a3 (ask F 'query q3)) (a4 (ask F 'query q4))
         (a5 (ask F 'query q5)))
    (want "SESS-7 name use: a query consulting uses-name says so; the same again (its tuples from the memo) says so; a query deriving a table through it says so, and one reading only that table says so; a query reading no such table does not; one reading it under a not says so; each equal to the local route's"
          (list (map name-use? (list a1 a1b a2 a3 a4 a5))
                (map (lambda (a g) (equal? a (local F 'query g))) (list a1 a1b a2 a3 a4 a5) (list q1 q1 q2 q3 q4 q5)))
          '((#t #t #t #t #f #t) (#t #t #t #t #t #t)))
    (let* ((qs (format "(score ?b ~s ?s)" "narwhal"))
           (before (traced-ask F 'query qs))
           (exported (ask F 'export-code out))
           (files (list-sort (lambda (x y) (string<? (car x) (car y)))
                             (map (lambda (n) (list n (sha-of (string-append out "/" n))))
                                  (filter (lambda (n) (not (file-directory? (string-append out "/" n)))) (directory-list out)))))
           (file (string-append scratch-base "/dss-supply-" pid-text ".sexp"))
           (w (write-text! file (string-append
                                  (format "~s\n" (list 'supply 'signatures '(writer "-") '(language "javascript")
                                                       '(source (vscode "1.140.0")) (list 'files files) (list 'replaces (map car files))))
                                  (format "~s\n" (list 'keywords alpha '("narwhal") (list 'depends (list alpha)))))))
           (supplied (ask F 'supply "signatures" file))
           (after (traced-ask F 'query qs))
           (ctx (traced-ask F 'context "--for" alpha "--budget" "4000")))
      (want "SESS-12 a supply through the daemon for a scored block: the next ask drops the session (derived changed) and its rows hold the new score, equal to the local route's"
            (list (rows-of-answer (answer-of before)) (and (pair? supplied) (car supplied))
                  (and (assq 'dropped (decisions-of after)) #t) (map car (rows-of-answer (answer-of after)))
                  (same-as-local? F after 'query qs))
            (list '() 'ok #t (list alpha) #t))
      (want "SESS-2f after the supply, the socket's context answer equals the local route's too"
            (same-as-local? F ctx 'context "--for" alpha "--budget" "4000")
            #t))
    (stop-session-daemon! F)))

;; ---- the bound (SESS-6) ----------------------------------------------------------------------
(define (bound-rows)
  (let* ((G (start-session-daemon! "g" 50))
         (ids (let loop ((i 0) (out '())) (if (= i 12) (reverse out) (loop (+ i 1) (cons (new-id (ask G 'insert "--title" (format "g~a" i))) out)))))
         (links (let loop ((l ids)) (when (pair? (cdr l)) (ask G 'link (car l) "cites" (cadr l)) (loop (cdr l)))))
         (derived (ask G 'derive "rch" (reach-value 'rch)))
         (q (format "(rch ~s ?x)" (car ids)))
         (t1 (traced-ask G 'query q)) (t2 (traced-ask G 'query q))
         (M (start-session-daemon! "m" 50))
         (memo-asks (let loop ((i 0) (out '()))
                      (if (= i 40) (reverse out) (loop (+ i 1) (cons (traced-ask M 'query (format "(score ?b ~s ?s)" (format "absent~a" i))) out)))))
         (Z (start-session-daemon! "z" 0))
         (z1 (traced-ask Z 'query "(in-force ?x)")) (z2 (traced-ask Z 'query "(in-force ?x)")))
    (want "SESS-6 --session-size 50: a query holding more than 50 entries, then the next ask drops the session over the bound and starts a fresh one; asks growing only the memo pass the bound too; --session-size 0: every ask a fresh session (the repeated ask expands its named keys again)"
          (list (and (number? (size-of t1)) (> (size-of t1) 50)) (map car (decisions-of t2))
                (and (assq 'bound (map (lambda (e) (if (and (eq? (car e) 'dropped) (pair? (cadr e))) (list (car (cadr e))) '(none))) (apply append (map decisions-of memo-asks)))) #t)
                (words-of z1) (words-of z2) (and (number? (expanded-of z2)) (> (expanded-of z2) 0)))
          '(#t (dropped fresh) #t ((fresh disabled)) ((fresh disabled)) #t))
    (for-each stop-session-daemon! (list G M Z))))

;; ---- two clients (SESS-9) --------------------------------------------------------------------
;; The two verbs are the store process's, which serves one request at a time,
;; so one actor touches the session. A connection held open (the conn-hold
;; fault holds the first connection-local read, an outline here) does not
;; hold the other connection's queries, which are answered and reuse.
(define (two-client-rows)
  (let* ((H (start-session-daemon! "h" #f "conn-hold@conn"))
         (h1 (new-id (ask H 'insert "--title" "h1")))
         ;; a library rule: its named keys are expanded by a fresh session, not by a reused one
         (q "(in-force ?x)")
         (main self)
         (held (spawn (lambda () (send main (list 'held (ask H 'outline))))))
         (pause (sleep-ms 300))
         (t1 (traced-ask H 'query q)) (t2 (traced-ask H 'query q))
         (early (receive (after 0 'still-held) (`(held ,a) 'answered-already)))
         (late (receive (after 20000 'no-answer) (`(held ,a) a)))
         (words (map car (session-events H))))
    (want "SESS-9 two connections: while one is held open by the conn-hold fault, the other's two queries are answered (the local route's answers) and the session is fresh once (expanding its named keys), reused after (expanding none); the held one is answered afterwards"
          (list early (equal? (answer-of t1) (local H 'query q)) (equal? (answer-of t2) (local H 'query q)) (equal? late (local H 'outline))
                (filter (lambda (w) (not (eq? w 'served))) words)
                (and (number? (expanded-of t1)) (> (expanded-of t1) 0)) (expanded-of t2))
          '(still-held #t #t #t (fresh reuse) #t 0))
    (stop-session-daemon! H)))

;; ---- in process: the provider over a state this fixture holds (SESS-4c, 5, 8b, 10, 11, 13, NOTE D1)
(define (in-process-rows)
  (if (not (late 'make-session-provider))
      (for-each (lambda (name) (want name 'absent 'present))
                '("SESS-5 in process" "SESS-8b in process" "SESS-10 in process" "SESS-13 in process" "SESS-11 in process" "SESS-4c in process" "SESS-14 in process"))
      (in-process-rows-present)))
(define (in-process-rows-present)
  (let* ((st (local-store! "p"))
         (p1 (new-id (on st 'insert "--title" "p1")))
         (p2 (new-id (on st 'insert "--title" "p2")))
         (p3 (new-id (on st 'insert "--title" "p3")))
         (l (list (on st 'link p1 "cites" p2) (on st 'derive "rch" (reach-value 'rch))))
         (s1 (open-and-reduce st))
         (box (vector s1))
         (P (in-process-provider box 1000000))
         (provide (car P)) (seen (cadr P)) (bump! (caddr P))
         (q (list 'rch p1 '?x)))
    ;; SESS-5: a view that is not the publication
    (let* ((S1 (provide st s1)) (a1 (session-answer S1 q)) (w1 (seen))
           (ins (on st 'insert "--title" "elsewhere")) (other (open-and-reduce st)) (S2 (provide st other)) (w2 (seen))
           (k '(kind ?b ?k))
           (s2-rows (session-rows-of S2 k)) (other-rows (session-rows-of (make-query-session other) k))
           (S3 (provide st s1)) (w3 (seen)) (s1-rows (session-rows-of S3 k)))
      (want "SESS-5 in process: a view that is not the publication (a later state) gets a fresh session over that view and leaves the kept one, which the next call over the publication reuses"
            (list w1 w2 w3 (eq? S3 S1) (eq? S2 S1) (equal? s2-rows other-rows) (equal? s2-rows s1-rows))
            '(((fresh none)) ((fresh cut)) ((reuse)) #t #f #t #f)))
    ;; SESS-8b: a refused query leaves no synthetic key; the next ask reuses
    (let* ((S (provide st s1)) (w (seen))
           (fg (list 'and (list 'kind '?b 'section) (list 'not (list 'edge '?b 'cites p3))))
           (answered (session-answer S fg)) (left0 (session-synthetic-count S))
           ;; a form no cut satisfies (kind's column is a symbol, 42 is not): never expanded
           (empty-answered (session-answer S '(and (kind ?b section) (not (kind ?b 42))))) (left-empty (session-synthetic-count S))
           (budget (session-budget-set! S 1))
           (refused (session-answer S (list 'and (list 'kind '?b 'section) (list 'not (list 'edge '?b 'cites p2)))))
           (left (session-synthetic-count S)) (back (session-budget-set! S 1000000))
           (S2 (provide st s1)) (w2 (seen)))
      (want "SESS-8b in process: a query with a form answered leaves no synthetic key, nor one with a form no cut satisfies (never expanded), a query refused by its budget leaves none either, and the next ask reuses the session"
            (list (and (pair? answered) (car answered)) left0 (and (pair? empty-answered) (car empty-answered)) left-empty
                  (and (pair? refused) (list (car refused) (cadr refused))) left w2 (eq? S2 S))
            '(ok 0 ok 0 (error query-budget) 0 ((reuse)) #t)))
    ;; SESS-10: an advance refused by its budget
    (let* ((S (provide st s1)) (w0 (seen)) (held (session-answer S q)) (small (session-budget-set! S 1))
           (w (on st 'link p2 "cites" p3)) (s2 (open-and-reduce st)) (set (vector-set! box 0 s2))
           (S2 (provide st s2)) (w2 (seen)))
      (want "SESS-10 in process: an advance the session's budget refuses gives a fresh session, kept, whose answer is a fresh session's"
            (list w2 (equal? (session-rows-of S2 q) (fresh-rows st q)))
            '(((fresh advance-refused query-budget)) #t)))
    ;; SESS-13: a record applied into the adopted object moves its cut
    (let* ((s3 (open-and-reduce st)) (box13 (vector s3)) (P13 (in-process-provider box13 1000000))
           (provide (car P13)) (seen (cadr P13))
           (S (provide st s3)) (w (seen))
           (applied (reduce-apply! s3 "zzzz0000" 1 '() (list 'put (list (cons 'kind 'section) (cons 'title "late") '(parent . root) '(ord . 9)))))
           (S2 (provide st s3)) (w2 (seen)))
      (want "SESS-13 in process (its own provider, the object adopted first): a record applied into the adopted object (its cut moved) drops the session, and a fresh one is kept, reading the applied record"
            (list w applied w2 (eq? S2 S)
                  (and (exists (lambda (r) (and (pair? r) (pair? (cdr r)) (equal? (cadr r) "late"))) (session-rows-of S2 '(title ?b ?t))) #t))
            '(((fresh none)) accepted ((dropped moved) (fresh none)) #f #t)))
    ;; SESS-11: nothing carriable
    (let* ((cst (local-store! "q"))
           (code (on cst 'batch (format "~s" (list (list 'insert 'root #f (list (cons 'kind 'code) (cons 'lang "javascript") (cons 'title "h use") (cons 'src "h()")))))))
           (c1 (open-and-reduce cst)) (cbox (vector c1)) (CP (in-process-provider cbox 1000000))
           (S (( car CP) cst c1)) (w0 ((cadr CP))) (held (session-answer S '(def-for ?b ?n ?d)))
           (w (on cst 'insert "--title" "more")) (c2 (open-and-reduce cst)) (set (vector-set! cbox 0 c2))
           (calls (session-advance-calls)) (S2 ((car CP) cst c2)) (w2 ((cadr CP)))
           (rows2 (session-rows-of S2 '(def-for ?b ?n ?d))) (S3 ((car CP) cst c2)) (w3 ((cadr CP))))
      (want "SESS-11 in process: a session holding only keys that reach uses-name, a write: a fresh session (nothing held) over the new state, kept (the next call reuses it), and session-advance was not called"
            (list w2 (eqv? calls (session-advance-calls)) (eq? S2 S) (equal? rows2 (fresh-rows cst '(def-for ?b ?n ?d))) w3 (eq? S3 S2))
            '(((fresh nothing-held)) #t #f #t ((reuse)) #t)))
    ;; SESS-4c: a rebuild that withdrew an applied event: not forward
    (let* ((fo "fw000000")
           (plan (lambda (r) (reduce-apply! r fo 1 '() (list 'plan "w" "fp" (cons fo 0) (list (cons 0 (list 'insert 'root #f (list (cons 'kind 'section) (cons 'title "w0"))))))
                                            (list "test" (cons fo "w") 'plan "fp" #f (cons fo 0)))))
           (member (lambda (r w seq deps) (reduce-apply! r w seq deps (list 'put (list (cons 'kind 'section) (cons 'title "w0") '(parent . root) '(ord . 1)))
                                                         (list "test" (cons fo "w") 0 "fp" (cons fo 1) (cons fo 0)))))
           (r-applied (let ((r (reduce-empty))) (plan r) (member r fo 2 '()) r))
           (r-withdrawn (let ((r (reduce-clone r-applied))) (member r "fw100000" 1 (list (cons fo 1))) r))
           (rbox (vector r-applied)) (RP (in-process-provider rbox 1000000))
           (S ((car RP) st r-applied)) (held (session-answer S '(unit ?t ?u))) (w0 ((cadr RP)))
           (set (vector-set! rbox 0 r-withdrawn)) (S2 ((car RP) st r-withdrawn)) (w2 ((cadr RP)))
           (rows2 (session-rows-of S2 '(unit ?t ?u))) (S3 ((car RP) st r-withdrawn)) (w3 ((cadr RP))))
      (want "SESS-4c in process: a rebuild that withdrew an event the kept session's state had applied: not forward, a fresh session over the rebuilt state, kept"
            (list w0 w2 (eq? S2 S) (equal? rows2 (session-rows-of (make-query-session r-withdrawn) '(unit ?t ?u))) w3 (eq? S3 S2))
            '(((fresh none)) ((fresh not-forward)) #f #t ((reuse)) #t)))
;; SESS-14: an advance that evaluates a demanded aggregate group leaves no
    ;; synthetic key in the session it returns: sg(y, s) sums y's field n, upsum
    ;; reads sg through an edge; a cites c added demands sg(c, ?) inside the advance
    (let* ((ast (local-store! "agg"))
           (ia (new-id (on ast 'insert "--title" "a"))) (ib (new-id (on ast 'insert "--title" "b"))) (ic (new-id (on ast 'insert "--title" "c")))
           (setup (list (on ast 'batch (format "~s" (list (list 'set ib 'n 5) (list 'set ic 'n 7))))
                        (on ast 'link ia "cites" ib)))
           (d1 (on ast 'derive "sg" (format "~s" '((head (sg ID INT)) (clause (sg ?y ?s) (kind ?y section) (sum ?s ?v (field ?y "n" ?v)))))))
           (d2 (on ast 'derive "upsum" (format "~s" '((head (upsum ID INT)) (clause (upsum ?x ?s) (edge ?x cites ?y) (sg ?y ?s))))))
           (st1 (open-and-reduce ast)) (S (make-query-session st1)) (held (session-answer S (list 'upsum ia '?s)))
           (w (on ast 'link ia "cites" ic)) (st2 (open-and-reduce ast))
           (S2 ((q-proc 'session-advance) S st2)))
      (want "SESS-14 in process: an advance that evaluates a demanded aggregate group (sg of c, a sum) leaves no synthetic key in the session it returns, and that session answers as a fresh one"
            (list (and (pair? held) (car held)) (and (not (symbol? S2)) (not (pair? S2))) (session-synthetic-count S2)
                  (equal? (session-rows-of S2 (list 'upsum ia '?s)) (fresh-rows ast (list 'upsum ia '?s))))
            '(ok #t 0 #t)))
    ;; NOTE D1: the budget divergence
    (let* ((s4 (open-and-reduce st)) (set (vector-set! box 0 s4)) (S (provide st s4)) (w (seen))
           (full (session-answer S q)) (cost (let ((F (make-query-session s4))) (session-answer F q) (session-spent F)))
           (half (max 1 (div cost 2)))
           (fresh-refused (let ((F (make-query-session s4 '() half))) (session-answer F q)))
           (kept (begin (session-budget-set! S half) (session-answer S q))))
      (printf "NOTE D1 a query costing ~a, at a budget of ~a: a fresh session answers ~s; the kept session (its keys already complete) answers ~s\n"
              cost half (and (pair? fresh-refused) (car fresh-refused)) (and (pair? kept) (car kept))))))

;; ---- a daemon over a small store, asked inside the scheduler (the socket
;; client is an actor's, as daemon-e1's rows are) --------------------------------------------
(define D #f)
(define a1 #f)
(define a2 #f)
(define a3 #f)
(start-scheduler
  (lambda ()
    (begin
      ;; ---- a daemon over a small store --------------------------------------------------------------
      (set! D (start-session-daemon! "a"))
      (set! a1 (new-id (ask D 'insert "--title" "alpha")))
      (set! a2 (new-id (ask D 'insert "--title" "beta")))
      (set! a3 (new-id (ask D 'insert "--title" "gamma")))
      (ask D 'link a1 "cites" a2)
      (ask D 'link a2 "depends-on" a3)

      (printf "== A: query and context in the connection ==\n")
      (want "A-1 a query through the socket is answered by the store process from the publication, opening no log (no replay), and equals the local route's answer"
            (let* ((before (routed-count D 'query 'store)) (opens (log-opens D))
                   (a (ask D 'query "(edge ?a ?r ?b)")))
              (list (- (routed-count D 'query 'store) before) (routed-count D 'query 'connection) (- (log-opens D) opens)
                    (and (pair? a) (car a)) (equal? a (local D 'query "(edge ?a ?r ?b)"))))
            '(1 0 0 ok #t))
      (want "A-2 a context request through the socket is answered by the store process from the publication, opening no log, and equals the local route's answer"
            (let* ((before (routed-count D 'context 'store)) (opens (log-opens D))
                   (a (ask D 'context "--for" a1 "--budget" "4000")))
              (list (- (routed-count D 'context 'store) before) (routed-count D 'context 'connection) (- (log-opens D) opens)
                    (and (pair? a) (symbol? (car a))) (equal? a (local D 'context "--for" a1 "--budget" "4000"))))
            '(1 0 0 #t #t))

      (want "A-3 the census: query.sc and context.sc name no store-reaching procedure (and nothing with replay in its name), and both name the two allowed paths"
            (map (lambda (file)
                   (let ((syms (symbols-of (string-append "../" file))))
                     (list file
                           (filter (lambda (s) (or (memq s store-reaching) (contains-text? s "replay"))) syms)
                           (and (memq 'reduction-for syms) #t) (and (memq 'keyword-hook syms) #t))))
                 '("query.sc" "context.sc"))
            '(("query.sc" () #t #t) ("context.sc" () #t #t)))

      (want "A-4 a write through the daemon, then a query through the socket: the new block is there, and the answer equals the local route's"
            (let* ((a4 (new-id (ask D 'insert "--title" "delta")))
                   (q (format "(kind ~s ?k)" a4))
                   (a (ask D 'query q)))
              (list (and (pair? a) (car a))
                    (and (pair? a) (pair? (cdr a)) (pair? (cadr a)) (length (filter (lambda (i) (and (pair? i) (eq? (car i) 'row))) (cdr (cadr a)))))
                    (equal? a (local D 'query q))))
            '(ok 1 #t))

      ;; ---- search answers from the state the dispatcher hands it ----
      ;; A connection's search is handed the publication, as read is. Before
      ;; this change the verb ignored it and opened and reduced the store on
      ;; every search, which the trace shows as one log-open per search.
      (want "A-6 two searches through the socket in a row: both routed to the connection, neither opens the log, the two answers are equal, and each equals the local route's answer"
            (let* ((c (routed-count D 'search 'connection)) (opens (log-opens D))
                   (s1 (ask D 'search "alpha")) (s2 (ask D 'search "alpha")))
              (list (- (routed-count D 'search 'connection) c) (- (log-opens D) opens)
                    (hit-ids s1) (equal? s1 s2) (equal? s1 (local D 'search "alpha"))))
            (list 2 0 (list a1) #t #t))
      ;; THE BYTES, NOT THE DATUM: the socket's text of a populated, an empty
      ;; and a capped search (more hits than search-hit-limit) equals the text
      ;; of the local route's answer, which folds the store as the verb did
      ;; before this change. The capped one shows the cap's ten hits and says
      ;; how many it left out; --all shows every one.
      (let ((ks (let loop ((i 0) (out '()))
                  (if (= i 14) (reverse out)
                      (loop (+ i 1) (cons (new-id (ask D 'insert "--title" (format "kappa ~a" i))) out))))))
        (want "A-7 the socket's --wire text of a search equals the local route's text: populated, empty, capped, and capped with --all"
              (map (lambda (q) (let ((s (apply ask-text D 'search q)))
                                 (list (string? s) (equal? s (apply local-text D 'search q)))))
                   '(("alpha") ("nosuchwordanywhere") ("kappa") ("kappa" "--all")))
              '((#t #t) (#t #t) (#t #t) (#t #t)))
        (want "A-7 and the capped search shows ten hits and (truncated (hits 4)), the --all one fourteen, and the empty one none"
              (let ((capped (ask D 'search "kappa")) (all (ask D 'search "kappa" "--all")) (none (ask D 'search "nosuchwordanywhere")))
                (list (length (hit-ids capped)) (clause-of capped 'truncated)
                      (length (hit-ids all)) (equal? (list-sort string<? (hit-ids all)) (list-sort string<? ks))
                      (hit-ids none)))
              '(10 ((hits 4)) 14 #t ())))

      (let ((before (published-count D)))
        (outside-inserts! (daemon-store D) 300 pid-text)
        (ask D 'outline)
        (let ((pub (wait-for-more-publications D before 60000)))
          (printf "NOTE A-5 after 300 outside commits (~a): query median ~a ms, outline median ~a ms over 5 socket asks each\n"
                  pub (median-ms (lambda () (ask D 'query "(edge ?a ?r ?b)")) 5) (median-ms (lambda () (ask D 'outline)) 5))))

      (stop-session-daemon! D)
      ;; ---- search and read agree on the publication ----
      ;; A CONNECTION ANSWERS THE PUBLICATION AS IT STANDS while a reload the
      ;; probe asked for is still queued (daemon.sc, conn-local-read-verbs),
      ;; and search is one of those reads. The reload is held between its fold
      ;; and its publication (reload-before-publish) after an outside write
      ;; renames X and adds a second hit: while it is held, search and read
      ;; answer the same state, the one before the write; after it publishes,
      ;; the one after. Before this change the held search folded the disk and
      ;; answered the write that read did not show yet.
      (let* ((rel (string-append scratch-base "/dss-" pid-text "-h.release"))
             (H (begin (system (string-append "rm -f " rel " " rel ".held"))
                       (start-session-daemon! "h" #f #f (string-append "THEOURGIA_HOLD=reload-before-publish:" rel " "))))
             (X (new-id (ask H 'insert "--title" "omega")))
             (warm (list (ask H 'search "omega") (ask H 'read X)))
             (before (published-count H))
             (out (outside-requests! (daemon-store H)
                                     (list (list 'set X "title" "omega moved") (list 'insert "--title" "omega two"))
                                     pid-text))
             (probe (ask H 'read X))
             (held (wait-for-file (string-append rel ".held") 20000))
             (s (ask H 'search "omega")) (r (ask H 'read X))
             (released (begin (call-with-output-file rel (lambda (p) (display "go" p)) 'truncate)
                              (wait-for-more-publications H before 20000)))
             (s2 (ask H 'search "omega")) (r2 (ask H 'read X)))
        (stop-session-daemon! H)
        (want "A-8 while the reload is held, search answers the publication read answers: X's version is read's, the new block is not a hit; after it publishes, both answer the write, as the local route does"
              (list held
                    (hit-ids s)
                    (equal? (clause-of s 'versions) (clause-of r 'versions))
                    released
                    (length (hit-ids s2))
                    (equal? (assoc X (car (clause-of s2 'versions))) (assoc X (car (clause-of r2 'versions))))
                    (equal? s2 (local H 'search "omega")))
              (list #t (list X) #t 'published 2 #t #t)))

      (printf "== B: the session the daemon keeps ==\n")
      (kept-session-rows)
      (working-session-rows)
      (code-rows)
      (bound-rows)
      (two-client-rows)
      (in-process-rows)
      (printf "rows: ~a\n~a failures\ndaemon-session complete\n" rows bad)
      (exit (if (zero? bad) 0 1)))))
