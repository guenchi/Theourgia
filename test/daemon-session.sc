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
        (only (theourgia reduce) block-id))

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
(define (start-session-daemon! suffix)
  (let* ((tag (string-append pid-text "-" suffix))
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
                (string-append "(serve \"" st "\" \"" sk "\")"))))
      'truncate)
    (system (string-append "THEOURGIA_TRACE=1 THEOURGIA_INJECT=on "
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
(define (ask-again d verb args n)
  (let ((a (ask-once d verb args)))
    (if (and (> n 0) (pair? a) (eq? (car a) 'error) (pair? (cdr a)) (eq? (cadr a) 'store-busy))
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
;; the same request on the local route
(define (local d verb . args) (rpc-dispatch (daemon-store d) (cons verb args) "tester"))
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

      (let ((before (published-count D)))
        (outside-inserts! (daemon-store D) 300 pid-text)
        (ask D 'outline)
        (let ((pub (wait-for-more-publications D before 60000)))
          (printf "NOTE A-5 after 300 outside commits (~a): query median ~a ms, outline median ~a ms over 5 socket asks each\n"
                  pub (median-ms (lambda () (ask D 'query "(edge ?a ?r ?b)")) 5) (median-ms (lambda () (ask D 'outline)) 5))))

      (stop-session-daemon! D)
      (printf "rows: ~a\n~a failures\ndaemon-session complete\n" rows bad)
      (exit (if (zero? bad) 0 1)))))
