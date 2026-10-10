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

;; THE INCOMPLETE CLAUSE ON THE ROUTES THAT DO NOT OPEN THEIR OWN LOAD.
;;
;; An answer built while a writer cannot be read ends in
;; (incomplete (unreadable (writer w) (path p) (reason r)) ...) (K10). Most
;; answers hear it from the load they open. These rows are the answers that
;; do not: a read the daemon serves from the reduction a write published,
;; which opens no load of its own; and the eval worker's refusals that come
;; after its load -- a source with two forms, a cut that cannot be used --
;; where the reduction it would have answered from never comes back.
;;
;; Each row sets a mirror writer's directory to 000 and restores it before
;; anything is compared.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch rpc-ok?)
        (only (theourgia store) store-publish-hook!)
        (only (theourgia log) log-publish! segment-sha writer-directory)
        (only (theourgia trace) trace-enable!)
        (only (theourgia wire) encode-record storable-encode)
        ;; F100b M1's rows (at the end of this file).
        (only (theourgia ffi) EACCES EIO)
        (only (theourgia client) call! envelope-version socket-path serve-log-path)
        (only (theourgia render) render-wire)
        (only (theourgia store) store-evidence open-and-reduce)
        (only (theourgia request) actor-sub ev-actor ev-payload)
        (only (theourgia reduce) reduce-applied-cut block-id))

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
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) x)))))

;; THE SCRATCH ROOT FOLLOWS THEOURGIA_TEST_ROOT, and a directory already
;; there is refused rather than reused (F71).
(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                   (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/unreadable-routes-" (number->string (get-process-id))))
(when (file-exists? root)
  (assertion-violation 'unreadable-routes "scratch directory already exists" root))
(system (string-append "mkdir -p " root))

(define M "mirrorz9")
(define n 0)
(define (fresh-store!)
  (set! n (+ n 1))
  (let ((d (string-append root "/s" (number->string n))))
    (system (string-append "mkdir -p " d "/store " d "/home"))
    (putenv "THEOURGIA_HOME" (string-append d "/home"))
    (let ((store (string-append d "/store")))
      (rpc-dispatch store '(init) "test")
      (rpc-dispatch store '(insert "--under" "root" "--title" "A") "test")
      (let ((bytes (encode-record 1 1757300000001 "a" '() '(set "t" x "x"))))
        (log-publish! store M 1 bytes (segment-sha bytes)))
      store)))
(define (chmod! mode path) (system (string-append "chmod " mode " " path)))

;; The paths an answer's incomplete clause names, or no-clause.
(define (incomplete-dirs answer)
  (let ((c (and (pair? answer) (list? answer)
                (find (lambda (x) (and (pair? x) (eq? (car x) 'incomplete))) (cdr answer)))))
    (if (not c)
        'no-clause
        (map (lambda (u) (let ((p (and (pair? u) (assq 'path (cdr u))))) (and p (cadr p))))
             (cdr c)))))

;; The trace lines a thunk prints, counted by op, for the ops named.
(define (count-trace ops thunk)
  (let-values (((out get) (open-string-output-port)))
    (let ((value (parameterize ((current-error-port out))
                   (trace-enable! #t)
                   (let ((v (caught (thunk)))) (trace-enable! #f) v))))
      (let ((p (open-string-input-port (get))))
        (let loop ((acc (map (lambda (o) (cons o 0)) ops)))
          (let ((x (guard (e (#t 'skip)) (read p))))
            (cond
              ((eof-object? x) (list value acc))
              ((and (pair? x) (eq? (car x) 'trace) (pair? (cdr x)) (assq (cadr x) acc))
               => (lambda (e) (set-cdr! e (+ (cdr e) 1)) (loop acc)))
              (else (loop acc)))))))))

(printf "== a read served from the reduction a write published ==\n")
;; The daemon installs a publication hook and answers later reads from the
;; state it was handed (daemon.sc); these rows install the same hook in
;; process and pass that state to rpc-dispatch as the daemon does.
(define published #f)
(store-publish-hook! (lambda (state) (set! published state)))
(let ((s (fresh-store!)))
  (set! published #f)
  (rpc-dispatch s '(insert "--under" "root" "--title" "B") "test")
  (let ((r (count-trace '(log-open enter-critical)
                        (lambda () (rpc-dispatch s '(outline) "test" published #f #f #f)))))
    (want "CONTROL on a healthy store a write publishes a state, and a read from it carries no clause"
          (list (and published #t) (car (car r)) (incomplete-dirs (car r)))
          '(#t ok no-clause))))
(let* ((s (fresh-store!))
       (dir (writer-directory s M)))
  (set! published #f)
  (chmod! "000" dir)
  (let* ((w (caught (rpc-dispatch s '(insert "--under" "root" "--title" "B") "test")))
         (r (count-trace '(log-open enter-critical)
                         (lambda () (rpc-dispatch s '(outline) "test" published #f #f #f)))))
    (chmod! "700" dir)
    (want "CONTROL the write beside the unreadable mirror succeeded and published a state"
          (list (and (pair? w) (car w)) (and published #t))
          '(ok #t))
    (want "CONTROL the read served from that state opens no load of its own"
          (cadr r)
          '((log-open . 0) (enter-critical . 0)))
    (want "a read served from the state a write published beside an unreadable mirror names the mirror as incomplete"
          (list (and (pair? (car r)) (car (car r))) (incomplete-dirs (car r)))
          (list 'ok (list dir)))))
(store-publish-hook! (lambda (state) (if #f #f)))

(printf "== the eval worker's refusals after its load ==\n")
;; core.sc eval runs through eval-supervise and a worker; driven as
;; test/eval-local.sc drives it, stdout read as the answer, stderr apart.
(define (eval-answer store . args)
  (let ((out (string-append root "/eval.out"))
        (err (string-append root "/eval.err")))
    (system (string-append "THEOURGIA_LOCAL=1 scheme --script ../core.sc eval "
                           (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
                           "--store " store " --wire > " out " 2> " err))
    (let ((d (guard (e (#t (eof-object))) (call-with-port (open-input-file out) read))))
      (if (eof-object? d)
          (list 'no-answer (let ((t (call-with-port (open-input-file err) get-string-all)))
                             (if (eof-object? t) "" (substring t (max 0 (- (string-length t) 200)) (string-length t)))))
          d))))
(define (head-of a) (if (and (pair? a) (pair? (cdr a))) (list (car a) (cadr a)) a))
(let* ((s (fresh-store!))
       (dir (writer-directory s M)))
  (want "CONTROL on a healthy store a two-form source is refused with no clause"
        (let ((a (eval-answer s "1 2"))) (list (head-of a) (incomplete-dirs a)))
        '((error bad-source) no-clause))
  (chmod! "000" dir)
  (let ((two-forms (eval-answer s "1 2"))
        (bad-cut (eval-answer s "--cut" "((\"zzzzzzzz\" . 3))" "(+ 1 2)")))
    (chmod! "700" dir)
    (want "a two-form source refused after the worker's load names the unreadable mirror"
          (list (head-of two-forms) (incomplete-dirs two-forms))
          (list '(error bad-source) (list dir)))
    (want "a cut refused as unusable after the worker's load names the unreadable mirror"
          (list (head-of bad-cut) (incomplete-dirs bad-cut))
          (list '(error eval-context) (list dir)))))

;; AN ANSWER THE SUPERVISOR CANNOT READ BACK STILL CARRIES THE CLAUSE (review
;; r4, A1-2). A raised pair headed `error` whose tail is a procedure is
;; passed through as the worker's answer, and no reader accepts what the
;; worker then writes; the supervisor answers eval-worker-exit, as on
;; a338bcd. The clause is the supervisor's, from the note the worker said
;; at its load, so it is appended to that answer too.
;;
;; Since F93 a raised value is carried as data and never written as the
;; answer's shape, so this source answers (error eval-exception (kind
;; raised) (reason unwritable-value) (type procedure)) from the worker, and
;; the clause is appended to it; on 86db404 both rows read eval-worker-exit.
(define unwritable-source "(raise (cons (string->symbol \"error\") (lambda () #f)))")
(let* ((s (fresh-store!))
       (dir (writer-directory s M)))
  (want "CONTROL on a healthy store a raised pair holding a procedure answers eval-exception with no clause (F93)"
        (let ((a (eval-answer s unwritable-source))) (list (head-of a) (incomplete-dirs a)))
        '((error eval-exception) no-clause))
  (chmod! "000" dir)
  (let ((a (eval-answer s unwritable-source)))
    (chmod! "700" dir)
    (want "the same source after the worker's load met the unreadable mirror answers eval-exception and names the mirror (F93)"
          (list (head-of a) (incomplete-dirs a))
          (list '(error eval-exception) (list dir)))))

;; ---- F79: writers/ itself unlistable, on the two routes that load at start --
(printf "== F79: a writers/ directory that cannot be listed, at eval and at a daemon's start ==\n")
;; Both routes load the store before anything can be heard, so the failure
;; reaches their catch-alls: eval-worker's answered (error eval-exception
;; ...), the daemon's start (error store-load-failed (reason "failed for
;; ~a: ~(~a~)")). Each now names the entry and the reason, as K1's routes do.
(define (writers-of store) (string-append store "/writers"))
(define (names-writers? a store)
  (and (pair? a) (eq? (car a) 'error) (pair? (cdr a)) (eq? (cadr a) 'unreadable)
       (let ((p (assq 'path (cddr a))))
         (and p (equal? (cadr p) (writers-of store))))))
(let ((s (fresh-store!)))
  (let ((healthy (eval-answer s "(+ 1 2)")))
    (chmod! "000" (writers-of s))
    (let ((a (eval-answer s "(+ 1 2)")))
      (chmod! "700" (writers-of s))
      (want "CONTROL F79-2 on the same store, readable, eval answers ok"
            (and (pair? healthy) (car healthy))
            'ok)
      (want "F79-2 eval on a store whose writers/ cannot be listed answers unreadable, naming writers/"
            (list (head-of a) (names-writers? a s))
            (list '(error unreadable) #t)))))
(define sock-base
  (let ((v (getenv "THEOURGIA_TEST_SOCK")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
;; THE LOCK IS PRE-CREATED (F100b brief G), so main records nothing and the
;; store's failure is answered by the table alone: the exact datum, with its
;; errno and the attempt clause of a start made by hand.
(let* ((s (fresh-store!))
       (sock (string-append sock-base "/ur-" (number->string (get-process-id)) ".sock"))
       (lock (string-append sock-base "/.ur-" (number->string (get-process-id)) ".sock.lock"))
       (out (string-append root "/start.out")))
  (system (string-append "touch " lock))
  (chmod! "000" (writers-of s))
  (let* ((rc (system (string-append "perl -e 'alarm 60; exec @ARGV' scheme --script ../theourgiad.sc serve "
                                    s " --socket " sock " > " out " 2> /dev/null < /dev/null")))
         (said (guard (e (#t '()))
                 (call-with-port (open-input-file out)
                   (lambda (p) (let loop ((acc '()))
                                 (let ((x (read p))) (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))))
    (chmod! "700" (writers-of s))
    (system (string-append "rm -f " lock))
    (want "F79-3 a daemon started on a store whose writers/ cannot be listed names writers/ and leaves as a failed start does: exit 75, store-actor-down, no socket"
          (list (and (pair? said) (car said))
                (and (pair? said) (pair? (cdr said)) (cadr said))
                rc
                (file-exists? sock))
          (list (list 'error 'unreadable (list 'path (writers-of s)) '(reason "Permission denied") '(errno EACCES) '(attempt #f))
                '(exiting (reason store-actor-down)) 75 #f))))

;; ---- F100b M1: the answers at the synchronous points ---------------------------
;;
;; Brief v7 rows P1-a, P1-b, P1-c, P2b, P3-a, P4, NO1, NO2, NO3 and their F
;; rows. Every run is a program in a subprocess -- the client, core.sc, or a
;; raw frame to a daemon -- its stdout read as one datum and its exit code
;; kept. A daemon a row starts is stopped by its pid, found as the one line of
;; `ps -o pid=,command=` holding the exact string "theourgiad.sc serve
;; <store>" (ruling Q6); a count other than one fails the row that asked.
(define m1-run-root (string-append sock-base "/m1r-" (number->string (get-process-id))))
(system (string-append "mkdir -p " m1-run-root))
(putenv "THEOURGIA_RUN" (string-append m1-run-root "/run"))
(define m1-n 0)
;; Every store this section makes, so a daemon started on one can be found
;; again by its exact argv line (below, on a raise and at the end).
(define m1-stores '())
(define (m1-store!)
  (set! m1-n (+ m1-n 1))
  (let ((d (string-append root "/m1-" (number->string m1-n))))
    (system (string-append "mkdir -p " d "/parent/store " d "/home"))
    (putenv "THEOURGIA_HOME" (string-append d "/home"))
    (let ((store (string-append d "/parent/store")))
      (set! m1-stores (cons store m1-stores))
      (rpc-dispatch store '(init) "test")
      store)))
(define (parent-of store) (substring store 0 (- (string-length store) (string-length "/store"))))
(define denied "Permission denied")
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
;; -> (rc datum)
(define (m1-run env program args)
  (let ((out (string-append root "/m1.out"))
        (err (string-append root "/m1.err")))
    (let ((rc (system (string-append env " perl -e 'alarm 60; exec @ARGV' scheme --script " program " "
                                     (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                                     "> " out " 2> " err " < /dev/null"))))
      (list rc (let ((d (guard (e (#t (eof-object))) (call-with-input-file out read))))
                 (if (eof-object? d)
                     (list 'no-answer (let ((t (call-with-input-file err get-string-all)))
                                        (if (eof-object? t) "" (substring t (max 0 (- (string-length t) 300)) (string-length t)))))
                     d))))))
(define (lines-of-command cmd)
  (let* ((p (process cmd)) (t (get-string-all (car p))))
    (close-port (car p)) (close-port (cadr p))
    (let loop ((cs (if (eof-object? t) '() (string->list t))) (cur '()) (acc '()))
      (cond ((null? cs) (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
            ((char=? (car cs) #\newline) (loop (cdr cs) '() (cons (list->string (reverse cur)) acc)))
            (else (loop (cdr cs) (cons (car cs) cur) acc))))))
(define (has-substring? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (daemon-pids store)
  (let ((needle (string-append "theourgiad.sc serve " store)))
    (map (lambda (l) (string->number (car (filter (lambda (w) (> (string-length w) 0))
                                                   (let split ((cs (string->list l)) (cur '()) (acc '()))
                                                     (cond ((null? cs) (reverse (cons (list->string (reverse cur)) acc)))
                                                           ((char=? (car cs) #\space) (split (cdr cs) '() (cons (list->string (reverse cur)) acc)))
                                                           (else (split (cdr cs) (cons (car cs) cur) acc))))))))
         (filter (lambda (l) (has-substring? l needle)) (lines-of-command "ps -ax -o pid= -o command=")))))
(define daemon-counts '())
(define (stop-daemon! label store)
  (set! daemon-counts (append daemon-counts (list (cons label (length (daemon-pids store))))))
  (for-each (lambda (pid) (system (string-append "kill -TERM " (number->string pid) " 2>/dev/null")))
            (daemon-pids store))
  (let loop ((k 0))
    (when (and (< k 50) (pair? (daemon-pids store)))
      (system "sleep 0.1") (loop (+ k 1))))
  ;; A DAEMON THAT OUTLIVES TERM IS NOT LEFT (F100b M1 review r3, F1): after
  ;; five seconds whatever still answers to the exact argv line is sent
  ;; KILL, and the wait is repeated.
  (for-each (lambda (pid) (system (string-append "kill -KILL " (number->string pid) " 2>/dev/null")))
            (daemon-pids store))
  (let loop ((k 0))
    (when (and (< k 20) (pair? (daemon-pids store)))
      (system "sleep 0.1") (loop (+ k 1)))))
;; A ROW THAT RAISES MUST NOT LEAVE ITS DAEMON. The daemons here detach,
;; so one outlives this script: in M1's first run P1-b raised before its
;; stop, and its daemon went on serving for 39 minutes after the scratch
;; root was removed. An uncaught raise now stops the daemon of every store
;; this section made, then does what it did before.
(let ((previous (base-exception-handler)))
  (base-exception-handler
    (lambda (c)
      (for-each (lambda (s) (stop-daemon! 'on-raise s)) m1-stores)
      (previous c))))
(define m1-seg (string-append root "/m1-seg1"))
;; An answer's clause of the given head, or #f; an answer's second element
;; is its kind, a symbol, so the clauses are searched and not assq'd.
(define (clause-of answer head)
  (and (pair? answer) (list? answer)
       (find (lambda (c) (and (pair? c) (eq? (car c) head))) (cdr answer))))
(let ((rec1 (encode-record 1 1757300000001 "a" '() '(set "t" x "x"))))
  (call-with-port (open-file-output-port m1-seg (file-options no-fail)) (lambda (o) (put-bytevector o rec1))))

(printf "== F100b M1: the translation points ==\n")
;; P1-a (rpc-dispatch, durable, empty record; PR-01).
(let* ((s (m1-store!)) (w (string-append s "/writers")))
  (chmod! "500" w)
  (let ((r (m1-run "" "../theourgia.sc" (list "publish" M "1" m1-seg "--store" s))))
    (chmod! "700" w)
    (stop-daemon! "P1-a" s)
    (want "P1-a a publish whose writer directory cannot be made answers unwritable (op mkdir) with errno 13, rc 1"
          r (list 1 (list 'error 'unwritable '(op mkdir) (list 'path (string-append w "/" M))
                          (list 'reason denied) (list 'errno EACCES))))))

;; P1-b (dir-fsync with entries; PR-02). The four entries share one
;; temporary name, `<W>/publish.tmp-<n>-<n>`.
(define (digits? t) (and (> (string-length t) 0) (for-all char-numeric? (string->list t))))
(define (publish-tmp? t wd)
  (let ((prefix (string-append wd "/publish.tmp-")))
    (and (string? t) (> (string-length t) (string-length prefix))
         (string=? (substring t 0 (string-length prefix)) prefix)
         (let* ((rest (substring t (string-length prefix) (string-length t)))
                (dash (let loop ((i 0)) (cond ((= i (string-length rest)) #f) ((char=? (string-ref rest i) #\-) i) (else (loop (+ i 1)))))))
           (and dash (digits? (substring rest 0 dash)) (digits? (substring rest (+ dash 1) (string-length rest))))))))
(let* ((s (m1-store!)) (wd (string-append s "/writers/" M)))
  (system (string-append "mkdir -p " wd))
  (let* ((r (m1-run (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=fsync-fail@publish:dir=" M)
                    "../theourgia.sc" (list "publish" M "1" m1-seg "--store" s)))
         (a (cadr r))
         (written (clause-of a 'written))
         (tmp (and written (pair? (cadr written)) (pair? (car (cadr written))) (cadr (car (cadr written))))))
    (stop-daemon! "P1-b" s)
    (want "P1-b a publish whose directory flush fails after the link answers incomplete with the four entries, one temporary name"
          (list (car r) (publish-tmp? tmp wd)
                (if (and tmp (equal? a (list 'error 'incomplete
                                         (list 'failed '(op dir-fsync) (list 'path wd) '(reason "Input/output error") (list 'errno EIO))
                                         (list 'written (list (list 'create tmp) (list 'write tmp)
                                                              (list 'link tmp (string-append wd "/000001.sexp"))
                                                              (list 'unlink tmp))))))
                    #t
                    a))
          '(1 #t #t))))

;; P1-c (parity, errno clause; PR-03).
(let* ((s (m1-store!)) (k (string-append m1-run-root "/k1")) (sock (string-append k "/s")))
  (system (string-append "mkdir -p " k))
  (m1-run "" "../theourgia.sc" (list "read" "root" "--store" s "--socket" sock))
  (chmod! "000" (parent-of s))
  (let ((r (m1-run "" "../theourgia.sc" (list "read" "root" "--store" s "--socket" sock))))
    (chmod! "755" (parent-of s))
    (stop-daemon! "P1-c" s)
    (want "P1-c a read the daemon answers while the store's parent is 000 names meta.sexp, the reason and the errno, rc 1"
          r (list 1 (list 'error 'unreadable (list 'path (string-append s "/meta.sexp")) (list 'reason denied) '(errno EACCES))))))

;; P2b (dispatch-frame's store check; PR-06): a raw frame naming the store as
;; <parent>/./store, as test/client-socket.sc sends one.
(define (raw-frame sock spelling verb . args)
  (call! sock (string->utf8 (render-wire (append (list 'request (envelope-version) spelling "test" #f 'wire (current-directory) #f verb) args)))
         5000))
(define (origin-and-datum outcome)
  (if (and (pair? outcome) (eq? (car outcome) 'answer))
      (let* ((env (read (open-string-input-port (utf8->string (cadr outcome)))))
             (field (lambda (k) (cadr (assq k (cdr env))))))
        (list (field 'origin) (read (open-string-input-port (field 'stdout)))))
      (list 'outcome outcome)))
(let* ((s (m1-store!)) (k (string-append m1-run-root "/k2")) (sock (string-append k "/s"))
       (dot (string-append (parent-of s) "/./store")))
  (system (string-append "mkdir -p " k))
  (m1-run "" "../theourgia.sc" (list "read" "root" "--store" s "--socket" sock))
  (let ((control (origin-and-datum (raw-frame sock dot 'read "root")))
        ;; device-inode's absence (F100b M1 review r1, F9): a spelling that
        ;; names no entry is another store, as on the base -- ENOTDIR
        ;; (beneath a regular file) and ENOENT each asked once.
        (enotdir (string-append s "/meta.sexp/store"))
        (enoent (string-append (parent-of s) "/nothere/store")))
    (want "P2b-absent a spelling beneath a regular file (ENOTDIR) is another store: transport-store-mismatch, as on the base"
          (origin-and-datum (raw-frame sock enotdir 'read "root"))
          (list 'transport (list 'error 'transport-store-mismatch (list 'serving s) (list 'asked enotdir))))
    (want "P2b-absent a spelling that names nothing (ENOENT) is another store: transport-store-mismatch, as on the base"
          (origin-and-datum (raw-frame sock enoent 'read "root"))
          (list 'transport (list 'error 'transport-store-mismatch (list 'serving s) (list 'asked enoent))))
    (chmod! "000" (parent-of s))
    (let ((r (origin-and-datum (raw-frame sock dot 'read "root"))))
      (chmod! "755" (parent-of s))
      (stop-daemon! "P2b" s)
      ;; F (F110, point 2b): this row compares the WHOLE answer on the success
      ;; input, so it is also the route's F row: no record clause (ruling Q-M3b-1).
      (want "F / CONTROL P2b the dot spelling, the parent readable, is the same store: the core answers"
            control '(core (error unknown-id "root" (nearest ()))))
      (want "P2b the dot spelling under a 000 parent is refused by the transport with the table's unreadable"
            r (list 'transport (list 'error 'unreadable (list 'path dot) (list 'reason denied) '(errno EACCES)))))))

;; P3-a (core.sc main, the socket derivation; PR-07) and P4 (the thin client;
;; PR-10): the store's parent at 000, no --socket.
(let* ((s (m1-store!))
       (locked (chmod! "000" (parent-of s))))
  (let ((core (m1-run "" "../core.sc" (list "read" "root" "--store" s)))
        (thin (m1-run "" "../theourgia.sc" (list "read" "root" "--store" s))))
    (chmod! "755" (parent-of s))
    ;; NO DAEMON IS EXPECTED HERE, and none may be left if one started
    ;; anyway -- a chmod that failed or did not block would let the thin
    ;; client start one (F100b M1 review r2, F5). Stopped without being
    ;; counted; the final row reads that none remains.
    (for-each (lambda (pid) (system (string-append "kill -TERM " (number->string pid) " 2>/dev/null")))
              (daemon-pids s))
    (want "CONTROL P3-a/P4 the chmod 000 of the store's parent succeeded"
          locked 0)
    (let ((answer (list 'error 'unreadable (list 'path s) (list 'reason denied) '(errno EACCES))))
      (want "P3-a core.sc read with the store's parent at 000 answers the table's unreadable on stdout, exit 1"
            core (list 1 answer))
      (want "P4 the thin client read with the store's parent at 000 answers the same on stdout, exit 75"
            thin (list 75 answer)))))

;; NO1 (working.sc `problem`; PR-15 (1)): restore of a revoked version with
;; the working area's flush failing. The setup is revoke-restore.sc's
;; RR-00..RR-01: insert A, write it, commit, a rival plan.
(define (m1-call store . req) (rpc-dispatch store req "test"))
(define (m1-writer store) (cadr (assq 'local-writer (cdr (m1-call store 'check)))))
(define (m1-insert-a store)
  (let* ((a (m1-call store 'insert "--under" "root" "--title" "A" "--text" "old"))
         (ev (car (cadr (assq 'events (cdr a))))))
    (block-id (car ev) (cdr ev))))
(define (m1-working-version store a writer)
  (list-ref (assq 'projection (cdr (m1-call store 'read a "--working-info" "--writer" writer))) 4))
(define (m1-writer-dir store writer)
  (let ((d (find (lambda (n) (let ((k (string-length writer)))
                               (and (>= (string-length n) k) (string=? (substring n 0 k) writer))))
                 (directory-list (string-append store "/writers")))))
    (and d (string-append store "/writers/" d))))
(define (dot-tmp? t wd)
  (and (string? t) (> (string-length t) (string-length wd))
       (string=? (substring t 0 (string-length wd)) wd)
       (let* ((marker ".tmp-")
              (i (let loop ((i (- (string-length t) (string-length marker))))
                   (cond ((< i 0) #f) ((string=? (substring t i (+ i (string-length marker))) marker) i) (else (loop (- i 1)))))))
         (and i (let* ((rest (substring t (+ i (string-length marker)) (string-length t)))
                       (dash (let loop ((j 0)) (cond ((= j (string-length rest)) #f) ((char=? (string-ref rest j) #\-) j) (else (loop (+ j 1)))))))
                  (and dash (digits? (substring rest 0 dash)) (digits? (substring rest (+ dash 1) (string-length rest)))))))))
(let* ((s (m1-store!))
       (a (m1-insert-a s))
       (writer (m1-writer s)))
  (m1-call s 'write a "the text I wrote" "--writer" writer)
  (let* ((v1 (m1-working-version s a writer))
         (cursor (string-append writer ":" (number->string (cdr (assoc writer (reduce-applied-cut (open-and-reduce s)))))))
         (committed (m1-call s 'commit a "--req" "R1" "--cursor" cursor "--working-version" v1 "--writer" writer))
         (evidence (store-evidence s (cons writer "R1")))
         (plan-ev (find (lambda (e) (eq? 'plan (actor-sub (ev-actor e)))) evidence))
         (rival (encode-record 1 1789000000001 (ev-actor plan-ev) '() (storable-encode (ev-payload plan-ev)))))
    (log-publish! s "rivalzzz" 1 rival (segment-sha rival))
    (let* ((wd (m1-writer-dir s writer))
           (r (m1-run "THEOURGIA_LOCAL=1 THEOURGIA_INJECT=on THEOURGIA_FAULT=fsync-fail@working:file=.tmp-"
                      "../core.sc" (list "restore" v1 "--writer" writer "--store" s)))
           (ans (cadr r))
           (written (clause-of ans 'written))
           (tmp (and written (pair? (cadr written)) (pair? (car (cadr written))) (cadr (car (cadr written))))))
      (want "NO1 a restore whose working flush fails answers working-unavailable carrying its create and write of one temporary"
            (list (car r) (dot-tmp? tmp wd)
                  (or (equal? ans (list 'error 'working-unavailable '(message "Working storage failed")
                                        (list 'written (list (list 'create tmp) (list 'write tmp)))))
                      ans))
            '(1 #t #t)))))

;; NO2 (replay-barrier-failed; PR-15 (2)): the F-row -- byte-identical to the
;; base, replay creating nothing before its barrier.
(let* ((s (m1-store!)) (writer (m1-writer s)))
  (m1-run "THEOURGIA_LOCAL=1" "../core.sc"
          (list "insert" "--under" "root" "--title" "One" "--req" "r-1" "--cursor" (string-append writer ":0") "--store" s))
  (let ((r (m1-run "THEOURGIA_LOCAL=1 THEOURGIA_INJECT=on THEOURGIA_FAULT=fsync-fail@commit:file=000001.sexp" "../core.sc"
                   (list "insert" "--under" "root" "--title" "One" "--req" "r-1" "--cursor" (string-append writer ":0") "--store" s))))
    (want "NO2 a resent request whose barrier fails answers as the base did, no written clause (replay creates nothing before it)"
          r '(1 (error unknown (replay-barrier-failed "unexpected failure"))))))

;; NO3 (the two write-outcome->answer callers; PR-15 (3), (3b)). The
;; record holds the machine registry's reservation first -- reserve! writes
;; <home>/instances.sexp through one temporary before the log write (log.sc
;; reserve!) -- and then the segment's write. The brief's first literal
;; named the segment alone; it was written from a base with no written
;; clause and corrected from this row's first reading (ruling, NO3 (a)).
(define (m1-home store)
  (string-append (substring store 0 (- (string-length store) (string-length "/parent/store"))) "/home"))
;; `^<home>/instances\.sexp\.tmp-[0-9]+-[0-9]+$`
(define (registry-tmp? t home)
  (let ((prefix (string-append home "/instances.sexp.tmp-")))
    (and (string? t) (> (string-length t) (string-length prefix))
         (string=? (substring t 0 (string-length prefix)) prefix)
         (let* ((rest (substring t (string-length prefix) (string-length t)))
                (dash (let loop ((i 0)) (cond ((= i (string-length rest)) #f) ((char=? (string-ref rest i) #\-) i) (else (loop (+ i 1)))))))
           (and dash (digits? (substring rest 0 dash)) (digits? (substring rest (+ dash 1) (string-length rest))))))))
;; -> (rc registry-tmp-ok answer-matches-or-the-answer)
(define (no3-reading r store wd usage)
  (let* ((ans (cadr r))
         (written (clause-of ans 'written))
         (tmp (and written (pair? (cadr written)) (pair? (car (cadr written))) (cadr (car (cadr written)))))
         (home (m1-home store)))
    (list (car r) (registry-tmp? tmp home)
          (if (and tmp
                   (equal? ans (append (list 'error 'unknown '(partial-write (sequence 2))
                                             (list 'written (list (list 'create tmp) (list 'write tmp)
                                                                  (list 'rename tmp (string-append home "/instances.sexp"))
                                                                  (list 'write (string-append wd "/000001.sexp")))))
                                       usage)))
              #t
              ans))))
(define commit-usage
  (read (open-string-input-port "(usage (commit (<block> ...) (\"--writer\" <name>) (\"--working-version\" <block>=<version>) (\"--premises\" <datum>)))")))
(let* ((s (m1-store!))
       (a (m1-insert-a s))
       (writer (m1-writer s)))
  (m1-call s 'write a "the text I wrote" "--writer" writer)
  (let* ((v1 (m1-working-version s a writer))
         (wd (m1-writer-dir s writer))
         (r (m1-run "THEOURGIA_LOCAL=1 THEOURGIA_INJECT=on THEOURGIA_FAULT=write-eio-after-partial@commit:file=000001.sexp"
                    "../core.sc" (list "commit" a "--writer" writer "--working-version" v1 "--store" s))))
    (want "NO3 a commit whose record is partly written answers unknown partial-write, written (before usage) naming the registry's reservation and the segment"
          (no3-reading r s wd (list commit-usage))
          '(1 #t #t))))
(let* ((s (m1-store!)) (writer (m1-writer s)))
  (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "insert" "--under" "root" "--title" "Zero" "--store" s))
  (let* ((wd (m1-writer-dir s writer))
         (r (m1-run "THEOURGIA_LOCAL=1 THEOURGIA_INJECT=on THEOURGIA_FAULT=write-eio-after-partial@commit:file=000001.sexp"
                    "../core.sc" (list "insert" "--under" "root" "--title" "One" "--store" s))))
    (want "NO3 an insert with no --req, the same fault: unknown partial-write with written naming the registry's reservation and the segment"
          (no3-reading r s wd '())
          '(1 #t #t))))

;; P1-d (F100b M1 review r1 F8; r2 F1/F6): rpc.sc `guarded` re-raises an
;; unreadable-entry to the owning dispatch, which answers it with what the
;; verb had already changed. export-code is guarded (rpc.sc) and its
;; code-project `answer` passes on anything that is not an (error ...)
;; list. It writes the first file whole (create, write, rename of one
;; temporary), then makes the second file's parent: mkdir-p! asks
;; file-is-directory? of <out>/z/deeper, whose parent is at 000, and
;; entry-type raises unreadable-entry. THE ORDER OF THE TWO FILES IS
;; READ, not assumed: the CONTROL row says a.sc was written.
(let* ((s (m1-store!))
       (src (string-append root "/p1d-src"))
       (out (string-append root "/p1d-out")))
  (system (string-append "mkdir -p " src "/z/deeper " out "/z"))
  (call-with-output-file (string-append src "/a.sc") (lambda (o) (put-string o "(define (a) 1)\n")))
  (call-with-output-file (string-append src "/z/deeper/b.sc") (lambda (o) (put-string o "(define (b) 2)\n")))
  (let ((imported (rpc-dispatch s (list 'import-code src) "test")))
    (chmod! "000" (string-append out "/z"))
    (let* ((r (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "export-code" out "--store" s)))
           (ans (cadr r))
           (written (clause-of ans 'written))
           (tmp (and written (pair? (cadr written)) (pair? (car (cadr written))) (cadr (car (cadr written))))))
      (chmod! "755" (string-append out "/z"))
      (want "CONTROL P1-d the import answered ok and the export wrote a.sc before it failed"
            (list (and (pair? imported) (car imported)) (file-exists? (string-append out "/a.sc")))
            '(ok #t))
      (want "P1-d a guarded verb meeting an unreadable entry after a change answers incomplete, written holding the change, rc 1"
            (list (car r) (dot-tmp? tmp (string-append out "/a.sc"))
                  (if (and tmp
                           (equal? ans (list 'error 'incomplete
                                             (list 'failed (list 'path (string-append out "/z/deeper")) (list 'reason denied) '(errno EACCES))
                                             (list 'written (list (list 'create tmp) (list 'write tmp)
                                                                  (list 'rename tmp (string-append out "/a.sc")))))))
                      #t
                      ans))
            '(1 #t #t)))))

;; NO3-nw (F100b M1 review r1, F1): the other write-outcome->answer shape,
;; `(error not-written reserved-not-written (sequence n))`, whose third
;; element is an atom. write-eio-first fails the segment's first write
;; after the registry's reservation, so the record holds the reservation's
;; three entries and no segment entry (the failed write notes nothing).
(let* ((s (m1-store!))
       (a (m1-insert-a s))
       (writer (m1-writer s)))
  (m1-call s 'write a "the text I wrote" "--writer" writer)
  (let* ((v1 (m1-working-version s a writer))
         (home (m1-home s))
         (r (m1-run "THEOURGIA_LOCAL=1 THEOURGIA_INJECT=on THEOURGIA_FAULT=write-eio-first@commit:file=000001.sexp"
                    "../core.sc" (list "commit" a "--writer" writer "--working-version" v1 "--store" s)))
         (ans (cadr r))
         (written (clause-of ans 'written))
         (tmp (and written (pair? (cadr written)) (pair? (car (cadr written))) (cadr (car (cadr written))))))
    (want "NO3-nw a commit whose first segment write fails answers not-written, its atom kept, written naming the registry's reservation, then usage"
          (list (car r) (registry-tmp? tmp home)
                (if (and tmp
                         (equal? ans (list 'error 'not-written 'reserved-not-written '(sequence 2)
                                           (list 'written (list (list 'create tmp) (list 'write tmp)
                                                                (list 'rename tmp (string-append home "/instances.sexp"))))
                                           commit-usage)))
                    #t
                    ans))
          '(1 #t #t))))

;; F (M1's routes with their success inputs, F100b M1 review r1, F3/F4):
;; each answer's head is the one expected, read independently of the
;; answer, and none carries written or client-written. The routes: publish
;; through the thin client, read through core.sc and the thin client (root
;; is not a block of a fresh store, a named non-filesystem refusal), a
;; local insert, a commit of a draft, and a restore of a revoked version.
(define (carries-a-record? a) (and (or (clause-of a 'written) (clause-of a 'client-written)) #t))
(let* ((s (m1-store!)) (writer (m1-writer s))
       (answers
        (list (cadr (m1-run "" "../theourgia.sc" (list "publish" M "1" m1-seg "--store" s)))
              (cadr (m1-run "" "../core.sc" (list "read" "root" "--store" s)))
              (cadr (m1-run "" "../theourgia.sc" (list "read" "root" "--store" s)))
              (cadr (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "insert" "--under" "root" "--title" "Two" "--store" s))))))
  (stop-daemon! "F" s)
  (want "F publish, read (core.sc and thin) and a local insert on success inputs: the expected heads, and no record clause"
        (map (lambda (a) (list (if (and (pair? a) (pair? (cdr a))) (list (car a) (if (eq? (car a) 'error) (cadr a) '_)) a)
                               (carries-a-record? a)))
             answers)
        '(((ok _) #f) ((error unknown-id) #f) ((error unknown-id) #f) ((ok _) #f))))
(let* ((s (m1-store!))
       (a (m1-insert-a s))
       (writer (m1-writer s)))
  (m1-call s 'write a "the text I wrote" "--writer" writer)
  (let* ((v1 (m1-working-version s a writer))
         (r (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "commit" a "--writer" writer "--working-version" v1 "--store" s))))
    (want "F a commit of a draft on a success input answers ok, rc 0, and no record clause"
          (list (car r) (and (pair? (cadr r)) (car (cadr r))) (carries-a-record? (cadr r)))
          '(0 ok #f))))
(let* ((s (m1-store!))
       (a (m1-insert-a s))
       (writer (m1-writer s)))
  (m1-call s 'write a "the text I wrote" "--writer" writer)
  (let* ((v1 (m1-working-version s a writer))
         (cursor (string-append writer ":" (number->string (cdr (assoc writer (reduce-applied-cut (open-and-reduce s)))))))
         (committed (m1-call s 'commit a "--req" "R1" "--cursor" cursor "--working-version" v1 "--writer" writer))
         (evidence (store-evidence s (cons writer "R1")))
         (plan-ev (find (lambda (e) (eq? 'plan (actor-sub (ev-actor e)))) evidence))
         (rival (encode-record 1 1789000000001 (ev-actor plan-ev) '() (storable-encode (ev-payload plan-ev)))))
    (log-publish! s "rivalzzz" 1 rival (segment-sha rival))
    (let ((r (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "restore" v1 "--writer" writer "--store" s))))
      (want "F a restore of a revoked version on a success input answers ok, rc 0, and no record clause"
            (list (car r) (and (pair? (cadr r)) (car (cadr r))) (carries-a-record? (cadr r)))
            '(0 ok #f)))))
;; F replay and F export-code (F100b M1 review r3, F2): a request resent
;; after it was applied, and an export into a directory that takes it.
(let* ((s (m1-store!)) (writer (m1-writer s))
       (args (list "insert" "--under" "root" "--title" "One" "--req" "r-1" "--cursor" (string-append writer ":0") "--store" s))
       (first (m1-run "THEOURGIA_LOCAL=1" "../core.sc" args))
       (again (m1-run "THEOURGIA_LOCAL=1" "../core.sc" args)))
  (want "F a request resent after it was applied (the replay) answers ok, rc 0, and no record clause"
        (list (car first) (car again) (and (pair? (cadr again)) (car (cadr again))) (carries-a-record? (cadr again)))
        '(0 0 ok #f)))
(let* ((s (m1-store!))
       (src (string-append root "/fx-src"))
       (out (string-append root "/fx-out")))
  (system (string-append "mkdir -p " src "/z/deeper " out))
  (call-with-output-file (string-append src "/a.sc") (lambda (o) (put-string o "(define (a) 1)\n")))
  (call-with-output-file (string-append src "/z/deeper/b.sc") (lambda (o) (put-string o "(define (b) 2)\n")))
  (rpc-dispatch s (list 'import-code src) "test")
  (let ((r (m1-run "THEOURGIA_LOCAL=1" "../core.sc" (list "export-code" out "--store" s))))
    (want "F export-code into a directory that takes both files answers ok, rc 0, and no record clause"
          (list (car r) (and (pair? (cadr r)) (car (cadr r))) (carries-a-record? (cadr r))
                (file-exists? (string-append out "/z/deeper/b.sc")))
          '(0 ok #f #t))))
;; ---- F100b M2a: the daemon's startup reports -----------------------------------
;;
;; Brief v7 rows P2-startup and its TWIN, P8, P9 a-d, NO4, and their F row.
;; Each is a DIRECT serve (`theourgiad.sc serve`, no client, no token): its
;; stdout read as datums, its stderr kept, its exit code. A startup report
;; is framed by a newline, so the rows read datums, not lines (J4). A daemon
;; that serves is stopped by its exact argv line, as above.
(printf "== F100b M2a: the daemon's startup reports ==\n")
(define (text-of-file p)
  (if (file-exists? p)
      (let ((t (call-with-input-file p get-string-all))) (if (eof-object? t) "" t))
      ""))
;; A tail that does not read is NOT the end of the output (M2a review r2,
;; F7): it is kept as the marker UNREADABLE-TAIL, so a report followed by a
;; torn one does not compare equal to the report alone.
(define (datums-of-text t)
  (let ((port (open-string-input-port t)))
    (let loop ((acc '()))
      (let ((x (guard (e (#t 'UNREADABLE-TAIL)) (read port))))
        (cond ((eof-object? x) (reverse acc))
              ((eq? x 'UNREADABLE-TAIL) (reverse (cons x acc)))
              (else (loop (cons x acc))))))))
(define (has-panic? t) (has-substring? t "PANIC"))
;; -> (rc stdout-datums stderr-text): a start that is expected to leave.
(define (serve-direct env store args)
  (let ((out (string-append root "/sd.out")) (err (string-append root "/sd.err")))
    (let ((rc (system (string-append env " perl -e 'alarm 30; exec @ARGV' scheme --script ../theourgiad.sc serve "
                                     (quoted store) " "
                                     (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                                     "> " out " 2> " err " < /dev/null"))))
      (list rc (datums-of-text (text-of-file out)) (text-of-file err) (text-of-file out)))))
(define (m2-sock-dir! name)
  (let ((d (string-append m1-run-root "/" name)))
    (system (string-append "mkdir -p " d))
    d))

;; P2-startup (store-loop's entry, point 2; PR-04): the store at 000; the
;; socket directory and its lock PRE-CREATED, so main records nothing.
(let* ((s (m1-store!)) (d (m2-sock-dir! "p2")) (sock (string-append d "/sock")))
  (system (string-append "touch " d "/.sock.lock"))
  (chmod! "000" s)
  (let ((r (serve-direct "" s (list "--socket" sock))))
    (chmod! "755" s)
    (want "P2-startup a store at 000: the first datum is the table's unreadable with the attempt clause, then store-actor-down, rc 75, no socket"
          (list (car r) (let ((ds (cadr r))) (and (pair? ds) (car ds)))
                (let ((ds (cadr r))) (and (pair? ds) (pair? (cdr ds)) (cadr ds)))
                (file-exists? sock))
          (list 75 (list 'error 'unreadable (list 'path (string-append s "/meta.sexp")) (list 'reason denied) '(errno EACCES) '(attempt #f))
                '(exiting (reason store-actor-down)) #f))))
;; The TWIN (aggregation (a) through the daemon): the same with the lock NOT
;; pre-created -- main creates it, and main's record joins the store's
;; answer, which becomes incomplete.
(let* ((s (m1-store!)) (d (m2-sock-dir! "p2t")) (sock (string-append d "/sock")))
  (chmod! "000" s)
  (let ((r (serve-direct "" s (list "--socket" sock))))
    (chmod! "755" s)
    (want "P2-startup TWIN main made the lock: the store's unreadable is promoted to incomplete, written naming the lock"
          (list (car r) (let ((ds (cadr r))) (and (pair? ds) (car ds))))
          (list 75 (list 'error 'incomplete
                         (list 'failed (list 'path (string-append s "/meta.sexp")) (list 'reason denied) '(errno EACCES))
                         (list 'written (list (list 'create (string-append d "/.sock.lock"))))
                         '(attempt #f))))))

;; P8 (theourgiad serve-and-exit!, point 8; PR-13): no --socket, the store's
;; parent at 000, so the socket's derivation cannot read the store.
(let ((s (m1-store!)))
  (chmod! "000" (parent-of s))
  (let ((r (serve-direct "" s '())))
    (chmod! "755" (parent-of s))
    (want "P8 serve with no --socket under a 000 parent: stdout holds ONE datum, the table's unreadable naming the store with the attempt clause, rc 75"
          (list (car r) (cadr r) (has-panic? (caddr r)))
          (list 75 (list (list 'error 'unreadable (list 'path s) (list 'reason denied) '(errno EACCES) '(attempt #f))) #f))))

;; P9 (daemon main, point 9; PR-14), a valid store, --socket <dir>/<name>.
;; (a) stat-fail@report aimed at "/<name>", which is in the socket's path and
;; not in the lock's (`<dir>/.<name>.lock`): the lock is made, then the probe
;; of the socket path fails -- the guard in occupied-by-a-non-socket? used to
;; answer #f and the daemon served.
(let* ((s (m1-store!)) (d (m2-sock-dir! "p9a")) (name "sockq7") (sock (string-append d "/" name)))
  (let ((r (serve-direct (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=stat-fail@report:file=/" name) s (list "--socket" sock))))
    (want "P9-a a probe of the socket path that fails after the lock was made: ONE datum, incomplete, written naming the lock, rc 75, no PANIC, no socket"
          (list (car r) (let ((ds (cadr r))) (and (= 1 (length ds)) (car ds))) (has-panic? (caddr r)) (file-exists? sock))
          (list 75 (list 'error 'incomplete
                         (list 'failed (list 'path sock) '(reason "Input/output error") '(errno EIO))
                         (list 'written (list (list 'create (string-append d "/." name ".lock"))))
                         '(attempt #f))
                #f #f))))
;; (b) the socket directory at 500: the lock cannot be created.
(let* ((s (m1-store!)) (d (m2-sock-dir! "p9b")) (sock (string-append d "/sock")))
  (chmod! "500" d)
  (let ((r (serve-direct "" s (list "--socket" sock))))
    (chmod! "755" d)
    (want "P9-b a socket directory at 500: ONE datum, unwritable (op create) naming the lock, errno 13, rc 75, no PANIC"
          (list (car r) (let ((ds (cadr r))) (and (= 1 (length ds)) (car ds))) (has-panic? (caddr r)))
          (list 75 (list 'error 'unwritable '(op create) (list 'path (string-append d "/.sock.lock")) (list 'reason denied)
                         (list 'errno EACCES) '(attempt #f))
                #f))))
;; (c, D6) the lock file pre-created at 000: it cannot be opened, which is not
;; contention -- the table's unreadable, not serve-busy.
(let* ((s (m1-store!)) (d (m2-sock-dir! "p9c")) (sock (string-append d "/sock")) (lock (string-append d "/.sock.lock")))
  (system (string-append "touch " lock))
  (chmod! "000" lock)
  (let ((r (serve-direct "" s (list "--socket" sock))))
    (chmod! "644" lock)
    ;; THE PHYSICAL FRAMING TOO (M2a review r1, F9): stdout's bytes are
    ;; exactly newline, the datum, newline -- one report, written by
    ;; write-report-line!, not by `report`.
    (want "P9-c a lock file at 000 is unreadable, not serve-busy: stdout is exactly \\n + the one report + \\n, rc 75"
          (list (car r) (cadddr r))
          (list 75 (string-append "\n"
                                  (call-with-string-output-port
                                    (lambda (p) (write (list 'error 'unreadable (list 'path lock) (list 'reason denied) '(errno EACCES) '(attempt #f)) p)))
                                  "\n")))))
;; P9-c with a token (M2a review r1, F5): a supplied --attempt is echoed as
;; the report's last clause.
(let* ((s (m1-store!)) (d (m2-sock-dir! "p9t")) (sock (string-append d "/sock")) (lock (string-append d "/.sock.lock")))
  (system (string-append "touch " lock))
  (chmod! "000" lock)
  (let ((r (serve-direct "" s (list "--socket" sock "--attempt" "0123456789abcdef"))))
    (chmod! "644" lock)
    (want "P9-c with --attempt: the one report ends with the supplied token"
          (list (car r) (cadr r))
          (list 75 (list (list 'error 'unreadable (list 'path lock) (list 'reason denied) '(errno EACCES) '(attempt "0123456789abcdef")))))))
;; serve-path-occupied carries main's record (M2a review r1, F2): a regular
;; file on the socket path, the lock NOT pre-created -- main made it.
(let* ((s (m1-store!)) (d (m2-sock-dir! "p9o")) (sock (string-append d "/sock")))
  (system (string-append "printf x > " sock))
  (let ((r (serve-direct "" s (list "--socket" sock))))
    (want "P9-occupied a file on the socket path: ONE datum, serve-path-occupied with written naming the lock main made, rc 75; the file survives"
          (list (car r) (cadr r) (file-exists? sock))
          (list 75 (list (list 'error 'serve-path-occupied (list 'path sock)
                               (list 'written (list (list 'create (string-append d "/.sock.lock"))))
                               '(attempt #f)))
                #t))))
;; The store process's two non-table outcomes (M2a review r1, F14):
;; store-load-failed (a meta.sexp that does not parse) and an (error ...)
;; passthrough (store-busy: the store's lock held elsewhere past the 5 s
;; budget). Both locks pre-created, so main records nothing.
(let* ((s (m1-store!)) (d (m2-sock-dir! "p2f")) (sock (string-append d "/sock")))
  (system (string-append "touch " d "/.sock.lock"))
  (call-with-output-file (string-append s "/meta.sexp") (lambda (o) (put-string o "(broken")) 'truncate)
;; The parse failure is a log-error, not a condition, so its reason is the
  ;; symbol `raised`, as on the base (PR-11 u).
  (let ((r (serve-direct "" s (list "--socket" sock))))
    (want "P2-load-failed a meta.sexp that does not parse: store-load-failed (reason raised) with the attempt clause, then store-actor-down, rc 75"
          (list (car r) (cadr r))
          (list 75 (list '(error store-load-failed (reason raised) (attempt #f)) '(exiting (reason store-actor-down)))))))
(let* ((s (m1-store!)) (d (m2-sock-dir! "p2b")) (sock (string-append d "/sock"))
       (holder-pid (string-append root "/holder.pid")))
  (system (string-append "touch " d "/.sock.lock"))
  (system (string-append "perl -e 'use Fcntl qw(:flock); open(my $f, \">>\", $ARGV[0]) or die; flock($f, LOCK_EX) or die; sleep 20' "
                         (quoted (string-append s "/lock")) " > /dev/null 2>&1 & echo $! > " holder-pid))
  (system "sleep 0.5")
  (let ((r (serve-direct "" s (list "--socket" sock))))
    (system (string-append "kill -TERM $(cat " holder-pid ") 2>/dev/null"))
    (want "P2-busy the store's lock held elsewhere past the budget: store-busy passed through with the attempt clause, then store-actor-down, rc 75"
          (list (car r) (cadr r))
          (list 75 (list (list 'error 'store-busy (list 'path (string-append s "/lock")) '(attempt #f))
                         '(exiting (reason store-actor-down)))))))
;; (d, D5, and the F row for these routes) a first start with no socket and
;; no lock serves; its first datum is `serving`, with no record clause.
(let* ((s (m1-store!)) (d (m2-sock-dir! "p9d")) (sock (string-append d "/sock"))
       (out (string-append root "/p9d.out")))
  (system (string-append "scheme --script ../theourgiad.sc serve " (quoted s) " --socket " (quoted sock)
                         " > " out " 2> /dev/null < /dev/null &"))
  (let wait ((k 0))
    (when (and (< k 100) (not (has-substring? (text-of-file out) "(serving")))
      (system "sleep 0.1") (wait (+ k 1))))
  (let ((first (let ((ds (datums-of-text (text-of-file out)))) (and (pair? ds) (car ds)))))
    (stop-daemon! "P9-d" s)
    (want "P9-d / F a first start with no socket and no lock serves: the first datum is serving, with no written, client-written or attempt clause"
          (list (and (pair? first) (car first))
                (and (clause-of first 'written) #t) (and (clause-of first 'client-written) #t) (and (clause-of first 'attempt) #t))
          '(serving #f #f #f))))

;; THE REPORT CHANNEL ITSELF FAILS (M2a review r2, F1, F2, F4): stdout
;; closed, so the report's one write fails. Each startup that refused still
;; leaves with 75 -- main's own refusal (P9-c's setup), point 8 (P8's), and
;; the store's failure through main's combined report and the store-actor-
;; down line after it (P2-startup's) -- and never with the scheduler's
;; panic 70.
(define (serve-direct-closed env store args)
  (let ((err (string-append root "/sdc.err")))
    (let ((rc (system (string-append env " perl -e 'alarm 30; exec @ARGV' scheme --script ../theourgiad.sc serve "
                                     (quoted store) " "
                                     (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                                     ">&- 2> " err " < /dev/null"))))
      (list rc (has-panic? (text-of-file err))))))
(let* ((s (m1-store!)) (d (m2-sock-dir! "cl9")) (sock (string-append d "/sock")) (lock (string-append d "/.sock.lock")))
  (system (string-append "touch " lock))
  (chmod! "000" lock)
  (let ((r (serve-direct-closed "" s (list "--socket" sock))))
    (chmod! "644" lock)
    (want "P9-c closed stdout: main's refusal cannot be written, and the start still leaves with 75, no PANIC"
          r '(75 #f))))
(let ((s (m1-store!)))
  (chmod! "000" (parent-of s))
  (let ((r (serve-direct-closed "" s '())))
    (chmod! "755" (parent-of s))
    (want "P8 closed stdout: point 8's report cannot be written, and the start still leaves with 75, no PANIC"
          r '(75 #f))))
(let* ((s (m1-store!)) (d (m2-sock-dir! "cl2")) (sock (string-append d "/sock")))
  (system (string-append "touch " d "/.sock.lock"))
  (chmod! "000" s)
  (let ((r (serve-direct-closed "" s (list "--socket" sock))))
    (chmod! "755" s)
    (want "P2-startup closed stdout: neither main's combined report nor store-actor-down can be written, and the start still leaves with 75, no PANIC"
          r '(75 #f))))
;; detach-failed with STDERR closed: the exit stays 71. THEOURGIA_HOME is
;; UNSET for this run: ffi.sc announces a set home on stderr when the
;; library loads (machine-home-announced) and flushes it, so with stderr
;; closed and the home set, the program ends at load with 255 -- before
;; detach-step exists. That is the base's behaviour and outside M2a; the
;; detach fails before anything reads the home.
(let* ((s (m1-store!)) (d (m2-sock-dir! "no4c")) (logdir (string-append root "/no4c-logs")) (log (string-append logdir "/serve.log")))
  (system (string-append "mkdir -p " logdir))
  (chmod! "000" logdir)
  (let ((rc (system (string-append "env -u THEOURGIA_HOME perl -e 'alarm 30; exec @ARGV' scheme --script ../theourgiad.sc serve " (quoted s)
                                   " --socket " (quoted (string-append d "/sock")) " --detach --log " (quoted log)
                                   " > /dev/null 2>&- < /dev/null"))))
    (chmod! "755" logdir)
    (want "NO4 closed stderr: detach-failed cannot be written, and the exit is still 71"
          rc 71)))

;; THE TOKEN AND THE FRAMING THROUGH THE OTHER TWO EMITTERS (M2a review r2,
;; F5, F6): point 8 and the store's failure reported by main from its
;; state. Stdout's exact bytes, the supplied token last.
(define (framed . datums)
  (apply string-append
         (map (lambda (d) (call-with-string-output-port (lambda (p) (write d p)))) datums)))
(let ((s (m1-store!)))
  (chmod! "000" (parent-of s))
  (let ((r (serve-direct "" s (list "--attempt" "0123456789abcdef"))))
    (chmod! "755" (parent-of s))
    (want "P8 with --attempt: stdout is exactly \\n + the report ending with the token + \\n, rc 75"
          (list (car r) (cadddr r))
          (list 75 (string-append "\n" (framed (list 'error 'unreadable (list 'path s) (list 'reason denied) '(errno EACCES)
                                                     '(attempt "0123456789abcdef")))
                                  "\n")))))
(let* ((s (m1-store!)) (d (m2-sock-dir! "p2k")) (sock (string-append d "/sock")))
  (system (string-append "touch " d "/.sock.lock"))
  (chmod! "000" s)
  (let ((r (serve-direct "" s (list "--socket" sock "--attempt" "0123456789abcdef"))))
    (chmod! "755" s)
    (want "P2-startup with --attempt: stdout is exactly \\n + main's combined report ending with the token + \\n, then store-actor-down, rc 75"
          (list (car r) (cadddr r))
          (list 75 (string-append "\n" (framed (list 'error 'unreadable (list 'path (string-append s "/meta.sexp")) (list 'reason denied)
                                                     '(errno EACCES) '(attempt "0123456789abcdef")))
                                  "\n" (framed '(exiting (reason store-actor-down))) "\n")))))

;; NO4b (M2a review r1, F4 and F12): the log's directory writable and the log
;; absent, its open failed by open-fail@report: fd-open's file-ensure! has
;; created the log first, so detach-failed carries that creation -- and the
;; fault fires only because detach! runs under the `report` stage.
(let* ((s (m1-store!)) (d (m2-sock-dir! "no4b")) (logdir (string-append root "/no4b-logs")) (log (string-append logdir "/serve.log")))
  (system (string-append "mkdir -p " logdir))
  (let ((r (serve-direct "THEOURGIA_INJECT=on THEOURGIA_FAULT=open-fail@report:file=serve.log:errno=EACCES"
                         s (list "--socket" (string-append d "/sock") "--detach" "--log" log))))
    (want "NO4b a log created and then not opened: detach-failed on stderr carries the creation, exit 71, nothing on stdout"
          (list (car r) (cadr r)
                (filter (lambda (x) (or (eq? x 'UNREADABLE-TAIL) (and (pair? x) (eq? (car x) 'error)))) (datums-of-text (caddr r))))
          (list 71 '() (list (list 'error 'detach-failed '(step log) (list 'path log) '(errno EACCES)
                                   (list 'written (list (list 'create log)))))))))

;; NO4 (theourgiad detach-step, a named outcome; PR-13/E2): --detach with a
;; log beneath a 000 directory. detach-failed goes to STDERR, keeps its name,
;; carries no written (nothing was created), exit 71; stdout holds nothing.
(let* ((s (m1-store!)) (d (m2-sock-dir! "no4")) (logdir (string-append root "/no4-logs")) (log (string-append logdir "/serve.log")))
  (system (string-append "mkdir -p " logdir))
  (chmod! "000" logdir)
  (let ((r (serve-direct "" s (list "--socket" (string-append d "/sock") "--detach" "--log" log))))
    (chmod! "755" logdir)
    ;; stderr's (error ...) datums: the daemon also prints its
    ;; `(theourgia machine-home ...)` notice there, which is not an answer.
    ;; A torn tail is KEPT (M2a review r3, F3): the filter passes the
    ;; UNREADABLE-TAIL marker, so a report followed by half of another fails.
    (want "NO4 a log that cannot be opened: detach-failed on stderr, no written, exit 71, nothing on stdout"
          (list (car r) (cadr r)
                (filter (lambda (d) (or (eq? d 'UNREADABLE-TAIL) (and (pair? d) (eq? (car d) 'error)))) (datums-of-text (caddr r))))
          (list 71 '() (list (list 'error 'detach-failed '(step log) (list 'path log) '(errno EACCES)))))))

;; ---- F100b M2b1: the client's start (point 6), the token and the pid ----------
;;
;; Brief rows P6 (unreadable, incomplete, unwritable, exited, store-not-found),
;; TK1, TK2 a/b, TK5, PID1, PID2. Each start goes through the thin client
;; (`theourgia.sc read root`), which spawns a daemon with --detach, a log
;; under THEOURGIA_RUN and this start's token. The token is 16 hex and is
;; read here as its shape: `(attempt <16-hex>)`.
(printf "== F100b M2b1: the client's start, the token and the pid ==\n")
(define (mask-attempt a)
  (if (list? a)
      (map (lambda (c)
             (if (and (pair? c) (eq? (car c) 'attempt) (pair? (cdr c)) (string? (cadr c)) (= 16 (string-length (cadr c))))
                 '(attempt <16-hex>)
                 c))
           a)
      a))
;; A run root of the row's own; the key directory and the log follow the
;; client's own rule (socket-path, serve-log-path) under it.
(define p6-n 0)
(define (p6-run-root!)
  (set! p6-n (+ p6-n 1))
  (let ((r (string-append m1-run-root "/p6-" (number->string p6-n))))
    (system (string-append "mkdir -p " r))
    (putenv "THEOURGIA_RUN" r)
    r))
(define (restore-run!) (putenv "THEOURGIA_RUN" (string-append m1-run-root "/run")))
(define (key-dir s) (let ((p (socket-path s))) (substring p 0 (- (string-length p) (string-length "/socket")))))
;; -> (rc stdout-datum stderr-text elapsed-ms)
(define (client-read env s)
  (let ((out (string-append root "/cr.out")) (err (string-append root "/cr.err")) (t0 (real-time)))
    (let ((rc (system (string-append env " perl -e 'alarm 60; exec @ARGV' scheme --script ../theourgia.sc read root --store "
                                     (quoted s) " > " out " 2> " err " < /dev/null"))))
      (list rc (guard (e (#t 'no-answer)) (call-with-input-file out read)) (text-of-file err) (- (real-time) t0)))))
;; The P6 setup (E13): meta.sexp at 000; the key directory and .socket.lock
;; PRE-CREATED, so neither the client nor main records a creation.
(define (p6-store! pre-create?)
  (let* ((s (m1-store!)) (run (p6-run-root!)) (kd (key-dir s)))
    (when pre-create?
      (system (string-append "mkdir -p " kd " && touch " kd "/.socket.lock")))
    (chmod! "000" (string-append s "/meta.sexp"))
    (list s run kd)))
(define (p6-done! s) (chmod! "644" (string-append s "/meta.sexp")) (restore-run!))

;; P6-unreadable, with PID1's bound (answered within 4 s of the launch).
(let* ((st (p6-store! #t)) (s (car st)) (kd (caddr st))
       (r (client-read "" s)))
  (p6-done! s)
  (want "P6-unreadable the thin client answers serve-start-failed with the report's kind and clauses, its token, and (exit 75); rc 75"
        (list (car r) (mask-attempt (cadr r)))
        (list 75 (list 'error 'serve-start-failed '(kind unreadable) (list 'path (string-append s "/meta.sexp"))
                       (list 'reason denied) '(errno EACCES) '(attempt <16-hex>) '(exit 75))))
  (want "PID1 the P6-unreadable start is answered within 4 s of the client's launch (the pid wait, not the 10 s budget)"
        (< (cadddr r) 4000) #t))
;; P6-incomplete: the key directory absent -- the client makes it (its
;; client-written), main creates the lock (the report's written).
(let* ((st (p6-store! #f)) (s (car st)) (kd (caddr st))
       (r (client-read "" s)))
  (p6-done! s)
  (want "P6-incomplete the report's written names main's lock; the client's own mkdir is a separate client-written clause, last"
        (list (car r) (mask-attempt (cadr r)))
        (list 75 (list 'error 'serve-start-failed '(kind incomplete)
                       (list 'failed (list 'path (string-append s "/meta.sexp")) (list 'reason denied) '(errno EACCES))
                       (list 'written (list (list 'create (string-append kd "/.socket.lock"))))
                       '(attempt <16-hex>) '(exit 75)
                       (list 'client-written (list (list 'mkdir kd)))))))
;; P6-unwritable (G2): the run root at 555, the key directory absent -- the
;; client's own mkdir fails; no daemon, no attempt clause, no client-written.
(let* ((s (m1-store!)) (run (p6-run-root!)) (kd (key-dir s)) (t0 (real-time)))
  (chmod! "555" run)
  (let ((r (client-read "" s)))
    (chmod! "755" run)
    (restore-run!)
    (want "P6-unwritable a run root at 555: the client's own failure, unwritable (op mkdir) naming the key directory, errno 13, rc 75, under 1 s"
          (list (car r) (cadr r) (< (cadddr r) 1000))
          (list 75 (list 'error 'serve-start-failed '(kind unwritable) '(op mkdir) (list 'path kd) (list 'reason denied) (list 'errno EACCES))
                #t))))
;; P6-exited: the daemon's log open fails (open-fail@report); the client's
;; stdout is the exited answer, its stderr the daemon's inherited line.
(let* ((st (p6-store! #t)) (s (car st)) (kd (caddr st)))
  (system (string-append "touch " kd "/serve.log"))
  (let ((r (client-read "THEOURGIA_INJECT=on THEOURGIA_FAULT=open-fail@report:file=serve.log:errno=EACCES" s)))
    (p6-done! s)
    (want "P6-exited a daemon that exits before any report: (kind exited) (exit 71) (attempt T) on stdout, rc 75, and detach-failed on the client's stderr"
          (list (car r) (mask-attempt (cadr r))
                (filter (lambda (d) (or (eq? d 'UNREADABLE-TAIL) (and (pair? d) (eq? (car d) 'error)))) (datums-of-text (caddr r))))
          (list 75 '(error serve-start-failed (kind exited) (exit 71) (attempt <16-hex>))
                (list (list 'error 'detach-failed '(step log) (list 'path (string-append kd "/serve.log")) '(errno EACCES)))))))
;; P6-store-not-found (D4): the store removed before the client runs.
(let* ((s (m1-store!)) (run (p6-run-root!)) (kd (key-dir s)))
  (system (string-append "mkdir -p " kd " && touch " kd "/.socket.lock && rm -rf " s))
  (let ((r (client-read "" s)))
    (restore-run!)
    (want "P6-store-not-found the report's kind is store-not-found, its store clause, the token and (exit 75), within 4 s"
          (list (car r) (mask-attempt (cadr r)) (< (cadddr r) 4000))
          (list 75 (list 'error 'serve-start-failed '(kind store-not-found) (list 'store s) '(attempt <16-hex>) '(exit 75)) #t))))
;; PID2: an extra child of the client that has exited, unreaped, before the
;; daemon spawns; a wait that collects any child would answer (exit 0).
(let* ((st (p6-store! #t)) (s (car st)))
  (let ((r (client-read "THEOURGIA_INJECT=on THEOURGIA_FAULT=client-extra-child@client" s)))
    (p6-done! s)
    (want "PID2 with an extra exited child the status is the daemon's own: (exit 75)"
          (let ((a (cadr r))) (and (list? a) (assq 'exit (filter pair? a))))
          '(exit 75))))
;; TK2 (a): the framing in the log -- a line break before the report, and
;; the report line complete.
(let* ((st (p6-store! #t)) (s (car st)) (kd (caddr st)))
  (client-read "" s)
  (p6-done! s)
  (let ((log (text-of-file (string-append kd "/serve.log"))))
    (want "TK2-a serve.log begins with a line break and the report line is complete"
          (list (and (> (string-length log) 0) (char=? (string-ref log 0) #\newline))
                (has-substring? log "(attempt \"")
                (let ((d (datums-of-text log))) (and (pair? d) (pair? (car d)) (eq? 'error (car (car d))) (pair? (cdr d)) (cadr d))))
          '(#t #t (exiting (reason store-actor-down))))))
;; TK2 (b): a planted unterminated line before the start; the client answers
;; its own report, and the planted text is terminated by the report's line break.
(let* ((st (p6-store! #t)) (s (car st)) (kd (caddr st)) (lp (string-append kd "/serve.log")))
  (call-with-output-file lp (lambda (o) (put-string o "(error planted (attempt \"X\")")) 'truncate)
  (let ((r (client-read "" s)))
    (p6-done! s)
    (let ((log (text-of-file lp)))
      (want "TK2-b a planted unterminated line: the client selects its own report by the token, and the log reads planted, newline, the report, exiting"
;; The rest after the planted text, read as lines: the report
            ;; carrying a token, then the exiting line, then the end.
            (let* ((planted "(error planted (attempt \"X\")")
                   (n (string-length planted))
                   (rest (and (> (string-length log) n) (substring log n (string-length log))))
                   (lines (and rest (let split ((cs (string->list rest)) (cur '()) (acc '()))
                                      (cond ((null? cs) (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
                                            ((char=? (car cs) #\newline) (split (cdr cs) '() (cons (list->string (reverse cur)) acc)))
                                            (else (split (cdr cs) (cons (car cs) cur) acc)))))))
              (list (let ((a (mask-attempt (cadr r)))) (and (list? a) (cadr (assq 'kind (filter pair? (cddr a))))))
                    (and (string=? (substring log 0 n) planted) #t)
                    (and lines (length lines))
                    (and lines (= 3 (length lines)) (string=? (car lines) ""))
                    (and lines (= 3 (length lines)) (has-substring? (cadr lines) "(error unreadable") (has-substring? (cadr lines) "(attempt \""))
                    (and lines (= 3 (length lines)) (caddr lines))))
            (list 'unreadable #t 3 #t #t "(exiting (reason store-actor-down))")))))
;; TK5 (D8): the client's log read fails under stage `client`.
(let* ((st (p6-store! #t)) (s (car st)) (kd (caddr st)))
  (let ((r (client-read "THEOURGIA_INJECT=on THEOURGIA_FAULT=read-fail-after@client:file=serve.log:errno=EIO" s)))
    (p6-done! s)
    (want "TK5 the log cannot be read after the exit: (kind unreadable) naming serve.log with EIO, then the status"
          (list (car r) (cadr r))
          (list 75 (list 'error 'serve-start-failed '(kind unreadable) (list 'path (string-append kd "/serve.log"))
                         '(reason "Input/output error") '(errno EIO) '(exit 75))))))
;; TK1 (the token): client A held at client-scan after its daemon reported;
;; client B starts and answers its own token; then A, released, answers ITS
;; token although B's report came later in the same log.
(let* ((st (p6-store! #t)) (s (car st))
       (ra (string-append root "/tk1-release")) (aout (string-append root "/tk1-a.out")))
  (system (string-append "THEOURGIA_INJECT=on THEOURGIA_HOLD=client-scan:" ra
                         " perl -e 'alarm 60; exec @ARGV' scheme --script ../theourgia.sc read root --store " (quoted s)
                         " > " aout " 2> /dev/null < /dev/null &"))
  ;; THE ORDER IS ASSERTED, NOT HOPED FOR (M2b1 review r1, F5): A's marker
  ;; was seen before B started, and A had not answered when B had.
  (let* ((held (let wait ((k 0))
                 (cond ((file-exists? (string-append ra ".held")) #t)
                       ((>= k 250) #f)
                       (else (system "sleep 0.1") (wait (+ k 1))))))
         (b (client-read "" s))
         (a-silent (= 0 (string-length (text-of-file aout)))))
    (call-with-output-file ra (lambda (o) (write 'go o)))
    (let wait ((k 0))
      (when (and (< k 100) (= 0 (string-length (text-of-file aout))))
        (system "sleep 0.1") (wait (+ k 1))))
    (system "sleep 0.2")
    (p6-done! s)
    (let* ((a (guard (e (#t 'no-answer)) (call-with-input-file aout read)))
           (ta (and (list? a) (assq 'attempt (filter pair? a))))
           (tb (and (list? (cadr b)) (assq 'attempt (filter pair? (cadr b))))))
      (want "TK1 A held (its marker seen before B started, no answer when B had one), then A and B answer different tokens, each its own start's, both unreadable"
            (list held a-silent
                  (and ta (cadr ta) #t) (and tb (cadr tb) #t) (and ta tb (not (equal? ta tb)))
                  (and (list? a) (assq 'kind (filter pair? a))) (and (list? (cadr b)) (assq 'kind (filter pair? (cadr b)))))
            '(#t #t #t #t #t (kind unreadable) (kind unreadable))))))

;; ---- F100b M2b2: the daemon's holds, AG-a, A-record, the STARTING signals ----
;;
;; Brief v7 rows P6-timeout, PID3, AG-a, A-record and its TWIN, and the
;; STARTING+signal rows S-a, S-b, S-c (the startup-exit design v3 R5, R6;
;; M2b2 rulings Q-5 to Q-9). Every hold here waits inside the daemon, which
;; yields while it waits (main sets the sleeper), so the other actors run.
(printf "== F100b M2b2: the daemon's holds ==\n")
(define (wait-until-file p limit-ms)
  (let loop ((k 0))
    (cond ((file-exists? p) #t)
          ((>= (* k 100) limit-ms) #f)
          (else (system "sleep 0.1") (loop (+ k 1))))))
(define (wait-until-text p limit-ms)
  (let loop ((k 0))
    (cond ((> (string-length (text-of-file p)) 0) #t)
          ((>= (* k 100) limit-ms) #f)
          (else (system "sleep 0.1") (loop (+ k 1))))))
(define (touch! p) (call-with-output-file p (lambda (o) (write 'go o)) 'truncate))
;; A thin-client command in the background; its stdout goes to `out`.
(define (client-bg! env args out)
  (system (string-append env " perl -e 'alarm 60; exec @ARGV' scheme --script ../theourgia.sc "
                         (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                         "> " out " 2> " out ".err < /dev/null &")))
(define (first-datum-of p) (let ((ds (datums-of-text (text-of-file p)))) (and (pair? ds) (car ds))))
(define (lines-of-text t)
  (let loop ((cs (string->list t)) (cur '()) (acc '()))
    (cond ((null? cs) (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
          ((char=? (car cs) #\newline) (loop (cdr cs) '() (cons (list->string (reverse cur)) acc)))
          (else (loop (cdr cs) (cons (car cs) cur) acc)))))
;; Each line read on its own, so one line that does not read (a banner, a
;; torn line) cannot hide the lines after it.
(define (line-datums p)
  (let ((ds (map (lambda (l) (guard (e (#t 'UNREADABLE-LINE)) (read (open-string-input-port l))))
                 (lines-of-text (text-of-file p)))))
    (filter (lambda (d) (not (eof-object? d))) ds)))
(define (pid-alive? p) (= 0 (system (string-append "kill -0 " (number->string p) " 2>/dev/null"))))

;; P6-timeout (item 4, G3, E8): a READABLE valid store, the P6 directory
;; setup, the daemon held before its bind. At the budget the client answers
;; timeout naming the pid and the log and LEAVES THE DAEMON ALONE: it is
;; alive, and once released it binds.
(let* ((s (m1-store!)) (run (p6-run-root!)) (kd (key-dir s)) (sock (socket-path s))
       (rel (string-append root "/p6t-release")))
  (system (string-append "mkdir -p " kd " && touch " kd "/.socket.lock"))
  (let* ((r (client-read (string-append "THEOURGIA_INJECT=on THEOURGIA_HOLD=bind:" rel) s))
         (a (cadr r))
         (p (let ((c (and (list? a) (assq 'pid (filter pair? a))))) (and c (cadr c))))
         (alive (and (integer? p) (pid-alive? p)))
         (is-daemon (and (integer? p) (equal? (daemon-pids s) (list p)))))
    (touch! rel)
    (let* ((bound (wait-until-file sock 5000))
           (stopped (begin (when (integer? p) (system (string-append "kill -TERM " (number->string p))))
                           (let loop ((k 0))
                             (cond ((not (file-exists? sock)) #t)
                                   ((>= k 50) #f)
                                   (else (system "sleep 0.1") (loop (+ k 1))))))))
      (restore-run!)
      ;; THE BUDGET IS MEASURED (M2b2 review r1, F2): the answer comes after
      ;; the 10 s budget and not long after it.
      (want "P6-timeout a daemon held before its bind: at the 10 s budget (kind timeout) (pid p) (log <key>/serve.log), rc 75; p is the daemon, alive; released, it binds; TERM removes the socket"
            (list (car r) a (and (>= (cadddr r) 10000) (< (cadddr r) 15000)) alive is-daemon bound stopped)
            (list 75 (list 'error 'serve-start-failed '(kind timeout) (list 'pid p) (list 'log (string-append kd "/serve.log")))
                  #t #t #t #t #t)))))

;; PID3: the P6-unreadable store, the daemon held before its report's one
;; write and stopped by SIGKILL there, so no report carries the token. The
;; kill waits for the hold's marker, not a fixed second, so the daemon is
;; known to be at the hold when it is stopped.
(let* ((st (p6-store! #t)) (s (car st))
       (never (string-append root "/pid3-never")) (out (string-append root "/pid3.out")))
  (client-bg! (string-append "THEOURGIA_INJECT=on THEOURGIA_HOLD=report-write:" never)
              (list "read" "root" "--store" s) out)
  (let* ((held (wait-until-file (string-append never ".held") 10000))
         (pids (daemon-pids s)))
    (for-each (lambda (p) (system (string-append "kill -KILL " (number->string p)))) pids)
    (let* ((answered (wait-until-text out 10000))
           (a (first-datum-of out)))
      (p6-done! s)
      (want "PID3 a daemon stopped by SIGKILL while held before its report: (kind exited) (signal 9) (attempt T)"
            (list held (length pids) answered (mask-attempt a))
            (list #t 1 #t '(error serve-start-failed (kind exited) (signal 9) (attempt <16-hex>)))))))

;; AG-a (D15, G8): a valid store with the P6 directory setup; the store
;; process raises at its entry before any report, so main hears only the
;; DOWN while STARTING. The client answers exited with the status and its
;; token; serve.log holds the store-actor-down line and no report carrying
;; the token.
;; THE TRACE ROW MEASURES WHICH BRANCH WAS TAKEN, NOT THE SKIPPED CLEANUP
;; (M2b2 ruling Q-5; the startup-exit design R7): the STARTING branch traces
;; daemon-down with a two-element subject (store <reason>), the SERVING
;; clause with three (store <handling> <reason>). The cleanup it skips has
;; nothing to release here, so no row can see it.
(let* ((st (p6-store! #t)) (s (car st)) (kd (caddr st)) (lp (string-append kd "/serve.log")))
  (chmod! "644" (string-append s "/meta.sexp"))
  (let* ((r (client-read "THEOURGIA_INJECT=on THEOURGIA_FAULT=store-raise-early@report THEOURGIA_TRACE=1" s))
         (a (cadr r))
         (token (let ((c (and (list? a) (assq 'attempt (filter pair? a))))) (and c (cadr c))))
         (log-ds (line-datums lp))
         (downs (filter (lambda (d) (and (list? d) (= 4 (length d)) (eq? (car d) 'trace) (eq? (cadr d) 'daemon-down)))
                        log-ds)))
    (p6-done! s)
    (want "AG-a a store that raises before any report: (kind exited) (exit 75) with the token, within 4 s; serve.log has store-actor-down and no report carrying the token"
          (list (car r) (mask-attempt a) (< (cadddr r) 4000)
                (and (member '(exiting (reason store-actor-down)) log-ds) #t)
                (and (string? token) (has-substring? (text-of-file lp) (string-append "(attempt \"" token "\")"))))
          (list 75 '(error serve-start-failed (kind exited) (exit 75) (attempt <16-hex>)) #t #t #f))
    ;; The role and the length (M2b2 review r1, F3): the dead actor is named
    ;; as the store, and the subject has the STARTING branch's two elements.
    (want "AG-a the STARTING branch was taken: exactly one daemon-down trace, its subject (store <reason>): the role store, two elements"
          (map (lambda (d) (let ((subj (caddr d))) (and (list? subj) (pair? subj) (list (car subj) (length subj))))) downs)
          '((store 2)))))

;; A-record (PR-16; G5, G7, J1): two actors each held inside its own record
;; scope, the writer's scope ending FIRST. The publish's answer must name its
;; own four entries and nothing of the writer's.
(define (a-record-store!)
  (let* ((s (m1-store!)) (a (m1-insert-a s)))
    (system (string-append "mkdir -p " s "/writers/mirrora1"))
    (list s a)))
(define (a-record-expected s tmp)
  (let ((wd (string-append s "/writers/mirrora1")))
    (list 'error 'incomplete
          (list 'failed '(op dir-fsync) (list 'path wd) '(reason "Input/output error") (list 'errno EIO))
          (list 'written (list (list 'create tmp) (list 'write tmp)
                               (list 'link tmp (string-append wd "/000001.sexp"))
                               (list 'unlink tmp))))))
(define (a-record-tmp a)
  (let ((w (and (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) 'written))) (cdr a)))))
    (and w (pair? (cadr w)) (pair? (car (cadr w))) (cadr (car (cadr w))))))
(let* ((sa (a-record-store!)) (s (car sa)) (a (cadr sa))
       (r1 (string-append root "/arec-r1")) (r2 (string-append root "/arec-r2"))
       (wout (string-append root "/arec-w.out")) (pout (string-append root "/arec-p.out"))
       (env (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=fsync-fail@publish:dir=mirrora1 THEOURGIA_HOLD='write-after-create:"
                           r1 ";publish-after-link:" r2 "'")))
  ;; The daemon starts with the holds and the flush failure armed.
  (client-read env s)
  (client-bg! "" (list "write" a "the text W wrote" "--writer" "draftw1" "--store" s) wout)
  (let* ((held1 (wait-until-file (string-append r1 ".held") 10000))
         (publish-sent (client-bg! "" (list "publish" "mirrora1" "1" m1-seg "--store" s) pout))
         (held2 (wait-until-file (string-append r2 ".held") 10000))
         (r1-made (touch! r1))
         (w-answered (wait-until-text wout 10000))
         (p-pending (= 0 (string-length (text-of-file pout))))
         (r2-made (touch! r2))
         (p-answered (wait-until-text pout 10000))
         (w (first-datum-of wout))
         (p (first-datum-of pout))
         (tmp (a-record-tmp p)))
    (stop-daemon! "A-record" s)
    (want "A-record W held in its scope, the publish held in its own, W resumes and answers FIRST; the publish then answers incomplete with its own four entries only"
          (list held1 held2 w-answered p-pending (and (pair? w) (car w)) p-answered
                (publish-tmp? tmp (string-append s "/writers/mirrora1"))
                (if (and tmp (equal? p (a-record-expected s tmp))) #t p))
          '(#t #t #t #t ok #t #t #t))))
;; The TWIN: the same daemon and store with no holds, W's write completed
;; before the publish starts.
(let* ((sa (a-record-store!)) (s (car sa)) (a (cadr sa))
       (wout (string-append root "/arect-w.out")) (pout (string-append root "/arect-p.out")))
  (client-read "THEOURGIA_INJECT=on THEOURGIA_FAULT=fsync-fail@publish:dir=mirrora1" s)
  (client-bg! "" (list "write" a "the text W wrote" "--writer" "draftw1" "--store" s) wout)
  (let* ((w-answered (wait-until-text wout 10000))
         (publish-sent (client-bg! "" (list "publish" "mirrora1" "1" m1-seg "--store" s) pout))
         (p-answered (wait-until-text pout 10000))
         (w (first-datum-of wout))
         (p (first-datum-of pout))
         (tmp (a-record-tmp p)))
    (stop-daemon! "A-record TWIN" s)
    (want "A-record TWIN sequential: W's write answers ok, then the publish answers incomplete with its own four entries"
          (list w-answered (and (pair? w) (car w)) p-answered
                (publish-tmp? tmp (string-append s "/writers/mirrora1"))
                (if (and tmp (equal? p (a-record-expected s tmp))) #t p))
          '(#t ok #t #t #t))))

;; S-a, S-b, S-c (the startup-exit design R5, R6; ruling Q-6): a DIRECT serve
;; with the store held at store-start. The signal is sent while the store is
;; held, and the row reads `(trace signal-remembered <kind> #f)` on stderr
;; BEFORE it creates the release, so the order is read, not assumed. Then the
;; store's outcome decides the exit. A remembered `again` and a remembered
;; `drain` exit alike after startup-failed (R5), so that case has one row.
(define (serve-held! name store sock)
  (let ((rel (string-append root "/" name "-release"))
        (out (string-append root "/" name ".out"))
        (err (string-append root "/" name ".err"))
        (rcf (string-append root "/" name ".rc")))
    ;; THE WRAPPER'S OWN ARGV MUST NOT NAME THE STORE: daemon-pids finds the
    ;; daemon by `theourgiad.sc serve <store>` in a process's argv, and an
    ;; `sh -c '<command>'` carrying that text would be signalled too. The
    ;; command goes in a file and sh is given the file.
    (let ((script (string-append root "/" name ".sh")))
      (call-with-output-file script
        (lambda (o)
          (put-string o (string-append
                          "THEOURGIA_INJECT=on THEOURGIA_TRACE=1 THEOURGIA_HOLD=store-start:" rel
                          " perl -e 'alarm 60; exec @ARGV' scheme --script ../theourgiad.sc serve " (quoted store)
                          " --socket " (quoted sock) " > " out " 2> " err " < /dev/null\necho $? > " rcf "\n")))
        'truncate)
      (system (string-append "sh " script " &")))
    (list rel out err rcf)))
(define (trace-kinds err)
  (let ((ds (line-datums err)))
    (map caddr (filter (lambda (d) (and (list? d) (= 4 (length d)) (eq? (car d) 'trace)
                                        (eq? (cadr d) 'signal-remembered)))
                       ds))))
(define (wait-for-kinds err ok? limit-ms)
  (let loop ((k 0))
    (cond ((ok? (trace-kinds err)) #t)
          ((>= (* k 100) limit-ms) #f)
          (else (system "sleep 0.1") (loop (+ k 1))))))
(define (just-drain? ks) (equal? ks '(drain)))
;; After a second TERM the signal watcher sends `again` ONCE (F112), so the
;; trace is exactly drain, again. It used to be sent on every poll.
(define (drain-then-again? ks) (equal? ks '(drain again)))
(define (term! store) (for-each (lambda (p) (system (string-append "kill -TERM " (number->string p)))) (daemon-pids store)))
(define (finish-held! h)
  (touch! (car h))
  (wait-until-text (cadddr h) 15000)
  (let ((t (text-of-file (cadddr h)))) (and (> (string-length t) 0) (read (open-string-input-port t)))))
(define (heads out) (map (lambda (d) (and (pair? d) (car d))) (datums-of-text (text-of-file out))))
;; THE SIGNAL IS NOT ACTED ON WHILE STARTING (M2b2 reviews r1 F1, r2 F1):
;; after the trace and before the release, the daemon
;; - has written NOTHING on stdout (a drain entered early prints its
;;   draining line);
;; - is still alive and has not exited, after a settle LONGER than the drain
;;   budget (drain-budget-ms, 5000): a drain armed early, even silently,
;;   would end in drain-timeout 75 inside it, and an exit at once would land
;;   at the start of it.
(define (still-held? store h)
  (system "sleep 6")
  (and (= 0 (string-length (text-of-file (cadr h))))
       (pair? (daemon-pids store))
       (= 0 (string-length (text-of-file (cadddr h))))))

(let* ((s (m1-store!)) (d (m2-sock-dir! "sa")) (sock (string-append d "/sock"))
       (h (serve-held! "sa" s sock))
       (held (wait-until-file (string-append (car h) ".held") 10000))
       (seen (begin (term! s) (wait-for-kinds (caddr h) just-drain? 5000)))
       (released-before (not (file-exists? (car h))))
       (waiting (still-held? s h))
       (rc (finish-held! h)))
  (want "S-a one TERM while the store is held: signal-remembered drain is traced BEFORE the release and the daemon is still waiting then; after ready it drains at once with no listener: draining then drained, rc 0, no socket"
        (list held seen released-before waiting rc (heads (cadr h))
              (datums-of-text (text-of-file (cadr h))) (file-exists? sock))
        (list #t #t #t #t 0 '(draining exiting) '((draining (in-flight 0)) (exiting (reason drained))) #f)))

(let* ((s (m1-store!)) (d (m2-sock-dir! "sb")) (sock (string-append d "/sock"))
       (h (serve-held! "sb" s sock))
       (held (wait-until-file (string-append (car h) ".held") 10000))
       (seen1 (begin (term! s) (wait-for-kinds (caddr h) just-drain? 5000)))
       (seen2 (begin (term! s) (wait-for-kinds (caddr h) drain-then-again? 5000)))
       (waiting (still-held? s h))
       ;; READ AGAIN AFTER THE SETTLE: a watcher that resent `again` would
       ;; show a third kind by now, although the first reading saw two.
       (kinds-after (trace-kinds (caddr h)))
       (rc (finish-held! h)))
  (want "S-b two TERMs while the store is held: exactly drain then again (sent once) traced BEFORE the release and the daemon is still waiting then; after ready: second-signal, rc 75, no listener, no socket"
        (list held seen1 seen2 waiting kinds-after rc (datums-of-text (text-of-file (cadr h))) (file-exists? sock))
        (list #t #t #t #t '(drain again) 75 '((exiting (reason second-signal))) #f)))

(let* ((s (m1-store!)) (d (m2-sock-dir! "sc")) (sock (string-append d "/sock")))
  (system (string-append "touch " d "/.sock.lock"))
  (chmod! "000" s)
  (let* ((h (serve-held! "sc" s sock))
         (held (wait-until-file (string-append (car h) ".held") 10000))
         (seen (begin (term! s) (wait-for-kinds (caddr h) just-drain? 5000)))
         (waiting (still-held? s h))
         (rc (finish-held! h)))
    (chmod! "755" s)
    (want "S-c one TERM while a store at 000 is held: drain traced BEFORE the release and the daemon is still waiting then; after startup-failed: main's report, then store-actor-down, rc 75"
          (list held seen waiting rc (datums-of-text (text-of-file (cadr h))) (file-exists? sock))
          (list #t #t #t 75
                (list (list 'error 'unreadable (list 'path (string-append s "/meta.sexp")) (list 'reason denied) '(errno EACCES) '(attempt #f))
                      '(exiting (reason store-actor-down)))
                #f))))

;; F (F110, point 8): theourgiad's report channel after --detach is the log.
;; A thin client's first start detaches a daemon whose serve.log begins with
;; its serving line, and on this success that line carries no written,
;; client-written or attempt clause (serving is not a startup report).
(let* ((s (m1-store!))
       (r (client-read "" s))
       (lp (string-append (key-dir s) "/serve.log"))
       (first (let ((ds (filter pair? (line-datums lp)))) (and (pair? ds) (car ds)))))
  (stop-daemon! "F detach" s)
  (want "F a detached start's serve.log: its first datum is serving, with no written, client-written or attempt clause; the thin answer carries none either"
        (list (and (pair? first) (car first))
              (and (pair? first) (or (clause-of first 'written) (clause-of first 'client-written) (clause-of first 'attempt)) #t)
              (carries-a-record? (cadr r)))
        '(serving #f #f)))

(want "the daemons M1's rows started were one process each, by the exact argv line"
      daemon-counts (map (lambda (c) (cons (car c) 1)) daemon-counts))
(want "no daemon of a store this section made is left running"
      (apply + (map (lambda (s) (length (daemon-pids s))) m1-stores))
      0)
;; THE ROW ABOVE DETECTS; THIS CLEANS (F100b M1 review r2, F5): whatever
;; it found is stopped here, so a red reading does not also leave a
;; process behind.
(for-each (lambda (s) (unless (null? (daemon-pids s)) (stop-daemon! 'sweep s))) m1-stores)
(system (string-append "rm -rf " m1-run-root))

(system (string-append "chmod -R u+rwx " root " 2>/dev/null; rm -rf " root))
(printf "\n~a failures\nrows: ~a\nunreadable-routes complete\n" bad rows)
