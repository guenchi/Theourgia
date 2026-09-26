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
        (only (theourgia client) call! envelope-version)
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
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

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
(let* ((s (fresh-store!))
       (sock (string-append sock-base "/ur-" (number->string (get-process-id)) ".sock"))
       (out (string-append root "/start.out")))
  (chmod! "000" (writers-of s))
  (let* ((rc (system (string-append "perl -e 'alarm 60; exec @ARGV' scheme --script ../theourgiad.sc serve "
                                    s " --socket " sock " > " out " 2> /dev/null < /dev/null")))
         (said (guard (e (#t '()))
                 (call-with-port (open-input-file out)
                   (lambda (p) (let loop ((acc '()))
                                 (let ((x (read p))) (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))))
    (chmod! "700" (writers-of s))
    (want "F79-3 a daemon started on a store whose writers/ cannot be listed names writers/ and leaves as a failed start does: exit 75, store-actor-down, no socket"
          (list (and (pair? said) (names-writers? (car said) s))
                (and (pair? said) (pair? (cdr said)) (cadr said))
                rc
                (file-exists? sock))
          (list #t '(exiting (reason store-actor-down)) 75 #f))))

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
         (filter (lambda (l) (has-substring? l needle)) (lines-of-command "ps -axo pid=,command=")))))
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
      (want "CONTROL P2b the dot spelling, the parent readable, is the same store: the core answers"
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
  (read (open-string-input-port "(usage (commit (<block> ...) (\"--writer\" <name>) (\"--working-version\" <block>=<version>)))")))
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
