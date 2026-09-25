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
        (only (theourgia wire) encode-record))

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
(define unwritable-source "(raise (cons (string->symbol \"error\") (lambda () #f)))")
(let* ((s (fresh-store!))
       (dir (writer-directory s M)))
  (want "CONTROL on a healthy store a raised pair holding a procedure answers eval-worker-exit with no clause"
        (let ((a (eval-answer s unwritable-source))) (list (head-of a) (incomplete-dirs a)))
        '((error eval-worker-exit) no-clause))
  (chmod! "000" dir)
  (let ((a (eval-answer s unwritable-source)))
    (chmod! "700" dir)
    (want "the same source after the worker's load met the unreadable mirror answers eval-worker-exit and names the mirror"
          (list (head-of a) (incomplete-dirs a))
          (list '(error eval-worker-exit) (list dir)))))

(system (string-append "chmod -R u+rwx " root " 2>/dev/null; rm -rf " root))
(printf "\n~a failures\nrows: ~a\nunreadable-routes complete\n" bad rows)
