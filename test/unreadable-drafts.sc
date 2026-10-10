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

;; Draft storage this store cannot read is not a writer with no drafts.
;;
;; Measured on the tree before this change, with the writer's working/
;; directory at mode 000: `read --working` answered the committed text, as
;; if there were no draft; `drafts` answered working-unavailable with an
;; unformatted message and no reason; a commit of an explicit block answered
;; no-draft. And with draft.lock at 000, a commit whose records had landed
;; answered working-unavailable -- a durable commit reported as a failure.
;;
;; The rules (F77 R1a, working drafts and post-commit cleanup): the read
;; failure is recorded and handed to preflight, so a replay is still
;; recognised first; an explicit-id commit with no request asks for that
;; failure before it answers no-draft; and the cleanup after a commit keeps
;; the committed answer and adds (cleanup-failed (path ...) (reason ...)).

(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce)
        (only (theourgia log) writer-directory))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (let ((ok (equal? got expected)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expected)))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e)
                                     e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expected)
     (begin (set! rows (+ rows 1))
            (with-expected label expected (x) (want-1 label (caught got) (caught x)))))))

;; THE SCRATCH ROOT FOLLOWS THEOURGIA_TEST_ROOT, and a directory already
;; there is refused rather than reused (F71).
(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                   (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/unreadable-drafts-" (number->string (get-process-id))))
(when (file-exists? root)
  (assertion-violation 'unreadable-drafts "scratch directory already exists" root))
(system (string-append "mkdir -p " root))

(define n 0)
;; `(store writer)`, each in a directory and machine home of its own.
(define (fresh-store!)
  (set! n (+ n 1))
  (let ((d (string-append root "/s" (number->string n))))
    (system (string-append "mkdir -p " d "/home"))
    (putenv "THEOURGIA_HOME" (string-append d "/home"))
    (let* ((store (string-append d "/store"))
           (init (rpc-dispatch store '(init) "test")))
      (list store (cadr (assq 'writer (cdr init)))))))
(define draft-verbs '(write restore drafts discard commit))
(define (call s w . args)
  (rpc-dispatch s
                (if (or (memq (car args) draft-verbs)
                        (and (eq? (car args) 'read)
                             (or (member "--working" (cdr args)) (member "--working-info" (cdr args)))))
                    (append args (list "--writer" w))
                    args)
                "test"))
(define (insert s w title)
  (let* ((a (call s w 'insert "--title" title "--text" "old"))
         (ev (car (cadr (assq 'events (cdr a))))))
    (block-id (car ev) (cdr ev))))
(define (chmod! mode path) (system (string-append "chmod " mode " " path)))
(define (working-dir s w) (string-append (writer-directory s w) "/working"))
(define (draft-file s w id) (string-append (working-dir s w) "/" id))
(define (cursor-of s w)
  (string-append w ":" (number->string (cdr (assoc w (reduce-applied-cut (open-and-reduce s)))))))
(define (version-of s w id)
  (list-ref (assq 'projection (cdr (call s w 'read id "--working-info"))) 4))
;; The kind of an answer: (error <kind>) or ok.
(define (kind a)
  (cond ((and (pair? a) (eq? (car a) 'error) (pair? (cdr a))) (list 'error (cadr a)))
        ((and (pair? a) (eq? (car a) 'ok)) 'ok)
        (else a)))
(define (field a name)
  (and (pair? a) (list? a)
       (let ((f (find (lambda (x) (and (pair? x) (eq? (car x) name))) (cdr a))))
         (and f (pair? (cdr f)) (cadr f)))))
;; A clause anywhere in the answer, however deep the items nest it.
(define (clause-in a head)
  (cond ((and (pair? a) (eq? (car a) head)) a)
        ((pair? a) (or (clause-in (car a) head) (clause-in (cdr a) head)))
        (else #f)))
(define (text-of s w id)
  (let ((a (call s w 'read id)))
    (and (pair? a) (eq? (car a) 'ok)
         (let ((fs (assq 'fields (cadr a)))) (and fs (let ((t (assq 'src (cdr fs)))) (and t (cdr t))))))))

(printf "== U10: working/ cannot be read ==\n")
(let* ((sw (fresh-store!)) (s (car sw)) (w (cadr sw))
       (A (insert s w "A")))
  (call s w 'write A "draft text")
  (want "CONTROL with working/ readable, read --working answers the draft"
        (call s w 'read A "--working")
        '(ok (text "draft text")))
  (chmod! "000" (working-dir s w))
  (let* ((read (caught (call s w 'read A "--working")))
         (listed (caught (call s w 'drafts)))
         (commit (caught (call s w 'commit A))))
    (chmod! "700" (working-dir s w))
    (want "U10 read --working answers working-unavailable, not the committed text"
          (kind read)
          '(error working-unavailable))
    (want "U10 drafts answers working-unavailable with its reason"
          (list (kind listed) (string? (field listed 'reason)))
          '((error working-unavailable) #t))
    (want "U10 commit of an explicit block with no --req answers working-unavailable, not no-draft"
          (kind commit)
          '(error working-unavailable))
    (want "U10 and that commit wrote nothing: the block still reads its committed text"
          (text-of s w A)
          "old")))

(printf "== U10: a replay is recognised with working/ unreadable ==\n")
(let* ((sw (fresh-store!)) (s (car sw)) (w (cadr sw))
       (A (insert s w "A")))
  (call s w 'write A "draft text")
  (let* ((cursor (cursor-of s w))
         (version (version-of s w A))
         (request (lambda () (call s w 'commit A "--req" "R1" "--cursor" cursor
                                   "--working-version" (string-append A "=" version))))
         (first (request))
         (healthy-replay (request)))
    (want "CONTROL the first commit lands and its retry is a replay"
          (list (kind first) (and (clause-in healthy-replay 'replay) (cadr (clause-in healthy-replay 'replay))))
          '(ok #t))
    (chmod! "000" (working-dir s w))
    (let ((replay (caught (request))))
      (chmod! "700" (working-dir s w))
      (want "U10 with working/ unreadable the same retry is still a replay"
            (list (kind replay) (and (clause-in replay 'replay) (cadr (clause-in replay 'replay))))
            '(ok #t)))))

(printf "== U10: cleanup after a commit that landed ==\n")
;; CONTROL: a commit retires its draft file.
(let* ((sw (fresh-store!)) (s (car sw)) (w (cadr sw))
       (A (insert s w "A")))
  (call s w 'write A "draft text")
  (let ((a (call s w 'commit A)))
    (want "CONTROL a commit lands, retires its draft file, and carries no cleanup clause"
          (list (kind a) (file-exists? (draft-file s w A)) (clause-in a 'cleanup-failed))
          '(ok #f #f))))
;; AT DELETION: working/ readable and searchable, not writable.
(let* ((sw (fresh-store!)) (s (car sw)) (w (cadr sw))
       (A (insert s w "A")))
  (call s w 'write A "draft text")
  (chmod! "500" (working-dir s w))
  (let ((a (caught (call s w 'commit A))))
    (chmod! "700" (working-dir s w))
    (want "U10 precondition: the draft file could not be deleted"
          (file-exists? (draft-file s w A))
          #t)
    (want "U10 cleanup failing at deletion keeps the committed answer and adds cleanup-failed with path and reason"
          (let ((c (clause-in a 'cleanup-failed)))
            (list (kind a)
                  (text-of s w A)
                  (and c (list? c) (assq 'path (cdr c)) #t)
                  (and c (list? c) (assq 'reason (cdr c)) #t)))
          '(ok "draft text" #t #t))))
;; AT LOCK ACQUISITION: draft.lock present and unopenable.
(let* ((sw (fresh-store!)) (s (car sw)) (w (cadr sw))
       (A (insert s w "A"))
       (lock (string-append (writer-directory s w) "/draft.lock")))
  (call s w 'write A "draft text")
  (want "CONTROL the draft lock file exists once a draft has been written"
        (file-exists? lock)
        #t)
  (chmod! "000" lock)
  (let ((a (caught (call s w 'commit A))))
    (chmod! "600" lock)
    (want "U10 precondition: the commit's records landed"
          (text-of s w A)
          "draft text")
    (want "U10 cleanup failing at the lock keeps the committed answer and adds cleanup-failed"
          (list (kind a) (and (clause-in a 'cleanup-failed) #t))
          '(ok #t))))

;; ---- review round 2: restore, discard, and a tracked commit that landed ----
;; ADDED BY THE CODE SESSION (F77b review 2, the main session's request).
(define (lookup-any name)
  (let loop ((libs '((theourgia store) (theourgia reduce) (theourgia log) (theourgia wire) (theourgia request))))
    (cond ((null? libs) #f)
          ((guard (e (#t #f)) (eval name (environment (car libs)))))
          (else (loop (cdr libs))))))
(printf "== review 2: restore and discard over a draft that cannot be read ==\n")
(let* ((sw (fresh-store!)) (s (car sw)) (w (cadr sw))
       (A (insert s w "A")))
  ;; A REVOKED VERSION TO RESTORE, as revoke-restore.sc makes one: commit a
  ;; draft, then publish a rival plan record that conflicts with the plan.
  (call s w 'write A "the text I wrote")
  (let* ((v1 (version-of s w A))
         (committed (call s w 'commit A "--req" "R1" "--cursor" (cursor-of s w)
                          "--working-version" (string-append A "=" v1)))
         (evidence ((lookup-any 'store-evidence) s (cons w "R1")))
         (plan-ev (find (lambda (e) (eq? 'plan ((lookup-any 'actor-sub) ((lookup-any 'ev-actor) e)))) evidence))
         (rival ((lookup-any 'encode-record) 1 1789000000001 ((lookup-any 'ev-actor) plan-ev) '()
                 ((lookup-any 'storable-encode) ((lookup-any 'ev-payload) plan-ev)))))
    ((lookup-any 'log-publish!) s "rivalzzz" 1 rival ((lookup-any 'segment-sha) rival))
    (call s w 'write A "a later draft")
    (chmod! "000" (draft-file s w A))
    (let ((restored (caught (call s w 'restore v1))))
      (chmod! "600" (draft-file s w A))
      (want "REVIEW2 restore over a draft this process cannot read answers working-unavailable, and the draft is not replaced"
            (list (kind committed) (kind restored)
                  (let ((r (call s w 'read A "--working"))) (and (pair? r) (list? r) (assq 'text (cdr r)) (cadr (assq 'text (cdr r))))))
            '(ok (error working-unavailable) "a later draft")))))
(let* ((sw (fresh-store!)) (s (car sw)) (w (cadr sw))
       (A (insert s w "A")))
  (call s w 'write A "draft text")
  (chmod! "000" (draft-file s w A))
  (let ((discarded (caught (call s w 'discard A))))
    (chmod! "600" (draft-file s w A))
    (want "REVIEW2 discard of a draft this process cannot read answers working-unavailable, and the draft is still there"
          (list (kind discarded) (file-exists? (draft-file s w A)))
          '((error working-unavailable) #t))))
(printf "== review 2: a tracked commit that landed keeps its answer ==\n")
(let* ((sw (fresh-store!)) (s (car sw)) (w (cadr sw))
       (A (insert s w "A"))
       (lock (string-append (writer-directory s w) "/draft.lock")))
  (call s w 'write A "draft text")
  (call s w 'discard A)
  (chmod! "000" lock)
  (let ((a (caught (call s w 'commit "--req" "R-empty" "--cursor" (cursor-of s w)))))
    (chmod! "600" lock)
    (want "REVIEW2 a tracked commit of no drafts lands its empty plan and keeps ok, carrying cleanup-failed for the lock"
          (list (kind a) (and (clause-in a 'cleanup-failed) #t))
          '(ok #t))))

(printf "rows: ~a\n" rows)
(printf "~a failures\n" bad)
(system (string-append "chmod -R u+rwX " root " 2>/dev/null"))
;; THE SENTINEL THE RUNNER READS: without it a fixture is red whatever its
;; rows say (run-fixtures.sh counts "<name> complete").
(printf "unreadable-drafts complete\n")
(exit (if (= bad 0) 0 1))
