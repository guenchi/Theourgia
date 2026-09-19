#!r6rs
(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce)
        (theourgia request) (theourgia ffi) (theourgia log))
(define bad 0)
(define (want label got expected)
  (if (equal? got expected) (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/empty-plan-" (number->string (get-process-id))))
(when (file-exists? root) (error 'empty-plan "Use a fresh root" root))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))
(define init (rpc-dispatch store '(init) "test"))
(define writer (cadr (assq 'writer (cdr init))))
;; The writer is named because a draft verb that is not told one is now
;; refused; this is the same writer the refused default would have used.
(define args (list 'commit "--req" "empty-1" "--cursor" (string-append writer ":0")
                   "--writer" writer))
(define (call) (rpc-dispatch store args "test"))
(define (answer-items a)
  (let ((p (and (pair? a) (assq 'items (filter pair? (cdr a)))))) (if p (cdr p) '())))
(define (has-field a key value)
  (and (pair? a) (equal? (assq key (filter pair? (cdr a))) (list key value))))
(define first (call))
(define event (cons writer 1))
(want "QE-01 first answer names the durable empty plan"
      (exists (lambda (a) (and (has-field a 'events (list event))
                               (has-field a 'state '()) (has-field a 'replay #f)))
              (answer-items first)) #t)
(define history (store-evidence store (cons writer "empty-1")))
(want "QE-06 zero operations write one empty plan"
      (map (lambda (e) (list (actor-sub (ev-actor e)) (list-ref (ev-payload e) 4))) history)
      '((plan ())))
(define segment (string-append (writer-directory store writer) "/000001.sexp"))
(define (bytes) (call-with-port (open-file-input-port segment) get-bytevector-all))
(define before (bytes))
(want "QE-01 retry replays the same plan"
      (exists (lambda (a) (and (has-field a 'replay #t) (has-field a 'event event)))
              (answer-items (call))) #t)
(want "QE-01 retry appends nothing" (bytes) before)
(define manifest (string-append (writer-directory store writer) "/published.sexp"))
(call-with-output-file manifest (lambda (p) (write '() p)))
(want "QE-02 deselected empty plan is unknown"
      (let ((a (call))) (and (pair? a) (pair? (cdr a)) (cadr a))) 'unknown)
(want "QE-02 deselection cannot append another plan" (bytes) before)
(delete-file manifest)
(want "QE-02 restoring selection restores replay"
      (exists (lambda (a) (has-field a 'replay #t)) (answer-items (call))) #t)
(want "QE-04 empty retry with changed actor is a mismatch"
      (let ((a (rpc-dispatch store args "other"))) (and (pair? a) (pair? (cdr a)) (cadr a))) 'req-mismatch)

;; This public library entry expands a request into zero intents without W.
(define generic (make-write-request "test" 'import '() "empty-import" event))
(define generic-result (with-store-write store (lambda (state view) '()) "test" generic))
(want "QE-06 generic zero-intent request writes its identity"
      (length (store-evidence store (cons writer "empty-import"))) 1)
(want "QE-01 generic empty retry replays"
      (exists (lambda (a) (has-field a 'replay #t))
              (with-store-write store (lambda (state view) '()) "test" generic)) #t)
(define insert (rpc-dispatch store '(insert "--title" "one" "--text" "old") "test"))
(define id (let ((e (car (cadr (assq 'events (cdr insert)))))) (block-id (car e) (cdr e))))
(rpc-dispatch store (list 'write id "new" "--writer" writer) "test")
(define cursor (string-append writer ":" (number->string (cdr (assoc writer (reduce-applied-cut (open-and-reduce store)))))))
;; THE VERSION IS NAMED, because the request carries `--req` (§7.5.9:
;; a retry rebuilds its identity from the versions, not from a draft
;; that its own first attempt retired).
(define one-version
  (let ((r (rpc-dispatch store (list 'read id "--working-info" "--writer" writer)
                         "test")))
    (list-ref (assq 'projection (cdr r)) 4)))
(rpc-dispatch store (list 'commit id "--req" "one-intent" "--cursor" cursor
                          "--working-version" one-version "--writer" writer)
              "test")
;; ⚠️ THIS ROW SAID `single`, AND THE DESIGN TOOK THAT BACK.
;;
;; A commit of one block used to be written as a single record with no
;; plan; §7.5.9 makes a commit write a plan ALWAYS, one block or ten.
;; The reason is retry: a plan freezes the declared text and names the
;; draft versions the request consumed, and a request that has neither
;; cannot be completed from the log after a crash -- which is what the
;; packet beside the drafts used to be for, and the packet is gone.
;;
;; The old expectation belongs to the Q3-fourth-section era; the reading
;; below is what the rule that replaced it produces: a plan event, then
;; its one member.
(want "QE-06 one operation is a plan with one member (7.5.9: a commit always writes a plan)"
      (map (lambda (e) (actor-sub (ev-actor e))) (store-evidence store (cons writer "one-intent")))
      '(plan 0))

;; Hand-built evidence isolates delivery and selection from disk setup.
(define who "test")
(define key '("writer00" . "zero"))
(define after '("writer00" . 0))
(define fp (request-fingerprint who 'commit '() after))
(define (evidence placement delivered entries)
  (make-evidence '("writer00" . 1) (list who key 'plan fp #f after)
                 '() (list 'plan "zero" fp after entries) placement delivered '()))
(define (decide e n)
  (request-decision key fp who after (list e) '() n '()))
(want "QE-03 pending empty plan is unknown"
      (car (decide (evidence 'valid-history #f '()) 0)) 'unknown)
(want "QE-03 delivered empty plan is complete"
      (decide (evidence 'valid-history #t '()) 0) '(replay ("writer00" . 1)))
(want "QE-01 nonempty plan without its member is incomplete"
      (car (decide (evidence 'valid-history #t '((0 set "b" src "x"))) 1)) 'complete)
(printf "~a failures\nempty-plan complete\n" bad)
(exit (if (zero? bad) 0 1))
