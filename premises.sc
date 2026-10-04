#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;     http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; (theourgia premises) -- what a reader was given, checked when its result
;;; is accepted.
;;;
;;; A committed write may carry `--premises <datum>`: a list of forms, each
;;;   (premise <id> <hash>)                a block at the version read
;;;   (premise (query <goal>) <digest>)    a query's answer as read
;;; and, so that a receipt is given back as it was received, the clauses a
;;; read answers: (versions ((<id> . <hash>) ...)), each pair a block
;;; premise; (cut <alist>), kept to say who changed a block since; and
;;; (receipt <form> ...), holding forms of this list.
;;;
;;; THE SET IS READ ONCE, BEFORE ANYTHING ELSE: a form the store cannot check
;;; is refused, never accepted as checked, and two versions named for one
;;; block are refused as inconsistent. What it compiles to is a procedure of
;;; a state, asked by the write as its preflight -- under the store's lock,
;;; before the first record of a fresh request -- and answering #f when every
;;; premise holds, or a refusal naming every one that does not, in the order
;;; given. The store loads this library only for a request that carries
;;; premises (store.sc, premises-preflight).
(library (theourgia premises)
  (export premises-compile premises-faces premises-check-calls)
  (import (rnrs)
          (only (theourgia reduce) state-read block-hash state-field-events state-edges cut-covers?)
          (only (theourgia wire) string->sexpr-extended sexpr->string-extended)
          (only (theourgia rpc) dispatch-helper)
          (only (theourgia query) make-query-session session-answer query-budget-default))

  ;; HOW MANY TIMES A COMPILED CHECK WAS ASKED, in this process: a fixture
  ;; reads it beside each answer to tell a check that ran from one that did
  ;; not.
  (define calls 0)
  (define (premises-check-calls) calls)

  (define (spelling x) (guard (e (#t "<unprintable>")) (sexpr->string-extended x)))
  (define (not-understood form)
    (raise (list 'error 'bad-request 'premise-not-understood (list 'form (spelling form)))))

  ;; -> (values <block premises: (id . hash), in order> <query premises: (goal . digest), in order> <cut or #f>),
  ;; or raises the refusal. A block premise named twice with one version is
  ;; one premise; with two it is refused.
  (define (read-set text)
    (let ((datum (guard (e (#t (not-understood text))) (string->sexpr-extended text)))
          (premises '())
          (cut #f))
      (define (add! p) (set! premises (cons p premises)))
      (define (hash? h) (and (string? h) (> (string-length h) 0)))
      (define (cut? c) (and (list? c) (for-all (lambda (e) (and (pair? e) (string? (car e)) (integer? (cdr e)) (exact? (cdr e)))) c)))
      (define (form! f)
        (cond
          ((and (list? f) (= (length f) 3) (eq? (car f) 'premise) (string? (cadr f)) (hash? (caddr f)))
           (add! (list 'block (cadr f) (caddr f))))
          ((and (list? f) (= (length f) 3) (eq? (car f) 'premise)
                (list? (cadr f)) (= (length (cadr f)) 2) (eq? (car (cadr f)) 'query) (hash? (caddr f)))
           (add! (list 'query (cadr (cadr f)) (caddr f))))
          ((and (list? f) (= (length f) 2) (eq? (car f) 'versions) (list? (cadr f))
                (for-all (lambda (v) (and (pair? v) (string? (car v)) (hash? (cdr v)))) (cadr f)))
           (for-each (lambda (v) (add! (list 'block (car v) (cdr v)))) (cadr f)))
          ((and (list? f) (= (length f) 2) (eq? (car f) 'cut) (cut? (cadr f)))
           (if (and cut (not (equal? cut (cadr f))))
               (raise (list 'error 'bad-request 'premise-inconsistent (list 'cuts cut (cadr f))))
               (set! cut (cadr f))))
          ((and (list? f) (pair? f) (eq? (car f) 'receipt))
           (for-each form! (cdr f)))
          (else (not-understood f))))
      (unless (list? datum) (not-understood datum))
      (for-each form! datum)
      ;; ONE VERSION PER BLOCK: the same pair twice is one premise, two
      ;; versions for one block are refused before anything is read.
      (let loop ((ps (reverse premises)) (seen '()) (out '()))
        (cond
          ((null? ps) (values (reverse out) cut))
          ((eq? (car (car ps)) 'block)
           (let* ((id (cadr (car ps))) (h (caddr (car ps))) (prior (assoc id seen)))
             (cond ((not prior) (loop (cdr ps) (cons (cons id h) seen) (cons (car ps) out)))
                   ((equal? (cdr prior) h) (loop (cdr ps) seen out))
                   (else (raise (list 'error 'bad-request 'premise-inconsistent
                                      (list 'block id) (list 'versions (cdr prior) h)))))))
          (else (loop (cdr ps) seen (cons (car ps) out)))))))

  ;; A block's version now: its hash, or `deleted`, `unknown`, or
  ;; (unavailable (reason ...)) for one that cannot be hashed.
  (define (current-version state id)
    (let ((row (state-read state id)))
      (cond ((not row) 'unknown)
            ((cdr (assq 'deleted row)) 'deleted)
            (else (guard (e (#t (list 'unavailable (list 'reason (if (message-condition? e) (condition-message e) "hash")))))
                    (block-hash state id))))))

  ;; WHO CHANGED IT SINCE THE READ: the events of the block's surviving field
  ;; candidates and of its outgoing links that the cut does not cover, sorted.
  (define (since state cut id)
    (let* ((events (append (state-field-events state id)
                           (apply append (map (lambda (e) (cdddr e))
                                              (filter (lambda (e) (equal? (car e) id)) (state-edges state))))))
           (later (filter (lambda (ev) (not (cut-covers? cut (list ev)))) events))
           (unique (let loop ((l later) (out '())) (cond ((null? l) out) ((member (car l) out) (loop (cdr l) out)) (else (loop (cdr l) (cons (car l) out)))))))
      (list-sort (lambda (a b) (or (string<? (car a) (car b)) (and (string=? (car a) (car b)) (< (cdr a) (cdr b))))) unique)))

  ;; -> a procedure of a state: #f when every premise holds, else the refusal.
  ;; STORE gives a query premise the search verb's keyword hook, so `score`
  ;; answers as `search` does.
  (define (premises-compile text store)
    (let-values (((premises cut) (read-set text)))
      (lambda (state)
        (set! calls (+ calls 1))
        (let ((session #f))
          (define (S) (or session (begin (set! session (make-query-session state '() query-budget-default
                                                                            ((dispatch-helper 'keyword-hook) store)))
                                         session)))
          (let loop ((ps premises) (failed '()))
            (if (null? ps)
                (and (pair? failed) (cons* 'error 'premise-changed (reverse failed)))
                (let ((p (car ps)))
                  (case (car p)
                    ((block)
                     (let ((now (current-version state (cadr p))))
                       (if (equal? now (caddr p))
                           (loop (cdr ps) failed)
                           (loop (cdr ps)
                                 (cons (append (list 'block (cadr p) (list 'expected (caddr p)) (list 'current now))
                                               (if (and cut (not (eq? now 'unknown))) (list (cons 'since (since state cut (cadr p)))) '()))
                                       failed)))))
                    (else
                     (let ((a (session-answer (S) (cadr p))))
                       (cond
                         ((not (eq? (car a) 'ok))
                          (list 'error 'premise-unevaluable (list 'query (cadr p)) (cons 'reason (cdr a))))
                         ((equal? (cadddr a) (caddr p)) (loop (cdr ps) failed))
                         (else (loop (cdr ps)
                                     (cons (list 'query (cadr p) (list 'expected (caddr p)) (list 'current (cadddr a)))
                                           failed))))))))))))))
;; THE THREE FACES OF ONE COMPILATION, for store.sc's premises-preflight:
  ;;   (premises-faces store text own raise-answer) -> gate, finish, run
  ;; THE GATE goes to with-store-write: its check asks the premises first and
  ;; then OWN; its enter is called once the write's session has begun; its
  ;; also composes a site's own check after it, keeping the same entry.
  ;; RUN takes the site's body: a raise after the write was entered and
  ;; before the check was asked is answered by RAISE-ANSWER, as the
  ;; dispatcher would answer it, and given to FINISH, so it says the premises
  ;; were not examined; any other raise goes on as it came. A lock that
  ;; cannot be taken is a refusal before entry.
  ;; FINISH takes the answer the call site would give and returns the one it
  ;; gives: the premise refusal itself when the check refused (a wrapping
  ;; verb would otherwise bury it); the answer as it is when the check was
  ;; called and passed; and when the check was NOT called -- a replay, the
  ;; completion of a plan already durable, a verdict that refused before any
  ;; write -- the answer with `(premises not-checked (reason replay |
  ;; not-fresh))`, so a premise set is never left unexamined without the
  ;; answer saying so.
  ;; KEY: A CALL SITE HANDS FINISH ONLY THE ANSWERS OF A REQUEST THAT
  ;; ENTERED THE WRITE (or commit's arm that writes nothing). A refusal made
  ;; before it wrote nothing and will write nothing, and carries no clause.
  (define (premises-faces store text own raise-answer)
    (let ((check (premises-compile text store))
          (called #f)
          (entered #f)
          (refusal #f))
      (define (finish answer)
        (cond
          (refusal refusal)
          (called answer)
          ((not (and (pair? answer) (list? answer))) answer)
          (else
           (append answer
                   (list (list 'premises 'not-checked
                               (list 'reason (if (marks-replay? answer 3) 'replay 'not-fresh))))))))
      (define (gate-of checker)
        (vector 'premises-gate checker (lambda () (set! entered #t))
                (lambda (more) (gate-of (lambda (state) (or (checker state) (more state)))))))
      (values
        (gate-of (lambda (state)
                   (set! called #t)
                   (let ((r (check state)))
                     (if r
                         (begin (set! refusal r) r)
                         (and own (own state))))))
        finish
        (lambda (thunk)
          (guard (e ((and entered (not called)) (finish (raise-answer e))))
            (thunk))))))

  ;; An answer is a replay's when it, or an answer it holds (a batch's items,
  ;; an import's items), carries `(replay #t)`.
  (define (marks-replay? x depth)
    (and (> depth 0) (pair? x) (list? x)
         (or (and (member '(replay #t) (cdr x)) #t)
             (exists (lambda (y) (marks-replay? y (- depth 1))) x))))
)
