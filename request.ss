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

;; Request identity, and what the records say about it.
;;
;; EVERY REQUEST HAS AN EXPLICIT ANSWER AND THERE IS NO SILENT SUCCESS.
;; A client that does not hear one retries, and the store has to be able
;; to say which of three things happened: it never ran, it ran and here
;; is what it did, or -- the answer nobody likes and everybody needs --
;; it may have run and this store cannot tell.
;;
;; IDENTITY IS `(origin-writer . req-id)`, AND IT IS CARRIED, NOT
;; INFERRED. Every record a request writes names it in the actor, so any
;; one record answers "did this run". Inferring the origin from the
;; physical writer would be wrong for every record a successor carries
;; after an adopt -- which is exactly the situation in which the question
;; gets asked.
;;
;; THE FINGERPRINT COVERS WHO ASKED, WHAT THEY ASKED, AND WHAT THEY ASKED
;; IT OF. A retry is the same identity AND the same fingerprint; the same
;; identity with a different fingerprint is a client reusing an id for a
;; different request, which is a mistake to report rather than a history
;; to reconstruct. `after` is inside the fingerprint because the same
;; words against a different cursor are a different request.
(library (theourgia request)
  (export request-fingerprint request-identity identity=?
          req-id-ok? actor-identity actor-fingerprint actor-sub
          actor-plan-event actor-after request-actor?
          make-evidence ev-event ev-actor ev-deps ev-payload ev-placement
          ev-delivered? ev-marks ev-marked?
          comparison-set executed-member-set historical-set membership
          duplicate-slots plan-conflicts present-indices prefix?
          request-decision request-gates plan? store-supplied-fields
          receipt? receipt-req receipt-fingerprint receipt-after
          receipt-entries receipt-item-after receipt-covers?
          receipt-duplicate batch-item-state batch-state bind-new created-id
          intent-produced?
          resolution? resolution-target resolution-fingerprint resolution-sub
          resolution-verdict resolution-interval resolution-supersedes
          resolution-evidence resolution-state resolution-problems)
  (import (rnrs base) (rnrs control) (rnrs lists) (rnrs sorting) (rnrs unicode)
          (rnrs bytevectors) (rnrs io ports) (rnrs exceptions)
          (only (theourgia wire) sexpr->string-extended)
          (only (theourgia digest) sha256 bytevector->hex)
          (only (theourgia trace) trace-event!))

  ;; 1 to 64 characters of [A-Za-z0-9._-], or a batch item naming the
  ;; batch and its index. The set is small on purpose: a request id
  ;; travels in a file name and in an actor, and a character that needs
  ;; quoting in either is a character that will one day be quoted in only
  ;; one of them.
  (define (req-id-string? x)
    (and (string? x)
         (<= 1 (string-length x) 64)
         (let loop ((i 0))
           (or (= i (string-length x))
               (let ((c (string-ref x i)))
                 (and (or (char<=? #\a c #\z) (char<=? #\A c #\Z)
                          (char<=? #\0 c #\9)
                          (char=? c #\.) (char=? c #\_) (char=? c #\-))
                      (loop (+ i 1))))))))

  (define (req-id-ok? x)
    (or (req-id-string? x)
        (and (list? x) (= 3 (length x)) (eq? (car x) 'batch)
             (req-id-string? (cadr x))
             (integer? (caddr x)) (exact? (caddr x)) (>= (caddr x) 0))))

  (define (event-id? x)
    (and (pair? x) (string? (car x)) (integer? (cdr x)) (exact? (cdr x)) (>= (cdr x) 0)))

  ;; THE SAME SHAPE THE FORMAT CHECKS, asked here so that a reader of an
  ;; actor does not have to trust that someone else checked it first.
  (define (request-actor? x)
    (and (list? x) (= 6 (length x))
         (string? (list-ref x 0))
         (pair? (list-ref x 1))
         (string? (car (list-ref x 1)))
         (req-id-ok? (cdr (list-ref x 1)))
         (let ((sub (list-ref x 2)))
           (or (memq sub '(single plan))
               (and (integer? sub) (exact? sub) (>= sub 0))))
         (string? (list-ref x 3))
         (or (not (list-ref x 4)) (event-id? (list-ref x 4)))
         (event-id? (list-ref x 5))))

  (define (actor-identity actor) (and (request-actor? actor) (list-ref actor 1)))
  (define (actor-sub actor) (and (request-actor? actor) (list-ref actor 2)))
  (define (actor-fingerprint actor) (and (request-actor? actor) (list-ref actor 3)))
  (define (actor-plan-event actor) (and (request-actor? actor) (list-ref actor 4)))
  (define (actor-after actor) (and (request-actor? actor) (list-ref actor 5)))

  ;; THE ORIGIN IS THE WRITER THE REQUEST WAS WRITTEN AGAINST, which is
  ;; the writer in its `after` cursor -- not whichever writer happens to
  ;; hold the record now.
  (define (request-identity after req-id)
    (cons (car after) req-id))

  (define (identity=? a b)
    (and (pair? a) (pair? b)
         (string=? (car a) (car b))
         (equal? (cdr a) (cdr b))))



  ;; ---- duplicates before order ---------------------------------------------

  ;; TWO RECORDS CLAIMING ONE SLOT ARE BOTH WRONG, and that has to be
  ;; decided before anything asks whether either is in the right place.
  ;; Filtering by order first would silently keep whichever copy happened
  ;; to sit where the order test wanted, and apply it -- a duplicate
  ;; history imported twice would then be applied once, quietly, with no
  ;; record of the choice.
  ;;
  ;; It is a pure function of the record set: the same records give the
  ;; same verdict however they arrived.
  (define (slot-of e) (cons (actor-plan-event (ev-actor e)) (actor-sub (ev-actor e))))

  (define (same-slot? a b)
    (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))

  (define (duplicate-slots evidence)
    (let ((members (executed-member-set evidence)))
      (let loop ((es members) (seen (quote ())) (dups (quote ())))
        (cond
          ((null? es) dups)
          (else
           (let ((slot (slot-of (car es))))
             (cond
               ((exists (lambda (s) (same-slot? s slot)) dups)
                (loop (cdr es) seen dups))
               ((exists (lambda (s) (same-slot? s slot)) seen)
                (loop (cdr es) seen (cons slot dups)))
               (else (loop (cdr es) (cons slot seen) dups)))))))))

  ;; Every record standing in a contested slot, so the caller can mark
  ;; them all: neither is applied, and `(#%new k)` binds to neither.
  (define (plan-conflicts evidence)
    (let ((dups (duplicate-slots evidence)))
      (filter (lambda (e)
                (exists (lambda (s) (same-slot? s (slot-of e))) dups))
              (executed-member-set evidence))))

  ;; WHICH INDICES ARE PRESENT, once the contested ones are set aside.
  (define (present-indices evidence)
    (let ((contested (plan-conflicts evidence)))
      (let loop ((es (executed-member-set evidence)) (out (quote ())))
        (cond
          ((null? es) (list-sort < out))
          ((memq (car es) contested) (loop (cdr es) out))
          ((integer? (actor-sub (ev-actor (car es))))
           (loop (cdr es) (cons (actor-sub (ev-actor (car es))) out)))
          (else (loop (cdr es) out))))))

  ;; A PREFIX, OR NOTHING. `{0,2}` is not "1 is missing": it is a set no
  ;; correct execution produces, because sub-operations are written in
  ;; order. Appending 1 after 2 would put the history in an order its
  ;; author never had.
  (define (prefix? indices)
    (let loop ((xs indices) (want 0))
      (cond ((null? xs) #t)
            ((= (car xs) want) (loop (cdr xs) (+ want 1)))
            (else #f))))

  ;; ---- the rules, in the order they are asked ------------------------------

  ;; THE ORDER IS THE DESIGN. Asking about a fingerprint before asking
  ;; whether the evidence can be read at all would report `req-mismatch`
  ;; for a store that cannot say what it holds -- a client would then
  ;; "fix" its request id and execute the thing a second time.
  (define (request-decision identity fingerprint who after evidence intervals plan-size successors)
    (let* ((resolved (resolution-state evidence plan-size (quote all)))
           ;; A RESOLUTION IS A RECORD ABOUT THIS IDENTITY, NOT A RECORD
           ;; OF IT. Its actor is the operator who wrote it, with their
           ;; own cursor and their own request -- so leaving it among the
           ;; evidence would make rule 3 compare the operator's actor
           ;; against this request's and answer `req-mismatch` for every
           ;; identity anyone has ever resolved.
           (records (filter (lambda (e) (not (resolution? (ev-payload e)))) evidence))
           (all (historical-set records)))
      (cond
        ;; 0. WHAT AN OPERATOR HAS DETERMINED, ABOVE EVERYTHING. A
        ;; resolution exists to settle an identity the store cannot
        ;; settle itself, so it is asked before the readability of the
        ;; evidence is: putting it below rule 1 would make it useless in
        ;; exactly the case it was written for.
        ;;
        ;; `executed` does not manufacture a replay. The answer to a
        ;; replay carries the event id and the bindings the original
        ;; execution produced, and a determination that something ran
        ;; does not recover either of them.
        ((eq? (car resolved) (quote executed))
         (list (quote resolved-executed) (list-ref resolved 3)))
        ((eq? (car resolved) (quote incomparable))
         (list (quote unknown) (cons (quote resolve-incomparable) (cdr resolved))))
        ;; 1. anything unreadable, broken, contested or waiting
        ((find-unknown records) => (lambda (why) (list (quote unknown) why)))
        ;; 2. no evidence at all: the range test, and only here
        ((null? all)
         (let ((clash (overlapping-interval after (exempt intervals resolved (quote all)) successors)))
           (if clash
               (list (quote unknown) (list (quote range-overlaps) clash))
               (list (quote execute)))))
        ;; 3. seen, and not the same request
        ((mismatched records fingerprint who after)
         => (lambda (e) (list (quote req-mismatch) (ev-event e))))
        ;; 4. seen, and a plan with a hole in it
        ((and plan-size (> plan-size 0)
              (not (equal? (present-indices records)
                           (let loop ((i 0) (out (quote ())))
                             (if (= i plan-size) (reverse out) (loop (+ i 1) (cons i out)))))))
         (let ((present (present-indices records)))
           (cond
             ((not (prefix? present))
              (list (quote unknown) (list (quote plan-order) present)))
             ;; THE MISSING SUB-OPERATIONS GET THE SAME RANGE TEST THE
             ;; WHOLE REQUEST GOT IN RULE 2, and for the same reason:
             ;; completing a plan writes records at indices believed
             ;; empty, so "empty" has to mean more than "I did not find
             ;; anything". Their positions begin after the last record
             ;; this execution left, which is the highest present index,
             ;; or after the plan itself when none is present.
             ;; A settled answer is returned as it stands; everything
             ;; else this rule finds is an uncertainty.
             ((missing-sub-problem evidence records present plan-size intervals successors)
              => (lambda (why)
                   (if (eq? (car why) (quote settled))
                       (cdr why)
                       (list (quote unknown) why))))
             (else (list (quote complete) present)))))
        ;; 5. SEEN AND WHOLE -- and "whole" has to be checked, not
        ;; inferred from the absence of a plan. A request with no plan is
        ;; one sub-operation, so a replay needs exactly that: an applied
        ;; record, in verifiable history, whose actor calls itself
        ;; `single`. Evidence that is only a plan record satisfies every
        ;; rule above and establishes nothing about a single execution --
        ;; this path does not write such a plan, but an imported history
        ;; can hold one, and a `replay` answered over it would tell a
        ;; client that work was done which nothing did.
        ((and (not plan-size) (not (applied-single records)))
         (list (quote unknown) (list (quote no-applied-record))))
        ;; THE ANSWER NAMES THE RECORD IT IS ABOUT. A caller that has to
        ;; make something durable before repeating the word needs to know
        ;; which record the word is about -- it may sit in an earlier
        ;; segment, or on a generation since retired -- and deriving it a
        ;; second time from the same evidence would be a second supplier
        ;; of the one fact this rule established.
        ;;
        ;; FOR A PLAN IT IS THE LAST SUB-OPERATION, which is the last
        ;; thing the request wrote and therefore the furthest point the
        ;; promise has to reach. A `replay` with no record named would
        ;; leave the caller nothing to make durable, and the barrier it
        ;; is about to run would have to guess.
        ((applied-single records)
         => (lambda (e) (list (quote replay) (ev-event e))))
        ((last-applied-index records plan-size)
         => (lambda (e) (list (quote replay) (ev-event e))))
        ;; AN EMPTY PLAN IS COMPLETE THE MOMENT IT IS PRESENT -- there are
        ;; no sub-operations to have run -- so the record the promise is
        ;; about is the plan itself.
        ((and (eqv? plan-size 0) (plan-record-event records))
         => (lambda (id) (list (quote replay) id)))
        (else (list (quote unknown) (list (quote no-applied-record)))))))

  (define (last-applied-index records plan-size)
    (and plan-size (> plan-size 0)
         (let loop ((i (- plan-size 1)))
           (cond
             ((< i 0) #f)
             ((applied-index records i) => (lambda (e) e))
             (else (loop (- i 1)))))))

  (define (applied-single evidence)
    (let loop ((es (executed-member-set evidence)))
      (cond
        ((null? es) #f)
        ((and (eq? (actor-sub (ev-actor (car es))) (quote single))
              (eq? (ev-placement (car es)) (quote valid-history)))
         (car es))
        (else (loop (cdr es))))))

  ;; A `not-executed` RESOLUTION EXEMPTS ONE SUB-OPERATION FROM ONE
  ;; INTERVAL, and the match on the interval is exact. An operator
  ;; determines that a named sub-operation did not run in a named
  ;; stretch; reading that as "and not in any stretch inside it either"
  ;; would widen a determination the operator never made.
  ;; CONTAINMENT ONE WAY ONLY. "This sub-operation did not run anywhere in
  ;; [10,20]" already covers [12,15], so clearing the smaller stretch
  ;; narrows the exemption rather than widening the determination. The
  ;; other direction is not available: an operator who examined [10,20]
  ;; has said nothing about [10,40], and the store's own stretches are
  ;; merged and split by the store, not by the operator, so an exact
  ;; match alone would lose the exemption the first time two stretches
  ;; were joined.
  ;;
  ;; A stretch with no top is never cleared. It is not contained in
  ;; anything closed, and that is the same rule that makes an open
  ;; interval illegal in the resolution itself.
  (define (exempt intervals resolved sub)
    (if (and (eq? (car resolved) (quote not-executed))
             (equal? (list-ref resolved 1) sub))
        (let ((r (list-ref resolved 2)))
          (remp (lambda (i) (interval-within? i r)) intervals))
        intervals))

  (define (interval-within? i r)
    (and (string=? (car i) (car r))
         (>= (cadr i) (cadr r))
         (caddr i)
         (caddr r)
         (<= (caddr i) (caddr r))))

  ;; The cursor a missing sub-operation would have been written against:
  ;; the event of the highest present index, or the plan's own event when
  ;; nothing is present. Sequence numbers are never compared across
  ;; writers here -- the records of one execution are one writer's, and
  ;; the index order is what puts them in order.
  (define (last-present-event records present)
    (let loop ((i (- (length present) 1)))
      (cond
        ((< i 0) #f)
        ((find-index-record records (list-ref present i))
         => (lambda (e) (ev-event e)))
        (else (loop (- i 1))))))

  (define (find-index-record records index)
    (let loop ((es (executed-member-set records)))
      (cond
        ((null? es) #f)
        ((eqv? (actor-sub (ev-actor (car es))) index) (car es))
        (else (loop (cdr es))))))

  ;; THE PLAN'S OWN EVENT, FOUND THE WAY THE PLAN IS FOUND EVERYWHERE
  ;; ELSE HERE: by its payload. Reading it out of an actor's plan-event
  ;; slot instead finds it only through some OTHER record that points at
  ;; it -- so a plan delivered with no sub-operation yet has no such
  ;; record, the cursor comes back #f, and every missing index skips its
  ;; range test in silence. That is a plan with nothing applied being
  ;; told to go ahead and apply all of it.
  (define (plan-record-event records)
    (let loop ((es records))
      (cond
        ((null? es) #f)
        ((and (pair? (ev-payload (car es)))
              (eq? (car (ev-payload (car es))) (quote plan)))
         (ev-event (car es)))
        (else (loop (cdr es))))))

  ;; EACH MISSING SUB-OPERATION IS ITS OWN TARGET, so its resolutions are
  ;; read for it alone and its exemption applies to it alone.
  ;;
  ;; AND AN ANCHOR THAT CANNOT BE RECOVERED IS NOT "NO OVERLAP". If
  ;; neither a present record nor the plan itself can be found, the test
  ;; has nothing to measure from -- and answering `complete` on the
  ;; strength of having nothing to measure is the failure this whole rule
  ;; exists to prevent.
  (define (missing-sub-problem evidence records present plan-size intervals successors)
    (let ((cursor (or (last-present-event records present)
                      (plan-record-event records))))
      (if (not cursor)
          (list (quote no-cursor))
          (let loop ((i 0))
            (cond
              ((= i plan-size) #f)
              ((memv i present) (loop (+ i 1)))
              (else
               (let ((r (resolution-state evidence plan-size i)))
                 (cond
                   ;; AN OPERATOR WHO CONFIRMED THAT THIS SUB-OPERATION
                   ;; RAN HAS SAID THE ONE THING COMPLETION CANNOT ACT
                   ;; ON. Writing it again would be a second execution,
                   ;; and skipping it is not available either: the
                   ;; confirmation carries no event id and no bindings,
                   ;; so there is nothing to put in its place.
                   ;;
                   ;; IT IS A SETTLED ANSWER, NOT AN UNCERTAINTY. Nothing
                   ;; further will make this request executable, so the
                   ;; client is told the same thing a request-level
                   ;; confirmation tells it -- it ran, and the result is
                   ;; not recoverable -- with the sub-operation named.
                   ((eq? (car r) (quote executed))
                    (list (quote settled) (quote resolved-executed)
                          (list (quote sub) i) (list (quote event) (list-ref r 3))))
                   ((eq? (car r) (quote incomparable))
                    (cons (quote resolve-incomparable) (cdr r)))
                   ((overlapping-interval cursor (exempt intervals r i) successors)
                    => (lambda (clash) (list (quote range-overlaps) clash)))
                   (else (loop (+ i 1)))))))))))

  ;; ---- resolutions and the fence --------------------------------------------

  ;; A RESOLUTION IS AN OPERATOR SAYING WHAT THE STORE COULD NOT WORK
  ;; OUT. It is written into the current writer's log like any other
  ;; record, and it is the only thing that can settle an identity whose
  ;; evidence is unreadable -- which is why it is asked about before the
  ;; readability of the evidence is.
  ;;
  ;;     (resolve <identity> <fingerprint> <sub|all> executed|not-executed
  ;;              <interval> (supersedes <event-id> ...))
  ;;
  ;; ITS SCOPE IS EXACTLY ONE IDENTITY AND ONE SUB-OPERATION. `all` is a
  ;; scope and not a shortcut: it is legal only where there is one
  ;; sub-operation to speak for, a single request or an empty plan. An
  ;; `all` over a plan of five would be one sentence standing in for five
  ;; different determinations, and the operator would have made only one
  ;; of them.
  (define (resolution? x)
    (and (list? x) (= 7 (length x))
         (eq? (car x) (quote resolve))
         (pair? (list-ref x 1))
         (string? (car (list-ref x 1)))
         (req-id-ok? (cdr (list-ref x 1)))
         (string? (list-ref x 2))
         (or (eq? (list-ref x 3) (quote all))
             (and (integer? (list-ref x 3)) (exact? (list-ref x 3))
                  (>= (list-ref x 3) 0)))
         (memq (list-ref x 4) (quote (executed not-executed)))
         (interval? (list-ref x 5))
         (supersedes-list? (list-ref x 6))))

  ;; `(writer low high)`, with high = #f meaning unbounded above. An
  ;; interval whose top is below its bottom is not a stretch of history.
  (define (interval? x)
    (and (list? x) (= 3 (length x))
         (string? (car x))
         (integer? (cadr x)) (exact? (cadr x)) (>= (cadr x) 0)
         (or (not (caddr x))
             (and (integer? (caddr x)) (exact? (caddr x))
                  (>= (caddr x) (cadr x))))))

  (define (interval-closed? i) (and (caddr i) #t))

  (define (supersedes-list? x)
    (and (pair? x) (eq? (car x) (quote supersedes))
         (list? (cdr x))
         (for-all event-pair? (cdr x))))

  (define (resolution-target r) (list-ref r 1))
  (define (resolution-fingerprint r) (list-ref r 2))
  (define (resolution-sub r) (list-ref r 3))
  (define (resolution-verdict r) (list-ref r 4))
  (define (resolution-interval r) (list-ref r 5))
  (define (resolution-supersedes r) (cdr (list-ref r 6)))

  ;; RESOLUTIONS ARE ORDERED BY `supersedes` AND BY NOTHING ELSE. Not by
  ;; sequence number -- two of them can sit on different writers, where
  ;; the numbers do not compare -- and not by position in the scan.
  ;;
  ;; A MARKED RESOLUTION IS NOT ELIGIBLE, and eligibility is a different
  ;; question from order. `superseded` and `plan-mismatch` say this
  ;; record does not speak for this target at all; `supersedes` ranks the
  ;; records that do. Admitting a marked one would be worse here than
  ;; anywhere else: resolutions are taken out of the evidence before the
  ;; readability rule runs, so nothing downstream would ever see its
  ;; mark, and an ineligible `not-executed` would silently clear the very
  ;; range test that stands between a completion and a second execution.
  (define (resolution-evidence evidence)
    (filter (lambda (e)
              (and (resolution? (ev-payload e))
                   (not (ev-marked? e (quote superseded)))
                   (not (ev-marked? e (quote plan-mismatch)))))
            evidence))

  ;; ONE TARGET IS ONE IDENTITY AND ONE SUB-OPERATION, so the resolutions
  ;; are selected per sub-operation and not per request. Two operators
  ;; determining different things about different sub-operations have not
  ;; contradicted each other, and a request-wide reading would report
  ;; them as an unreconciled pair and freeze both.
  (define (resolutions-for evidence sub)
    (filter (lambda (e) (equal? (resolution-sub (ev-payload e)) sub))
            (resolution-evidence evidence)))

  (define (scope-problem r plan-size)
    (and (eq? (resolution-sub r) (quote all)) plan-size (> plan-size 0)
         (quote resolve-scope)))

  ;; THE FENCE STOPS A RETRACTION, NOT AN OLDER DETERMINATION. A
  ;; confirmed execution cannot be un-confirmed, so a `not-executed` that
  ;; NAMES a confirmation in its `supersedes` -- which is the only way
  ;; one record here can be said to come after another -- is invalid.
  ;;
  ;; It does not reach further than that. A `not-executed` that was
  ;; written earlier and knows nothing of the confirmation has not
  ;; retracted anything; it is simply a second determination nobody has
  ;; reconciled, and that is `resolve-incomparable`. Treating it as
  ;; fenced would let the confirmation supply, after the fact, the
  ;; supersession its author never wrote -- and would turn an
  ;; unreconciled pair into a confident answer.
  (define (fenced? r rs plan-size)
    (and (eq? (resolution-verdict r) (quote not-executed))
         (exists (lambda (e)
                   (let ((x (ev-payload e)))
                     (and (eq? (resolution-verdict x) (quote executed))
                          (not (scope-problem x plan-size))
                          (exists (lambda (id) (equal? id (ev-event e)))
                                  (resolution-supersedes r)))))
                 rs)))

  ;; WHY A RESOLUTION CAN BE INVALID, in the order the questions are
  ;; asked. Shape before history, as everywhere else here.
  ;;
  ;; AN OPEN INTERVAL IS THE ONE `not-executed` MAY NEVER USE. The
  ;; exemption says "this sub-operation did not run in this stretch", and
  ;; a stretch with no top is a promise about a living writer's future. A
  ;; true observation of the past cannot stand in for that.
  (define (resolution-problem r rs plan-size)
    (cond
      ((scope-problem r plan-size) => (lambda (why) why))
      ((eq? (resolution-verdict r) (quote executed)) #f)
      ((not (interval-closed? (resolution-interval r))) (quote resolve-open-interval))
      ((fenced? r rs plan-size) (quote resolve-fenced))
      (else #f)))

  ;; EVERY INVALID RESOLUTION, WITH ITS REASON, whatever else the set
  ;; says. The decision below ignores invalid resolutions -- an operator
  ;; who writes a sentence the format refuses must not be able to freeze
  ;; a target by writing it -- but the store still has to record the
  ;; integrity kind, and a set holding one valid resolution and one
  ;; invalid one would otherwise report the valid one and lose the other
  ;; entirely.
  (define (resolution-problems evidence plan-size)
    (let ((rs (resolution-evidence evidence)))
      (let loop ((es rs) (out (quote ())))
        (cond
          ((null? es) (reverse out))
          ((resolution-problem (ev-payload (car es))
                               (resolutions-for evidence
                                                (resolution-sub (ev-payload (car es))))
                               plan-size)
           => (lambda (why) (loop (cdr es) (cons (list why (ev-event (car es))) out))))
          (else (loop (cdr es) out))))))

  ;; WHAT THE RESOLUTIONS ON ONE TARGET ADD UP TO.
  ;;
  ;;   (none)
  ;;   (executed <sub> <interval> <event>)
  ;;   (not-executed <sub> <interval> <event>)
  ;;   (incomparable <event> ...)
  ;;   (invalid (<reason> <event>) ...)
  ;;
  ;; TWO THAT DO NOT REFERENCE EACH OTHER ARE NOT A TIE TO BE BROKEN.
  ;; Choosing either would make the answer depend on which the scan
  ;; reached first, and the target stays unknown until a third supersedes
  ;; them both.
  (define (resolution-state evidence plan-size sub)
    (let ((rs (resolutions-for evidence sub)))
      (if (null? rs)
          (list (quote none))
          (let ((valid (filter (lambda (e)
                                 (not (resolution-problem (ev-payload e) rs plan-size)))
                               rs)))
            (cond
              ((null? valid)
               (cons (quote invalid)
                     (map (lambda (e)
                            (list (resolution-problem (ev-payload e) rs plan-size)
                                  (ev-event e)))
                          rs)))
              (else
               (let ((standing (filter (lambda (e) (not (superseded-by? e valid))) valid)))
                 (cond
                   ;; A CYCLE IS NOT AN ORDER. Every resolution being
                   ;; superseded by another leaves none of them standing.
                   ((null? standing) (list (quote incomparable)))
                   ((> (length standing) 1)
                    (cons (quote incomparable) (map ev-event standing)))
                   ;; AND ONE RECORD STANDING IS NOT THE SAME AS ONE
                   ;; STORY. Two resolutions that supersede each other sit
                   ;; in a cycle nothing outside it names, so neither
                   ;; stands -- and a third, unrelated one then stands
                   ;; alone over a set it never reconciled. What has to be
                   ;; true is that the standing record reaches every other
                   ;; valid one through `supersedes`.
                   ((not (reaches-all? (car standing) valid))
                    (cons (quote incomparable) (map ev-event valid)))
                   (else
                    (let ((r (ev-payload (car standing))))
                      (list (resolution-verdict r) (resolution-sub r)
                            (resolution-interval r) (ev-event (car standing)))))))))))))

  (define (superseded-by? e valid)
    (exists (lambda (other)
              (and (not (eq? other e))
                   (exists (lambda (id) (equal? id (ev-event e)))
                           (resolution-supersedes (ev-payload other)))))
            valid))

  (define (reaches-all? head valid)
    (let loop ((frontier (list head)) (seen (list head)))
      (if (null? frontier)
          (= (length seen) (length valid))
          (let* ((named (resolution-supersedes (ev-payload (car frontier))))
                 (next (filter (lambda (e)
                                 (and (not (memq e seen))
                                      (exists (lambda (id) (equal? id (ev-event e))) named)))
                               valid)))
            (loop (append next (cdr frontier)) (append next seen))))))

  ;; ---- batch receipts -------------------------------------------------------

  ;; A RECEIPT IS WRITTEN BEFORE ANY ITEM RUNS, and it is the reason a
  ;; retry can say anything at all about a batch. Without it a crash
  ;; between two items leaves a set of records and no statement of what
  ;; the set was supposed to be, so "item 3 is missing" and "there were
  ;; only three items" are the same picture. The receipt says which
  ;; indices the batch covers and, for each, the cursor its record was
  ;; written against -- so the range test for a missing item is asked
  ;; about a position the batch DECLARED rather than one reconstructed
  ;; from its neighbours.
  ;;
  ;;     (batch <req-id> <fingerprint> <after> ((0 . after0) (1 . after1) ...))
  ;;
  ;; THE SHAPE IS CHECKED WHOLE, not field by field as each is used. A
  ;; receipt read from a log is a record some other version of this
  ;; program wrote, and a half-understood one is worse than an
  ;; unreadable one: it answers some questions and lies about the rest.
  (define (receipt? x)
    (and (list? x) (= 5 (length x))
         (eq? (car x) (quote batch))
         (req-id-string? (list-ref x 1))
         (string? (list-ref x 2))
         (event-pair? (list-ref x 3))
         (entry-list? (list-ref x 4))))

  (define (event-pair? x)
    (and (pair? x) (string? (car x))
         (integer? (cdr x)) (exact? (cdr x)) (>= (cdr x) 0)))

  ;; ASCENDING AND DISTINCT, which is a statement about the writing and
  ;; not a preference: the items are written in index order, so a receipt
  ;; whose entries are out of order or repeat an index did not come from
  ;; a correct execution and nothing below should treat it as if it had.
  (define (entry-list? x)
    (and (list? x)
         (let loop ((xs x) (last -1))
           (or (null? xs)
               (and (pair? (car xs))
                    (integer? (caar xs)) (exact? (caar xs))
                    (> (caar xs) last)
                    (event-pair? (cdar xs))
                    (loop (cdr xs) (caar xs)))))))

  (define (receipt-req r) (list-ref r 1))
  (define (receipt-fingerprint r) (list-ref r 2))
  (define (receipt-after r) (list-ref r 3))
  (define (receipt-entries r) (list-ref r 4))

  (define (receipt-covers? r index)
    (and (assv index (receipt-entries r)) #t))

  (define (receipt-item-after r index)
    (let ((e (assv index (receipt-entries r))))
      (and e (cdr e))))

  ;; A SECOND RECEIPT FOR ONE BATCH IS INTEGRITY, NOT A CHOICE. Two
  ;; receipts are two statements of what the batch was, and picking
  ;; either would make the answer depend on which the scan reached first
  ;; -- so neither is used and the second one is named.
  ;;
  ;; It is the second in the order the records were found, which is the
  ;; order of the log; the first stands as the one that was acted on.
  (define (receipt-duplicate evidence)
    (let loop ((es evidence) (seen #f))
      (cond
        ((null? es) #f)
        ((not (receipt-record? (car es))) (loop (cdr es) seen))
        (seen (list (quote batch-duplicate) (ev-event (car es))))
        (else (loop (cdr es) #t)))))

  ;; A MARKED RECEIPT IS NOT A RECEIPT. `superseded` and `plan-mismatch`
  ;; are the store saying "not this one", and a receipt is the record
  ;; every other answer here is built on -- so a receipt that has been
  ;; set aside leaves the batch with no receipt at all, which is
  ;; `receipt-absent` and not a quiet fall-back to the next one found.
  (define (receipt-record? e)
    (and (receipt? (ev-payload e))
         (not (ev-marked? e 'superseded))
         (not (ev-marked? e 'plan-mismatch))))

  (define (receipt-of evidence)
    (let loop ((es evidence))
      (cond
        ((null? es) #f)
        ((receipt-record? (car es)) (ev-payload (car es)))
        (else (loop (cdr es))))))

  ;; NOT-STARTED IS ABOUT THE RECEIPT, NOT ABOUT THE RECORDS. An index
  ;; the receipt does not list was never part of this batch, so there is
  ;; nothing for a retry to finish and nothing for a range test to be
  ;; uncertain about. It is the one state that can be answered without
  ;; looking at any history.
  (define (batch-item-state receipt index)
    (cond
      ((not receipt) (quote unknown))
      ((receipt-covers? receipt index) (quote covered))
      (else (quote not-started))))

  ;; THE STATE OF A BATCH, asked by a retry that is holding the request
  ;; again and wants to know what its earlier self got through.
  ;;
  ;; `items` is the evidence for the batch: the receipt record and every
  ;; record whose actor names one of the batch's items. `n` is how many
  ;; items the request declares, read from the request the client is
  ;; holding and not from the receipt -- comparing the two is one of the
  ;; things this answers.
  ;;
  ;; THE ORDER OF THE TESTS IS THE SAME ORDER AS THE DEDUPE RULES, and
  ;; for the same reason: a store that cannot say what it holds must not
  ;; answer a question about what the client did. Everything unreadable,
  ;; contested or waiting is `unknown` before any comparison is made.
  (define (batch-state n evidence fingerprint after intervals chain-readable? successors)
    ;; THE SIZE IS NOT OPTIONAL. A retry holds the request, so it knows
    ;; how many items the batch has; without it the coverage test and
    ;; every missing-index test have nothing to range over and quietly
    ;; pass. Refusing is the one answer that cannot be mistaken for a
    ;; batch that checked out.
    (unless (and (integer? n) (exact? n) (>= n 0))
      (assertion-violation 'batch-state "the request's item count is required" n))
    (let* ((receipt (receipt-of evidence))
           (items (filter (lambda (e) (not (receipt-record? e))) evidence)))
      (cond
        ;; 1. UNREADABLE, BROKEN, CONTESTED OR WAITING, receipt included.
        ((receipt-duplicate evidence) => (lambda (why) (list (quote unknown) why)))
        ;; READABILITY IS ASKED OF EACH IDENTITY SEPARATELY. `find-unknown`
        ;; also looks for two records standing in one slot, and a slot is
        ;; `(plan-event . sub)` -- which every item of a batch shares,
        ;; because each is a `single` record belonging to no plan. Asked
        ;; of the whole set at once it calls every batch of more than one
        ;; item contested. They are not two records in one slot; they are
        ;; different requests.
        ((find-unknown-per-identity evidence) => (lambda (why) (list (quote unknown) why)))
        ;; 2. NO RECEIPT. Two very different pictures share this arm and
        ;; they are separated by whether anything ran: with no evidence
        ;; at all the batch provably did not begin, provided its cursor
        ;; is clear of every uncertain stretch. With item records but no
        ;; receipt, the one thing written before everything else is the
        ;; one thing missing -- the store cannot say what the batch was,
        ;; and reconstructing it from the items it can see would be
        ;; taking the survivors for the whole.
        ((not receipt)
         (cond
           ((pair? items) (list (quote unknown) (list (quote receipt-absent))))
           ;; THE CURSOR COMES FROM THE REQUEST, NOT FROM THE EVIDENCE.
           ;; This arm is reached when there is no evidence, so a cursor
           ;; read out of the evidence would be read out of an empty
           ;; list -- and the range test, the only thing standing
           ;; between "nothing ran" and "nothing that I can see ran",
           ;; would quietly never run at all.
           ((overlapping-interval after intervals successors)
            => (lambda (clash) (list (quote unknown) (list (quote range-overlaps) clash))))
           (else (list (quote not-committed)))))
        ;; 3. A RECEIPT THAT IS NOT THIS REQUEST'S. The fingerprint is
        ;; recomputed by the retry from the whole immutable request, so a
        ;; disagreement means the id was reused for different content.
        ;; Executing would then append a second, different batch under an
        ;; id that already names one.
        ((not (equal? (receipt-fingerprint receipt) fingerprint))
         (list (quote unknown) (list (quote req-mismatch) (receipt-fingerprint receipt))))
        ;; 4. AN ITEM THAT DOES NOT NAME THE RECEIPT. Every item record
        ;; carries the receipt's event id among its dependencies, which
        ;; is what makes a record evidence of THIS batch rather than a
        ;; record that merely shares its identity. One that does not name
        ;; it cannot have been written by an execution that went through
        ;; the receipt, and counting it towards completion would let a
        ;; stray record finish a batch.
        ((unlinked-item items receipt evidence)
         => (lambda (e) (list (quote unknown) (list (quote receipt-unlinked) e))))
        ;; 5. A RECEIPT THAT DOES NOT COVER THE REQUEST. The request in
        ;; hand declares n items; the receipt declares which indices the
        ;; batch covers. An index below n that the receipt never lists is
        ;; `not-started` as an ITEM -- there is nothing to finish -- but
        ;; it is not a hole the batch can be `partial` over, because
        ;; partial means "the rest have provably not run" and the
        ;; receipt, the one thing written before anything else, does not
        ;; mention them. The fingerprint cannot catch this: it is taken
        ;; over the request's content, and the entry list is not part of
        ;; the content.
        ((uncovered-index receipt n)
         => (lambda (i) (list (quote unknown) (list (quote receipt-coverage) i))))
        ;; 6. A RECORD THAT CANNOT SAY WHICH ITEM IT IS.
        ;;
        ;; AN ITEM'S INDEX IS ITS IDENTITY'S, NOT ITS ACTOR'S SLOT. Each
        ;; item of a batch is its own request -- `(batch <req> <k>)` --
        ;; and its actor's sub-operation slot describes that item's OWN
        ;; internal shape: `single` when it is one record, an index into
        ;; its own plan when it is several. The two answer different
        ;; questions and must not be read for each other.
        ((no-item-index items)
         => (lambda (e) (list (quote unknown) (list (quote item-index-missing) e))))
        ((let* ((receipt-ev (find receipt-record? evidence))
                (who (and receipt-ev (car (ev-actor receipt-ev)))))
           (find (lambda (e)
                   (let ((a (ev-actor e)) (i (identity-index e)))
                     (or (not (equal? (actor-fingerprint a) fingerprint))
                         (not (equal? (car a) who))
                         (not (equal? (actor-after a) (receipt-item-after receipt i)))))) items))
         => (lambda (e) (list 'unknown (list 'item-mismatch (ev-event e)))))
        (else (batch-progress receipt n items intervals chain-readable? successors
                              (receipt-event evidence))))))

  (define (identity-index e)
    (let ((id (actor-identity (ev-actor e))))
      (and (pair? id)
           (let ((req (cdr id)))
             (and (list? req) (= 3 (length req)) (eq? (car req) (quote batch))
                  (caddr req))))))

  (define (no-item-index items)
    (let loop ((es items))
      (cond
        ((null? es) #f)
        ((not (identity-index (car es))) (ev-event (car es)))
        (else (loop (cdr es))))))

  ;; THE BATCH LAYER READS INDICES FROM IDENTITIES, and these are its own
  ;; versions of the three questions the plan layer asks about
  ;; sub-operations. They are separate functions and not a flag, because
  ;; a flag would let one call site ask the wrong question of the wrong
  ;; layer and still typecheck.
  ;; A RECORD THAT SAYS WHAT AN ITEM WAS ABOUT TO DO IS NOT THE ITEM. An
  ;; item that is several records writes its own plan first, and that
  ;; record carries the item's identity -- so it carries the item's index
  ;; -- while stating only an intention. Counting it as the item being
  ;; present gives a batch whose items all stopped just after writing
  ;; their plans the same word a batch that ran to the end gets:
  ;; `committed`, naming a plan record as the thing the promise reaches.
  ;;
  ;; THE SLOT IS WHAT TELLS THEM APART, and rule 6 above already says why
  ;; the two are separate questions: the identity says WHICH item a
  ;; record belongs to, the slot says what the record is within that
  ;; item. Only this function needs the second question. `item-naming-
  ;; missing` must keep counting plan records, because its question is
  ;; whether anything ever wrote an index down -- and an intention is
  ;; exactly that.
  ;; AN ITEM IS PRESENT WHEN ITS OWN REQUEST SAYS IT RAN, and the only
  ;; thing that can say so is the decision function every single request
  ;; goes through. An earlier fix here asked a narrower question -- is
  ;; this record an execution rather than a plan -- which closed the case
  ;; where an item had written nothing but its plan, and left the one
  ;; where it had written its plan and some of its sub-operations. Both
  ;; are the same mistake: the batch layer deciding for itself what
  ;; finishing means, in words of its own, while the layer that owns that
  ;; question is one call away.
  ;;
  ;; SO IT DELEGATES, and a batch is committed only when every item is.
  ;; The item's plan size comes from its own plan record where it has
  ;; one; an item with no plan is a single request and answers for itself.
  (define (item-completion-event items index)
    (let* ((es (filter (lambda (e) (eqv? (identity-index e) index)) items))
           (plans (filter (lambda (e) (and (eq? (actor-sub (ev-actor e)) 'plan)
                                           (plan? (ev-payload e))))
                          es)))
      (and (pair? es) (<= (length plans) 1)
           (let* ((a (ev-actor (car es)))
                  (size (and (pair? plans) (length (list-ref (ev-payload (car plans)) 4))))
                  (v (request-decision (actor-identity a) (actor-fingerprint a)
                                       (car a) (actor-after a) es '() size '())))
             (and (eq? (car v) 'replay) (cadr v))))))

  (define (item-present-indices items)
    (list-sort <
      (filter (lambda (i) (item-completion-event items i))
        (fold-left (lambda (out e)
                     (let ((i (identity-index e)))
                       (if (and i (not (memv i out))) (cons i out) out)))
                   '() items))))

  (define (item-at items index)
    (let loop ((es items))
      (cond
        ((null? es) #f)
        ((eqv? (identity-index (car es)) index) (car es))
        (else (loop (cdr es))))))

  (define (item-naming-missing items present n)
    (let loop ((es items))
      (cond
        ((null? es) #f)
        ((let ((i (identity-index (car es))))
           (and i (< i n) (not (memv i present)) i))
         => (lambda (i) i))
        (else (loop (cdr es))))))

  ;; The receipt's own event id, found through the record that carried
  ;; it: the receipt datum does not name itself, and it must not -- a
  ;; record that stated its own event id would have two suppliers of it.
  (define (receipt-event evidence)
    (let loop ((es evidence))
      (cond
        ((null? es) #f)
        ((receipt-record? (car es)) (ev-event (car es)))
        (else (loop (cdr es))))))

  ;; NAMING THE RECEIPT IN `deps` IS ONE OF TWO WAYS TO BE LINKED TO IT,
  ;; and on the same writer it is not the one that is used. A record's
  ;; dependencies list the OTHER writers it builds on; its own writer is
  ;; left out on purpose, because the sequence already says which of two
  ;; records on one writer came first. So an item written after the
  ;; receipt on the receipt's own writer is linked by the log's order,
  ;; and repeating that in `deps` would be a second supplier of an
  ;; ordering the sequence already gives -- one that could disagree with
  ;; it.
  ;;
  ;; An item on ANOTHER writer has no such order to appeal to, and there
  ;; the dependency is the only link there is.
  (define (unlinked-item items receipt evidence)
    (let ((id (receipt-event evidence)))
      (and id
           (let loop ((es items))
             (cond
               ((null? es) #f)
               ((linked-to? (car es) id) (loop (cdr es)))
               (else (ev-event (car es))))))))

  (define (linked-to? e id)
    (or (exists (lambda (d) (and (string=? (car d) (car id)) (>= (cdr d) (cdr id)))) (ev-deps e))
        (let ((at (ev-event e)))
          (and (string=? (car at) (car id)) (> (cdr at) (cdr id))))))

  ;; COVERAGE IS COMPARED IN BOTH DIRECTIONS. A receipt that lists fewer
  ;; indices than the request leaves items the batch never promised; one
  ;; that lists more describes a different batch. Checking only the first
  ;; direction reports `committed` for a request of two items against a
  ;; receipt for three -- the extra index simply never gets asked about.
  ;; The fingerprint cannot separate them: it is taken over the request's
  ;; content and the entry list is not part of the content.
  (define (uncovered-index receipt n)
    (let loop ((i 0))
      (cond
        ((= i n)
         (let ((extra (filter (lambda (e) (>= (car e) n)) (receipt-entries receipt))))
           (and (pair? extra) (car (car extra)))))
        ((receipt-covers? receipt i) (loop (+ i 1)))
        (else i))))

  ;; Rules 7 to 9: how far it got.
  ;; PLANNED AND PARTIAL ARE THE SAME CLAIM WITH A DIFFERENT k. "The
  ;; receipt is durable and items 0..k ran, and the rest provably did
  ;; not" is what `partial` says; `planned` is that sentence with k = -1.
  ;; They therefore have to pass the same tests, and an arm that answered
  ;; `planned` on the strength of an empty item list alone was answering
  ;; "nothing that I can READ ran" -- a record whose bytes are torn
  ;; contributes an uncertain stretch and no readable identity, so it is
  ;; invisible to every test that looks at records and visible only to
  ;; the one that looks at intervals. A client reading `planned` starts
  ;; at index 0.
  (define (batch-progress receipt n items intervals chain-readable? successors receipt-id)
    (let ((present (item-present-indices items)))
      (cond
        ;; A SET THAT IS NOT A PREFIX IS NOT "SOMETHING IS MISSING". The
        ;; items are written in index order, so {0,2} is a set no correct
        ;; execution produces and the store may not guess which of the
        ;; two stories it is looking at.
        ((not (prefix? present))
         (list (quote unknown) (list (quote plan-order) present)))
        ;; AND IT NAMES THE RECORD IT IS ABOUT, for the same reason a
        ;; replay does: a caller that has to make something durable
        ;; before repeating the word needs to know which record the word
        ;; is about. For a batch that is the LAST item -- the furthest
        ;; point the promise has to reach.
        ((= (length present) n)
         (list (quote committed) present
               (if (= n 0) receipt-id (item-completion-event items (- n 1)))))
        ;; PARTIAL IS THE STRONGEST CLAIM HERE and it asks for four
        ;; things, not one. Each missing index must be clear of every
        ;; uncertain stretch AT THE CURSOR THE RECEIPT RECORDED FOR IT;
        ;; the writer chain must read to its end, or "there are no
        ;; records past here" is a statement about how far the reader
        ;; got; and nothing anywhere in the history may name a missing
        ;; index, including records set aside as superseded or
        ;; mismatched -- those are not evidence of what ran, but they are
        ;; evidence that something once wrote that index down.
        ((not chain-readable?)
         (list (quote unknown) (list (quote chain-unreadable))))
        ((item-naming-missing items present n)
         => (lambda (i) (list (quote unknown) (list (quote evidence-names-missing) i))))
        ((missing-index-overlap receipt present n intervals successors)
         => (lambda (clash) (list (quote unknown) (list (quote range-overlaps) clash))))
        ((null? present) (list (quote planned)))
        (else (list (quote partial) present)))))

  ;; THE HISTORICAL SET, and this is the question it exists for. A record
  ;; that was set aside -- superseded by a resolution, or marked as
  ;; disagreeing with its plan -- says nothing about what the batch did,
  ;; and everything about whether the index it names is really untouched.
  (define (named-missing-index items present n)
    (let loop ((es (historical-set items)))
      (cond
        ((null? es) #f)
        ((let ((sub (actor-sub (ev-actor (car es)))))
           (and (integer? sub) (exact? sub)
                (< sub n)
                (not (memv sub present))
                sub))
         => (lambda (sub) sub))
        (else (loop (cdr es))))))

  ;; Each missing index is asked about its OWN cursor. Taking the
  ;; batch's cursor for all of them would test a position no missing item
  ;; would have been written at, and an interval sitting between two
  ;; items would then be missed entirely.
  (define (missing-index-overlap receipt present n intervals successors)
    (let loop ((i 0))
      (cond
        ((= i n) #f)
        ((memv i present) (loop (+ i 1)))
        ((let ((after (receipt-item-after receipt i)))
           (and after (overlapping-interval after intervals successors)))
         => (lambda (clash) clash))
        (else (loop (+ i 1))))))

  (define (find-unknown-per-identity evidence)
    (let loop ((groups (group-by-identity evidence)))
      (cond
        ((null? groups) #f)
        ((find-unknown (car groups)) => (lambda (why) why))
        (else (loop (cdr groups))))))

  (define (group-by-identity evidence)
    (let loop ((es evidence) (seen (quote ())) (out (quote ())))
      (cond
        ((null? es) (reverse out))
        (else
         (let ((id (actor-identity (ev-actor (car es)))))
           (if (exists (lambda (x) (identity=? x id)) seen)
               (loop (cdr es) seen out)
               (loop (cdr es) (cons id seen)
                     (cons (filter (lambda (e)
                                     (identity=? (actor-identity (ev-actor e)) id))
                                   evidence)
                           out))))))))

  ;; Rule 1's conditions, each named so an answer can say which one.
  (define (find-unknown evidence)
    (let loop ((es evidence))
      (cond
        ((null? es) (if (pair? (plan-conflicts evidence))
                        (list (quote plan-conflict))
                        #f))
        ((broken-placement? (car es))
         (list (ev-placement (car es)) (ev-event (car es))))
        ((readable-but-unverifiable? (car es))
         (list (quote unverifiable) (ev-placement (car es)) (ev-event (car es))))
        ;; THE MARK AND THE COMPUTATION ARE THE SAME FACT. `invalid` is
        ;; written down as a persistent `plan-mismatch`, but the answer
        ;; must not depend on whether it has been written yet: a request
        ;; asked before the marker was persisted and after it would
        ;; otherwise get two different answers about one unchanged set of
        ;; records.
        ((or (ev-marked? (car es) (quote plan-mismatch))
             (and (ev-delivered? (car es))
                  (eq? (membership (car es) evidence) (quote invalid))))
         (list (quote plan-mismatch) (ev-event (car es))))
        ((ev-marked? (car es) 'plan-conflict)
         (list 'plan-conflict (ev-event (car es))))
        ((ev-marked? (car es) 'pending-plan)
         (list 'pending (ev-event (car es))))
        ((ev-marked? (car es) (quote plan-order))
         (list (quote plan-order) (ev-event (car es))))
        ((and (ev-delivered? (car es)) (eq? (membership (car es) evidence) (quote undetermined)))
         (list (quote pending) (ev-event (car es))))
        ;; A RECORD THAT IS THERE AND HAS NOT BEEN APPLIED IS NEITHER
        ;; ANSWER. It is not "no evidence" -- the bytes are in verifiable
        ;; history, so the request reached the store. And it is not a
        ;; replay either: a replay tells a client the work is DONE, and
        ;; nothing here says this record was ever applied to the state
        ;; the client will read. Delivery can stop short of a readable
        ;; record for reasons that have nothing to do with this request
        ;; -- a premise that has not arrived, a writer the load stopped
        ;; at -- so the honest answer is that the store cannot yet say.
        ((not (ev-delivered? (car es)))
         (list (quote undelivered) (ev-event (car es))))
        (else (loop (cdr es))))))

  ;; Rule 3 compares only within the comparison set, and compares the
  ;; whole of what identifies a request.
  (define (mismatched evidence fingerprint who after)
    (let loop ((es (comparison-set evidence)))
      (cond
        ((null? es) #f)
        ((let ((a (ev-actor (car es))))
           (or (not (equal? (actor-fingerprint a) fingerprint))
               (not (equal? (list-ref a 0) who))
               (not (equal? (actor-after a) after))))
         (car es))
        (else (loop (cdr es))))))

  ;; AN INTERVAL IS CLOSED OR OPEN AT THE TOP, and a request whose
  ;; possible positions touch one cannot be told apart from one that ran.
  (define (overlapping-interval after intervals successors)
    (let loop ((is intervals))
      (cond
        ((null? is) #f)
        ((interval-touches? (car is) after successors) (car is))
        (else (loop (cdr is))))))

  ;; `(writer low high)` with high = #f meaning unbounded above. The
  ;; request's possible positions start just after its cursor.
  ;; A SUCCESSOR'S WHOLE HISTORY IS IN RANGE. When a writer is retired
  ;; and another takes over, the request's possible positions are
  ;; everything above its cursor on its own writer AND every position on
  ;; every writer that succeeded it -- there is no cursor over there to
  ;; compare against, because the request never held one. So an uncertain
  ;; stretch anywhere in a successor touches it, wherever in that writer
  ;; it sits.
  ;;
  ;; Without this the test would answer "clear" for every stretch on
  ;; every generation after the one the client last spoke to, which is
  ;; the generation a retry is most likely to be looking at.
  (define (interval-touches? interval after successors)
    (cond
      ((exists (lambda (w) (string=? w (car interval))) successors) #t)
      ((not (string=? (car interval) (car after))) #f)
      (else
       ;; ITS POSITIONS START JUST ABOVE THE CURSOR AND RUN UP WITHOUT A
       ;; BOUND, so only the stretch's top can put it out of reach. The
       ;; bottom is never compared: a stretch that begins above the
       ;; cursor is still somewhere the request could have landed.
       (let ((high (caddr interval)))
         (or (not high) (> high (cdr after)))))))

  ;; ---- evidence -------------------------------------------------------------

  ;; ONE SCAN, THREE VIEWS. The design names three sets over an identity
  ;; and they answer different questions, so they are three selections
  ;; from one list rather than one list with flags: a set built by
  ;; filtering another cannot answer "which set was this from", and that
  ;; question is exactly what the rules below are asking each time.
  ;;
  ;; A piece of evidence is any readable record whose actor names this
  ;; identity -- applied or pending, in valid history or quarantined,
  ;; torn, in damaged/ or incoming/. Where it was found is carried with
  ;; it, because two of the three sets are defined by where.
  (define (make-evidence event actor deps payload placement delivered? marks)
    (list event actor deps payload placement delivered? marks))
  (define (ev-event e) (list-ref e 0))
  (define (ev-actor e) (list-ref e 1))
  (define (ev-deps e) (list-ref e 2))
  (define (ev-payload e) (list-ref e 3))
  (define (ev-placement e) (list-ref e 4))
  (define (ev-delivered? e) (list-ref e 5))
  (define (ev-marks e) (list-ref e 6))
  (define (ev-marked? e mark) (and (memq mark (ev-marks e)) #t))

  ;; Where a record was found, and whether that place is part of the
  ;; history a reader can verify.
  (define (in-valid-history? e) (eq? (ev-placement e) 'valid-history))
  (define (readable-but-unverifiable? e)
    (and (memq (ev-placement e) '(damaged incoming unlisted)) #t))
  (define (broken-placement? e)
    (and (memq (ev-placement e) '(quarantined torn index-evidence-missing)) #t))

  ;; ① THE COMPARISON SET: what may be compared for sameness. A record
  ;; already known to be superseded or to have disagreed with its plan is
  ;; not evidence of what the request said -- comparing against it would
  ;; report a mismatch the client cannot act on.
  (define (comparison-set evidence)
    (filter (lambda (e)
              (not (or (ev-marked? e 'superseded) (ev-marked? e 'plan-mismatch))))
            evidence))

  ;; ② THE EXECUTED-MEMBER SET: what actually ran. Candidates and
  ;; completion come only from here, and `(#%new k)` is bound only from
  ;; here -- binding it from the plan would be taking an intention for a
  ;; fact.
  ;; A MARK THAT SAYS "NOT THIS ONE" KEEPS A RECORD OUT OF HERE. A
  ;; resolution that names the real execution marks the other candidates
  ;; `superseded`, and a record that disagrees with its plan is marked
  ;; `plan-mismatch`; both are statements that the record is not the
  ;; execution. Leaving them in would let a record that is known not to
  ;; have run be counted as the thing that ran -- and for a record with
  ;; no plan above it, where `membership` answers `valid` on the strength
  ;; of having no plan to disagree with, the mark is the only thing that
  ;; says so.
  (define (executed-member-set evidence)
    (filter (lambda (e)
              (and (ev-delivered? e)
                   (not (ev-marked? e 'superseded))
                   (not (ev-marked? e 'plan-mismatch))
                   (eq? (membership e evidence) 'valid)))
            evidence))

  ;; ③ THE HISTORICAL-UNCERTAIN SET: everything. It answers only "is
  ;; there any evidence at all" and "does anything name the index that is
  ;; missing" -- never what the request did.
  (define (historical-set evidence) evidence)

  ;; ---- membership -----------------------------------------------------------

  ;; THREE STATES AND THE MIDDLE ONE IS NOT A VERDICT. A record whose
  ;; inputs have not arrived is undetermined: it is recomputed on every
  ;; delivery and never marked, because marking it would make the answer
  ;; depend on the order records happened to arrive in. Only when every
  ;; input is present and something is provably unequal is it invalid,
  ;; and that is a persistent mark owned by the plan's identity.
  ;;
  ;; Dependencies that have not been applied are PENDING, which is not a
  ;; membership question at all.
  (define (membership e evidence)
    (guard (ex (#t 'invalid)) (membership-checked e evidence)))

  (define (membership-checked e evidence)
    (let ((plan-event (actor-plan-event (ev-actor e))))
      (cond
        ;; a record that belongs to no plan is its own member
        ((not plan-event) 'valid)
        ((not (plan-record-present? plan-event evidence)) 'undetermined)
        ((let ((p (find (lambda (p) (equal? (ev-event p) plan-event)) evidence)))
           (or (not (identity=? (actor-identity (ev-actor e)) (actor-identity (ev-actor p))))
               (not (equal? (actor-fingerprint (ev-actor e)) (actor-fingerprint (ev-actor p))))
               (not (equal? (car (ev-actor e)) (car (ev-actor p))))
               (not (equal? (actor-after (ev-actor e)) (actor-after (ev-actor p))))))
         'invalid)
        ((ev-marked? e 'plan-mismatch) 'invalid)
        (else
         (let ((declared (plan-payload-for plan-event (actor-sub (ev-actor e)) evidence)))
           (cond
             ;; THE PLAN IS HERE AND DECLARES NO SUCH INDEX. That is not
             ;; "an input has not arrived" -- the input arrived and said
             ;; no. Reading the two as one answer would leave a record
             ;; claiming a slot its plan never had waiting for ever for
             ;; something that already came.
             ((eq? declared 'absent) 'invalid)
             (else
              ;; `("#%new" k)` STANDS FOR THE BLOCK SUB-OPERATION k MADE,
              ;; and it is bound from the record that made it -- never
              ;; from the plan, which says only what was meant to happen.
              ;; Until k's record is here and applied there is nothing to
              ;; bind it to and the comparison cannot be made, which is
              ;; `undetermined` and not a verdict.
              (let ((bound (bind-new declared evidence)))
                (cond
                  ((eq? bound 'unbound) 'undetermined)
                  ((intent-produced? bound (ev-payload e)) 'valid)
                  (else 'invalid))))))))))

  ;; The immutable declaration has a complete, contiguous index set.
  (define (plan? p)
    (and (list? p) (= 5 (length p)) (eq? (car p) 'plan)
         (req-id-ok? (cadr p)) (string? (caddr p)) (event-id? (cadddr p))
         (list? (list-ref p 4))
         (let loop ((es (list-ref p 4)) (i 0))
           (or (null? es)
               (and (pair? (car es)) (eqv? (caar es) i)
                    (list? (cdar es)) (pair? (cdar es)) (symbol? (cadar es))
                    (loop (cdr es) (+ i 1)))))))

  ;; One admission table for reduction and request evidence. Inputs have
  ;; causal availability (before plan filtering); deps may include their
  ;; transitive past. Validate each index using only earlier, unique valid
  ;; bindings, detect duplicates, THEN check execution order.
  (define (request-gates evidence)
    (for-each (lambda (e) (trace-event! 'identity-probe (ev-event e) #f)) evidence)
    (apply append (map identity-gates (group-by-identity evidence))))

  (define (identity-gates es)
    (let ((statuses '()) (bindings '()))
      (define (status e)
        (let ((p (assoc (ev-event e) statuses))) (and p (cdr p))))
      (define (mark! e why)
        (set! statuses (cons (cons (ev-event e) why)
                              (filter (lambda (p) (not (equal? (car p) (ev-event e))))
                                      statuses))))
      (define (declaration-ok? e)
        (let ((p (ev-payload e)) (a (ev-actor e)))
          (if (not (eq? (actor-sub a) 'plan))
              (not (and (pair? p) (memq (car p) '(plan batch))))
              (and (or (plan? p) (receipt? p))
                   (equal? (cadr p) (cdr (actor-identity a)))
                   (equal? (caddr p) (actor-fingerprint a))
                   (equal? (cadddr p) (actor-after a))))))
      (define (classify-slot! slot)
        (let ((valid '()))
          (for-each
            (lambda (e)
              (cond
                ((not (ev-delivered? e)) (mark! e 'pending-plan))
                ((not (declaration-ok? e)) (mark! e 'plan-mismatch))
                ((and (integer? (actor-sub (ev-actor e)))
                      (not (actor-plan-event (ev-actor e))))
                 (mark! e 'plan-mismatch))
                ((not (actor-plan-event (ev-actor e)))
                 (set! valid (cons e valid)))
                (else
                 (case (membership e bindings)
                   ((valid) (set! valid (cons e valid)))
                   ((invalid) (mark! e 'plan-mismatch))
                   (else (mark! e 'pending-plan))))))
            slot)
          (for-each (lambda (e) (mark! e (if (> (length valid) 1) 'plan-conflict 'valid))) valid)
          (when (= 1 (length valid)) (set! bindings (cons (car valid) bindings)))))
      ;; Unindexed records first: the plan must be available to its items.
      (for-each
        (lambda (sub)
          (classify-slot! (filter (lambda (e) (eq? (actor-sub (ev-actor e)) sub)) es)))
        '(plan single))
      (let ((indices (list-sort <
                       (fold-left (lambda (out e)
                         (let ((i (actor-sub (ev-actor e))))
                           (if (and (integer? i) (not (memv i out))) (cons i out) out)))
                         '() es))))
        (for-each
          (lambda (i)
            ;; More than one plan event cannot supply one binding space.
            (let ((slot (filter (lambda (e) (eqv? (actor-sub (ev-actor e)) i)) es)))
              (for-each
                (lambda (pe)
                  (classify-slot!
                    (filter (lambda (e) (equal? (actor-plan-event (ev-actor e)) pe)) slot)))
                (fold-left (lambda (out e)
                             (let ((pe (actor-plan-event (ev-actor e))))
                               (if (member pe out) out (cons pe out)))) '() slot))))
          indices))
      (for-each
        (lambda (e)
          (let* ((a (ev-actor e)) (i (actor-sub a)) (pe (actor-plan-event a)))
            (when (and (integer? i) (eq? (status e) 'valid))
              (let ((previous
                      (filter (lambda (p)
                        (if (= i 0)
                            (equal? (ev-event p) pe)
                            (and (equal? (actor-plan-event (ev-actor p)) pe)
                                 (eqv? (actor-sub (ev-actor p)) (- i 1))))) es)))
                (let ((eligible (filter (lambda (p) (eq? (status p) 'valid)) previous)))
                  (cond
                    ((null? previous) (mark! e 'plan-order))
                    ((not (= 1 (length eligible))) (mark! e 'pending-plan))
                    ((not (linked-to? e (ev-event (car eligible)))) (mark! e 'plan-order))))))))
        (list-sort (lambda (a b)
                     (< (if (integer? (actor-sub (ev-actor a))) (actor-sub (ev-actor a)) -1)
                        (if (integer? (actor-sub (ev-actor b))) (actor-sub (ev-actor b)) -1))) es))
      statuses))

  ;; ---- did this record come from that intent --------------------------------

  ;; A PLAN DECLARES AN INTENT, NOT A RESULT. "Persist the plan before
  ;; executing" means the plan says what was meant to happen; what it
  ;; will look like is not knowable before the earlier sub-operations
  ;; have happened. So the comparison is not payload against payload.
  ;;
  ;; WHICH FIELDS COME FROM THE INTENT IS A TABLE, one row per verb, and
  ;; the table is the whole of the rule. `ord` is deliberately absent: it
  ;; is decided by the state at the time -- which siblings were there --
  ;; and a completion months later must compute it from the siblings it
  ;; finds, not replay a number from a state nobody has any more. A
  ;; record whose `ord` differs from what the plan would have produced is
  ;; still that intent carried out.
  ;;
  ;; `after` IS NOT COMPARED EITHER, and for a different reason: it never
  ;; reaches the payload at all. It is an argument to the ordering, and
  ;; the only trace it leaves is the `ord` this rule excludes. Listing it
  ;; would be listing a field the record does not have.
  (define (intent-produced? intent payload)
    (and (pair? intent) (pair? payload)
         (case (car intent)
           ;; (insert <parent> <after> <fields>) -> (put <fields + parent + ord>)
           ((insert)
            (and (eq? (car payload) (quote put))
                 (let ((got (cadr payload)))
                   (and (equal? (field-of got (quote parent)) (list-ref intent 1))
                        (fields-agree? (list-ref intent 3) got)))))
           ;; (set <id> <field> <value> [<expect>]) -> the same, verbatim
           ((set) (equal? intent payload))
           ((del) (equal? intent payload))
           ((link unlink) (equal? intent payload))
           ;; (move <id> <parent> <after>) -> (move <id> <parent> <ord>)
           ((move)
            (and (eq? (car payload) (quote move))
                 (equal? (list-ref payload 1) (list-ref intent 1))
                 (equal? (list-ref payload 2) (list-ref intent 2))))
           ;; (tag <name>) -> (tag <name> <cut>): the cut is the moment,
           ;; not the intent.
           ((tag)
            (and (eq? (car payload) (quote tag))
                 (equal? (list-ref payload 1) (list-ref intent 1))))
           ;; A VERB THE TABLE DOES NOT NAME IS NOT COMPARED LENIENTLY.
           ;; This build does not know which of its payload came from the
           ;; intent, so it cannot say the record carried the intent out.
           (else #f))))

  (define (field-of alist name)
    (let ((e (assq name alist))) (and e (cdr e))))

  ;; Insert adds only parent and ord. Extra caller fields would execute
  ;; work the plan did not declare; field presence also distinguishes #f
  ;; from absence. Ordering of the alist does not matter.
  ;; THE FIELDS THE STORE PUTS IN EVERY RECORD ITSELF, named once. Three
  ;; places need this list and they need the same one: the write path
  ;; refuses a caller who tries to set them, the reduction reads them as
  ;; position rather than as fields, and membership must not count them
  ;; against a record for carrying what nobody asked it to carry. Three
  ;; hand-written copies would agree until the day somebody added a
  ;; fourth name -- and the one that lagged would start calling correct
  ;; executions mismatches, which is the failure that reports a request
  ;; as never having run.
  (define store-supplied-fields '(parent ord))

  ;; BOTH DIRECTIONS, AND THE ANSWER IS A BOOLEAN. Every field the intent
  ;; named must be there with the value it named, and every field the
  ;; record carries must be one the intent named -- except the ones the
  ;; store supplies for every insert, which no intent ever states.
  ;;
  ;; `and`/`for-all` HAND BACK THE LAST VALUE, NOT #t. `memq` answers the
  ;; tail it found, so this returned `(ord)` to a caller comparing against
  ;; #t -- true enough to work everywhere the value was only tested, and
  ;; wrong everywhere it was compared. A predicate answers a boolean.
  (define (fields-agree? wanted got)
    (and (for-all (lambda (f)
                   (let ((at (assq (car f) got))) (and at (equal? (cdr at) (cdr f))))) wanted)
         (for-all (lambda (f) (or (memq (car f) store-supplied-fields)
                                  (assq (car f) wanted)))
                  got)
         #t))

  ;; THE ID A SUB-OPERATION'S RECORD CREATED, derived from where the
  ;; record sits rather than carried in it: the store derives a new
  ;; block's id from its writer and sequence, and a second rule for the
  ;; same derivation here would be a second supplier of every id.
  (define (created-id event)
    (string-append (car event) "." (number->base36 (cdr event))))

  (define base36-digits "0123456789abcdefghijklmnopqrstuvwxyz")

  (define (number->base36 n)
    (if (= n 0)
        "0"
        (let loop ((n n) (acc (quote ())))
          (if (= n 0)
              (list->string acc)
              (loop (div n 36)
                    (cons (string-ref base36-digits (mod n 36)) acc))))))

  ;; Replace every `("#%new" k)` with the id index k's applied record
  ;; created, or answer `unbound` if any of them has no such record.
  (define (bind-new x evidence)
    (cond
      ((and (pair? x) (equal? (car x) new-marker) (pair? (cdr x)))
       (let ((e (applied-index evidence (cadr x))))
         (if (and e (pair? (ev-payload e)) (eq? (car (ev-payload e)) 'put))
             (created-id (ev-event e)) (quote unbound))))
      ((pair? x)
       (let ((a (bind-new (car x) evidence)))
         (if (eq? a (quote unbound))
             (quote unbound)
             (let ((d (bind-new (cdr x) evidence)))
               (if (eq? d (quote unbound)) (quote unbound) (cons a d))))))
      (else x)))

  ;; IT MUST NOT ASK ABOUT MEMBERSHIP, and that is not an optimisation.
  ;; `membership` is what calls this, through `bind-new`; going back
  ;; through `executed-member-set` -- which asks `membership` of every
  ;; record -- would not terminate. It does not need to: what is wanted
  ;; here is WHERE index k's record sits, and a record's position is not
  ;; a question about whether its payload matched its plan.
  ;;
  ;; TWO RECORDS AT ONE INDEX BIND NOTHING. They are a contested slot,
  ;; reported separately, and choosing either would make every payload
  ;; that mentions the index depend on which was found first.
  (define (applied-index evidence index)
    (let ((at (filter (lambda (e)
                        (and (ev-delivered? e)
                             (eq? (ev-placement e) (quote valid-history))
                             (not (ev-marked? e (quote superseded)))
                             (not (ev-marked? e (quote plan-mismatch)))
                             (eqv? (actor-sub (ev-actor e)) index)))
                      evidence)))
      (and (= 1 (length at)) (car at))))

  (define (plan-record-present? plan-event evidence)
    (exists (lambda (e)
              (and (ev-delivered? e)
                   (equal? (ev-event e) plan-event)
                   (pair? (ev-payload e))
                   (plan? (ev-payload e))))
            evidence))

  ;; The payload the plan declared for one index, or `absent`.
  (define (plan-payload-for plan-event index evidence)
    (let loop ((es evidence))
      (cond
        ((null? es) 'absent)
        ((and (equal? (ev-event (car es)) plan-event)
              (pair? (ev-payload (car es)))
              (eq? (car (ev-payload (car es))) 'plan))
         (let ((entries (list-ref (ev-payload (car es)) 4)))
           (let ((entry (assv index entries)))
             (if entry (cdr entry) 'absent))))
        (else (loop (cdr es))))))

  ;; `(#%new k)` stands for a block a previous sub-operation created, so
  ;; a payload still holding one cannot yet be compared with what was
  ;; written.
  ;; The marker is a STRING, as every reserved marker in this format is:
  ;; the storable subset refuses uninterned symbols, and a bare `#%new`
  ;; is not a symbol an R6RS reader will take. `#%absent`, `#%quote` and
  ;; `#%dot` are spelled the same way.
  (define new-marker "#%new")

  (define (unresolved-new? x)
    (cond
      ((and (pair? x) (equal? (car x) new-marker)) #t)
      ((pair? x) (or (unresolved-new? (car x)) (unresolved-new? (cdr x))))
      (else #f)))

    ;; THE FINGERPRINT IS OVER A CANONICAL SERIALISATION, not over the
  ;; fields compared one by one. Comparing field by field invites a
  ;; comparison that forgets a field -- and the field it forgets is the
  ;; one nobody thought of, which is the same field the client changed.
  ;;
  ;; The verb is in it, and so is every argument, so `(set b "x")` and
  ;; `(set b "y")` and `(delete b)` under one request id are three
  ;; different requests and all report the mismatch.
  (define (request-fingerprint who verb args after)
    (bytevector->hex
      (sha256
        (string->utf8
          (sexpr->string-extended
            (append (list who verb) args (list after)))))))
)
