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

;;; (theourgia reduce) -- deterministic reduction (design 9.1 through 9.5).
;;;
;;; THE WHOLE POINT IS THAT ORDER DOES NOT MATTER. Two libraries that
;;; have received the same records must hold the same state, however
;;; those records arrived: replayed from empty, resumed from a snapshot,
;;; in one batch or twenty, in any enumeration order the segments happen
;;; to give. Everything below follows from that.
;;;
;;; WHICH IS WHY A WRITE DOES NOT OVERWRITE. A field holds a SET of
;;; candidates, and a new write removes only the candidates in its own
;;; causal past -- what it could have seen. Two writers who did not see
;;; each other leave two candidates, and that is a conflict rather than a
;;; race whose winner depends on arrival order. "Last write wins" needs a
;;; clock, and there is no clock two machines agree on.
;;;
;;; CAUSAL PAST IS COMPUTED ONCE PER EVENT, not per comparison. For an
;;; event e it is the per-writer high-water mark of everything e could
;;; have seen: its deps, its own predecessor, and transitively theirs.
;;; Records are applied in causal order, so each of those is already
;;; known when e is applied. Asking "is candidate c in past(e)" is then
;;; one lookup and one comparison, not a graph walk.
;;;
;;; NOTHING HERE TOUCHES A FILE. The log layer decides what has been
;;; received and hands it over; this layer decides what it means. Keeping
;;; the two apart is what lets the same records be reduced from a
;;; snapshot and from empty and be checked against each other.
(library (theourgia reduce)
  (export datum-spelling payload-reason caller-fields-reason caller-payload-reason
          reduce-empty reduce-clone reduce-rehearsal? state-written-records reduce-apply! reduce-pending reduce-noted
          reduce-applied-cut reduce-trace reduce-gates
          known-kinds kind-known? known-classes vocabulary-fields vocabulary-known?
          vocabulary-not-known-reason effect-relation-names state-edges state-effect-relation?
          declaration-kinds relation-kind state-declared-relations state-relation-contested
          state-declaration rule-value-reason state-rule state-declared-rules state-rule-contested
          text-field-types value-kind
          state-read state-outline outline-subtree state-dump state-hash state-datum block-hash
          state-path-claimants state-duplicated-paths
          reserved-relation-names state-reserved-relation-records
          state-structure reduction-facts state-refs state-tags state-event-cut state-event-past cut-usable? cut-id
          cut-covers? state-put-events block-creation event-actor state-field-events state-field-contested? state-block-ids state-link-events
          state->rows rows->state
          state-consumed? state-consumption state-consumed-completions
          state-consumed-parent-cuts state-seen state-revoked draft-version
          cut-join
          ord-between block-id
          reduction? reduction-state)
  (import (rnrs base) (rnrs control) (rnrs lists) (rnrs sorting)
          (rnrs records syntactic) (rnrs hashtables) (rnrs arithmetic fixnums)
          (rnrs unicode) (rnrs io simple)
          (only (rnrs io ports) call-with-string-output-port)
          (only (theourgia trace) trace-event!)
          (only (theourgia request) store-supplied-fields
                request-actor? actor-identity actor-sub
                actor-plan-event make-evidence request-gates
                plan? plan-consumes consumes?)
          (theourgia admission)
          (only (theourgia datum-metadata) datum-doc-marker? datum-doc-format?)
          (only (theourgia wire) sexpr->string-extended wire-safe-symbol? storable-encode)
          (only (theourgia digest) sha256 bytevector->hex)
          (only (rnrs bytevectors) string->utf8 bytevector? bytevector-copy! make-bytevector bytevector-length))

  ;; ---- ids ------------------------------------------------------------------

  ;; A BLOCK IS NAMED BY THE EVENT THAT CREATED IT, and the name is
  ;; derived rather than supplied: an id the caller chooses is an id two
  ;; callers can choose the same. base36 keeps it short without
  ;; introducing a separator that could occur in a writer id.
  (define (block-id writer seq)
    (string-append writer "." (number->base36 seq)))

  (define base36-digits "0123456789abcdefghijklmnopqrstuvwxyz")

  (define (number->base36 n)
    (if (= n 0)
        "0"
        (let loop ((n n) (acc '()))
          (if (= n 0)
              (list->string acc)
              (loop (div n 36)
                    (cons (string-ref base36-digits (mod n 36)) acc))))))

  ;; ---- causal past ----------------------------------------------------------

  ;; A per-writer high-water mark: writer -> highest seq this event could
  ;; have seen. Stored as a sorted alist so that two of them can be
  ;; compared and printed deterministically.
  (define (past-covers? past writer seq)
    (let ((e (assoc writer past)))
      (and e (>= (cdr e) seq))))

  (define (past-join a b)
    (let loop ((rest b) (out a))
      (cond
        ((null? rest) out)
        (else
         (let* ((w (car (car rest))) (s (cdr (car rest))) (have (assoc w out)))
           (loop (cdr rest)
                 (if have
                     (if (> s (cdr have))
                         (cons (cons w s) (remp (lambda (e) (string=? (car e) w)) out))
                         out)
                     (cons (cons w s) out))))))))

  (define (past-sorted past)
    (list-sort (lambda (x y) (string<? (car x) (car y))) past))

  ;; ---- the reduction --------------------------------------------------------

  ;; blocks   -- alist of id to block
  ;; links    -- list of (from rel to event-id)
  ;; tags     -- alist of name to list of (cut . event-id)
  ;; pasts    -- alist of event-id to that event's causal past
  ;; applied  -- alist of writer to the highest sequence applied
  ;; pending  -- records whose premises have not all arrived
  ;; trace    -- event-ids in the order they were applied
  ;; noted    -- integrity observations this layer made while applying
  ;; declared-relations -- alist of a declared relation name to its candidates
  ;; declared-rules -- alist of a rule name to its candidates, the same way
  (define-record-type reduction
    (fields (mutable blocks)
            (mutable links)
            (mutable tags)
            (mutable pasts)
            (mutable applied)
            (mutable pending)
            (mutable trace)
            (mutable noted) (mutable history) (mutable gates)
            (mutable admission-index)
            (mutable consumption)
            (mutable declared-relations)
            (mutable declared-rules)
            ;; A REHEARSAL'S COPY (reduce-clone), and only that: a trace line
            ;; emitted while reducing it is marked (rehearsal), and the
            ;; store's fault seam is not consulted for it.
            rehearsal?))

  ;; ---- the name of a draft's content ---------------------------------------
  ;;
  ;; `version = sha256(bytes || based-on || cut)`. It lives here, rather
  ;; than beside the drafts, because two layers need it and a second copy
  ;; of a hash rule is a second answer waiting to disagree: the draft
  ;; store computes it when a draft is saved, and the completion path
  ;; checks it against the text a plan froze.
  ;;
  ;; NOTE: THE CONCATENATION IS UNAMBIGUOUS ONLY BECAUSE THE MIDDLE FIELD IS
  ;; A FIXED-WIDTH HEX DIGEST. `bytes` is arbitrary and comes first, so a
  ;; variable-width baseline would let two different pairs hash alike --
  ;; and a collision here answers "this is the draft you sent" for a
  ;; draft nobody sent. Sixty-four characters of the hex alphabet, or
  ;; this refuses to name anything.
  (define draft-hash-width 64)
  (define (draft-hex-digit? c)
    (or (char<=? #\0 c #\9) (char<=? #\a c #\f)))

  (define (draft-version bytes based-on cut)
    (unless (and (string? based-on) (= (string-length based-on) draft-hash-width)
                 (for-all draft-hex-digit? (string->list based-on)))
      (assertion-violation 'draft-version "A baseline is a fixed-width block hash" based-on))
    (bytevector->hex
      (sha256
        (let* ((tail (string->utf8
                       (string-append based-on
                                      (sexpr->string-extended (storable-encode cut)))))
               (out (make-bytevector (+ (bytevector-length bytes) (bytevector-length tail)))))
          (bytevector-copy! bytes 0 out 0 (bytevector-length bytes))
          (bytevector-copy! tail 0 out (bytevector-length bytes) (bytevector-length tail))
          out))))

  ;; ---- what a commit consumed ----------------------------------------------
  ;;
  ;; A plan may name the draft versions its request took as input
  ;; (`request.sc`, `consumes?`). A draft is CONSUMED once such a plan
  ;; has completed, and the log is the only place that says so: the
  ;; earlier design kept an immutable packet beside each draft, which
  ;; meant two stores to keep in step and a scan of all of them on every
  ;; read.
  ;;
  ;; TWO TABLES, BECAUSE THERE ARE TWO QUESTIONS. "Is this draft
  ;; consumed" is asked per (owner, version) and must not walk the
  ;; history -- a store with a thousand unrelated commits answers it in
  ;; the same number of probes as one with ten. "Has this plan
  ;; completed" is asked per plan event, as its members arrive.
  ;;
  ;; COMPLETION IS A MEMBER EVENT, NOT THE PLAN EVENT. The plan is
  ;; written before its members, so its cut is earlier than the state the
  ;; commit produced; taking it as the causal parent of a sequential edit
  ;; reports that edit as stale against its own predecessor. An EMPTY
  ;; plan is the exception and completes at the plan event, because there
  ;; is no member to be later than.
  ;;
  ;; KEY: WHICH MEMBER: THE GREATEST, BY (writer, seq) -- NOT "the one that
  ;; happened to be applied last". Those agree for every plan this core
  ;; writes, whose members are consecutive records of one writer. They
  ;; would not agree for a plan whose members came from two writers, and
  ;; there the arrival order is not a fact about the store: two replicas
  ;; folding the same records would record different completions, and a
  ;; snapshot would preserve whichever the cutting replica saw. A
  ;; deterministic maximum is the same in both, which is also what lets
  ;; the index be rebuilt from a snapshot that did not carry it.
  ;;
  ;; DELIVERED IS NOT APPLIED. A member record that is present but gated
  ;; -- waiting on a dependency that has not arrived -- has not been
  ;; applied, and a plan whose members are all present but not all
  ;; applied has not completed. This table is fed from `apply-one!` for
  ;; that reason and no other.
  (define-record-type plan-entry
    (fields owner items declared (mutable applied) (mutable completion)))

  ;; A THIRD TABLE, AND IT IS NOT EVIDENCE OF ANYTHING.
  ;;
  ;; `by-seen` answers one question: which plan named this (owner,
  ;; version), whether or not that plan was ever applied. A plan that a
  ;; conflict has since gated never reaches `apply-one!`, so the other
  ;; two tables cannot say that a draft WAS consumed by a request that
  ;; has since been taken back -- and that is exactly the state a
  ;; retired draft is left in: the file is gone, the consumption is
  ;; revoked, and the person wants their text back.
  ;;
  ;; NEVER: IT IS NOT CONSUMPTION AND IT IS NOT REPLAY EVIDENCE. `consumed?`
  ;; reads `by-key`, which is fed from applied records only. This table
  ;; is read by `drafts` and by `restore`, to say "a plan named this,
  ;; here it is" and nothing more.
  (define-record-type (consumption make-consumption-record consumption?)
    (fields by-key by-plan by-seen by-seen-plan by-member))

  (define (make-consumption)
    (make-consumption-record (make-hashtable string-hash string=?)
                             (make-hashtable string-hash string=?)
                             (make-hashtable string-hash string=?)
                             (make-hashtable string-hash string=?)
                             (make-hashtable string-hash string=?)))

  ;; THE KEYS ARE STRINGS WITH A SEPARATOR THAT CANNOT OCCUR IN EITHER
  ;; HALF. A writer id and a version are both drawn from a restricted
  ;; alphabet that has no NUL in it, so `a\x0;b` cannot be spelled two
  ;; ways -- concatenating without a separator lets ("ab","c") and
  ;; ("a","bc") collide, and a collision here answers "consumed" for a
  ;; draft nobody consumed.
  (define (draft-key owner version) (string-append owner "\x0;" version))
  (define (event-key e) (string-append (car e) "\x0;" (number->string (cdr e))))

  (define (consumption-clear! r) (reduction-consumption-set! r (make-consumption)))

  ;; NOTED WHEN THE RECORD ARRIVES, not when it is applied -- that is the
  ;; whole difference between this table and the other two.
  ;; WHICH PLAN A MEMBER BELONGS TO, noted when the member ARRIVES.
  ;;
  ;; KEY: A REVOKED CONSUMPTION IS OFTEN GATED AT THE MEMBER, NOT AT THE
  ;; PLAN. A second record claiming a member's slot puts the MEMBERS in
  ;; conflict and leaves the plan itself applied; a lookup that used the
  ;; gated event as a plan key found nothing, so the draft the commit
  ;; had already retired was reported neither as a draft nor as revoked
  ;; -- and `restore` answered `unknown-version` for work that was only
  ;; in the log.
  (define (consumption-note-member-of! r event plan-event)
    (hashtable-set! (consumption-by-member (reduction-consumption r))
                    (event-key event) plan-event))

  (define (consumption-note-seen! r event payload)
    (let ((c (plan-consumes payload)))
      (when c
        (let ((owner (cadr c)))
          (for-each
            (lambda (item)
              (let* ((k (draft-key owner (cadr item)))
                     (have (hashtable-ref (consumption-by-seen (reduction-consumption r)) k '())))
                (unless (exists (lambda (x) (equal? (car x) event)) have)
                  (hashtable-set! (consumption-by-seen (reduction-consumption r)) k
                                  (cons (cons event item) have)))))
            (caddr c))
          ;; THE DECLARED SUB-OPERATIONS TRAVEL WITH IT, because the
          ;; text a `restore` puts back is the text the plan froze --
          ;; `(set <block> src <text>)` -- and looking it up later would
          ;; mean finding the record again.
          (hashtable-set! (consumption-by-seen-plan (reduction-consumption r))
                          (event-key event)
                          (list owner (caddr c) (list-ref payload 4)))))))

  ;; A CONSUMPTION THAT WAS TAKEN BACK.
  ;;
  ;; A commit retired the drafts it consumed -- the files are gone -- and
  ;; then a second record claiming its identity put the plan in conflict.
  ;; The consumption is revoked, and the person's text is now only in the
  ;; log. `drafts` has to say so, or the work looks deleted.
  ;;
  ;; KEY: THE CANDIDATES ARE THE GATED PLANS, NOT EVERY VERSION EVER NAMED.
  ;; Walking the whole `seen` table would be the packet scan again in
  ;; another shape -- a cost that grows with how long the writer has been
  ;; working. A revoked consumption is a plan that is GATED, and gates
  ;; are the conflicts, of which there are few.
  ;; THE TEXT THE PLAN FROZE FOR ONE BLOCK, or #f when the plan declared
  ;; no sub-operation for it -- which is what an `unchanged` item looks
  ;; like: it was consumed, and its bytes are the committed src at its
  ;; own cut rather than anything the plan carries.
  (define (declared-src entries block)
    (let loop ((es entries))
      (cond
        ((null? es) #f)
        ((and (pair? (car es)) (list? (cdar es)) (>= (length (cdar es)) 4)
              (eq? 'set (car (cdar es)))
              (equal? block (cadr (cdar es)))
              (eq? 'src (caddr (cdar es))))
         (cadddr (cdar es)))
        (else (loop (cdr es))))))

  ;; NOTE: ONE ENTRY PER VERSION, NOT PER GATED PLAN. Two plans claiming one
  ;; identity are both gated and both name the same version, so a caller
  ;; that mapped over the gates listed the same revoked draft twice --
  ;; and a person reading `drafts` would have seen one piece of work
  ;; described as two.
  ;; ONE PER VERSION, AND WHICH ONE IS NOT THE DELIVERY'S: of the plans that
  ;; name a version, the smallest plan event by (writer, seq); the versions
  ;; in order. The first one met was the one kept, and the plans are met in
  ;; gate order, which is delivery order -- two conflicting plans gave
  ;; `drafts` a different plan event by route.
  (define (state-revoked r owner)
    (define (event<? a b)
      (or (string<? (car a) (car b)) (and (string=? (car a) (car b)) (< (cdr a) (cdr b)))))
    (define (version-of x) (cadr (car x)))
    (let loop ((all (state-revoked-raw r owner)) (out '()))
      (if (null? all)
          (list-sort (lambda (x y) (string<? (version-of x) (version-of y))) out)
          (let* ((x (car all))
                 (have (find (lambda (o) (equal? (version-of o) (version-of x))) out)))
            (loop (cdr all)
                  (cond ((not have) (cons x out))
                        ((event<? (cadr x) (cadr have)) (cons x (remq have out)))
                        (else out)))))))

  (define (state-revoked-raw r owner)
    (let ((c (reduction-consumption r)))
      (apply append
        (map (lambda (g)
               ;; NEVER: ONLY A PLAN THAT WAS TAKEN BACK, NOT ONE THAT IS
               ;; WAITING. `pending-plan` means a dependency has not
               ;; arrived: nothing was consumed, nothing was retired, and
               ;; the draft is still on disk and still a draft. Reporting
               ;; it as revoked tells the person their work was undone by
               ;; a commit that has not happened yet.
               (let* ((plan-event
                        (or (and (hashtable-ref (consumption-by-seen-plan c) (event-key (car g)) #f)
                                 (car g))
                            (hashtable-ref (consumption-by-member c) (event-key (car g)) #f)))
                      (seen (and (memq (cdr g) '(plan-conflict plan-mismatch))
                                 plan-event
                                 (hashtable-ref (consumption-by-seen-plan c)
                                                (event-key plan-event) #f))))
                 (if (and seen (equal? owner (car seen)))
                     (map (lambda (item) (list item (car g) (declared-src (caddr seen) (car item))))
                          (filter (lambda (item)
                                    (not (state-consumed? r owner (cadr item))))
                                  (cadr seen)))
                     '())))
             (reduction-gates r)))))

  ;; WHAT A PLAN NAMED THIS VERSION, AND IS THAT PLAN'S CONSUMPTION
  ;; STANDING? A version that was named, is not consumed now, and whose
  ;; file is gone is a REVOKED consumption: the caller decides what to
  ;; do about it, this only reports it.
  (define (state-seen r owner version)
    (hashtable-ref (consumption-by-seen (reduction-consumption r))
                   (draft-key owner version) '()))

  (define (consumption-add-key! c owner version event)
    (let* ((k (draft-key owner version))
           (have (hashtable-ref (consumption-by-key c) k '())))
      (unless (member event have)
        (hashtable-set! (consumption-by-key c) k (cons event have)))))

  ;; A PLAN IS NOTED WHEN IT IS APPLIED, with no completion yet. Its
  ;; members may already be on disk; they are counted as they are
  ;; applied, below, and not before.
  (define (consumption-note-plan! r event payload)
    (let ((c (plan-consumes payload)))
      (when c
        (let* ((owner (cadr c))
               (items (caddr c))
               (declared (length (list-ref payload 4)))
               (entry (make-plan-entry owner items declared '()
                                       (and (= declared 0) event))))
          (hashtable-set! (consumption-by-plan (reduction-consumption r))
                          (event-key event) entry)
          (for-each (lambda (item)
                      (consumption-add-key! (reduction-consumption r)
                                            owner (cadr item) event))
                    items)))))

  (define (event>? a b)
    (if (string=? (car a) (car b)) (> (cdr a) (cdr b)) (string>? (car a) (car b))))

  (define (greatest-event events)
    (and (pair? events)
         (fold-left (lambda (best e) (if (event>? e best) e best)) (car events) (cdr events))))

  (define (consumption-note-member! r event plan-event index)
    (let* ((c (reduction-consumption r))
           (entry (hashtable-ref (consumption-by-plan c) (event-key plan-event) #f)))
      (when entry
        (unless (assv index (plan-entry-applied entry))
          (plan-entry-applied-set! entry (cons (cons index event) (plan-entry-applied entry))))
        (plan-entry-completion-set!
          entry
          (and (= (length (plan-entry-applied entry)) (plan-entry-declared entry))
               (greatest-event (map cdr (plan-entry-applied entry))))))))

  ;; IS THIS DRAFT CONSUMED. One probe for the key, then one pass over
  ;; the plans that named it -- which is bounded by how many requests
  ;; named this exact version, not by the size of the store.
  (define (state-consumed? r owner version)
    (let ((events (hashtable-ref (consumption-by-key (reduction-consumption r))
                                 (draft-key owner version) '())))
      ;; KEY: ONE PROBE PER PLAN THAT NAMED THIS EXACT VERSION, and the
      ;; count is on the trace so a row can read it. The question used to
      ;; be answered by opening every packet the writer had ever written,
      ;; which made a draft more expensive to read the longer the writer
      ;; had been working; a row that only checked the ANSWER would be
      ;; green for that implementation too.
      (let ((mark (and (reduction-rehearsal? r) '(rehearsal))))
        (trace-event! 'consumption-probe (cons owner version) mark)
        (for-each (lambda (e) (trace-event! 'consumption-probe e mark)) events))
      (exists (lambda (e)
                (let ((entry (hashtable-ref (consumption-by-plan (reduction-consumption r))
                                            (event-key e) #f)))
                  (and entry (plan-entry-completion entry) #t)))
              events)))

  ;; THE COMPLETED PLANS THAT NAMED THIS DRAFT, as their completion
  ;; events. A caller that wants the state this commit produced -- the
  ;; causal parent of a sequential edit -- reduces at that event's cut;
  ;; several completions mean several requests consumed the same
  ;; version, and choosing between them is the caller's rule, not this
  ;; table's.
  (define (state-consumed-completions r owner version)
    (let ((c (reduction-consumption r)))
      (filter values
        (map (lambda (e)
               (let ((entry (hashtable-ref (consumption-by-plan c) (event-key e) #f)))
                 (and entry (plan-entry-completion entry))))
             (hashtable-ref (consumption-by-key c) (draft-key owner version) '())))))

  ;; KEY: THE CAUSAL PARENT OF A CONSUMED DRAFT IS THE JOIN OF ITS PLAN'S
  ;; MEMBER CUTS, not the cut of any one member.
  ;;
  ;; For every plan this core writes the two agree: the members are
  ;; consecutive records of one writer, so the last one's cut covers all
  ;; the others. They part company for a plan whose members came from
  ;; two writers -- there the greatest member by (writer, seq) is a
  ;; deterministic CHOICE, and its cut does not contain the other
  ;; writer's member. A sequential edit rebased on it would be standing
  ;; on a state that does not include half of its own predecessor.
  ;;
  ;; AN EMPTY PLAN HAS NO MEMBER and answers with its own event's cut:
  ;; it completed there, and there is nothing later to join.
  (define (cut-join a b)
    (let loop ((rest b) (out a))
      (cond
        ((null? rest) out)
        (else
         (let* ((e (car rest)) (mine (assoc (car e) out)))
           (loop (cdr rest)
                 (if (and mine (>= (cdr mine) (cdr e)))
                     out
                     (cons e (remp (lambda (x) (equal? (car x) (car e))) out)))))))))

  (define (state-consumed-parent-cuts r owner version)
    (let ((c (reduction-consumption r)))
      (filter values
        (map (lambda (e)
               (let ((entry (hashtable-ref (consumption-by-plan c) (event-key e) #f)))
                 (and entry (plan-entry-completion entry)
                      (if (null? (plan-entry-applied entry))
                          (state-event-cut r (plan-entry-completion entry))
                          (fold-left (lambda (acc m)
                                       (let ((cut (state-event-cut r (cdr m))))
                                         (and acc cut (cut-join acc cut))))
                                     '()
                                     (plan-entry-applied entry))))))
             (hashtable-ref (consumption-by-key c) (draft-key owner version) '())))))

  ;; THE WHOLE INDEX AS DATA, sorted, for the snapshot and for a reader
  ;; that wants to say what it found rather than only whether.
  (define (state-consumption r)
    (let ((c (reduction-consumption r)))
      (list-sort
        (lambda (a b) (string<? (car a) (car b)))
        (let-values (((ks vs) (hashtable-entries (consumption-by-plan c))))
          (let loop ((i 0) (out '()))
            (if (= i (vector-length ks))
                out
                (let ((entry (vector-ref vs i)))
                  (loop (+ i 1)
                        (cons (list (vector-ref ks i)
                                    (plan-entry-owner entry)
                                    (plan-entry-items entry)
                                    (plan-entry-declared entry)
                                    (list-sort (lambda (x y) (< (car x) (car y)))
                                               (plan-entry-applied entry))
                                    (plan-entry-completion entry))
                              out)))))))))

  ;; A SNAPSHOT THAT DOES NOT CARRY THE INDEX STILL HAS THE RECORDS.
  ;;
  ;; Snapshots written before this existed have no `consumed` row, and
  ;; leaving the index empty there would make every draft those commits
  ;; consumed visible again as a draft. The records the snapshot does
  ;; carry are enough: a plan names its own consumes list and its
  ;; declared count, and a member's actor names the plan it belongs to.
  ;;
  ;; NEVER: THIS IS NOT A SECOND SUPPLIER. It computes completion by the same
  ;; rule the incremental path uses -- the greatest member event, once
  ;; every declared member is covered by the snapshot's cut -- so the two
  ;; paths answer alike; that is what `rows->state` of a snapshot WITH
  ;; the row and of the same snapshot WITHOUT it are compared on.
  ;;
  ;; ONLY WHAT THE CUT COVERS. A record that is in the history but not
  ;; yet applied has not been applied here either.
  (define (fill-seen-from-history! r)
    (for-each
      (lambda (rec)
        (let ((payload (rec-payload rec)))
          (let ((a (rec-actor rec)))
            (when (and (pair? payload) (eq? 'plan (car payload)) (plan? payload)
                       (not (plan-reason payload)))
              (consumption-note-seen! r (cons (rec-writer rec) (rec-seq rec)) payload))
            (let ((pe (and a (actor-plan-event a))) (sub (and a (actor-sub a))))
              (when (and pe (integer? sub))
                (consumption-note-member-of! r (cons (rec-writer rec) (rec-seq rec)) pe))))))
      (reverse (reduction-history r))))

  (define (rebuild-consumption-from-history! r)
    (let ((cut (reduce-applied-cut r))
          (records (reverse (reduction-history r))))
      (define (covered? rec)
        (past-covers? cut (rec-writer rec) (rec-seq rec)))
      (for-each
        (lambda (rec)
          (let ((payload (rec-payload rec)))
            (when (and (covered? rec) (pair? payload) (eq? 'plan (car payload)) (plan? payload)
                       (not (plan-reason payload)))
              (consumption-note-plan! r (cons (rec-writer rec) (rec-seq rec)) payload))))
        records)
      (for-each
        (lambda (rec)
          (let* ((actor (rec-actor rec))
                 (plan-event (and actor (actor-plan-event actor)))
                 (sub (and actor (actor-sub actor))))
            (when (and (covered? rec) plan-event (integer? sub))
              (consumption-note-member! r (cons (rec-writer rec) (rec-seq rec))
                                        plan-event sub))))
        records)))

  (define (restore-consumption! r rows)
    (let ((c (reduction-consumption r)))
      (for-each
        (lambda (row)
          (let ((key (list-ref row 0))
                (owner (list-ref row 1))
                (items (list-ref row 2))
                (declared (list-ref row 3))
                (applied (list-ref row 4))
                (completion (list-ref row 5)))
            (hashtable-set! (consumption-by-plan c) key
                            (make-plan-entry owner items declared applied completion))
            (let ((event (let ((i (let loop ((n 0))
                                    (cond ((= n (string-length key)) #f)
                                          ((char=? #\nul (string-ref key n)) n)
                                          (else (loop (+ n 1)))))))
                           (and i (cons (substring key 0 i)
                                        (string->number (substring key (+ i 1) (string-length key))))))))
              (when event
                (for-each (lambda (item) (consumption-add-key! c owner (cadr item) event))
                          items)))))
        rows)))

  ;; Named blk rather than block so that the record's own accessors do
  ;; not collide with block-id, which is the derivation rule and part of
  ;; this library's interface.
  ;; fields   -- alist of field name to list of (value . event-id)
  ;; position -- list of ((parent . ord) . event-id)
  ;; tomb     -- #f, or the event-id of the delete
  (define-record-type blk
    (fields name
            (mutable fields)
            (mutable position)
            (mutable tomb)))

  (define (reduce-empty)
    (make-reduction '() '() '() '() '() '() '() '() '() '() (make-admission) (make-consumption) '() '() #f))

  ;; A COMPLETE COPY OF A REDUCTION, for a rehearsal of a write: every
  ;; container a reduction changes in place is copied -- the blocks' records,
  ;; the admission tables, the consumption tables and the plan entries in
  ;; by-plan -- and everything it replaces on update is shared. The copy
  ;; answers every question the original answers, and reducing records into
  ;; it changes nothing the original holds. Not the snapshot round trip,
  ;; which drops pending records and filters the history to the cut.
  (define (reduce-clone r)
    (make-reduction (map (lambda (p)
                           (let ((b (cdr p)))
                             (cons (car p) (make-blk (blk-name b) (blk-fields b) (blk-position b) (blk-tomb b)))))
                         (reduction-blocks r))
                    (reduction-links r) (reduction-tags r) (reduction-pasts r)
                    (reduction-applied r) (reduction-pending r) (reduction-trace r)
                    (reduction-noted r) (reduction-history r) (reduction-gates r)
                    (admission-copy (reduction-admission-index r))
                    (consumption-copy (reduction-consumption r))
                    (reduction-declared-relations r) (reduction-declared-rules r)
                    #t))

  (define (consumption-copy c)
    (let ((by-plan (hashtable-copy (consumption-by-plan c) #t)))
      (vector-for-each
        (lambda (k)
          (let ((e (hashtable-ref by-plan k #f)))
            (hashtable-set! by-plan k (make-plan-entry (plan-entry-owner e) (plan-entry-items e)
                                                       (plan-entry-declared e) (plan-entry-applied e)
                                                       (plan-entry-completion e)))))
        (hashtable-keys by-plan))
      (make-consumption-record (hashtable-copy (consumption-by-key c) #t) by-plan
                               (hashtable-copy (consumption-by-seen c) #t)
                               (hashtable-copy (consumption-by-seen-plan c) #t)
                               (hashtable-copy (consumption-by-member c) #t))))

  ;; Whether R is a rehearsal's copy; #f for anything that is not a reduction.
  (define (reduce-rehearsal? r) (and (reduction? r) (reduction-rehearsal? r)))

  ;; THE RECORDS ONE WRITER ADDED FROM SEQ FROM UP TO SEQ TO (exclusive), in
  ;; sequence order: ((<seq> . <payload>) ...), applied or gated, as the
  ;; history holds every record reduced.
  (define (state-written-records r writer from to)
    (map (lambda (rec) (cons (rec-seq rec) (rec-payload rec)))
         (list-sort (lambda (a b) (< (rec-seq a) (rec-seq b)))
                    (filter (lambda (rec) (and (string=? (rec-writer rec) writer)
                                               (<= from (rec-seq rec)) (< (rec-seq rec) to)))
                            (reduction-history r)))))

  (define (reduction-state r) r)
  (define (reduce-pending r) (map record-of (reduction-pending r)))
  (define (reduce-noted r) (reduction-noted r))
  (define (reduce-gates r) (reduction-gates r))
  (define (reduce-trace r) (reverse (reduction-trace r)))
  (define (reduce-applied-cut r)
    (list-sort (lambda (x y) (string<? (car x) (car y)))
               (map (lambda (e) (cons (car e) (cdr e))) (reduction-applied r))))

  ;; A record as this layer receives it: everything the log layer knows
  ;; about one line, with the payload already decoded.
  (define (make-record writer seq deps payload actor) (list writer seq deps payload actor))
  (define (record-of x) (list (car x) (cadr x) (caddr x) (cadddr x)))
  (define (rec-writer x) (car x))
  (define (rec-seq x) (cadr x))
  (define (rec-deps x) (caddr x))
  (define (rec-payload x) (cadddr x))
  (define (rec-actor x) (list-ref x 4))

  ;; APPLICABLE MEANS EVERY PREMISE IS IN. Not "the deps have arrived" --
  ;; applied. A record whose deps are merely present but themselves
  ;; pending would be applied against a state that does not contain them.
  (define (applicable? r rec)
    (and (not (assoc (cons (rec-writer rec) (rec-seq rec)) (reduction-gates r)))
         (record-ready? (reduction-applied r) rec)))

  (define (record-ready? applied rec)
    (let ()
      (define (reached? w s)
        (let ((e (assoc w applied))) (and e (>= (cdr e) s))))
      (and (or (= (rec-seq rec) 1)
               (reached? (rec-writer rec) (- (rec-seq rec) 1)))
           (for-all (lambda (d) (reached? (car d) (cdr d))) (rec-deps rec)))))

  ;; Applies everything that can be applied, then looks again: applying
  ;; one record can make a pending one applicable.
  ;; AN EVENT IS APPLIED ONCE. Delivering it again used to regress the
  ;; applied cursor and bring superseded candidates back as conflicts --
  ;; the log layer delivers each record once, so no caller reaches it
  ;; today, but a reduction that quietly corrupts itself on a repeat is
  ;; not something to leave for the first caller who does.
  (define (reduce-apply! r writer seq deps payload . rest)
    (let ((have (assoc writer (reduction-applied r))))
      (cond
        ((and have (>= (cdr have) seq)) (list 'refused 'already-applied))
        ((exists (lambda (rec) (and (string=? (rec-writer rec) writer)
                                    (= (rec-seq rec) seq)))
                 (reduction-pending r))
         (list 'refused 'already-pending))
        (else
         (let* ((actor (and (pair? rest) (car rest)))
                (rec (make-record writer seq deps payload actor))
                (gates (admission-add! (reduction-admission-index r) rec (reduction-rehearsal? r)))
                (reversed?
                  (exists (lambda (p)
                            (and (not (assoc (car p) (reduction-gates r)))
                                 (event-applied? r (car p)))) gates)))
           (reduction-history-set! r (cons rec (reduction-history r)))
           (let ((payload (rec-payload rec)) (a (rec-actor rec)))
             (when (and (pair? payload) (eq? 'plan (car payload)) (plan? payload)
                        (not (plan-reason payload)))
               (consumption-note-seen! r (cons writer seq) payload))
             (let ((pe (and a (actor-plan-event a))) (sub (and a (actor-sub a))))
               (when (and pe (integer? sub))
                 (consumption-note-member-of! r (cons writer seq) pe))))
           (if reversed?
               (rebuild-request-state! r gates)
               (begin
                 (reduction-gates-set! r gates)
                 (reduction-noted-set! r (merge-gate-notes r gates))
                 (reduction-pending-set! r (append (reduction-pending r) (list rec)))
                 (drain! r))))
         'accepted))))

  (define (event-applied? r id)
    (let ((e (assoc (car id) (reduction-applied r))))
      (and e (>= (cdr e) (cdr id)))))

  ;; Causal availability and request buckets live in the admission index.
  ;; Payload application remains here, including the whole-set rebuild below.
  (define (gate-notes gates)
    (map (lambda (p) (list (cdr p) (list 'event (caar p) (cdar p))))
         (list-sort (lambda (a b) (event<? (car a) (car b))) gates)))

  ;; THE GATE NOTES REPLACE THE GATE NOTES AND NOTHING ELSE. A note is an
  ;; observation about a record the reduction could not use, and there
  ;; are two unrelated kinds: what the membership rule gated, and what
  ;; the interpreter refused -- a malformed payload, a verb this build
  ;; does not know. Writing the first kind over the whole list threw the
  ;; second away, and the record it was about had already been
  ;; interpreted, so nothing would ever say it again. `check` reads these
  ;; notes to decide `damaged`, so a readable but malformed record could
  ;; be reported clean by the arrival of an unrelated plan.
  ;;
  ;; IT IS NOT VISIBLE IN state-hash. The hash covers blocks; a case
  ;; comparing two orders by hash alone agrees while the notes differ,
  ;; which is why the set of notes is compared beside it.
  (define (merge-gate-notes r gates)
    (append (filter (lambda (n)
                      (not (memq (car n) '(plan-mismatch plan-conflict plan-order
                                           pending-plan req-mismatch))))
                    (reduction-noted r))
            (gate-notes gates)))

  ;; FOLDING THE WHOLE SET AGAIN, because there is no other way to take a
  ;; record back. Nothing here undoes anything: the state is dropped and
  ;; rebuilt from the records the reduction kept, so there is ONE path
  ;; from records to state and no second path whose agreement with the
  ;; first would have to be proved. That is the whole reason this is a
  ;; rebuild and not a retraction.
  ;;
  ;; IT IS NOT AN OPTIMISATION PATH; IT IS THE ONLY PATH. The cost is
  ;; paid only when a record reverses a membership that was already
  ;; applied, which is what makes it rare -- see the caller.
  (define (rebuild-request-state! r gates)
    (let ((records (reverse (reduction-history r))))
      (reduction-blocks-set! r '())
      (reduction-links-set! r '())
      (reduction-tags-set! r '())
      (reduction-declared-relations-set! r '())
      (reduction-declared-rules-set! r '())
      (reduction-pasts-set! r '())
      (reduction-applied-set! r '())
      (reduction-trace-set! r '())
      ;; THE INDEX IS DROPPED AND REBUILT WITH EVERYTHING ELSE. It is a
      ;; function of the applied records; keeping it across a rebuild
      ;; would make it a second supplier of the same fact, and the two
      ;; would disagree exactly when a record was taken back.
      (consumption-clear! r)
      ;; THE `seen` TABLE COMES BACK FROM THE RECORDS, because it is
      ;; about arrival and the records are all still here. Only the
      ;; applied half is re-derived by folding.
      (reduction-pending-set! r records)
      (reduction-gates-set! r gates)
      ;; A REBUILD STARTS THE NOTES OVER, and that is the opposite of
      ;; what the other path must do. The retained records are about to
      ;; be folded again, so every note the interpreter makes about a
      ;; record that still reaches it will be made again -- keeping the
      ;; old ones as well left a malformed record noted twice, and how
      ;; many times a rebuild has happened is a function of arrival
      ;; order.
      ;;
      ;; NOT EVERY NOTE COMES BACK, and that is correct rather than a
      ;; loss: a malformed record that depends on one the rebuild now
      ;; gates never reaches the interpreter, so nothing notes it -- and
      ;; a reduction built from those records for the first time says
      ;; the same thing. The
      ;; other path interprets only the new record, so there the older
      ;; notes are the only copy and have to be kept.
      (reduction-noted-set! r (gate-notes gates))
      (fill-seen-from-history! r)
      (drain! r)))

  ;; ONE AT A TIME, AND THEN LOOK AGAIN. Applying a record can make
  ;; another applicable, and that one may sort ahead of records already
  ;; waiting -- so a pass that took the whole ready set and applied it
  ;; before re-checking would run them in an order no scheduler that
  ;; reconsiders would produce. The state is the same either way; the
  ;; ORDER is what a case can assert, and it should be the order the
  ;; contract describes.
  (define (drain! r)
    (let loop ()
      (let ((ready (filter (lambda (rec) (applicable? r rec)) (reduction-pending r))))
        (unless (null? ready)
          (let ((next (car (list-sort
                             (lambda (a b)
                               (if (string=? (rec-writer a) (rec-writer b))
                                   (< (rec-seq a) (rec-seq b))
                                   (string<? (rec-writer a) (rec-writer b))))
                             ready))))
            (apply-one! r next)
            (reduction-pending-set!
              r (remp (lambda (rec) (eq? rec next)) (reduction-pending r)))
            (loop))))))

  (define (apply-one! r rec)
    (let* ((writer (rec-writer rec))
           (seq (rec-seq rec))
           (id (cons writer seq))
           (past (compute-past r writer seq (rec-deps rec))))
      (reduction-pasts-set! r (cons (cons id past) (reduction-pasts r)))
      (reduction-trace-set! r (cons id (reduction-trace r)))
      ;; NOTED HERE, WHERE "APPLIED" IS TRUE. A record that is present
      ;; but gated has not been applied, and a plan whose members are
      ;; all present but not all applied has not completed.
      (let ((payload (rec-payload rec)) (actor (rec-actor rec)))
        ;; NEVER: ONLY A PLAN THIS BUILD WILL APPLY. Registration used to run
        ;; before `interpret!` asked `payload-reason`, so a plan whose
        ;; `consumes` did not cover its own sub-operations was noted as
        ;; malformed AND entered the index: the version it named came
        ;; back as consumed by a record the reduction had refused.
        (when (and (pair? payload) (eq? 'plan (car payload)) (plan? payload)
                   (not (plan-reason payload)))
          (consumption-note-plan! r id payload))
        (let ((plan-event (and actor (actor-plan-event actor)))
              (sub (and actor (actor-sub actor))))
          (when (and plan-event (integer? sub))
            (consumption-note-member! r id plan-event sub))))
      (interpret! r id past (rec-payload rec))
      (reduction-applied-set!
        r (cons (cons writer seq)
                (remp (lambda (e) (string=? (car e) writer)) (reduction-applied r))))))

  (define (compute-past r writer seq deps)
    (let* ((direct (if (> seq 1) (cons (cons writer (- seq 1)) deps) deps)))
      (let loop ((ds direct) (out '()))
        (cond
          ((null? ds) out)
          (else
           (let* ((d (car ds))
                  (theirs (let ((e (assoc d (reduction-pasts r)))) (if e (cdr e) '()))))
             (loop (cdr ds)
                   (past-join (past-join out (list (cons (car d) (cdr d)))) theirs))))))))

  ;; ---- candidate sets -------------------------------------------------------

  ;; A WRITE REMOVES ONLY WHAT IT COULD HAVE SEEN. That is the whole
  ;; rule: candidates whose event is in this event's past are superseded,
  ;; and candidates concurrent with it survive alongside.
  (define (supersede past candidates)
    (remp (lambda (c) (past-covers? past (car (cdr c)) (cdr (cdr c)))) candidates))

  (define (put-candidate past candidates value event-id)
    (cons (cons value event-id) (supersede past candidates)))

  (define (find-block r id)
    (let ((e (assoc id (reduction-blocks r)))) (and e (cdr e))))

  (define (ensure-block! r id)
    (or (find-block r id)
        (let ((b (make-blk id '() '() #f)))
          (reduction-blocks-set! r (append (reduction-blocks r) (list (cons id b))))
          b)))

  ;; ---- interpreting one payload ---------------------------------------------

  ;; ABSENCE IS A TAGGED LIST, not a bare symbol. Section 9.2 spells it
  ;; #%absent, and the codec REFUSES that symbol: it is not wire-safe,
  ;; because reading the text back does not give the symbol again. The
  ;; codec's own convention for exactly this problem is a tagged list --
  ;; ("#%char" n), ("#%sym" text), ("#%quote" datum) -- and a literal
  ;; list that happened to look like the tag is escaped through
  ;; #%quote, so the marker cannot be forged by an ordinary value.
  ;;
  ;; Raised with the design: 9.2 says to print #%absent, and the printer
  ;; will not. This is the form that satisfies both intentions.
  (define absent (list "#%absent"))

  (define (absent? x) (equal? x absent))

;; THE VERB TABLE IS THE WHOLE OF WHAT THIS LAYER UNDERSTANDS, and a
  ;; record it does not understand is an observation, not a no-op. Until
  ;; now the last arm silently did nothing: a record carrying a verb from
  ;; a later version was applied, counted as applied, and changed
  ;; nothing -- so an older build reading a newer store dropped those
  ;; records in silence and reported a state that was missing them
  ;; without saying so. Refusing to understand is the honest answer.
  ;;
  ;; THE BOOKKEEPING VERBS ARE UNDERSTOOD AND FOLD TO NOTHING. `plan`,
  ;; `batch` and `resolve` record what a request did; they advance the
  ;; cursor, they carry deps, they take part in integrity, and they
  ;; change no block. Leaving them to the last arm would report every one
  ;; of them as a verb this build does not know.
  (define (note-verb! r event-id verb)
    (reduction-noted-set!
      r (append (reduction-noted r)
                (list (list 'unknown-verb
                            (list 'event (car event-id) (cdr event-id))
                            (list 'verb verb))))))

  ;; WHAT A RECORD HAS TO LOOK LIKE FOR THIS REDUCER TO APPLY IT, in one
  ;; place, answering a REASON rather than a boolean so that whoever
  ;; refuses can say which part was wrong.
  ;;
  ;; IT HAS TWO CALLERS AND THAT IS DELIBERATE. The write path asks it
  ;; before appending, so a record that cannot be applied is never made
  ;; durable; this reducer asks it before dispatching, so a record that
  ;; got onto disk some other way -- written by a different build, synced
  ;; from another machine, or appended by a version of the write path
  ;; that did not ask -- is reported and skipped instead of ending the
  ;; read. One definition, two call sites: the first half alone leaves
  ;; every store one foreign record away from being unreadable.
  (define (proper-len x)
    (let loop ((y x) (n 0))
      (cond ((null? y) n) ((pair? y) (loop (cdr y) (+ n 1))) (else #f))))

  ;; `parent` AND `ord` ARE THIS REDUCER'S, NOT THE CALLER'S. A `put`
  ;; carries the block's position as two fields the write path computes,
  ;; and `do-put!` reads each with `assq` -- which answers the FIRST
  ;; occurrence. A caller-supplied `ord` therefore came before the
  ;; computed one and won.
  ;;
  ;; WHAT THAT COST, MEASURED: `(insert root #f ((ord . "oops")))` was
  ;; answered `ok`, the record was appended and acknowledged, and the
  ;; store then answered an internal error to `outline` and to every
  ;; later write. It is worse than the collection that could not be
  ;; walked, because that one at least refused: this one told the caller
  ;; the work was done.
  ;;
  ;; REFUSED RATHER THAN DROPPED. Ignoring a field the caller wrote would
  ;; make `(ord . 3)` mean nothing at all, silently; a reserved name is a
  ;; thing to be told about.
  ;; THE SAME LIST THE MEMBERSHIP RULE EXEMPTS, imported rather than
  ;; repeated. What makes a name reserved here and exempt there is one
  ;; fact -- the store supplies it -- so it is written down once.
  (define reserved-fields store-supplied-fields)

  ;; A REPEATED KEY IS AN OVERRIDE CHANNEL, because every reader here asks
  ;; `assq`, which answers the FIRST occurrence. That is the same
  ;; mechanism the reserved names above were abusing -- a second `ord`
  ;; later in the list is invisible, and a second `title` means the one
  ;; the caller can see is not the one that applies.
  (define (duplicate-key alist)
    (let loop ((ps alist) (seen '()))
      (cond
        ((null? ps) #f)
        ((memq (car (car ps)) seen) (car (car ps)))
        (else (loop (cdr ps) (cons (car (car ps)) seen))))))

  ;; THE SHAPE ALONE, so that the caller's rule can ask about shape,
  ;; reserved names and duplicates in the order its remedies make sense.
  (define (shape-only-reason alist)
    (cond
      ((not (proper-len alist)) 'fields-not-a-list)
      (else
       (let loop ((ps alist))
         (cond
           ((null? ps) #f)
           ((not (pair? (car ps))) 'field-entry-not-a-pair)
           ((not (symbol? (car (car ps)))) 'field-name-not-a-symbol)
           (else (loop (cdr ps))))))))

  (define (field-alist-reason alist)
    (cond
      ((not (proper-len alist)) 'fields-not-a-list)
      (else
       (let loop ((ps alist))
         (cond
           ((null? ps)
            (and (duplicate-key alist) 'field-name-repeated))
           ((not (pair? (car ps))) 'field-entry-not-a-pair)
           ((not (symbol? (car (car ps)))) 'field-name-not-a-symbol)
           (else (loop (cdr ps))))))))

  ;; SHAPE, THEN RESERVED NAMES, THEN DUPLICATES -- in that order, and the
  ;; order is the policy. `((ord . 1) (ord . 2))` is both a repeated key
  ;; and a reserved one, and answering `field-name-repeated` tells the
  ;; caller to remove one of them; removing one leaves a field they were
  ;; never allowed to write. The reason that speaks has to be the one
  ;; whose remedy works.

  ;; THE FIELDS A CALLER MAY WRITE, asked separately from the shape,
  ;; because the write path checks this BEFORE it appends its own two and
  ;; the reducer must not: by the time a record is on disk its `parent`
  ;; and `ord` are this reducer's own and perfectly legitimate.
  ;; HOW A REFUSED DATUM IS NAMED IN AN ANSWER: as a string, always.
  ;;
  ;; An answer travels to a reader that accepts the wire whitelist, and
  ;; the thing a refusal is about is by definition something that layer
  ;; would not carry -- a symbol the writer will not emit, a value of a
  ;; type it does not know. Putting the datum itself into the answer
  ;; therefore produced a refusal its only reader could not parse:
  ;; `symbol-not-wire-safe` came back spelled `\x31;` and the client
  ;; reported a transport error at that byte instead of the reason.
  ;;
  ;; A STRING IS IN THE WHITELIST WHATEVER IT HOLDS, so this is safe by
  ;; construction rather than by a rule someone has to remember: the
  ;; refusal is readable no matter what was refused.
  ;; A SYMBOL IS SPELLED BY ITS NAME, not by the syntax that would be
  ;; needed to write it down again. `write` renders the symbol whose name
  ;; is "1" as `\x31;`, so a refusal about it said `(spelling "\\x31;")`
  ;; -- readable, and still not the thing the reader wanted to know.
  ;; Every other datum keeps `write`, which is what separates the string
  ;; "abc" from the symbol abc when both could be meant.
  (define (datum-spelling x)
    (if (symbol? x)
        (symbol->string x)
        (call-with-string-output-port (lambda (port) (write x port)))))

  (define (caller-fields-reason alist)
    (or (shape-only-reason alist)
        (let loop ((ps alist))
          (cond
            ((null? ps) #f)
            ((memq (car (car ps)) reserved-fields)
             (list 'field-name-reserved
                   (list 'field (datum-spelling (car (car ps))))))
            (else (loop (cdr ps)))))
        (let ((d (duplicate-key alist)))
          (and d (list 'field-name-repeated
                       (list 'field (datum-spelling d)))))))

  (define (args-reason args n)
    (let ((k (proper-len args)))
      (cond ((not k) 'arguments-not-a-list)
            ((< k n) 'too-few-arguments)
            (else #f))))

  ;; A POSITION IS A PARENT AND AN ORD, AND BOTH ARE READ AS VALUES.
  ;; The field collection's SHAPE was checked and its position values were
  ;; not, so a record carrying `(ord . "oops")` applied cleanly, left no
  ;; note, and then `state-outline` raised when it came to sort siblings.
  ;;
  ;; THE CALLER'S RESERVED-NAME RULE CANNOT PROTECT THIS. That rule stops
  ;; a caller writing `ord` at all; it says nothing about a record that
  ;; arrived from another machine or an older build, which is where a
  ;; position value with the wrong type actually comes from. The write
  ;; path and the read path need different rules about the same field.
  ;;
  ;; AN ORD IS AN EXACT REAL because `ord-between` answers integers and
  ;; the midpoints between them; a PARENT is `root` or a block id.
  (define (ord-ok? x) (and (number? x) (real? x) (exact? x)))
  (define (parent-ok? x) (or (eq? x 'root) (string? x)))

  ;; TWO SITUATIONS, NOT ONE. A `put` may omit either coordinate and the
  ;; interpreter fills it in; a `move` names both and `do-move!` stores
  ;; what it is given. Written as one procedure that skipped a #f, this
  ;; passed `(move <id> root #f)` and `(move <id> #f 0)` -- both applied,
  ;; left no note, and made the outline raise. The value #f is ABSENT in
  ;; one form and A WRONG VALUE in the other, and one predicate cannot
  ;; mean both.
  (define (move-position-reason parent ord)
    (cond
      ((not (parent-ok? parent)) 'parent-not-an-id)
      ((not (ord-ok? ord)) 'ord-not-a-number)
      (else #f)))

  ;; FIELDS WHOSE VALUE A READER INTERPRETS, and the type it needs. This
  ;; is the same finding as ids and relation names, one field further
  ;; out: `level` is used as a string length when a heading is rendered,
  ;; so `(set <id> level "2")` -- which is exactly what the command line
  ;; produces, since it passes every value as text -- applied cleanly and
  ;; then broke every Markdown read of that block.
  ;;
  ;; THE READER IS ALSO MADE TOLERANT, in `effective-heading`, because
  ;; this rule cannot reach a record that is already on disk or arrived
  ;; from elsewhere. Refusing to write one and surviving one that exists
  ;; are two different obligations.
  ;; A TITLE IS ONE LINE, AND THE STORE HAS TO SAY SO. `outline` prints
  ;; one row per block as text, so a title carrying a line terminator
  ;; produced an EXTRA ROW -- one that looked exactly like a real one and
  ;; carried a well-formed id that no block has. A client reading outline
  ;; as text saw a block that does not exist. Measured: two records, two
  ;; blocks, three rows.
  ;;
  ;; THE CONTROL CHARACTERS GO WITH IT for the same reason: they are not
  ;; text a heading can carry, and what a terminal does with them is not
  ;; something the store should be deciding by accident. TAB is allowed
  ;; because it is ordinary text in a title.
  ;;
  ;; U+0085, U+2028 AND U+2029 ARE LINE TERMINATORS TOO, and are the ones
  ;; a check written against `#\newline` alone would let through.
  (define (line-terminator? c)
    (let ((n (char->integer c)))
      (or (= n 10) (= n 13) (= n 133) (= n 8232) (= n 8233))))

  (define (control-character? c)
    (let ((n (char->integer c)))
      (and (not (= n 9))
           (or (< n 32) (and (>= n 127) (< n 160))))))

  ;; WHAT A TEXT FIELD'S VALUE MAY BE, ONE FIELD PER LINE.
  ;;
  ;; These are the fields `store-search` reads as text. Before this table
  ;; there was no rule at all: `set` at the command line always produced a
  ;; string, but `batch` carries its intents as DATA, so
  ;; `(set <id> title tinvor)` stored the SYMBOL and `(set <id> title 42)`
  ;; stored the NUMBER -- both accepted, both then invisible to every query,
  ;; because `field-strings` returns the empty list for anything that is not
  ;; a string. Measured, not inferred: all three landed in a store.
  ;;
  ;; THE RULE IS WRITTEN POSITIVELY -- what a value MAY be -- rather than as
  ;; a list of what is refused. A rule spelled "symbols are not allowed"
  ;; would have let `42` through, and the reading that found `42` is what
  ;; settled the spelling.
  ;;
  ;; `src` TAKES BYTES ON PURPOSE. A code block imported in text mode holds
  ;; its source as a bytevector, and that is the importer's own write path:
  ;; measured over this repository, 43 of 43 file blocks store `src` as a
  ;; bytevector while the markdown arm stores all 3531 as strings. Admitting
  ;; bytevectors here is not a concession, it is where the text mode's
  ;; payload is written down.
  ;;
  ;; THE TABLE IS DATA, NOT PREDICATES. Kinds are named by symbol so that
  ;; something reading this table can work out what to WRITE, not only what
  ;; to accept -- the cell that checks every accepted kind is reachable by a
  ;; query derives its cases from this list, and a field added here without
  ;; a reader turns that cell red.
  ;;
  ;; WHAT IS DELIBERATELY NOT HERE: `kind`, `lang` and `mode` are symbols and
  ;; `level` is an integer, by design and by measurement. They are not text,
  ;; nothing searches them as text, and this rule must not reach them.
  (define text-field-types
    (list (cons 'title    '(string))
          (cons 'keywords '(string))
          (cons 'src      '(string bytevector))))

  ;; THE NAME OF A VALUE'S KIND, so a refusal can say what it got. The last
  ;; branch catches everything: a refusal that could not name the kind would
  ;; be a refusal nobody can act on.
  (define (value-kind v)
    (cond ((string? v) 'string)
          ((bytevector? v) 'bytevector)
          ((symbol? v) 'symbol)
          ((and (integer? v) (exact? v)) 'integer)
          ((boolean? v) 'boolean)
          ((null? v) 'null)
          ((pair? v) 'pair)
          (else 'other)))

  ;; -> #f when this value is allowed in this field, else a named refusal.
  ;;
  ;; A FIELD THAT IS NOT IN THE TABLE IS NOT JUDGED HERE. This rule is about
  ;; the text fields and nothing else; every other field keeps whatever rule
  ;; it already had, and a field with no rule stays as it was.
  (define (text-field-reason name value)
    (let ((e (assq name text-field-types)))
      (and e
           (not (memq (value-kind value) (cdr e)))
           (list 'field-value-not-text
                 (list 'field name)
                 (list 'kind (value-kind value))
                 (list 'allowed (cdr e))))))

  ;; Every text field carried by a `put`'s field list, in the order written.
  (define (text-fields-reason alist)
    (let loop ((ps alist))
      (cond
        ((not (pair? ps)) #f)
        ((not (pair? (car ps))) (loop (cdr ps)))
        ((text-field-reason (car (car ps)) (cdr (car ps)))
         => (lambda (r) r))
        (else (loop (cdr ps))))))

  (define (title-text-reason v)
    (and (string? v)
         (let loop ((i 0))
           (cond
             ((= i (string-length v)) #f)
             ((line-terminator? (string-ref v i)) 'title-has-line-terminator)
             ((control-character? (string-ref v i)) 'title-has-control-character)
             (else (loop (+ i 1)))))))

  (define (typed-field-reason alist)
    (let ((e (assq 'level alist)) (doc (assq 'doc alist)))
      (or (and doc (datum-doc-marker? (cdr doc)) 'marker-in-doc)
          (and doc (not (datum-doc-format? (cdr doc))) 'invalid-doc)
          (and e
           (not (and (integer? (cdr e)) (exact? (cdr e))
                     (> (cdr e) 0) (< (cdr e) 7)))
           'level-not-a-heading-level))))

  ;; THE TITLE RULE IS THE WRITE PATH'S, NOT THE REDUCER'S. A title with
  ;; a line terminator is a thing this store will not WRITE; a record
  ;; that already carries one -- from another machine, or a build that
  ;; had no such rule -- is a record it must still be able to APPLY and
  ;; show. Putting this in `payload-reason` made the reducer refuse the
  ;; foreign record instead, so the block disappeared from the listing
  ;; rather than appearing on one line.
  ;;
  ;; SAME SHAPE AS THE RESERVED NAMES: one owner, two questions, asked by
  ;; name. `payload-reason` answers "can this be applied"; the caller's
  ;; question is "may this be written", and it is strictly narrower.
  ;; A SYMBOL THIS STORE WRITES MUST BE ONE THE WIRE WRITER WILL EMIT
  ;; BARE. A relation name or field name that is not -- a space, a
  ;; bracket, anything outside ASCII -- is encoded into the record
  ;; perfectly well as `("#%sym" ...)`, and then the state datum carrying
  ;; the raw symbol cannot be serialised. That raise happened while the
  ;; ANSWER was being built, after the record was durable, so the caller
  ;; was told a write had failed that had in fact succeeded.
  ;;
  ;; IT ASKS THE WIRE LAYER RATHER THAN RESTATING ITS RULE. Two copies of
  ;; "which symbols may be written bare" would drift, and the drift only
  ;; shows once a record is on disk: `("#%sym" "\u63d0;\u53ca;")` is
  ;; exactly what a writer that encoded it and a reader that could not
  ;; apply it leave behind.
  ;;
  ;; A NAME FOR PEOPLE BELONGS IN A STRING FIELD. This rule is about the
  ;; symbols that are part of the record's structure.
  ;; IT SAYS WHICH SYMBOL, HAVING JUST LOOKED AT IT. This answered the
  ;; bare word and threw away the one thing it had that the caller could
  ;; act on -- so the answer carried the whole intent back to make up for
  ;; it, and the intent contained the symbol the wire layer will not
  ;; write. A refusal about an unwritable datum was therefore unreadable
  ;; whenever it was right. Keeping what was found here is what makes the
  ;; echo unnecessary.
  (define (datum-symbols x)
    (cond ((symbol? x) (list x))
          ((pair? x) (append (datum-symbols (car x)) (datum-symbols (cdr x))))
          (else '())))
  (define (symbol-field-reason payload)
    (define (bad? x) (and (symbol? x) (not (wire-safe-symbol? x))))
    (define (say where x)
      (list 'symbol-not-wire-safe (list 'where where)
            (list 'spelling (datum-spelling x))))
    (and (pair? payload)
         (let ((args (cdr payload)))
           (case (car payload)
             ((link unlink)
              (and (pair? (cdr args)) (bad? (cadr args))
                   (say 'relation (cadr args))))
             ((relation)
              (cond ((and (pair? args) (bad? (car args))) (say 'relation (car args)))
                    ((and (pair? args) (pair? (cdr args)) (find bad? (datum-symbols (cadr args))))
                     => (lambda (x) (say 'selector x)))
                    (else #f)))
             ((rule)
              (cond ((and (pair? args) (bad? (car args))) (say 'rule (car args)))
                    ((and (pair? args) (pair? (cdr args)) (find bad? (datum-symbols (cadr args))))
                     => (lambda (x) (say 'rule-value x)))
                    (else #f)))
             ((set)
              (and (pair? (cdr args)) (bad? (cadr args))
                   (say 'field-name (cadr args))))
             ((put)
              (and (pair? args)
                   (let loop ((ps (car args)))
                     (cond
                       ((not (pair? ps)) #f)
                       ((not (pair? (car ps))) #f)
                       ((bad? (car (car ps))) (say 'field-name (car (car ps))))
                       ((and (not (memq (car (car ps)) '(body name))) (bad? (cdr (car ps))))
                        (say 'field-value (cdr (car ps))))
                       (else (loop (cdr ps)))))))
             (else #f)))))

  ;; ---- the kinds a block may have -------------------------------------------
  ;;
  ;; NEVER: ONE LIST, AND EVERY TEST OF LEGALITY ASKS IT. The hazard this
  ;; guards is not that somebody compares a kind -- dispatching on a known
  ;; value is ordinary, and the tree does it in a dozen places -- it is A
  ;; SECOND ALLOWLIST: another hardcoded set that can accept or reject the
  ;; same value and drift from this one. There is exactly one membership test
  ;; over kind literals in the shipped sources, and `facade-gate` counts it.
  ;;
  ;; NEVER: WHICH IS WHY THE ENTRIES ARE NOT SPELLED OUT ANYWHERE ELSE. The
  ;; write entries and the export report reach this through `kind-known?`, and
  ;; BEHAVIOUR CELLS ARE WHAT HOLD THEM TO IT; the census only says nobody
  ;; grew a second list. A census cannot see a route that asks nothing.
  ;;
  ;; The five that existed before this table, measured across the tree by
  ;; construction site: `code` 5, `section` 3, `library` 1, `file` 1, `doc` 1.
  ;; `other` appears only in a fixture and is deliberately NOT admitted -- a
  ;; placeholder a test picked is not a kind, and listing it here would make
  ;; it one.
  ;;
  ;; `decision` is the sixth, read by `commitments`. `task` and `template`
  ;; are the seventh and eighth: a task is read by `tasks` and by the rule
  ;; that discharges a decision, and the one template block of a store is
  ;; read by the template reader. THAT IS THE ONLY WAY AN ENTRY GETS IN --
  ;; measured in the tree, or
  ;; decided somewhere a reader can go and check. An earlier draft of this
  ;; line also carried `verification`, which was neither: nothing wrote it,
  ;; nothing read it, and admitting it would have been the fixture mistake
  ;; made one line higher up. `datum` is not here either, and looks like an
  ;; omission: it is a MODE (`(mode . datum)` beside `(kind . code)`), and
  ;; the two were only ever confusable from a distance.
  (define known-kinds '(code section file doc library decision task template))

  (define (kind-known? k) (and (symbol? k) (memq k known-kinds) #t))

  ;; NEVER: EVERY ROUTE THAT WRITES A KIND ARRIVES HERE, WHICH IS WHY THE CHECK
  ;; IS HERE AND NOT AT THE COMMAND LINE. `set` converts and refuses while
  ;; parsing, but `batch` hands intents over already built -- measured before
  ;; this existed, a batch wrote `(kind . "decision")` as a STRING and an
  ;; unknown symbol without complaint, and neither was ever going to match
  ;; anything, silently.
  ;;
  ;; NEVER: AND THIS IS THE CALLER'S PATH, NOT REPLAY. A record already on
  ;; disk carrying a kind this version does not know still loads; refusing it
  ;; here would make a later version's write unreadable by an earlier one,
  ;; which is the opposite of what unknown-field preservation promises.
  ;; NEVER: AND THE REFUSAL CARRIES THE LEGAL SET, ON EVERY ROUTE. The command
  ;; line already answered `(kind "nonsense") (known (…))`; a batch answering a
  ;; bare `kind-not-known` would leave its caller -- a plugin, an agent -- with
  ;; a refusal and no way to learn what would have been accepted. The envelope
  ;; may differ between an intent-level and a request-level answer; THE DETAIL
  ;; MAY NOT. `known-kinds` is the one supplier for both.
  ;; NEVER: THE REFUSAL QUOTES A SPELLING, NOT THE DATUM. This echoed the
  ;; value it had just refused, and a symbol is exactly what the wire layer
  ;; may not be able to write: measured, `set <id> kind |1|` through the
  ;; batch route produced `(kind-not-known (kind \x31;) ...)` and RENDERING
  ;; THAT ANSWER RAISED -- the caller got an exception where a refusal was
  ;; due. `symbol-field-reason` catches such a symbol in a `put`, field
  ;; values and all, which is why the `insert` route never reached here; for
  ;; `set` it checks the field NAME only, and the value position arrives.
  ;;
  ;; The tree already had this lesson written down, one screen up: a refusal
  ;; about an unwritable datum was unreadable whenever it was right. Same
  ;; answer here, and the two routes' answers become the same shape as well.
  ;; THE FIELDS WHOSE VALUE IS A WORD FROM A FIXED LIST, in one table: the
  ;; field and the list. The caller's path refuses a value outside it by one
  ;; rule, whose detail is `(<field>-not-known (<field> "<spelling>") (known
  ;; (...)))`; a record already in a log keeps whatever it holds (replay
  ;; tolerance). `class` says what kind of statement a block is: an
  ;; observation, an inference, a ruling, a verification, or external
  ;; material. `mode` says how a code block's text is held: text, a src the
  ;; projection splits, or datum, a body of forms; everything that reads a
  ;; mode compares it with those two symbols, so the command line's string
  ;; "datum" was stored and never matched.
  (define known-classes '(observation inference ruling verification external))
  (define known-modes '(text datum))
  (define vocabulary-fields
    (list (cons 'kind known-kinds) (cons 'class known-classes) (cons 'mode known-modes)))
  (define (vocabulary-known? field v)
    (let ((e (assq field vocabulary-fields))) (and e (symbol? v) (memq v (cdr e)) #t)))
  (define (vocabulary-not-known-reason field v)
    (list (string->symbol (string-append (symbol->string field) "-not-known"))
          (list field (datum-spelling v))
          (list 'known (cdr (assq field vocabulary-fields)))))

  (define (vocabulary-field-reason payload)
    (define (bad? field v) (not (vocabulary-known? field v)))
    (and (pair? payload)
         (case (car payload)
           ((put) (let loop ((fs vocabulary-fields))
                    (and (pair? fs)
                         (let ((e (assq (car (car fs)) (car (cdr payload)))))
                           (if (and e (bad? (car (car fs)) (cdr e)))
                               (vocabulary-not-known-reason (car (car fs)) (cdr e))
                               (loop (cdr fs)))))))
           ((set) (let ((args (cdr payload)))
                    (and (pair? (cdr args)) (assq (car (cdr args)) vocabulary-fields)
                         (pair? (cdr (cdr args)))
                         (bad? (car (cdr args)) (car (cdr (cdr args))))
                         (vocabulary-not-known-reason (car (cdr args)) (car (cdr (cdr args)))))))
           (else #f))))

  ;; THE TEXT-FIELD TYPE RULE RUNS BEFORE THE TITLE'S OWN RULE, and that
  ;; order is the repair. `title-text-reason` is wrapped in
  ;; `(and (string? v) ...)`, so for a value that is not a string it answers
  ;; #f -- "no reason to refuse" -- and the one existing value check on a
  ;; text field passed every non-string through by construction. The type
  ;; rule is what now answers for those, and the title's rule keeps doing
  ;; the job it was written for on the strings that reach it.
  (define (caller-payload-reason payload)
    (or (payload-reason payload)
        (symbol-field-reason payload)
        (vocabulary-field-reason payload)
        (and (pair? payload)
             (case (car payload)
               ((put) (let ((alist (car (cdr payload))))
                        (or (text-fields-reason alist)
                            (let ((e (assq 'title alist)))
                              (and e (title-text-reason (cdr e)))))))
               ((set) (let ((args (cdr payload)))
                        (and (pair? (cdr args)) (pair? (cdr (cdr args)))
                             (let ((name (car (cdr args)))
                                   (value (car (cdr (cdr args)))))
                               (or (text-field-reason name value)
                                   (and (eq? name 'title)
                                        (title-text-reason value)))))))
               (else #f)))))

  ;; PRESENCE IS ASKED SEPARATELY FROM VALUE, so a key holding #f is
  ;; refused while a key that is simply not there is allowed.
  (define (put-position-reason alist)
    (let ((p (assq 'parent alist)) (o (assq 'ord alist)))
      (cond
        ((and p (not (parent-ok? (cdr p)))) 'parent-not-an-id)
        ((and o (not (ord-ok? (cdr o)))) 'ord-not-a-number)
        (else #f))))

  ;; -> #f when this reducer can apply the record, else a reason symbol.
  (define (payload-reason payload)
    (cond
      ((not (pair? payload)) 'payload-not-a-form)
      ((not (symbol? (car payload))) 'verb-not-a-symbol)
      (else
       (let ((args (cdr payload)))
         (case (car payload)
           ((put) (or (args-reason args 1)
                      (field-alist-reason (car args))
                      (put-position-reason (car args))
                      (typed-field-reason (car args))))
           ;; THE TYPES DOWNSTREAM READERS NEED, not merely the arity.
           ;; A block id is sorted and compared as a string and a tag name
           ;; is sorted against other tag names, so a number in either
           ;; position applies cleanly and then breaks the reader --
           ;; `(set 7 title "x")` breaks the ordering of ids and
           ;; `(tag 7 ())` breaks tag listing as soon as a second tag
           ;; exists. Applying without raising is not the same as leaving
           ;; a state that can be read.
           ((set) (or (args-reason args 2)
                      (and (not (string? (car args))) 'id-not-a-string)
                      (and (not (symbol? (cadr args))) 'field-name-not-a-symbol)
                      (and (pair? (cddr args))
                           (typed-field-reason (list (cons (cadr args) (caddr args)))))))
           ((del) (or (args-reason args 1)
                      (and (not (string? (car args))) 'id-not-a-string)))
           ((move) (or (args-reason args 3)
                       (and (not (string? (car args))) 'id-not-a-string)
                       (move-position-reason (cadr args) (caddr args))))
           ((link unlink)
            (or (args-reason args 3)
                (and (not (string? (car args))) 'id-not-a-string)
                (and (not (string? (caddr args))) 'id-not-a-string)
                (and (not (symbol? (cadr args))) 'relation-not-a-symbol)))
           ((tag) (or (args-reason args 2)
                      (and (not (string? (car args))) 'tag-name-not-a-string)))
           ;; A DECLARATION: a relation name and its whole value
           ;; (declaration-reason).
           ((relation) (or (args-reason args 2)
                           (and (not (symbol? (car args))) 'relation-not-a-symbol)
                           (declaration-reason (car args) (cadr args))))
           ;; A RULE: a name and its whole value (rule-value-reason).
           ((rule) (or (args-reason args 2)
                       (and (not (symbol? (car args))) 'rule-name-not-a-symbol)
                       (rule-value-reason (cadr args))))
           ;; A PLAN IS CHECKED HERE TOO, and until now it was not: the
           ;; arm below answered #f for it, so a plan carrying a
           ;; malformed `consumes` was applied in silence and the index
           ;; built from it was whatever the malformed list happened to
           ;; say.
           ;;
           ;; TWO QUESTIONS, AND ONLY ONE OF THEM IS SHAPE. `plan?` says
           ;; whether the sixth element is well formed; it deliberately
           ;; does not look at the declared sub-operations, because a
           ;; shape predicate should not need them. Here they ARE in
           ;; hand, so the second question is asked: every sub-operation
           ;; that names a block must have a consumes item for that
           ;; block.
           ;;
           ;; NEVER: NOT THE CONVERSE. A mixed commit may consume a draft
           ;; whose bytes are what the block already says; it has no
           ;; sub-operation, and it is consumed all the same.
           ((plan) (plan-reason payload))
           ;; A VERB THIS BUILD DOES NOT KNOW IS NOT MALFORMED. It is
           ;; answered by the arm below, which says so in its own words;
           ;; calling it malformed would tell an operator to repair a
           ;; record that is merely newer than their binary.
           (else #f))))))

  ;; NOTE: ONLY THE SIXTH ELEMENT. The first version of this asked `plan?`
  ;; about the whole payload, and `plan?` is stricter than what the
  ;; reduction has ever required of a plan record: `q1` publishes
  ;; `(plan "r1" "fp" (("w" . 1)) ())` as a bookkeeping record that this
  ;; build applies without effect, and that shape was suddenly called
  ;; malformed. The field this batch adds is the field this checks;
  ;; records written before it existed are treated exactly as they were.
  (define (plan-reason payload)
    (cond
      ((not (list? payload)) #f)
      ((<= (length payload) 5) #f)
      ((> (length payload) 6) 'plan-extra-field)
      ((not (consumes? (list-ref payload 5))) 'consumes-malformed)
      ((not (list? (list-ref payload 4))) #f)
      (else
       (let ((blocks (map car (caddr (list-ref payload 5)))))
         (let loop ((es (list-ref payload 4)))
           (cond
             ((null? es) #f)
             ((not (and (pair? (car es)) (list? (cdar es)))) #f)
             (else
              (let* ((body (cdar es))
                     (target (and (pair? body) (pair? (cdr body)) (cadr body))))
                (if (and (string? target) (not (member target blocks)))
                    'consumes-does-not-cover-sub-operation
                    (loop (cdr es)))))))))))

  (define (note-malformed! r event-id reason)
    (reduction-noted-set!
      r (append (reduction-noted r)
                (list (list 'malformed-record
                            (list 'event (car event-id) (cdr event-id))
                            (list 'reason reason))))))

  (define (interpret! r event-id past payload)
    (cond
      ((payload-reason payload)
       => (lambda (reason) (note-malformed! r event-id reason)))
      (else
       (case (car payload)
         ((put) (do-put! r event-id past (cadr payload)))
         ((set) (do-set! r event-id past (cdr payload)))
         ((del) (do-del! r event-id (cadr payload)))
         ((move) (do-move! r event-id past (cdr payload)))
         ((link) (do-link! r event-id (cdr payload)))
         ((unlink) (do-unlink! r event-id past (cdr payload)))
         ((tag) (do-tag! r event-id past (cdr payload)))
         ((relation) (do-relation! r event-id past (cdr payload)))
         ((rule) (do-rule! r event-id past (cdr payload)))
         ;; bookkeeping: understood, and deliberately without effect
         ((plan batch resolve) (if #f #f))
         (else (note-verb! r event-id (car payload)))))))

  (define (do-put! r event-id past alist)
    (let* ((id (block-id (car event-id) (cdr event-id)))
           (b (ensure-block! r id)))
      (for-each
        (lambda (pair)
          ;; AND THE SAME LIST AGAIN, for the same reason: these are read
          ;; as the block's position below, not stored as fields.
          (unless (memq (car pair) store-supplied-fields)
            (blk-fields-set!
              b (put-field (blk-fields b) (car pair) (cdr pair) event-id past))))
        alist)
      (let ((parent (let ((e (assq 'parent alist))) (and e (cdr e))))
            (ord (let ((e (assq 'ord alist))) (and e (cdr e)))))
        (blk-position-set!
          b (put-candidate past (blk-position b)
                           (cons (or parent 'root) (or ord 0)) event-id)))))

  (define (put-field fields name value event-id past)
    (let* ((have (let ((e (assq name fields))) (if e (cdr e) '())))
           (next (put-candidate past have value event-id)))
      (cons (cons name next) (remp (lambda (e) (eq? (car e) name)) fields))))

  (define (do-set! r event-id past args)
    (let* ((id (car args))
           (name (cadr args))
           ;; TWO ARGUMENTS MEANS ABSENT, and absent is a value like any
           ;; other -- a candidate that can conflict, be superseded and
           ;; be replayed. Deleting a field by removing its candidates
           ;; would make the deletion invisible to a concurrent write.
           (value (if (null? (cddr args)) absent (caddr args)))
           (b (ensure-block! r id)))
      (blk-fields-set! b (put-field (blk-fields b) name value event-id past))))

  (define (do-del! r event-id id)
    (let ((b (ensure-block! r id)))
      ;; PERMANENT, AND IT DOES NOT CLEAR THE FIELDS. Later writes are
      ;; still recorded as evidence; the block is simply not visible and
      ;; cannot be revived.
      (unless (blk-tomb b) (blk-tomb-set! b event-id))))

  (define (do-move! r event-id past args)
    (let* ((id (car args))
           (parent (cadr args))
           (ord (caddr args))
           (b (ensure-block! r id)))
      (blk-position-set!
        b (put-candidate past (blk-position b) (cons parent ord) event-id))))

  ;; ---- edges (design 9.4) ---------------------------------------------------

  ;; AN EDGE IS A SET ELEMENT WITH AN IDENTITY. unlink removes the links
  ;; in its past and leaves concurrent ones -- so a link one writer never
  ;; saw is not silently undone by an unlink another writer issued.
  (define (do-link! r event-id args)
    (reduction-links-set!
      r (append (reduction-links r)
                (list (list (car args) (cadr args) (caddr args) event-id)))))

  (define (do-unlink! r event-id past args)
    (let ((from (car args)) (rel (cadr args)) (to (caddr args)))
      (reduction-links-set!
        r (remp (lambda (l)
                  (and (equal? (car l) from) (eq? (cadr l) rel) (equal? (caddr l) to)
                       (past-covers? past (car (cadddr l)) (cdr (cadddr l)))))
                (reduction-links r)))))

  (define (do-tag! r event-id past args)
    (let* ((name (car args))
           (cut (cadr args))
           (have (let ((e (assoc name (reduction-tags r)))) (if e (cdr e) '()))))
      (reduction-tags-set!
        r (cons (cons name (put-candidate past have cut event-id))
                (remp (lambda (e) (equal? (car e) name)) (reduction-tags r))))))

;; A DECLARATION IS A CANDIDATE OF ITS NAME, as a write is of a field: the
  ;; ones this record has seen are superseded, a concurrent one survives.
  ;; Relations and rules are two tables through this one procedure.
  (define (put-declaration! table table-set! r event-id past args)
    (let* ((name (car args))
           (have (let ((e (assq name (table r)))) (if e (cdr e) '()))))
      (table-set!
        r (cons (cons name (put-candidate past have (copy-datum (cadr args)) event-id))
                (remp (lambda (e) (eq? (car e) name)) (table r))))))
  (define (do-relation! r event-id past args)
    (put-declaration! reduction-declared-relations reduction-declared-relations-set! r event-id past args))
  (define (do-rule! r event-id past args)
    (put-declaration! reduction-declared-rules reduction-declared-rules-set! r event-id past args))

  ;; ---- ordering (design 9.5) ------------------------------------------------

  ;; THE SMALLEST DENOMINATOR BETWEEN TWO RATIONALS, by Stern-Brocot
  ;; descent. Not the midpoint: repeated midpoints double the denominator
  ;; every time, and after a hundred insertions in one place the number
  ;; no longer fits anything.
  (define ord-denominator-limit (expt 2 128))

  ;; THE SMALLEST DENOMINATOR STRICTLY BETWEEN TWO RATIONALS, found by
  ;; descending the Stern-Brocot tree from the root -- not by taking the
  ;; mediant of the endpoints. The first version did the latter, which
  ;; looks the same on (0, 1) and is wrong as soon as the endpoints are
  ;; not neighbours in the tree: between 1/5 and 1/2 it answered 2/7,
  ;; while 1/3 is simpler and sits in the same gap. Repeated, that is the
  ;; denominator growth the rule exists to avoid.
  ;;
  ;; NEGATIVES ARE HANDLED BY REFLECTION. The tree covers the positives;
  ;; a gap wholly below zero is the mirror of one above it, and a gap
  ;; that straddles zero has 0 itself as its simplest member.
  (define (ord-between lo hi)
    (cond
      ((and lo hi (= lo hi))
       ;; A BUCKET, NOT AN ERROR: two blocks share an ord because they
       ;; were inserted concurrently into one gap, and v1 declines to
       ;; order within it. The two places that ARE available are named.
       (list 'refused 'bucket (cons 'before lo) (cons 'after hi)))
      ((and lo hi (> lo hi)) (list 'refused 'not-ordered))
      ((and (not lo) (not hi)) 0)
      ((not lo) (- (ceiling hi) 1))
      ((not hi) (+ (floor lo) 1))
      ;; A gap straddling zero needs no special case: the descent's own
      ;; first step is floor(lo) + 1, which IS zero there. Writing it out
      ;; separately was a second supplier of the same answer.
      ((and (< lo 0) (<= hi 0))
       (let ((m (simplest-between (- hi) (- lo))))
         (if (pair? m) m (- m))))
      (else (simplest-between lo hi))))

  ;; Both endpoints non-negative, lo < hi, hi may be #f for infinity.
  ;;
  ;; BY CONTINUED FRACTION, NOT BY UNIT STEPS. Descending the tree one
  ;; mediant at a time is correct and takes as many steps as the answer's
  ;; denominator -- so the gap just inside 1/(2^128) needs 2^128 of them
  ;; and the case simply hangs. Each recursion here skips a whole run of
  ;; identical turns: if no integer lies strictly inside, every candidate
  ;; is floor(lo) + 1/y, and the problem becomes the same one for y.
  (define (simplest-between lo hi)
    (let ((answer (simplest-open lo hi)))
      (if (> (denominator answer) ord-denominator-limit)
          ;; THE LIMIT IS ON THE ANSWER. A descent passes through
          ;; fractions more complex than the one it lands on, and
          ;; refusing on those would refuse gaps that have a simple
          ;; member -- (-1/L, 1/L) contains 0.
          (list 'refused 'too-deep)
          answer)))

  (define (simplest-open lo hi)
    (let ((fl (floor lo)))
      (cond
        ((and (not hi) (> (+ fl 1) lo)) (+ fl 1))
        ((and hi (< lo (+ fl 1)) (< (+ fl 1) hi)) (+ fl 1))
        (else
         ;; every candidate is fl + 1/y, and y runs over the mirror gap
         (let* ((lo-part (- lo fl))
                (hi-part (and hi (- hi fl)))
                (y-lo (if (and hi-part (> hi-part 0)) (/ 1 hi-part) 0))
                (y-hi (if (> lo-part 0) (/ 1 lo-part) #f)))
           (+ fl (/ 1 (simplest-open y-lo y-hi))))))))

  ;; ---- reading the state ----------------------------------------------------

  ;; A READ HANDS BACK A COPY. Returning the stored value let a caller
  ;; mutate a vector it had been given and change the state -- and the
  ;; token with it -- without any event having happened.
  (define (copy-datum x)
    (cond
      ((pair? x) (cons (copy-datum (car x)) (copy-datum (cdr x))))
      ((vector? x) (let* ((n (vector-length x)) (o (make-vector n)))
                     (let loop ((i 0))
                       (if (= i n) o
                           (begin (vector-set! o i (copy-datum (vector-ref x i)))
                                  (loop (+ i 1)))))))
      ((string? x) (string-copy x))
      (else x)))

  (define (candidate-values cs) (map car cs))

  (define (settled cs) (and (= 1 (length cs)) (car (car cs))))

  ;; EVERY READ RETURNS AN IMMUTABLE DATUM, comparable with equal?. The
  ;; internal representation is this layer's business; what a case
  ;; asserts against must not be.
  ;; Text metadata is a view of the same settled src/lang candidates. There
  ;; is no independently timed metadata write that can lag behind set src,
  ;; and replay order cannot pair one source candidate with another name.
  ;; THE RAW FIELDS, AND ONLY THOSE. Until v129 this reader also derived
  ;; `name`/`doc` for a text block from the language table and
  ;; `names`/`name` for a datum block from its body, which made the
  ;; reduction -- the thing every hash and every `--based-on` is taken
  ;; over -- depend on a table that is edited at runtime. The derivation
  ;; moved to `(theourgia view)`; what a caller gets here is what was
  ;; written.
  (define (state-read r id)
    (let ((b (find-block r id)))
      (and b
           (list (cons 'id id)
                 (cons 'deleted (and (blk-tomb b) #t))
                 (cons 'fields
                       (list-sort
                         (lambda (x y) (string<? (symbol->string (car x))
                                                 (symbol->string (car y))))
                         ;; A FIELD WHOSE ONLY CANDIDATE IS ABSENT IS NOT
                       ;; THERE. It still exists in the canonical state --
                       ;; a concurrent write has to be able to conflict
                       ;; with the deletion -- but a reader asking what
                       ;; the block says should not be told about a field
                       ;; that was removed.
                       ;; NEVER: BY THE CANDIDATE, NOT BY A VALUE. The field was
                       ;; mapped to the symbol `omit` and then dropped for being
                       ;; `omit`, so a field SET to the symbol omit read as
                       ;; absent. Absence is the reducer's own marker (absent?),
                       ;; asked of the one candidate before anything is mapped.
                       (map (lambda (f)
                              (let ((cs (cdr f)))
                                (cons (car f)
                                      (cond
                                          ((= 1 (length cs)) (copy-datum (car (car cs))))
                                          (else
                                           (list 'conflict
                                                 (list-sort candidate<?
                                                            (map (lambda (c)
                                                                   (list (copy-datum (car c))
                                                                         (car (cdr c))
                                                                         (cdr (cdr c))))
                                                                 cs))))))))
                            (filter (lambda (f) (not (and (= 1 (length (cdr f))) (absent? (car (car (cdr f)))))))
                                    (blk-fields b)))))
                 (cons 'position
                       (let ((cs (blk-position b)))
                         (if (= 1 (length cs))
                             (car (car cs))
                             (list 'conflict (length cs)))))
                 ;; THE EDGES BELONG TO A READ. What a block says about
                 ;; itself includes what it points at: a reader that has
                 ;; to call a second interface to find out whether a link
                 ;; exists cannot answer "what does this block say" in
                 ;; one question, and the edge set is as much a part of
                 ;; the block as any field.
                 (cons 'edges (block-edges r id))))))

  ;; WHAT POINTS AT THIS BLOCK, the mirror of block-edges and held to the
  ;; same discipline: the logical edge set, one entry per (from, rel)
  ;; however many link events produced it, and no event ids. Two
  ;; libraries that agree on the edges must agree on this too.
  ;;
  ;; block-edges filters on `from` and so answers what a block points AT;
  ;; this filters on `to`. A reader that confused them would list a
  ;; block's own out-edges as references to it, which is why they are two
  ;; procedures and not one with a flag.
  ;; THE NAMES THIS STATE BINDS, and what each one binds to. A name is
  ;; SETTLED when exactly one candidate survives -- the same rule a field
  ;; uses, deliberately, because a tag written later by someone who had
  ;; seen the earlier one supersedes it while two written concurrently do
  ;; not. Reporting only a winner would hide a disagreement behind a
  ;; choice this layer has no grounds to make, so every candidate comes
  ;; back and the caller is told which case it is in.
  (define (state-tags r)
    (list-sort
      (lambda (x y) (string<? (car x) (car y)))
      (map (lambda (e)
             (list (car e)
                   (map (lambda (c)
                          (list (copy-datum (car c)) (car (cdr c)) (cdr (cdr c))))
                        (cdr e))))
           (reduction-tags r))))

  ;; EVERY SURVIVING EDGE, ONCE, WITH ITS SURVIVING LINK EVENTS: (<from> <rel>
  ;; <to> <event> ...), however many link events made it, in the order the
  ;; edges first appear. One pass, so a reader asking about every edge does
  ;; not filter every link once per edge.
  (define (state-edges r)
    (let ((by (make-hashtable equal-hash equal?)))
      (let loop ((ls (reduction-links r)) (order '()))
        (if (null? ls)
            (map (lambda (e) (append e (reverse (hashtable-ref by e '())))) (reverse order))
            (let* ((l (car ls)) (e (list (car l) (cadr l) (caddr l))) (seen (hashtable-ref by e #f)))
              (hashtable-set! by e (cons (cadddr l) (or seen '())))
              (loop (cdr ls) (if seen order (cons e order))))))))

  ;; Whether any surviving edge carries a relation of an effect kind, a
  ;; built-in or a declared one: `nothing` is a kind with no effect.
  (define (state-effect-relation? r)
    (exists (lambda (l) (memq (relation-kind r (cadr l)) effect-relation-names)) (reduction-links r)))

  (define (state-refs r id)
    (let ((pairs (map (lambda (l) (cons (car l) (cadr l)))
                      (filter (lambda (l) (equal? (caddr l) id)) (reduction-links r)))))
      (list-sort (lambda (x y)
                   (if (string=? (car x) (car y))
                       (string<? (symbol->string (cdr x)) (symbol->string (cdr y)))
                       (string<? (car x) (car y))))
                 (let dedupe ((ps pairs) (out (quote ())))
                   (cond ((null? ps) out)
                         ((member (car ps) out) (dedupe (cdr ps) out))
                         (else (dedupe (cdr ps) (cons (car ps) out))))))))

  (define (candidate<? a b)
    (if (string=? (cadr a) (cadr b))
        (< (caddr a) (caddr b))
        (string<? (cadr a) (cadr b))))

  ;; THE ORDER IS (ord, block-id) AND BOTH HALVES MATTER. Two blocks
  ;; inserted concurrently into one gap get the same ord -- that is a
  ;; bucket, not an error -- and the id breaks the tie so that every
  ;; library lists them the same way.
  ;; A BLOCK WITH NO ONE PLACE IS STILL SHOWN. Leaving it out of the
  ;; outline loses it -- the facts are all there in the candidates, and
  ;; a reader who cannot see the block cannot act on them. Design 9.2
  ;; says these appear under root and marked, so that is what comes back:
  ;; a fourth element naming why, absent on an ordinary row.
  ;; KEY: THE LIST IS SORTED BY (parent, ord, block-id), AND CALLERS RELY ON
  ;; IT. The parent compares as a string (`root` by its name, a block by its
  ;; id), so the rows sharing a parent are adjacent and in sibling order.
  ;; Callers that take their order from this list, each filtering it and all
  ;; but the last without sorting again: rpc.sc's code-title fallback, which
  ;; numbers a block among its siblings, and its `outline-text`, which draws
  ;; children in row order; `store-grep`, which answers and truncates in
  ;; outline order; `outline-subtree` below, which collects children in row
  ;; order and from which `md-tree` and `subtree-with-root` read document
  ;; order; project.sc's `doc-sections-of`, which walks the rows with its own
  ;; `children-of`; code-project.sc's `code-children`, whose order
  ;; `export-code` writes the source in; and store.sc's `siblings`, whose
  ;; sort by ord alone is stable and so keeps this list's id order among
  ;; equal ords. A caller that filters to one parent gets that parent's rows
  ;; already in order.
  ;;
  ;; NOTE: SORTED IS NOT THE SAME AS EQUAL ACROSS STORES. Block ids carry the
  ;; writer's name, so two stores built from one file hold different ids,
  ;; and a block's id can sort before or after `root`. The rows are a
  ;; function of one store's state, not of the file it was built from.
  ;;
  ;; HISTORY: before this sort the rows came in reduction order, which
  ;; follows the writer name. Over six fresh stores built from one file the
  ;; `doc` row came first in three and the `section` row in the other
  ;; three, and a fixture that took `(car ...)` of the list went red one run
  ;; in five.
  (define (state-outline r)
    (let* ((structure (state-structure r))
           (cyclic (cdr (assq 'conflicts structure)))
           (unplaced (cdr (assq 'unplaced structure)))
           (alive (filter (lambda (e) (not (blk-tomb (cdr e)))) (reduction-blocks r)))
           (placed (filter (lambda (e)
                             (and (= 1 (length (blk-position (cdr e))))
                                  (not (member (car e) cyclic))))
                           alive))
           (marked (map (lambda (e)
                          (list 'root 0 (car e)
                                (if (member (car e) cyclic) 'conflict 'unplaced)))
                        (filter (lambda (e)
                                  (or (member (car e) cyclic)
                                      (member (car e) unplaced)))
                                alive)))
           (rows (append
                   (map (lambda (e)
                          (let* ((b (cdr e))
                                 (p (car (car (blk-position b)))))
                            (list (car p) (cdr p) (car e))))
                        placed)
                   marked)))
      (list-sort (lambda (x y)
                   (if (equal? (car x) (car y))
                       (if (= (cadr x) (cadr y))
                           (string<? (caddr x) (caddr y))
                           (< (cadr x) (cadr y)))
                       (string<? (format-parent (car x)) (format-parent (car y)))))
                 rows)))

  (define (format-parent p) (if (symbol? p) (symbol->string p) p))

  ;; EVERY ID AT OR BELOW ROOT, IN THE OUTLINE'S SIBLING ORDER: the root
  ;; first, then each child followed by everything under it -- the order a
  ;; document is written in. ROWS are `state-outline`'s, which arrive sorted
  ;; by parent, ord and id, so collecting children in row order is
  ;; collecting them in sibling order.
  ;;
  ;; THIS IS THE ONE DEFINITION OF "UNDER" (F85 R1). It does not ask whether
  ;; the root exists, and it does not stop at any kind of block. `read
  ;; --recursive`, `grep --under` and the markdown export's files are made
  ;; from this walk with no rule of their own applied to what comes back, so
  ;; they cannot disagree about which blocks are under a block. (The export's
  ;; written/lost accounting walks the same rows through a child map it
  ;; builds once, for speed, and stops at nothing either.)
  ;;
  ;; IT NEVER ANSWERS #f. A root that appears in no row comes back alone.
  ;; `#f` already means "no such block" to one caller of this and "no filter"
  ;; to the other; giving it a third meaning is how a missing root would
  ;; switch a filter off.
  ;;
  ;; A block the outline finds in a cycle is listed under `root`, not under
  ;; its parent, so this walk does not meet one. The visited set is there so
  ;; that it terminates on rows that did not come from the outline.
  (define (outline-subtree rows root)
    (let ((children (make-hashtable string-hash string=?))
          (seen (make-hashtable string-hash string=?)))
      (for-each (lambda (r)
                  (let ((p (car r)))
                    (when (string? p)
                      (hashtable-update! children p
                                         (lambda (cs) (cons (caddr r) cs))
                                         (quote ())))))
                rows)
      (reverse
        (let walk ((id root) (acc (quote ())))
          (if (hashtable-ref seen id #f)
              acc
              (begin
                (hashtable-set! seen id #t)
                (fold-left (lambda (a c) (walk c a))
                           (cons id acc)
                           (reverse (hashtable-ref children id (quote ()))))))))))

  ;; SORTED THROUGHOUT. A dump that kept the order records happened to
  ;; arrive in is not a function of the state, and two libraries holding
  ;; the same records would print different things.
  (define (state-dump r)
    (list (cons 'blocks
                (map (lambda (e)
                       (let ((b (cdr e)))
                         (list (car e)
                               (cons 'tomb (and (blk-tomb b) #t))
                               (cons 'fields
                                     (map (lambda (f)
                                            (cons (car f)
                                                  (list-sort candidate<?
                                                             (map (lambda (c)
                                                                    (list (car c)
                                                                          (car (cdr c))
                                                                          (cdr (cdr c))))
                                                                  (cdr f)))))
                                          (list-sort (lambda (x y)
                                                       (string<? (symbol->string (car x))
                                                                 (symbol->string (car y))))
                                                     (blk-fields b))))
                               (cons 'position
                                     (list-sort candidate<?
                                                (map (lambda (c)
                                                       (list (car c)
                                                             (car (cdr c))
                                                             (cdr (cdr c))))
                                                     (blk-position b)))))))
                     (list-sort (lambda (x y) (string<? (car x) (car y)))
                                (reduction-blocks r))))
          (cons 'links (list-sort link<? (reduction-links r)))
          (cons 'tags (list-sort (lambda (x y) (string<? (car x) (car y)))
                                 (map (lambda (t)
                                        (cons (car t) (candidates->datum (cdr t))))
                                      (reduction-tags r))))
          (cons 'pending (length (reduction-pending r)))))

  (define (link<? a b)
    (let ((sa (sexpr->string-extended (list (car a) (cadr a) (caddr a))))
          (sb (sexpr->string-extended (list (car b) (cadr b) (caddr b)))))
      (string<? sa sb)))

  ;; ---- state-hash (design 9.2, the token of 5.3) ----------------------------

  ;; COVERS THE APPLIED STATE AND ONLY IT: the candidate sets with their
  ;; event ids, the tombstones and the explicit edge set. Not pending --
  ;; the token answers "has what I read changed", and a record that has
  ;; not been applied has not changed what anyone read. Not the
  ;; observed-remove bookkeeping either: that is one implementation's way
  ;; of getting the edge set right, and two implementations that agree on
  ;; the edges must agree on the token.
  ;; THE SHAPE IS THE DESIGN'S, NOT THIS FILE'S. Section 9.2 pins the
  ;; datum and the sort order so that a second implementation -- or a
  ;; script outside the tree -- computes the same bytes. A hash whose
  ;; input shape lived only here would agree with nothing.
  (define (event<? a b)
    (if (string=? (car a) (car b)) (< (cdr a) (cdr b)) (string<? (car a) (car b))))

  (define (candidates->datum cs)
    (list-sort (lambda (x y) (event<? (cdr x) (cdr y)))
               (map (lambda (c) (cons (car c) (cdr c))) cs)))

  (define (block-edges r id)
    (let ((pairs (map (lambda (l) (cons (cadr l) (caddr l)))
                      (filter (lambda (l) (equal? (car l) id)) (reduction-links r)))))
      ;; THE LOGICAL EDGE SET: one entry per (rel, to) however many link
      ;; events produced it, and no event ids. Two libraries that agree
      ;; on the edges must agree on the token even if they took different
      ;; routes to them.
      (list-sort (lambda (x y)
                   (if (eq? (car x) (car y))
                       (string<? (cdr x) (cdr y))
                       (string<? (symbol->string (car x)) (symbol->string (car y)))))
                 (let dedupe ((ps pairs) (out '()))
                   (cond ((null? ps) out)
                         ((member (car ps) out) (dedupe (cdr ps) out))
                         (else (dedupe (cdr ps) (cons (car ps) out))))))))

  (define (block->datum r id b)
    (list 'block id
          (list 'fields
                (list-sort (lambda (x y) (string<? (symbol->string (car x))
                                                   (symbol->string (car y))))
                           (map (lambda (f) (list (car f) (candidates->datum (cdr f))))
                                (blk-fields b))))
          (list 'position (candidates->datum (blk-position b)))
          (list 'deleted (and (blk-tomb b) #t))
          (list 'edges (block-edges r id))))

  (define (state-datum r)
    (list-sort (lambda (x y) (string<? (cadr x) (cadr y)))
               (map (lambda (e) (block->datum r (car e) (cdr e))) (reduction-blocks r))))

  ;; NEVER: WHO HOLDS A PATH IS ASKED HERE, AND ONLY HERE. An import that
  ;; resolves a file to the library it updates, an export that refuses two
  ;; blocks of one path, a write that rechecks what an import captured, and
  ;; `check` and `conflicts` all ask the same question; one answer keeps
  ;; them from disagreeing about what "holds a path" means.
  ;; A holder is an ALIVE block whose stored kind, mode and path are the
  ;; ones asked for: a datum library and a text file of one path are two
  ;; different questions, not a duplicate.
  ;; -> the holders' ids, sorted.
  (define (state-path-claimants r kind mode rel)
    (list-sort string<?
      (filter (lambda (id)
                (let ((b (state-read r id)))
                  (and b (not (cdr (assq 'deleted b)))
                       (let ((fs (cdr (assq 'fields b))))
                         (define (field k) (let ((p (assq k fs))) (and p (cdr p))))
                         (and (equal? (field 'kind) kind) (equal? (field 'mode) mode)
                              (equal? (field 'path) rel))))))
              (map car (reduction-blocks r)))))

  ;; THE RELATION NAMES SUPPLIED FACTS ANSWER UNDER, in one place: the
  ;; request path refuses a link or unlink carrying one (store.sc), and
  ;; `check` names every applied record that already does. The reducer
  ;; itself applies such a record as it always has; history is not
  ;; rewritten by a later rule.
  (define reserved-relation-names '(ref uses calls guards))

  ;; THE RELATIONS WITH AN EFFECT on a block's validity or a decision's
  ;; state, BY NAME ONLY; what each does is the lifecycle library's table,
  ;; checked against this list when that library loads. The list is here so
  ;; a reader can ask whether a state has any such link without loading the
  ;; library: a store that links none of them takes the fast path.
  (define effect-relation-names '(supersedes refutes depends-on implements verifies conflicts-with))

  ;; ---- declared relations ---------------------------------------------------
  ;;
  ;; A DECLARED RELATION binds a name, in one store, to one EFFECT KIND: one
  ;; of the six above, or `nothing`, a listed edge with no effect. The
  ;; record is `(relation <name> <value>)`, the value the WHOLE declaration,
  ;; `(<kind> <from> <to>)`, or `retired`. Each end is a selector: () for
  ;; any block, else `((kind <k>))` or `((kind <k>) (field <name> <value>))`.
  ;; The ends are stored and printed; nothing here enforces them.
  ;;
  ;; THE DECLARATIONS OF A NAME ARE CANDIDATES, as a field's writes are: one
  ;; a later record has seen is superseded (do-relation!). They are read BY
  ;; VALUE, which a field is not: the name is in force when every surviving
  ;; candidate holds one value, and CONTESTED when two values survive -- two
  ;; writers' declarations, or a retirement beside a declaration, neither
  ;; having seen the other. A contested name has no effect until a writer
  ;; who has seen every candidate declares it again. A function of the
  ;; applied set, whatever the arrival order.
  ;;
  ;; THE EDGE KEEPS ITS NAME; THE RULES READ ITS KIND. relation-kind is the
  ;; one accessor every reader of an effect asks.
  (define declaration-kinds (append effect-relation-names '(nothing)))

  (define (selector-reason s)
    (define (scalar? v)
      (or (string? v) (symbol? v) (and (integer? v) (exact? v))))
    (cond ((null? s) #f)
          ((not (and (list? s) (<= 1 (length s) 2))) 'selector-malformed)
          ((not (let ((k (car s))) (and (list? k) (= 2 (length k)) (eq? (car k) 'kind) (kind-known? (cadr k)))))
           'selector-malformed)
          ((null? (cdr s)) #f)
          ((not (let ((f (cadr s)))
                  (and (list? f) (= 3 (length f)) (eq? (car f) 'field) (symbol? (cadr f)) (scalar? (caddr f)))))
           'selector-malformed)
          (else #f)))

  ;; NEVER: A BUILT-IN NAME IS NOT DECLARED. Its kind is itself, always; a
  ;; record that declares one is malformed, and the write path refuses it
  ;; as relation-is-built-in.
  (define (declaration-reason name value)
    (cond ((memq name effect-relation-names) 'relation-is-built-in)
          ((eq? value 'retired) #f)
          ((not (and (list? value) (= 3 (length value)))) 'declaration-malformed)
          ((not (memq (car value) declaration-kinds)) 'declaration-malformed)
          (else (or (selector-reason (cadr value)) (selector-reason (caddr value))))))

  (define (relation-candidates r name)
    (let ((e (assq name (reduction-declared-relations r)))) (if e (cdr e) '())))

  ;; THE ONE AGREEMENT RULE (r5-4), for declarations and nothing else -- a
  ;; field keeps its own rule: -> the value every surviving candidate holds,
  ;; or #f when there is none or more than one distinct value survives.
  (define (candidates-agreed cs)
    (and (pair? cs)
         (let ((v (car (car cs))))
           (and (for-all (lambda (c) (equal? (car c) v)) (cdr cs)) v))))

  ;; -> the kind of a relation name: a built-in's own, a declared name's in
  ;; force -- `nothing` included, one of the seven kinds -- or #f for a plain
  ;; edge: a name never declared, retired, or contested.
  (define (relation-kind r name)
    (cond ((memq name effect-relation-names) name)
          (else
           (let ((v (candidates-agreed (relation-candidates r name))))
             (and (pair? v) (car v))))))

  ;; -> #f for a name never declared; else (in-force <value>), (retired), or
  ;; (contested (<value> <writer> <seq>) ...), every surviving candidate.
  (define (state-declaration r name)
    (candidates-state (relation-candidates r name)))

  (define (declared-names r)
    (list-sort (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
               (map car (reduction-declared-relations r))))

  ;; -> the declared table, by name: (<name> <kind> (from <selector>) (to
  ;; <selector>)) for a name in force, (<name> (contested)) for one contested.
  ;; A retired name is a plain edge again and is not listed.
  (define (state-declared-relations r)
    (filter values
            (map (lambda (name)
                   (let ((d (state-declaration r name)))
                     (case (car d)
                       ((in-force) (let ((v (cadr d)))
                                     (list name (car v) (list 'from (cadr v)) (list 'to (caddr v)))))
                       ((contested) (list name '(contested)))
                       (else #f))))
                 (declared-names r))))

  ;; -> ((relation-contested <name> (candidates (<value> <writer> <seq>) ...))
  ;; ...), by name, for `conflicts`.
  (define (state-relation-contested r)
    (filter values
            (map (lambda (name)
                   (let ((d (state-declaration r name)))
                     (and (eq? (car d) 'contested)
                          (list 'relation-contested name (cons 'candidates (cdr d))))))
                 (declared-names r))))

;; ---- rules: the same candidate rule as declarations ----------------------
  ;;
  ;; A RULE'S VALUE, as a record carries it: `retired`, or a list of clauses
  ;; -- (class state|write), (on <kind> ...), [(where <goal>)], and one of
  ;; (must <goal>) or (must-not <goal>) -- or ((builtin <name>)). This is the
  ;; SHAPE a replay accepts; what a goal says and whether a kind is known is
  ;; the `rule` verb's to judge before it writes, as a block's kind is.
  (define rule-clause-heads '(class on where must must-not builtin))
  (define (rule-value-reason v)
    (define (clause k) (assq k v))
    (cond
      ((eq? v 'retired) #f)
      ((not (and (list? v) (pair? v)
                 (for-all (lambda (c) (and (list? c) (pair? c) (memq (car c) rule-clause-heads))) v)))
       'rule-malformed)
      ((let dup ((cs v) (seen '()))
         (cond ((null? cs) #f) ((memq (caar cs) seen) #t) (else (dup (cdr cs) (cons (caar cs) seen)))))
       'rule-malformed)
      ((clause 'builtin)
       (if (and (= 1 (length v)) (= 2 (length (clause 'builtin))) (symbol? (cadr (clause 'builtin))))
           #f
           'rule-malformed))
      ((not (= 1 (length (filter (lambda (c) (memq (car c) '(must must-not))) v)))) 'rule-malformed)
      ((not (let ((o (clause 'on))) (and o (pair? (cdr o)) (for-all symbol? (cdr o))))) 'rule-malformed)
      ((not (let ((c (clause 'class))) (and c (= 2 (length c)) (memq (cadr c) '(state write))))) 'rule-malformed)
      ((exists (lambda (k) (let ((c (clause k))) (and c (not (= 2 (length c)))))) '(where must must-not))
       'rule-malformed)
      (else #f)))

  (define (rule-candidates r name)
    (let ((e (assq name (reduction-declared-rules r)))) (if e (cdr e) '())))

  (define (rule-names r)
    (list-sort (lambda (x y) (string<? (symbol->string x) (symbol->string y)))
               (map car (reduction-declared-rules r))))

  ;; -> what a name's surviving candidates say, by the one agreement rule:
  ;; #f for none, (in-force <value>), (retired), or (contested (<value>
  ;; <writer> <seq>) ...). Declarations and rules read their candidates
  ;; through this.
  (define (candidates-state cs)
    (and (pair? cs)
         (let ((v (candidates-agreed cs)))
           (cond ((not v)
                  (cons 'contested
                        (list-sort candidate<?
                                   (map (lambda (c) (list (copy-datum (car c)) (car (cdr c)) (cdr (cdr c)))) cs))))
                 ((eq? v 'retired) '(retired))
                 (else (list 'in-force (copy-datum v)))))))

  (define (state-rule r name) (candidates-state (rule-candidates r name)))

  ;; -> the rules in force, by name: ((<name> <value>) ...). A retired or a
  ;; contested rule is not among them: a contested rule is not evaluated.
  (define (state-declared-rules r)
    (filter values
            (map (lambda (name)
                   (let ((d (state-rule r name)))
                     (and (eq? (car d) 'in-force) (list name (cadr d)))))
                 (rule-names r))))

  ;; -> ((rule-contested <name> (candidates (<value> <writer> <seq>) ...)) ...),
  ;; by name, for `conflicts`.
  (define (state-rule-contested r)
    (filter values
            (map (lambda (name)
                   (let ((d (state-rule r name)))
                     (and (eq? (car d) 'contested)
                          (list 'rule-contested name (cons 'candidates (cdr d))))))
                 (rule-names r))))

  ;; -> ((<from> <rel> <to> (event <writer> <seq>)) ...), in the order the
  ;; records were DELIVERED (as state-put-events: not application order, and
  ;; the route's), for every APPLIED link or unlink record whose relation is
  ;; reserved.
  (define (state-reserved-relation-records r)
    (let loop ((recs (reverse (reduction-history r))) (out '()))
      (if (null? recs)
          (reverse out)
          (let ((p (rec-payload (car recs))))
            (loop (cdr recs)
                  ;; FOUR OR MORE: a record with a surplus argument still
                  ;; applies (the reducer reads the first three).
                  (if (and (list? p) (>= (length p) 4) (memq (car p) '(link unlink))
                           (memq (caddr p) reserved-relation-names)
                           (event-applied? r (cons (rec-writer (car recs)) (rec-seq (car recs)))))
                      (cons (list (cadr p) (caddr p) (cadddr p)
                                  (list 'event (rec-writer (car recs)) (rec-seq (car recs))))
                            out)
                      out))))))

  ;; -> `((duplicate-path <rel> (ids (<id> ...))) ...)`, sorted by path, for
  ;; every path two or more alive datum libraries, or two or more alive text
  ;; files, hold.
  (define (state-duplicated-paths r)
    (let loop ((ids (map car (reduction-blocks r))) (seen '()))
      (if (pair? ids)
          (let* ((b (state-read r (car ids)))
                 (fs (and b (not (cdr (assq 'deleted b))) (cdr (assq 'fields b))))
                 (field (lambda (k) (let ((p (and fs (assq k fs)))) (and p (cdr p)))))
                 (key (and fs (member (list (field 'kind) (field 'mode)) '((library datum) (file text)))
                           (string? (field 'path))
                           (list (field 'kind) (field 'mode) (field 'path)))))
            (loop (cdr ids) (if (and key (not (member key seen))) (cons key seen) seen)))
          (list-sort (lambda (a b) (string<? (cadr a) (cadr b)))
            (filter values
              (map (lambda (key)
                     (let ((holders (state-path-claimants r (car key) (cadr key) (caddr key))))
                       (and (> (length holders) 1) (list 'duplicate-path (caddr key) (list 'ids holders)))))
                   seen))))))

  ;; THE TOKEN IS PER BLOCK. Section 9.2 pins the datum for ONE block,
  ;; and case R10 requires that a change to a different block leave the
  ;; token valid -- which a digest over the whole state cannot do, since
  ;; every block is in it. A whole-state comparison is still available by
  ;; comparing state-datum, which is what the three-way equality case
  ;; wants; a token is a different question and gets a different answer.
  (define (block-hash r id)
    (let ((e (assoc id (reduction-blocks r))))
      (and e
           (bytevector->hex
             (sha256 (string->utf8
                       (sexpr->string-extended (storable-encode (block->datum r id (cdr e))))))))))

  ;; THE DECLARATIONS ARE PART OF THE STATE, after the blocks, a name's
  ;; candidates by event; a store with none hashes the blocks alone, as it
  ;; always has.
  ;; The rules are a term of their own, after the declarations, only when
  ;; the store has one: a store without rules hashes as it did.
  (define (state-hash r)
    (let ((ds (append (map (lambda (name) (list 'relation name (candidates->datum (relation-candidates r name))))
                           (declared-names r))
                      (map (lambda (name) (list 'rule name (candidates->datum (rule-candidates r name))))
                           (rule-names r)))))
      (bytevector->hex
        (sha256 (string->utf8 (sexpr->string-extended
                                (storable-encode (if (null? ds) (state-datum r) (append (state-datum r) ds)))))))))


  ;; ---- derived structure (design 9.2) ---------------------------------------

  ;; DERIVED AFTER EVERY REDUCTION, NEVER WRITTEN BACK. The candidate
  ;; sets are the facts; the tree, the orphans and the cycles are a
  ;; reading of them. Recording a derived conclusion would make it a
  ;; second fact that can disagree with the first, and resolving a
  ;; conflict would then have to remember to clear it.
  ;;
  ;; ONLY SETTLED POSITIONS PARTICIPATE. A block whose position is in
  ;; conflict has no one parent, so it cannot be placed -- it is shown
  ;; under root and marked, rather than guessed at.
  (define (settled-parent b)
    (let ((cs (blk-position b)))
      (and (= 1 (length cs)) (car (car (car cs))))))

  (define (state-structure r)
    (let* ((blocks (reduction-blocks r))
           (alive (filter (lambda (e) (not (blk-tomb (cdr e)))) blocks))
           (parent-of (lambda (id)
                        (let ((e (assoc id blocks)))
                          (and e (settled-parent (cdr e))))))
           (tombed? (lambda (id)
                      (let ((e (assoc id blocks))) (and e (blk-tomb (cdr e)) #t))))
           ;; A CYCLE IS FOUND BY WALKING UP, and the walk is bounded by
           ;; the number of blocks: anything longer has revisited a name.
           (on-cycle?
             (lambda (id)
               (let loop ((cur id) (steps 0) (seen '()))
                 (cond
                   ((> steps (length blocks)) #t)
                   ((member cur seen) (equal? cur id))
                   (else
                    (let ((p (parent-of cur)))
                      (cond
                        ((not p) #f)
                        ((symbol? p) #f)
                        ((equal? p id) #t)
                        (else (loop p (+ steps 1) (cons cur seen)))))))))))
      (list
        (cons 'conflicts
              (list-sort string<?
                         (map car (filter (lambda (e)
                                            (and (not (blk-tomb (cdr e)))
                                                 (on-cycle? (car e))))
                                          alive))))
        ;; A DELETE DOES NOT CASCADE. The child is still there and still
        ;; readable; what it has lost is a place to be shown.
        ;;
        ;; A PARENT THAT IS NOT HERE AT ALL LEAVES THE CHILD IN THE SAME
        ;; position as one that was deleted: nothing to hang it under.
        ;; The local verbs refuse an unknown parent, so this arrives from
        ;; a writer whose record named a block this store has not
        ;; received -- which is exactly when a reader most needs telling.
        (cons 'orphans
              (list-sort string<?
                         (map car (filter (lambda (e)
                                            (let ((p (settled-parent (cdr e))))
                                              (and p (string? p)
                                                   (or (tombed? p)
                                                       (not (assoc p blocks))))))
                                          alive))))
        (cons 'unplaced
              (list-sort string<?
                         (map car (filter (lambda (e)
                                            (not (= 1 (length (blk-position (cdr e))))))
                                          alive))))
        ;; A DOCUMENT IS A FILE, AND A FILE IS NOT INSIDE ANOTHER FILE.
        ;; The write path refuses to put one there, so this arrives only
        ;; from history written before that rule, or from another store.
        ;; It is reported rather than repaired: the record exists and
        ;; says what it says, and a reader that silently moved the block
        ;; would be inventing a history nobody wrote.
        ;;
        ;; IT IS NOTICED WHERE IT IS MADE AND WHERE IT IS SEEN (F85 R3): the
        ;; three write refusals in store.sc, and this report, which
        ;; `conflicts` answers as `nested-document` for each, so a malformed
        ;; store is loud. Readers do not branch on it: to `read --recursive`,
        ;; `grep --under` and the markdown export it is a block like any other
        ;; under its parent, written into the file of the top-level document
        ;; above it when that one is written.
        (cons 'nested-documents
              (list-sort string<?
                         (map car (filter (lambda (e)
                                            (and (eq? 'doc (blk-kind (cdr e)))
                                                 (let ((p (settled-parent (cdr e))))
                                                   (and p (string? p)))))
                                          alive)))))))

  ;; A FIELD THAT IS CONTESTED HAS NO VALUE HERE. Two candidates for
  ;; `kind` is a conflict a reader is told about separately, and picking
  ;; one of them to answer a different question would make this rule's
  ;; verdict depend on which candidate sorted first.
  (define (blk-kind b)
    (let ((e (assq 'kind (blk-fields b))))
      (and e (= 1 (length (cdr e))) (car (car (cdr e))))))

  ;; THE FACTS A FRAME IS COMPUTED FROM, for the change stream's library,
  ;; which is loaded only when somebody subscribes: each block, in creation
  ;; order, as #(id tombstone? fields position kind settled-parent), and the
  ;; links. One procedure, so the reduction's own records stay this
  ;; library's and the frame logic stays out of every start-up.
  (define (reduction-facts r)
    (list (map (lambda (e)
                 (let ((b (cdr e)))
                   (vector (car e) (blk-tomb b) (blk-fields b) (blk-position b)
                           (blk-kind b) (settled-parent b))))
               (reduction-blocks r))
          (reduction-links r)))

  ;; ---- cuts (design 9.3) ----------------------------------------------------

  ;; A CUT IS WELL FORMED ONLY IF IT IS CAUSALLY CLOSED: every record it
  ;; contains has its premises inside it too. A cut that is not gives a
  ;; state nobody ever had, and the honest answer is that it is
  ;; unavailable -- not a truncated one, and not an empty one.
  (define (cut-usable? r cut)
    (cond
      ((not (and (list? cut)
                 (for-all (lambda (e) (and (pair? e) (string? (car e))
                                           (integer? (cdr e))))
                          cut)))
       (list 'unusable 'malformed))
      ((let loop ((es cut) (seen '()))
         (cond ((null? es) #f)
               ((member (car (car es)) seen) #t)
               (else (loop (cdr es) (cons (car (car es)) seen)))))
       (list 'unusable 'duplicate-writer))
      (else
       (let ((applied (reduction-applied r)))
         (let loop ((es cut))
           (cond
             ((null? es) 'usable)
             (else
              (let* ((w (car (car es))) (sq (cdr (car es)))
                     (have (assoc w applied)))
                (cond
                  ((or (not have) (< (cdr have) sq)) (list 'unusable 'not-received))
                  ((not (closed-under? r cut w sq)) (list 'unusable 'not-closed))
                  (else (loop (cdr es)))))))))))) 

  ;; Every premise of everything up to (w . s) has to be inside the cut.
  (define (closed-under? r cut w s)
    (let loop ((n 1))
      (cond
        ((> n s) #t)
        (else
         (let ((past (let ((e (assoc (cons w n) (reduction-pasts r)))) (and e (cdr e)))))
           (cond
             ((not past) (loop (+ n 1)))
             ((for-all (lambda (p)
                         (let ((in (assoc (car p) cut)))
                           (and in (>= (cdr in) (cdr p)))))
                       past)
              (loop (+ n 1)))
             (else #f)))))))

  ;; A CUT COVERS ANOTHER when each of the other's components is at or
  ;; below its own: a writer the first does not name covers nothing of
  ;; that writer. Order between cuts is this, never a comparison of
  ;; sequence numbers across writers.
  (define (cut-covers? a b)
    (for-all (lambda (e) (past-covers? a (car e) (cdr e))) b))

  ;; THE APPLIED PUTS, as event ids in the order they were DELIVERED to this
  ;; reduction -- not the order they were applied: a record waiting on
  ;; another writer's is delivered, and kept in the history, before it is
  ;; applied. Delivery order is the route's (a replay's order, or a
  ;; snapshot's history and then what came after it), so no order across
  ;; writers is to be read from this list; within one writer it is seq
  ;; order. A block's id is derived from the put that created it (block-id),
  ;; so this is what tells a block that has a creating put from one that a
  ;; set, move or delete materialised under an id no put made.
  ;;
  ;; NEVER: APPLIED IS NOT ENOUGH. A malformed put advances its writer's
  ;; applied cursor like any record, but `interpret!` notes it and makes no
  ;; block; `payload-reason` is the same question `interpret!` asks.
  (define (state-put-events r)
    (let loop ((recs (reverse (reduction-history r))) (out '()))
      (if (null? recs)
          (reverse out)
          (let* ((rec (car recs)) (event (cons (rec-writer rec) (rec-seq rec))))
            (loop (cdr recs)
                  (if (and (pair? (rec-payload rec)) (eq? 'put (car (rec-payload rec)))
                           (not (payload-reason (rec-payload rec)))
                           (event-applied? r event))
                      (cons event out)
                      out))))))

  ;; THE STATED NAME OF A RECORD'S ACTOR: the string itself, or, for a
  ;; record a request wrote, the name the request stated (its first
  ;; element; request.sc, request-actor?). #f for anything else.
  (define (actor-name a)
    (cond ((string? a) a)
          ((request-actor? a) (list-ref a 0))
          (else #f)))

  ;; ONE PASS OVER THE HISTORY, KEPT FOR THE LAST HISTORY ASKED. The two
  ;; accessors below are asked once per block by a caller walking a whole
  ;; store -- at a write's locked planning point, among others -- and a scan
  ;; of the history per question is the square of the store. The tables are
  ;; built once and kept while the history and the applied cut are the ones
  ;; they were built from: a record can be in the history before it is
  ;; applied, so the history alone does not say the tables still hold. They
  ;; are a function of those two and nothing else of the reduction (an event
  ;; is applied when the cut covers it), so a rehearsal's copy, which shares
  ;; the history, shares the tables while its cut is the original's, and the
  ;; rehearsal and the real run do not rebuild them in turn. ONE SLOT, AND
  ;; THAT IS WHY THIS LIBRARY STAYS PURE R6RS: a table per history would need
  ;; a weak table to let go of histories nobody holds, and R6RS has none; one
  ;; slot keeps at most one such history alive.
  (define history-tables #f)
  (define (history-table r)
    (let* ((h (reduction-history r)) (cut (reduce-applied-cut r))
           (have history-tables))
      (if (and have (eq? (vector-ref have 0) h) (equal? (vector-ref have 1) cut))
          have
          (let ((creations (make-hashtable string-hash string=?))
                (actors (make-hashtable equal-hash equal?)))
            (for-each
              (lambda (rec)
                (let ((event (cons (rec-writer rec) (rec-seq rec))))
                  (when (event-applied? r event)
                    (hashtable-set! actors event (actor-name (rec-actor rec)))
                    (when (and (pair? (rec-payload rec)) (eq? 'put (car (rec-payload rec)))
                               (not (payload-reason (rec-payload rec))))
                      (hashtable-set! creations (block-id (rec-writer rec) (rec-seq rec))
                                      (cons (actor-name (rec-actor rec)) (rec-payload rec)))))))
              h)
            (let ((built (vector h cut creations actors)))
              (set! history-tables built)
              built)))))

  ;; WHO CREATED A BLOCK, AND WITH WHAT: the actor's stated name and the
  ;; payload of the applied creating put -- the record `state-put-events`
  ;; selects, whose event names the block (block-id) -- or #f when no such
  ;; put is applied. A block's identity and its author are what it was
  ;; created with, read through the history the reduction keeps for the
  ;; store's life, not a field that a later set can change. -> (actor .
  ;; payload) or #f.
  (define (block-creation r id)
    (hashtable-ref (vector-ref (history-table r) 2) id #f))

  ;; WHO WROTE AN APPLIED EVENT: its record's actor's stated name, or #f. A
  ;; store has one log writer, so the event id names the store, not the
  ;; session that asked for the write; the actor is what tells two writers
  ;; of one block's fields apart.
  (define (event-actor r event)
    (hashtable-ref (vector-ref (history-table r) 3) event #f))

  ;; THE LINK EVENTS THAT STILL HOLD ONE EDGE UP, as event ids. An edge is
  ;; one set element however many links made it, and an unlink removes only
  ;; the links in its past (do-unlink!), so after concurrent links several
  ;; can survive, and each says when that writer stated the edge.
  (define (state-link-events r from rel to)
    (map cadddr
         (filter (lambda (l) (and (equal? (car l) from) (eq? (cadr l) rel) (equal? (caddr l) to)))
                 (reduction-links r))))

  ;; THE IDS OF THE BLOCKS THIS STATE HOLDS ALIVE, a deleted block left out,
  ;; in the order the reduction made them. Nothing else of the block.
  (define (state-block-ids r)
    (map car (filter (lambda (e) (not (blk-tomb (cdr e)))) (reduction-blocks r))))

  ;; THE EVENTS OF A BLOCK'S OWN SURVIVING FIELD CANDIDATES, an absence
  ;; included: what the block itself says and who said it last. Its
  ;; position and its edges are not among them -- moving a block or
  ;; linking to it is not an edit of the block. '() for an unknown id.
  ;; The optional list names fields whose events are left out.
  ;; WHETHER A FIELD IS CONTESTED: two or more of its candidates survive. The
  ;; value state-read gives such a field is `(conflict ((<value> <writer>
  ;; <seq>) ...))`, and a value written once can have exactly that shape, so no
  ;; reader of the value can tell the two apart; only the candidates can. #f
  ;; for an unknown id or a field the block does not have.
  ;; VALUE is the field's value as the caller read it. The reducer renders
  ;; every contested field in that shape, so a value without it is never
  ;; contested and is answered at once: the shape only FILTERS, the candidates
  ;; decide. NEVER: ASKING THE CANDIDATES FOR EVERY VALUE IS NOT CHEAP. Finding
  ;; the block is a search of the reduction's blocks (find-block), and every
  ;; reader asks for every field it reads.
  (define (state-field-contested? r id name value)
    (and (pair? value) (eq? (car value) 'conflict)
         (let ((b (find-block r id)))
           (and b (let ((f (assq name (blk-fields b)))) (and f (> (length (cdr f)) 1)))))))

  (define (state-field-events r id . except)
    (let ((b (find-block r id)) (skip (if (pair? except) (car except) '())))
      (if (not b)
          '()
          (apply append (map (lambda (f) (map cdr (cdr f)))
                             (filter (lambda (f) (not (memq (car f) skip))) (blk-fields b)))))))

  ;; Return the causal cut immediately after this applied event.
  (define (state-event-cut state event)
    (let ((past (assoc event (reduction-pasts state))))
      (and past (past-sorted (past-join (cdr past) (list event))))))

  ;; Return the causal cut immediately BEFORE this applied event: its
  ;; stored past, transitively closed (compute-past joins each premise's own
  ;; past), without the event itself. () for a writer's first event with no
  ;; dependencies; #f for an event this state has not applied.
  (define (state-event-past state event)
    (let ((past (assoc event (reduction-pasts state))))
      (and past (past-sorted (cdr past)))))

  ;; THE CANONICAL IDENTITY OF A CUT (design 9.3): writers in order, one
  ;; agreed spelling, sha256 of that text. Anything that uses a cut as a
  ;; key uses this rather than inventing its own.
  (define (cut-id cut)
    (bytevector->hex
      (sha256
        (string->utf8
          (sexpr->string-extended
            (list-sort (lambda (x y) (string<? (car x) (car y)))
                       (map (lambda (e) (cons (car e) (cdr e))) cut)))))))

  ;; ---- rows (the snapshot shape) --------------------------------------------

  ;; ONE ROW PER BLOCK, carrying the candidate sets rather than a
  ;; resolved value: a snapshot that stored resolutions would not be
  ;; equivalent to replaying, because the next write's supersession
  ;; depends on which events the candidates came from.
  ;; THE PASTS TRAVEL COMPRESSED. A clock per applied event makes the
  ;; snapshot grow with history. But a writer's clock only jumps where
  ;; that writer declared a dep: with no deps, past(w,s) is just
  ;; past(w,s-1) with w's own slot raised, because a writer's previous
  ;; event is always a premise. So only the jumps are stored, and the
  ;; rest are recomputed.
  ;; THE STORED CLOCK IS EXCLUSIVE OF ITS OWN EVENT: past(w,s) carries
  ;; (w . s-1), never (w . s), so rebuilding (w . s) from a stored
  ;; (w . t) raises w's slot to s-1. Raising it to s instead is NOT
  ;; observable through anything here: the one place a rebuilt clock is
  ;; read joins the dep's own id into the result anyway, and the clock
  ;; used for supersession is always computed fresh from the record. The
  ;; exclusive form is chosen because it is what a full replay stores,
  ;; so the rebuilt table equals the replayed one -- but no assertion
  ;; can currently tell the two apart, and this is the note saying so.
  (define (past-raise clock w s)
    (if (< s 1) clock (past-join clock (list (cons w s)))))

  ;; Only the events whose clock is not what the pure-predecessor rule
  ;; predicts. That is exactly "the writer declared a dep that told it
  ;; something new" -- a dep that adds nothing need not be stored, and
  ;; deriving the condition from the clocks rather than from the deps
  ;; keeps this independent of what the record happened to declare.
  (define (compress-pasts r)
    (let ((by-writer
            (let group ((es (reduction-pasts r)) (out '()))
              (if (null? es)
                  out
                  (let* ((w (car (car (car es))))
                         (seq (cdr (car (car es))))
                         (clock (cdr (car es)))
                         (have (assoc w out)))
                    (group (cdr es)
                           (if have
                               (cons (cons w (cons (cons seq clock) (cdr have)))
                                     (remp (lambda (e) (string=? (car e) w)) out))
                               (cons (list w (cons seq clock)) out))))))))
      (map
        (lambda (we)
          (let ((w (car we))
                (es (list-sort (lambda (a b) (< (car a) (car b))) (cdr we))))
            (list w
                  (let loop ((es es) (base '()) (out '()))
                    (if (null? es)
                        (reverse out)
                        (let* ((seq (car (car es)))
                               (clock (cdr (car es)))
                               (predicted (past-raise base w (- seq 1))))
                          (if (equal? (past-sorted clock) (past-sorted predicted))
                              (loop (cdr es) predicted out)
                              (loop (cdr es) clock (cons (cons seq clock) out)))))))))
        by-writer)))

  (define (past-lookup table w s)
    (let* ((we (assoc w table))
           (es (if we (cadr we) '()))
           (best (let loop ((es es) (best #f))
                   (cond ((null? es) best)
                         ((and (<= (car (car es)) s)
                               (or (not best) (> (car (car es)) (car best))))
                          (loop (cdr es) (car es)))
                         (else (loop (cdr es) best))))))
      (past-raise (if best (cdr best) '()) w (- s 1))))

  ;; The reducer wants a clock per applied event; the applied cut says
  ;; how far each writer got, so the whole table comes back from the
  ;; jumps plus that cut.
  (define (expand-pasts table cut)
    (let loop ((ws cut) (out '()))
      (if (null? ws)
          out
          (let ((w (car (car ws))) (top (cdr (car ws))))
            (loop (cdr ws)
                  (let inner ((s 1) (out out))
                    (if (> s top)
                        out
                        (inner (+ s 1)
                               (cons (cons (cons w s) (past-lookup table w s)) out)))))))))

  ;; THE PASTS TRAVEL AS THEIR OWN ROW, NOT ATTACHED TO CANDIDATES.
  ;; Attaching each event's clock to the candidates it wrote covers only
  ;; the events that still have a candidate standing. An event whose
  ;; every candidate has since been superseded leaves nothing to hang a
  ;; clock on, and it is still nameable as a premise: a later record may
  ;; declare it as a dep, and then its past is what tells the reduction
  ;; which earlier writes that record has transitively seen. Dropping it
  ;; makes the resumed reduction under-supersede -- a field keeps two
  ;; candidates where a full replay keeps one.
  ;; past(e) is a prefix per writer, so a clock is a cut, not a set of
  ;; ids, and the whole table is one entry per applied event.
  (define (state->rows r)
    (append
      (list (list 'applied (reduce-applied-cut r)))
      (list (list 'request-history
              (filter (lambda (rec)
                        (past-covers? (reduce-applied-cut r) (rec-writer rec) (rec-seq rec)))
                      (reverse (reduction-history r)))))
      (list (list 'pasts (compress-pasts r)))
      ;; NOTE: THIS ROW IS A SERIALISATION OF THE INDEX, NOT A SECOND
      ;; SUPPLIER OF IT. A snapshot may be cut while a plan is still
      ;; incomplete, so the bookkeeping that decides when it completes --
      ;; how many members were declared and which have been applied --
      ;; has to survive with it; without that, a resumed reduction can
      ;; never finish a plan whose last member arrives after the cut.
      ;; A snapshot WITHOUT this row is not an error: the index is
      ;; rebuilt from `request-history`, and the two paths must agree.
      (list (list 'consumed (state-consumption r)))
      (map (lambda (e)
             (let ((b (cdr e)))
               (list 'block (car e)
                     (list (cons 'tomb (blk-tomb b))
                           (cons 'fields (blk-fields b))
                           (cons 'position (blk-position b))))))
           (reduction-blocks r))
      (map (lambda (l) (list 'link l)) (reduction-links r))
      ;; THE NOTES SURVIVE THE SNAPSHOT. They are observations about
      ;; records the snapshot's cut COVERS -- a record this build could
      ;; not apply, or a verb it does not know -- and replay skips
      ;; everything the snapshot covers. Leaving them out meant taking a
      ;; snapshot silently erased them: `conflicts` went quiet about a
      ;; damaged record that is still on disk, and the store looked
      ;; healthier for having been snapshotted.
      (map (lambda (n) (list 'noted n)) (reduction-noted r))
      (map (lambda (t) (list 'tag (car t) (cdr t))) (reduction-tags r))
      ;; A DECLARATION'S CANDIDATES, one row a name; none in a store with
      ;; no declaration, whose rows are as they were.
      (map (lambda (e) (list 'relation (car e) (cdr e))) (reduction-declared-relations r))
      ;; A RULE'S CANDIDATES the same way, one row a name.
      (map (lambda (e) (list 'rule (car e) (cdr e))) (reduction-declared-rules r))))

  (define (rows->state rows)
    (let ((r (reduce-empty))
          (cut (let loop ((rs rows))
                 (cond ((null? rs) '())
                       ((eq? (car (car rs)) 'applied) (cadr (car rs)))
                       (else (loop (cdr rs))))))) 
      (for-each
        (lambda (row)
          (case (car row)
            ;; THE APPLIED CUT COMES BACK TOO. Without it a resumed
            ;; reduction thinks nothing has been applied, so the next
            ;; record's premises are unmet and it waits forever.
            ((applied) (reduction-applied-set! r (cadr row)))
            ;; THE RECORDS COME BACK, and with them the ability to fold
            ;; them again. A snapshot without this row restores a state
            ;; and no way to revise it, which is why `replay` refuses to
            ;; seed from one: the reduction would be carrying an
            ;; obligation it could not meet.
            ((request-history)
             (reduction-history-set! r (reverse (cadr row)))
             (for-each (lambda (rec) (admission-add! (reduction-admission-index r) rec))
                       (cadr row)))
            ((noted)
             (reduction-noted-set! r (append (reduction-noted r) (list (cadr row)))))
            ((pasts) (reduction-pasts-set! r (expand-pasts (cadr row) cut)))
            ((consumed) (restore-consumption! r (cadr row)))
            ((block)
             (let* ((id (cadr row))
                    (body (caddr row))
                    (b (ensure-block! r id)))
               (blk-tomb-set! b (cdr (assq 'tomb body)))
               (blk-fields-set! b (cdr (assq 'fields body)))
               (blk-position-set! b (cdr (assq 'position body)))))
            ((link) (reduction-links-set! r (append (reduction-links r) (list (cadr row)))))
            ((tag)
             (reduction-tags-set!
               r (cons (cons (cadr row) (caddr row)) (reduction-tags r))))
            ((relation)
             (reduction-declared-relations-set!
               r (cons (cons (cadr row) (caddr row)) (reduction-declared-relations r))))
            ((rule)
             (reduction-declared-rules-set!
               r (cons (cons (cadr row) (caddr row)) (reduction-declared-rules r))))
            (else (if #f #f))))
        rows)
      (reduction-gates-set! r (admission-gates (reduction-admission-index r)))
      ;; KEY: `seen` IS REFILLED EITHER WAY. It is about what a plan NAMED,
      ;; which is a fact about the records the snapshot carries and not
      ;; about what was applied -- so it does not travel in the
      ;; `consumed` row, and a snapshot that does carry that row still
      ;; needs it. A build that filled it only on the rebuild path would
      ;; answer "nothing was ever named" for every resumed reduction.
      (fill-seen-from-history! r)
      (unless (exists (lambda (row) (eq? 'consumed (car row))) rows)
        (rebuild-consumption-from-history! r))
      r)))
