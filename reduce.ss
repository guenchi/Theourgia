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
  (export reduce-empty reduce-apply! reduce-pending reduce-applied-cut reduce-trace
          state-read state-outline state-dump state-hash state-datum block-hash
          state-structure state-refs cut-usable? cut-id
          state->rows rows->state
          ord-between block-id
          reduction? reduction-state)
  (import (rnrs base) (rnrs control) (rnrs lists) (rnrs sorting)
          (rnrs records syntactic) (rnrs hashtables) (rnrs arithmetic fixnums)
          (rnrs unicode) (rnrs io simple)
          (only (theourgia wire) sexpr->string-extended)
          (only (igropyr crypto) sha256 bytevector->hex)
          (only (rnrs bytevectors) string->utf8))

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
  (define-record-type reduction
    (fields (mutable blocks)
            (mutable links)
            (mutable tags)
            (mutable pasts)
            (mutable applied)
            (mutable pending)
            (mutable trace)))

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

  (define (reduce-empty) (make-reduction '() '() '() '() '() '() '()))

  (define (reduction-state r) r)
  (define (reduce-pending r) (map record-of (reduction-pending r)))
  (define (reduce-trace r) (reverse (reduction-trace r)))
  (define (reduce-applied-cut r)
    (list-sort (lambda (x y) (string<? (car x) (car y)))
               (map (lambda (e) (cons (car e) (cdr e))) (reduction-applied r))))

  ;; A record as this layer receives it: everything the log layer knows
  ;; about one line, with the payload already decoded.
  (define (make-record writer seq deps payload) (list writer seq deps payload))
  (define (record-of x) x)
  (define (rec-writer x) (car x))
  (define (rec-seq x) (cadr x))
  (define (rec-deps x) (caddr x))
  (define (rec-payload x) (cadddr x))

  ;; APPLICABLE MEANS EVERY PREMISE IS IN. Not "the deps have arrived" --
  ;; applied. A record whose deps are merely present but themselves
  ;; pending would be applied against a state that does not contain them.
  (define (applicable? r rec)
    (let ((applied (reduction-applied r)))
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
  (define (reduce-apply! r writer seq deps payload)
    (let ((have (assoc writer (reduction-applied r))))
      (cond
        ((and have (>= (cdr have) seq)) (list 'refused 'already-applied))
        ((exists (lambda (rec) (and (string=? (rec-writer rec) writer)
                                    (= (rec-seq rec) seq)))
                 (reduction-pending r))
         (list 'refused 'already-pending))
        (else
         (reduction-pending-set! r (append (reduction-pending r)
                                           (list (make-record writer seq deps payload))))
         (drain! r)
         'accepted))))

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

  (define (interpret! r event-id past payload)
    (cond
      ((not (pair? payload)) (if #f #f))
      (else
       (case (car payload)
         ((put) (do-put! r event-id past (cadr payload)))
         ((set) (do-set! r event-id past (cdr payload)))
         ((del) (do-del! r event-id (cadr payload)))
         ((move) (do-move! r event-id past (cdr payload)))
         ((link) (do-link! r event-id (cdr payload)))
         ((unlink) (do-unlink! r event-id past (cdr payload)))
         ((tag) (do-tag! r event-id past (cdr payload)))
         (else (if #f #f))))))

  (define (do-put! r event-id past alist)
    (let* ((id (block-id (car event-id) (cdr event-id)))
           (b (ensure-block! r id)))
      (for-each
        (lambda (pair)
          (case (car pair)
            ((parent ord) (if #f #f))
            (else
             (blk-fields-set!
               b (put-field (blk-fields b) (car pair) (cdr pair) event-id past)))))
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
                       (filter
                         (lambda (f) (not (eq? (cdr f) 'omit)))
                         (map (lambda (f)
                                (let ((cs (cdr f)))
                                  (cons (car f)
                                        (cond
                                          ((and (= 1 (length cs)) (absent? (car (car cs))))
                                           'omit)
                                          ((= 1 (length cs)) (copy-datum (car (car cs))))
                                          (else
                                           (list 'conflict
                                                 (list-sort candidate<?
                                                            (map (lambda (c)
                                                                   (list (copy-datum (car c))
                                                                         (car (cdr c))
                                                                         (cdr (cdr c))))
                                                                 cs))))))))
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
                       (sexpr->string-extended (block->datum r id (cdr e)))))))))

  (define (state-hash r)
    (bytevector->hex
      (sha256 (string->utf8 (sexpr->string-extended (state-datum r))))))


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
        (cons 'orphans
              (list-sort string<?
                         (map car (filter (lambda (e)
                                            (let ((p (settled-parent (cdr e))))
                                              (and p (string? p) (tombed? p))))
                                          alive))))
        (cons 'unplaced
              (list-sort string<?
                         (map car (filter (lambda (e)
                                            (not (= 1 (length (blk-position (cdr e))))))
                                          alive)))))))

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
      (list (list 'pasts (compress-pasts r)))
      (map (lambda (e)
             (let ((b (cdr e)))
               (list 'block (car e)
                     (list (cons 'tomb (blk-tomb b))
                           (cons 'fields (blk-fields b))
                           (cons 'position (blk-position b))))))
           (reduction-blocks r))
      (map (lambda (l) (list 'link l)) (reduction-links r))
      (map (lambda (t) (list 'tag (car t) (cdr t))) (reduction-tags r))))

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
            ((pasts) (reduction-pasts-set! r (expand-pasts (cadr row) cut)))
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
            (else (if #f #f))))
        rows)
      r)))
