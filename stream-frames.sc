#!r6rs
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

;; WHAT CHANGED BETWEEN TWO PUBLICATIONS: THE CHANGE STREAM'S FRAMES.
;;
;; THIS IS ITS OWN LIBRARY because only a daemon with a subscriber needs it:
;; the store process enters it when a subscription is accepted, and a start
;; that subscribes nothing -- every command line, every daemon nobody
;; follows -- never loads it. It reads a reduction only through
;; reduction-facts, one exported procedure, so the reduction's records stay
;; the reducer's.

(library (theourgia stream-frames)
  (export structural-sets change-items)
  (import (rnrs) (only (theourgia reduce) reduction-facts))

  ;; A block's facts, #(id tombstone? fields position kind settled-parent),
  ;; read by the names the reducer's own records use, so the rules below
  ;; read as they did there.
  (define (blk-tomb v) (vector-ref v 1))
  (define (blk-fields v) (vector-ref v 2))
  (define (blk-position v) (vector-ref v 3))
  (define (blk-kind v) (vector-ref v 4))
  (define (settled-parent v) (and v (vector-ref v 5)))
  ;; (id . facts) for every block, in creation order.
  (define (fact-blocks facts) (map (lambda (v) (cons (vector-ref v 0) v)) (car facts)))

  ;; ---- what changed between two publications ---------------------------------
  ;;
  ;; THE SAME FOUR SETS AS state-structure, IN ONE PASS. state-structure walks
  ;; to the root from every live block and measures and searches the block
  ;; list at every step, which is the square of the block count and its cube
  ;; on a deep chain; a frame is computed while a writer holds the store's
  ;; lock, so it cannot cost that. Here a hashtable maps id to block once,
  ;; and the cycles are found by colouring the parent relation: a block has
  ;; at most one settled parent, so the relation is a functional graph, and
  ;; one walk that visits each block once finds every cycle's members.
  ;; The predicates are state-structure's, read through the table:
  ;;   - a settled parent exists only with exactly one position candidate;
  ;;   - a symbol parent ends a walk, and so does a string naming no block;
  ;;   - a walk passes through tombstoned blocks, and the answer leaves them
  ;;     out;
  ;;   - a cycle's members are the blocks ON it, not those leading into it;
  ;;   - an orphan is a live block whose settled parent is a string naming no
  ;;     block or a tombstoned one;
  ;;   - unplaced is zero or several position candidates;
  ;;   - nested is a live block of settled kind doc with any string parent.
  ;; The answer has state-structure's shape, so the two are compared with
  ;; equal? (a change-stream row compares them): two implementations of one rule.
  (define (structural-sets r)
    (let* ((blocks (fact-blocks (reduction-facts r)))
           (table (make-hashtable string-hash string=?))
           (colour (make-hashtable string-hash string=?))
           (on-cycle (make-hashtable string-hash string=?)))
      (for-each (lambda (e) (hashtable-set! table (car e) (cdr e))) blocks)
      (let ((next (lambda (id)
                    (let ((p (settled-parent (hashtable-ref table id #f))))
                      (and (string? p) (hashtable-ref table p #f) p))))
            (alive? (lambda (e) (not (blk-tomb (cdr e)))))
            (ids-where (lambda (keep?)
                         (list-sort string<? (map car (filter keep? blocks))))))
        ;; colour 1: on the walk being made; 2: finished
        (for-each
          (lambda (e)
            (unless (hashtable-ref colour (car e) #f)
              (let walk ((cur (car e)) (path '()))
                (cond
                  ((not cur) (for-each (lambda (id) (hashtable-set! colour id 2)) path))
                  ((eqv? (hashtable-ref colour cur #f) 1)
                   (let mark ((ps path))
                     (hashtable-set! on-cycle (car ps) #t)
                     (unless (string=? (car ps) cur) (mark (cdr ps))))
                   (for-each (lambda (id) (hashtable-set! colour id 2)) path))
                  ((eqv? (hashtable-ref colour cur #f) 2)
                   (for-each (lambda (id) (hashtable-set! colour id 2)) path))
                  (else
                   (hashtable-set! colour cur 1)
                   (walk (next cur) (cons cur path)))))))
          blocks)
        (list
          (cons 'conflicts
                (ids-where (lambda (e) (and (alive? e) (hashtable-ref on-cycle (car e) #f)))))
          (cons 'orphans
                (ids-where (lambda (e)
                             (and (alive? e)
                                  (let ((p (settled-parent (cdr e))))
                                    (and p (string? p)
                                         (let ((pb (hashtable-ref table p #f)))
                                           (or (not pb) (and (blk-tomb pb) #t)))))))))
          (cons 'unplaced
                (ids-where (lambda (e) (and (alive? e) (not (= 1 (length (blk-position (cdr e)))))))))
          (cons 'nested-documents
                (ids-where (lambda (e)
                             (and (alive? e) (eq? 'doc (blk-kind (cdr e)))
                                  (let ((p (settled-parent (cdr e)))) (and p (string? p)))))))))))

  ;; THE ITEMS OF ONE FRAME: what changed from the reduction OLD to NEW, by
  ;; comparing the two in one pass over the union of their block ids and the
  ;; union of their links (a retraction can remove a block or bring an old
  ;; candidate back without a new event, so the event suffix is not the
  ;; whole story). OLD-SETS and NEW-SETS are the two reductions' structural
  ;; sets, kept with each publication so that a frame computes one.
  ;; Per block: live before and not now -> (removed id); not before and live
  ;; now -> (added id); live in both -> each field's candidate set compared
  ;; as a set ((changed id f), and (conflict id f)/(resolved id f) when its
  ;; size crosses one, or stays above one and changed), and the position
  ;; set the same way, with (changed id parent) and (changed id ord) when
  ;; the settled parent or ord differs. A tombstone that stays one says
  ;; nothing. Then the structural sets ((conflict id cycle) ...), and the
  ;; raw links, which are per event.
  ;; THE SAME OBJECT IS NO CHANGE: a refold answered by the resident
  ;; reduction hands back what is already published.
  (define (change-items old new old-sets new-sets)
    (if (eq? old new)
        '()
        (let* ((of (reduction-facts old)) (nf (reduction-facts new))
               (old-blocks (fact-blocks of)) (new-blocks (fact-blocks nf))
               (old-links (cadr of)) (new-links (cadr nf))
               (ot (make-hashtable string-hash string=?))
              (nt (make-hashtable string-hash string=?))
              (out '()))
          (define (emit! item) (set! out (cons item out)))
          (define (live b) (and b (not (blk-tomb b)) b))
          (define (same-set? a b)
            (and (= (length a) (length b))
                 (for-all (lambda (x) (member x b)) a)))
          (define (settled-of cs part)
            (and (= 1 (length cs)) (part (car (car cs)))))
          (define (crossing! id what o n differ?)
            (let ((so (length o)) (sn (length n)))
              (cond ((and (<= so 1) (> sn 1)) (emit! (list 'conflict id what)))
                    ((and (> so 1) (<= sn 1)) (emit! (list 'resolved id what)))
                    ((and (> so 1) (> sn 1) differ?) (emit! (list 'conflict id what))))))
          (define (compare-live! id ob nb)
            (let ((ofs (blk-fields ob)) (nfs (blk-fields nb)))
              (for-each
                (lambda (f)
                  (let* ((o (let ((e (assq f ofs))) (if e (cdr e) '())))
                         (n (let ((e (assq f nfs))) (if e (cdr e) '())))
                         (differ? (not (same-set? o n))))
                    (when differ? (emit! (list 'changed id f)))
                    (crossing! id f o n differ?)))
                (let union ((fs (map car ofs)) (acc '()))
                  (cond ((null? fs)
                         (append (reverse acc) (filter (lambda (f) (not (memq f acc))) (map car nfs))))
                        ((memq (car fs) acc) (union (cdr fs) acc))
                        (else (union (cdr fs) (cons (car fs) acc)))))))
            (let* ((o (blk-position ob)) (n (blk-position nb)) (differ? (not (same-set? o n))))
              (when differ?
                (emit! (list 'changed id 'position))
                (unless (equal? (settled-of o car) (settled-of n car)) (emit! (list 'changed id 'parent)))
                (unless (equal? (settled-of o cdr) (settled-of n cdr)) (emit! (list 'changed id 'ord))))
              (crossing! id 'position o n differ?)))
          (for-each (lambda (e) (hashtable-set! ot (car e) (cdr e))) old-blocks)
          (for-each (lambda (e) (hashtable-set! nt (car e) (cdr e))) new-blocks)
          (for-each
            (lambda (id)
              (let ((ob (live (hashtable-ref ot id #f))) (nb (live (hashtable-ref nt id #f))))
                (cond ((and ob (not nb)) (emit! (list 'removed id)))
                      ((and nb (not ob)) (emit! (list 'added id)))
                      ((and ob nb) (compare-live! id ob nb)))))
            (append (map car old-blocks)
                    (filter (lambda (id) (not (hashtable-ref ot id #f))) (map car new-blocks))))
          (for-each
            (lambda (key kind)
              (let ((o (cdr (assq key old-sets))) (n (cdr (assq key new-sets))))
                (for-each (lambda (id) (unless (member id o) (emit! (list 'conflict id kind)))) n)
                (for-each (lambda (id) (unless (member id n) (emit! (list 'resolved id kind)))) o)))
            '(conflicts orphans unplaced nested-documents)
            '(cycle orphan unplaced nested))
          (let ((ol (make-hashtable equal-hash equal?)) (nl (make-hashtable equal-hash equal?)))
            (for-each (lambda (l) (hashtable-set! ol l #t)) old-links)
            (for-each (lambda (l) (hashtable-set! nl l #t)) new-links)
            (for-each (lambda (l)
                        (unless (hashtable-ref ol l #f)
                          (emit! (list 'edge-added (car l) (cadr l) (caddr l) (cons 'event (cadddr l))))))
                      new-links)
            (for-each (lambda (l)
                        (unless (hashtable-ref nl l #f)
                          (emit! (list 'edge-removed (car l) (cadr l) (caddr l)))))
                      old-links))
          (reverse out))))
)
