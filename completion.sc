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

;; COMPLETING A PERSISTED PLAN: THE JUDGEMENT.
;;
;; A retry finishes a persisted plan from the plan, and the plan's frozen
;; entries are not a licence to write over what others wrote since. The
;; request's premises were checked once, at admission; a completion checks
;; again, against the plan's own causal cut, which never moves however many
;; attempts the completion takes.
;;
;; THIS IS ITS OWN LIBRARY because only a completion needs it: the store
;; enters it when a retry reaches a persisted plan with members missing,
;; and a command line that never completes a plan never loads it.
;;
;; IN THIS ORDER, before anything is written, and the first that refuses is
;; the whole answer: the frozen text against the version its consumes item
;; names (the store makes that check before entering here); the markers
;; (each names an earlier insert of the plan); then every missing member's
;; target. A TARGET is the block a set, move, del, link or unlink names (an
;; insert makes a block and overwrites nothing). It is STALE when an APPLIED
;; record that touches it lies outside the plan's cut and is not one of this
;; plan's members, or, for a commit member, when the block's hash is not
;; the based-on its consumes item froze. One stale target and nothing is
;; written.
;;
;; AND AFTER EACH MEMBER IT WRITES: the plan and the record just written must
;; be applied, or the completion stops with `unknown`; and if anything else
;; became applied or was taken back because of the write, the members still
;; missing are judged again, as at first. Nothing is predicted: a record that
;; merely waits holds nothing up.
(library (theourgia completion)
  (export completion-run)
  (import (rnrs) (theourgia reduce)
          (only (theourgia baseline) baseline-touching baseline-stale-answer baseline-combine))

  ;; A marker is a position, the parent or the sibling of an insert or a
  ;; move, holding ("#%new" k): the block member k of the plan makes. The
  ;; same list in a member's value is data.
  (define (plan-marker? x)
    (and (pair? x) (equal? (car x) "#%new") (pair? (cdr x)) (null? (cddr x))))

  ;; A DECLARED ENTRY IS READ BY ITS SHAPE. Until the completion binds it, an
  ;; entry holds a marker in its parent or sibling position, which the
  ;; store's intent checks read as not an id; and a plan the reducer
  ;; admits can declare any shape. So these readers test pairs and lists
  ;; and catch nothing: a shape they do not recognise has no kind, no
  ;; references and no target, and the run then answers for it as any run
  ;; does.
  ;; The intent inside an (expect <subject> <intent> ...) wrapper.
  (define (declared-intent e)
    (if (and (pair? e) (eq? (car e) 'expect) (pair? (cdr e)) (pair? (cddr e))) (caddr e) e))
  (define (declared-kind e)
    (let ((u (declared-intent e))) (and (pair? u) (symbol? (car u)) (car u))))
  ;; Where an entry's references start: an insert's parent is its element
  ;; 1, a move's its element 2; the sibling follows the parent.
  (define (refs-start e)
    (case (declared-kind e) ((insert) 1) ((move) 2) (else #f)))
  ;; -> the references the entry has: its parent, then its sibling, each
  ;; read on its own through the pairs that reach it, so an entry that
  ;; ends after its parent has one and an entry that ends before it has
  ;; none. A plan the reducer admits can declare an insert or a move
  ;; shorter or longer than the four parts the store writes; a marker in
  ;; any reference it has is still a marker, and is still checked.
  (define (declared-refs e)
    (let ((k (refs-start e)))
      (if (not k)
          '()
          (let take ((l (declared-intent e)) (i 0) (out '()))
            (cond ((or (not (pair? l)) (= i (+ k 2))) (reverse out))
                  ((>= i k) (take (cdr l) (+ i 1) (cons (car l) out)))
                  (else (take (cdr l) (+ i 1) out)))))))
  ;; L with its elements from K on replaced by VS, as many as VS has, the
  ;; rest of its pairs and its tail as they were.
  (define (with-elements l k vs)
    (let loop ((l l) (i 0) (vs vs))
      (cond ((not (pair? l)) l)
            ((and (>= i k) (pair? vs)) (cons (car vs) (loop (cdr l) (+ i 1) (cdr vs))))
            (else (cons (car l) (loop (cdr l) (+ i 1) vs))))))
  ;; The entry with its references replaced in place by VS, one for each
  ;; reference declared-refs read: the rest of the entry, and its wrapper,
  ;; are kept as declared, and the run judges them as it judges any entry.
  (define (declared-with-refs e vs)
    (let* ((u (declared-intent e))
           (r (with-elements u (refs-start e) vs)))
      (if (eq? u e) r (with-elements e 2 (list r)))))

  ;; A member of this plan: a record whose actor names this plan event with
  ;; an integer index. A record that claims the request's identity without
  ;; that is foreign.
  (define (plan-member-index actor plan-event)
    (and (list? actor) (= (length actor) 6)
         (let ((k (list-ref actor 2)))
           (and (integer? k) (exact? k) (equal? (list-ref actor 4) plan-event) k))))

  ;; -> ((index . record) ...), the plan's members that are applied now,
  ;; read from the state's history (never counted from answers).
  (define (plan-members-applied state plan-event)
    (let loop ((rs (cadr (assq 'request-history (state->rows state)))) (out '()))
      (if (null? rs)
          (reverse out)
          (let ((k (plan-member-index (list-ref (car rs) 4) plan-event)))
            (loop (cdr rs) (if k (cons (cons k (car rs)) out) out))))))

  (define (completion-clause state plan-event of)
    (list 'completion (list 'plan plan-event)
          (cons 'present (list-sort < (map car (plan-members-applied state plan-event))))
          (list 'of of)))

  ;; -> (error no-such-intent k) for the first marker of a missing member
  ;; that does not name an earlier insert of the plan, or #f.
  (define (marker-refusal entries missing)
    (let loop ((ms missing))
      (if (null? ms)
          #f
          (let* ((m (caar ms)) (e (cdar ms))
                 (bad (find (lambda (x)
                              (and (plan-marker? x)
                                   (let* ((k (cadr x))
                                          (made (and (integer? k) (exact? k) (assv k entries))))
                                     (not (and made (< k m) (eq? 'insert (declared-kind (cdr made))))))))
                            (declared-refs e))))
            (if bad (list 'error 'no-such-intent (cadr bad)) (loop (cdr ms)))))))

  ;; THE INVERSE OF as-marker: a marker naming a PRESENT member becomes the
  ;; id of the block that member's applied record made; one naming a member
  ;; this run makes becomes the run's own back-reference (from j), j being
  ;; its place among the missing members.
  (define (bind-markers e made-ids run-indices)
    (let ((rs (declared-refs e)))
      (if (or (null? rs) (not (exists plan-marker? rs)))
          e
          (let ((bind (lambda (x)
                        (if (plan-marker? x)
                            (let ((p (assv (cadr x) made-ids)))
                              (if p
                                  (cdr p)
                                  ;; NOT REACHED WITH -1: the marker check has
                                  ;; made k an earlier insert of the plan, and
                                  ;; the members missing are a suffix of it, so
                                  ;; member k is applied (above) or in this
                                  ;; run. Were it reached, (from -1) is refused
                                  ;; by the run as not an index, never read
                                  ;; as a position.
                                  (list 'from (let index ((is run-indices) (j 0))
                                                (cond ((null? is) -1)
                                                      ((eqv? (car is) (cadr x)) j)
                                                      (else (index (cdr is) (+ j 1))))))))
                            x))))
            (declared-with-refs e (map bind rs))))))

  (define (member-target e)
    (let ((u (declared-intent e)))
      (and (memq (declared-kind e) '(set move del link unlink)) (pair? (cdr u)) (cadr u))))

  ;; -> the refusal for the stale targets among INTENTS, or #f.
  (define (stale-judgement state plan-event plan-cut consumes intents)
    (let* ((items (if consumes (caddr consumes) '()))
           (targets (let loop ((is intents) (out '()))
                      (if (null? is)
                          (reverse out)
                          (let ((t (member-target (car is))))
                            (loop (cdr is) (if (and t (not (member t out))) (cons t out) out))))))
           (refusals
             (let loop ((ts targets) (out '()))
               (if (null? ts)
                   (reverse out)
                   (let* ((t (car ts))
                          (item (assoc t items))
                          (based-on (if item (caddr item) 'unknown))
                          (now (block-hash state t))
                          (chosen (baseline-touching state t plan-cut
                                                     (lambda (r) (not (plan-member-index (list-ref r 4) plan-event))))))
                     (loop (cdr ts)
                           (if (or (pair? (car chosen)) (and item (not (equal? based-on now))))
                               (cons (baseline-stale-answer t based-on now (car chosen) (cadr chosen)) out)
                               out)))))))
      (baseline-combine refusals)))

  (define (applied-in? cut event)
    (let ((e (assoc (car event) cut)))
      (and e (>= (cdr e) (cdr event)))))

  (define (cut-advanced cut event)
    (cons (cons (car event) (cdr event))
          (filter (lambda (p) (not (equal? (car p) (car event)))) cut)))

  (define (same-cut? a b)
    (and (= (length a) (length b))
         (for-all (lambda (p) (equal? (assoc (car p) b) p)) a)))

  ;; THE ENTRY. ENTRIES are the plan's declared (index . intent) pairs,
  ;; MISSING those not present and INDICES their indices, CONSUMES the
  ;; plan's consumes clause or #f. RUNNER is the store's: given the intents
  ;; to run and the procedure to call after each member that answers ok, it
  ;; runs them and answers their answers. -> the answers of the completion.
  (define (completion-run state plan-event entries missing indices consumes runner)
    (let ((of (length entries))
          (finish (lambda (answer) (append answer (list (completion-clause state plan-event (length entries)))))))
      (cond
        ((marker-refusal entries missing) => list)
        (else
         (let* ((made-ids (let loop ((ms (plan-members-applied state plan-event)) (out '()))
                            (if (null? ms)
                                out
                                (let ((p (list-ref (cdar ms) 3)))
                                  (loop (cdr ms)
                                        (if (and (pair? p) (eq? (car p) 'put))
                                            (cons (cons (caar ms) (block-id (car (cdar ms)) (cadr (cdar ms)))) out)
                                            out))))))
                (run (map (lambda (e) (bind-markers (cdr e) made-ids indices)) missing))
                ;; THE PLAN IS APPLIED HERE: the verdict that sends a request
                ;; to its completion answers unknown for any of its records
                ;; that is waiting, the plan included (request.sc, rule 1,
                ;; find-unknown). If it ever were not, the plan's cut would
                ;; be unknown and every touching record would look foreign;
                ;; a plan this completion cannot see applied is answered
                ;; unknown with not-applied, as after a write.
                (plan-cut (state-event-cut state plan-event))
                (refused (and plan-cut (stale-judgement state plan-event plan-cut consumes run))))
           (cond
             ((not plan-cut)
              (list (finish (list 'error 'unknown (list 'not-applied plan-event)))))
             (refused
               (list (finish refused)))
             (else
               (let ((cut-before (reduce-applied-cut state)))
                 (runner run
                               (lambda (n answer)
                                 ;; EVERY OK ANSWER OF A MEMBER NAMES ITS RECORD:
                                 ;; one-intent! answers ok only once the record is
                                 ;; committed, with (events ((<writer> . <seq>))),
                                 ;; and write-outcome->answer never answers ok. A
                                 ;; member that answered ok with no event would be
                                 ;; a record this completion cannot see applied,
                                 ;; and that is answered unknown with not-applied:
                                 ;; naming the plan if it is the one not applied,
                                 ;; and otherwise the member, by its index, since
                                 ;; its event is what is missing.
                                 (let ((ev (let ((e (assq 'events (cdr answer))))
                                             (and e (pair? (cadr e)) (pair? (car (cadr e))) (car (cadr e)))))
                                       (cut (reduce-applied-cut state)))
                                   (cond
                                     ((not (applied-in? cut plan-event))
                                      (finish (list 'error 'unknown (list 'not-applied plan-event))))
                                     ((not ev)
                                      (finish (list 'error 'unknown (list 'not-applied (list 'member (list-ref indices n))))))
                                     ((not (applied-in? cut ev))
                                      (finish (list 'error 'unknown (list 'not-applied ev))))
                                     ((and (< (+ n 1) (length run))
                                           (not (same-cut? cut (cut-advanced cut-before ev))))
                                      (let ((again (stale-judgement state plan-event plan-cut consumes
                                                                    (list-tail run (+ n 1)))))
                                        (set! cut-before cut)
                                        (and again (finish again))))
                                     (else (set! cut-before cut) #f)))))))))))))
)
