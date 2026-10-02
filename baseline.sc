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
(library (theourgia baseline)
  (export baseline-refusal baseline-touching baseline-stale-answer baseline-combine)
  (import (rnrs) (theourgia reduce) (theourgia wire))

  (define (encoded-size value)
    (bytevector-length
      (string->utf8 (sexpr->string-extended (storable-encode value)))))

  (define (touches? rec id)
    (let ((p (list-ref rec 3)))
      (and (pair? p)
        (case (car p)
          ((put) (equal? id (block-id (car rec) (cadr rec))))
          ((set move del link unlink) (and (pair? (cdr p)) (equal? id (cadr p))))
          (else #f)))))

  ;; ---- what a refusal hands back -------------------------------------------
  ;;
  ;; A commit refused for a stale baseline gives the loser the winner's
  ;; commit, so they can merge without going and fetching it. The
  ;; promise is that the refusal's SIZE does not grow with the history:
  ;; at most eight entries, each of bounded size.
  ;;
  ;; THE WINNER IS FIRST AND IS ALWAYS THERE. It is the one record the
  ;; loser certainly needs -- their baseline is stale precisely because
  ;; of it -- and an answer that dropped it to make room would be
  ;; bounded and useless.
  ;;
  ;; THE REST ARE IN CAUSAL ORDER, not in the order they were ingested.
  ;; A reader merging by hand needs premises before the records that
  ;; depend on them, and ingestion order is an accident of who synced
  ;; first: the same two records delivered the other way round would
  ;; otherwise produce a different answer to the same question.
  ;;
  ;; AN ENTRY IS NEVER DROPPED FOR BEING BIG. Its identity and its actor
  ;; are what make it findable; only the content is replaced, by
  ;; `(content elided <bytes>)`, so the reader knows there is something
  ;; there and how much of it. NEVER: An oversized entry TAKES ITS SLOT --
  ;; skipping it to reach an older, smaller one would answer a different
  ;; question than "the most recent eight".
  ;;
  ;; `truncated` MEANS AN ENTRY WAS LEFT OUT, not that the log holds more
  ;; records. The two are not the same question: a block always has the
  ;; record that created it, so "is any history left" is true for every
  ;; refusal that reaches the count bound, and an answer carrying all
  ;; eight of the eight commits that existed still told its reader to go
  ;; and fetch the rest. Measured before that change, on one store per
  ;; row: one earlier commit gave one entry and no mark, seven gave seven
  ;; and no mark, eight gave eight WITH the mark, nine gave eight with
  ;; the mark. The loser was being sent back to `log` for a history it
  ;; already held in full.
  ;;
  ;; AND A REFUSAL WITH NO ENTRIES AT ALL SAYS WHY. The hash can differ
  ;; with no record about this block after the draft's cut: a conflict
  ;; was retracted, the applied cut moved back. NEVER: An empty `since` alone
  ;; leaves the client guessing, so that case carries its own reason and
  ;; a place to look.
  (define content-limit 1024)
  (define entry-limit 8)

  (define (eligible? r id cut)
    (let ((seen (assoc (car r) cut)))
      (and (touches? r id) (or (not seen) (> (cadr r) (cdr seen))))))

  (define (event-of r) (cons (car r) (cadr r)))

  ;; ONE RECORD IS BEFORE ANOTHER when the later one's causal cut has
  ;; reached it. Records neither of which reached the other are
  ;; CONCURRENT, and a total order has to come from somewhere: the pair
  ;; (writer, seq) is used, which is a fact about the records rather than
  ;; about the order they arrived in.
  ;; NEVER: A COMPARATOR MADE OF "CAUSAL, ELSE LEXICAL" IS NOT AN ORDER.
  ;;
  ;; Causality is partial. Falling back to the writer's name for the
  ;; pairs it does not relate produces a relation that CYCLES: with
  ;; `zzzzzzzz` causally before `aaaaaaaa`, and `mmmmmmmm` concurrent
  ;; with both, the ascending relation gives z < a causally, a < m
  ;; lexically and m < z lexically. A sort over a cycling relation
  ;; answers whatever the algorithm's traversal happens to produce, and
  ;; two delivery orders of the same records then give different
  ;; answers -- which is the exact thing ordering causally was for.
  ;; Measured on three records plus a dependent winner: the two
  ;; deliveries put z and a the other way round.
  ;;
  ;; KEY: SO THE ORDER IS A KEY, NOT A COMPARISON. Each record is ranked by
  ;; how much of the history its own causal cut covers; a record that
  ;; causally precedes another covers strictly less, so this key REFINES
  ;; causality, and being a key it is transitive by construction. The
  ;; writer and sequence break the remaining ties, which are exactly the
  ;; concurrent records -- for which any deterministic choice is as good
  ;; as another, so long as it is the same one every time.
  (define (causal-weight state r)
    (let ((cut (state-event-cut state (event-of r))))
      (if cut (apply + (map cdr cut)) 0)))

  (define (rank state r) (list (causal-weight state r) (car r) (cadr r)))

  (define (later? state a b)
    (let ((ra (rank state a)) (rb (rank state b)))
      (cond ((not (= (car ra) (car rb))) (> (car ra) (car rb)))
            ((not (string=? (cadr ra) (cadr rb))) (string>? (cadr ra) (cadr rb)))
            (else (> (caddr ra) (caddr rb))))))

  ;; NOTE: THE LIMIT IS ON THE BODY'S BYTES, NOT ON THE ENCODED CONTENT.
  ;;
  ;; They are not close to each other: a body of 1024 quotation marks
  ;; encodes to 2066 bytes, because every quote is escaped. Measuring the
  ;; encoded form elides a body the contract says to hand back whole, and
  ;; reports a number the reader cannot check against anything they hold.
  ;;
  ;; A record with no body -- a delete, a move -- has no text to measure
  ;; and no text to be long; its encoded form is used, which is bounded
  ;; by the shape of the verb.
  (define (content-body content)
    (and (pair? content) (eq? 'set (car content))
         (= 4 (length content)) (eq? 'src (caddr content))
         (string? (cadddr content))
         (cadddr content)))

  (define (entry-of r)
    (let* ((content (list-ref r 3))
           (body (content-body content))
           (n (if body (bytevector-length (string->utf8 body)) (encoded-size content))))
      (list (event-of r) (list-ref r 4)
            (if (> n content-limit) (list 'content 'elided n) content))))

  ;; ---- the three parts, for a fresh request and for a completion ------------
  ;;
  ;; THE TOUCHING RECORDS OUTSIDE A CUT THAT A CALLER'S FILTER KEEPS:
  ;; (baseline-touching state id cut keep?) -> (records omitted?), the
  ;; records selected and ordered as `since` is: the newest eight by causal
  ;; rank, then the winner first and the others oldest first; OMITTED? says
  ;; whether any were left out. A fresh commit keeps every record; a
  ;; completion keeps the records that are not its plan's own members.
  (define (baseline-touching state id cut keep?)
    (let* ((rows (state->rows state))
           (history (cadr (assq 'request-history rows)))
           (all (filter (lambda (r) (and (eligible? r id cut) (keep? r))) history))
           ;; NEWEST FIRST, so "the most recent eight" is a prefix.
           (ranked (list-sort (lambda (a b) (later? state a b)) all))
           (kept (if (> (length ranked) entry-limit)
                     (let loop ((xs ranked) (n 0) (out '()))
                       (if (or (null? xs) (= n entry-limit)) (reverse out)
                           (loop (cdr xs) (+ n 1) (cons (car xs) out))))
                     ranked))
           (omitted? (> (length ranked) (length kept)))
           ;; THE WINNER FIRST, THEN THE REST OLDEST-FIRST. The winner is
           ;; the newest of the kept; the others are given in the order a
           ;; reader would apply them.
           (ordered (if (null? kept) '()
                        (cons (car kept)
                              (list-sort (lambda (a b) (later? state b a)) (cdr kept))))))
      (list ordered omitted?)))

  ;; THE WORDING OF A STALE BASELINE from what baseline-touching chose:
  ;; `based-on` is the hash the writer started from (or `unknown`), `now`
  ;; the block's hash; with no record, the reason and a place to look; with
  ;; records left out, the truncated tail.
  (define (baseline-stale-answer id based-on now records omitted?)
    (append (list 'error 'stale-baseline (list 'block id)
                  (list 'based-on based-on) (list 'now now)
                  (cons 'since (map entry-of records)))
            (if (null? records)
                (list '(reason candidate-set-changed)
                      (list 'conflicts id))
                '())
            (if omitted?
                (list '(truncated #t) (list 'retrieve (list 'log id) (list 'read id)))
                '())))

  ;; SEVERAL BLOCKS' REFUSALS AS ONE ANSWER: none -> #f, one -> itself,
  ;; more -> (error stale-baseline (blocks <each refusal's clauses> ...)).
  (define (baseline-combine refusals)
    (cond ((null? refusals) #f)
          ((null? (cdr refusals)) (car refusals))
          (else (list 'error 'stale-baseline (cons 'blocks (map cddr refusals))))))

  ;; A FRESH REQUEST'S CHECK: silent when the block's hash is the one the
  ;; writer started from; otherwise every touching record outside the cut.
  (define (baseline-refusal state id wanted . rest)
    (let ((now (block-hash state id))
          (cut (if (pair? rest) (car rest) '())))
      (and (not (equal? wanted now))
           (let ((chosen (baseline-touching state id cut (lambda (r) #t))))
             (baseline-stale-answer id wanted now (car chosen) (cadr chosen))))))
)
