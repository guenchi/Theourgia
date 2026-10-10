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

;; The retained set is complete, and that is the only thing holding the
;; rebuild up.
;;
;; THE REDUCTION TAKES A RECORD BACK BY FOLDING EVERYTHING AGAIN. There
;; is no undo: when a record that was already applied turns out not to
;; belong, the state is dropped and rebuilt from the records the
;; reduction kept. That makes one path from records to state instead of
;; two, and removes the obligation to prove that an undo agrees with a
;; replay -- but it puts the whole weight on a different claim: that the
;; kept records ARE every record the reduction was ever given.
;;
;; A SET MISSING ONE RECORD STILL LOOKS RIGHT MOST OF THE TIME. It is
;; only wrong after a rebuild, only when the missing record mattered to
;; what was rebuilt, and only in the orders that trigger a rebuild at
;; all. Three hand-written orders cannot survey that; the failure is a
;; property of the arrival order, so the order is what has to vary.
;;
;; AND A GREEN READING FROM THIS FILE MEANS NOTHING UNTIL A MUTANT HAS
;; BEEN RUN THROUGH IT. The first version of this case passed all sixty
;; seeds against a reduction that kept ONLY the records carrying a
;; request -- the exact defect it exists to catch -- because every plain
;; record in its corpus hung off the contested slot and so never reached
;; the state either way. The claim was right and the stimulus did not
;; carry it. The corpus now includes records that belong to nothing
;; contested, and the CONTROL rows at the bottom are what keep them
;; there.
;;
;; SO THE ORACLE IS THE SAME SET IN A DIFFERENT ORDER. Records are
;; delivered in whatever order writers and mirrors produce them, and the
;; contract is that the state does not depend on it. Canonical order is
;; not privileged -- it is simply one of the orders -- so a disagreement
;; between two orders is a defect whichever of them is wrong.

(import (chezscheme) (theourgia reduce) (theourgia request) (theourgia wire)
        (theourgia log))

(define bad 0)
(define (want-1 name value expect)
  (unless (equal? value expect)
    (set! bad (+ bad 1))
    (printf "FAIL ~a -> ~s   WANT ~s\n" name value expect)))

;; A ROW THAT RAISES IS A FAILED ROW, NOT A FAILED FILE. Rows read an
;; answer apart, and a seeded defect that changes the answer's SHAPE
;; makes the accessor raise while the row is being computed -- outside
;; anything that was catching. The file then ends where it stood, every
;; row below goes unrun, and the runner sees no `FAIL` at all: a round
;; scored three such defects as crashes with no failures, for answers
;; the store had in fact got right and said plainly.
;;
;; BOTH SIDES, BECAUSE EITHER CAN RAISE. A row whose EXPECTATION is
;; derived from the program's own answer raises while the expectation
;; is built, and ends the file just the same.
;;
;; IT IS A MACRO FOR ONE REASON: an argument is evaluated before the
;; call, so a procedure could not have guarded either side.
;;
;; IT DOES NOT COVER EVERYTHING. Top-level definitions between rows are
;; outside it, and a raise there still ends the file.
;; HOW MANY ROWS ACTUALLY RAN. A file that ends early still
;; reports the failures it had already found, so a seeded defect
;; that kills the file after a few rows is scored as caught while
;; the rows below it never ran. The count is the only thing that
;; tells those apart, and it has to be compared against the same
;; file's count on unmutated code -- there is no static number to
;; compare it with, because rows are written inside loops and case
;; tables as well as one at a time.
(define rows-run 0)

(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) (caught x)))))))

(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))


;; A REPEATABLE STREAM, because a case that cannot be run again on the
;; seed that broke it reports a failure nobody can look at. The
;; generator is written out here rather than taken from the host: a
;; host's `random` is free to differ between builds, and the seed would
;; then name a different sequence on the machine the report reaches.
(define (make-rng seed)
  (let ((state (vector (if (= 0 seed) 1 seed))))
    (lambda (n)
      (let* ((x (vector-ref state 0))
             (x (bitwise-and (+ (* x 1103515245) 12345) #x7FFFFFFF)))
        (vector-set! state 0 x)
        (mod (div x 65536) n)))))

(define (shuffle rng xs)
  (let ((v (list->vector xs)))
    (let loop ((i (- (vector-length v) 1)))
      (if (<= i 0)
          (vector->list v)
          (let* ((j (rng (+ i 1)))
                 (t (vector-ref v i)))
            (vector-set! v i (vector-ref v j))
            (vector-set! v j t)
            (loop (- i 1)))))))

;; ---- the corpus ------------------------------------------------------------

;; ONE SEED MAKES ONE STORE'S WORTH OF RECORDS, and it deliberately
;; contains the shapes that make a rebuild happen: a plan with members
;; under it, a member that disagrees with what the plan declared, two
;; records standing in one slot, a record that depends on a contested
;; one, and a plain record that belongs to no request at all.
(define WA "writeraa")
(define WB "writerbb")

(define (put title) (list 'put (list (cons 'kind 'section) (cons 'title title)
                                     '(parent . root) '(ord . 0))))

(define (records-for seed)
  (let* ((rng (make-rng seed))
         (after (cons WA 0))
         (id (request-identity after (string-append "req-" (number->string seed))))
         (fp (request-fingerprint "gen" 'insert (list "root" "X") after))
         (actor (lambda (sub pe) (list "gen" id sub fp pe after)))
         (plan-event (cons WA 1))
         (intent '(insert root #f ((kind . section) (title . "Wanted"))))
         (plan (list 'plan (string-append "req-" (number->string seed)) fp after
                     (list (cons 0 intent))))
         (n (+ 2 (rng 3))))
    (append
      ;; THE PLAN DEPENDS ON AN ORDINARY RECORD, which is the shape the
      ;; first corpus lacked and the one that matters most. A plan whose
      ;; own premise has not arrived is gated as waiting; the record that
      ;; releases it carries no request, so a reduction that asked about
      ;; membership only when a REQUEST record arrived never looked
      ;; again. Measured on three records, before that was fixed:
      ;;
      ;;   plan, member, ordinary  ->  ("Base")
      ;;   ordinary, plan, member  ->  ("Base" "Wanted")
      ;;
      ;; Two orders, two documents -- and delivery goes writer by writer
      ;; in name order, so an ordinary reopening can land on either.
      (list (list "premisezz" 1 '() (put "Base") "plain")
            (list WA 1 (list (cons "premisezz" 1)) plan (actor 'plan #f))
            (list WA 2 '() (put "Wanted") (actor 0 plan-event)))
      ;; a second record in the same slot, on the other writer, which is
      ;; what makes the slot contested
      (if (even? (rng 2))
          (list (list WB 1 (list (cons WA 1)) (put "Wanted") (actor 0 plan-event)))
          '())
      ;; a member that does not match what the plan declared
      (if (even? (rng 2))
          (list (list WB 2 (list (cons WA 1)) (put "Other") (actor 0 plan-event)))
          '())
      ;; RECORDS THAT BELONG TO NO REQUEST AND TO NOTHING CONTESTED.
      ;; These are what a rebuild has to carry through, and they are the
      ;; reason the retained set has to hold EVERY record rather than the
      ;; ones with a request on them. A first version of this corpus hung
      ;; every plain record off the contested slot, so none of them ever
      ;; reached the state -- and a reduction that kept only
      ;; request-bearing records passed all sixty seeds. The dimension
      ;; was missing from the stimulus, not from the claim.
      (let loop ((k 0) (out '()))
        (if (= k n)
            (reverse out)
            (loop (+ k 1)
                  (cons (list (string-append "solo" (number->string k) "00")
                              1 '() (put (string-append "S" (number->string k)))
                              "gen")
                        out))))
      ;; RECORDS THAT CHANGE AN EXISTING BLOCK RATHER THAN MAKING ONE.
      ;; The corpus was almost entirely creation and its retraction, so a
      ;; rebuild only ever had to put blocks back -- it never had to
      ;; replay a supersession, a move, a delete or an edge. Those are
      ;; where a fold that keeps the wrong records shows up as the wrong
      ;; FIELD rather than a missing block, and the hash is the same kind
      ;; of witness either way.
      (if (even? (rng 2))
          (list (list "mutatorzz" 1 (list (cons WA 2))
                      (list 'set (block-id WA 2) 'title "Renamed") "plain"))
          '())
      (if (even? (rng 2))
          (list (list "moverzzzz" 1 (list (cons WA 2))
                      (list 'move (block-id WA 2) 'root 3) "plain"))
          '())
      (if (even? (rng 2))
          (list (list "taggerzzz" 1 '()
                      (list 'tag (string-append "v" (number->string seed))
                            (list (cons WA 2)))
                      "plain"))
          '())
      ;; A RECORD THE INTERPRETER REFUSES, so the note comparison above
      ;; has something to compare. Notes come in two unrelated kinds --
      ;; what membership gated, and what the interpreter could not read --
      ;; and a build that wrote the first kind over the whole list agreed
      ;; on every hash while losing the second. Without a malformed
      ;; record in the corpus that comparison is a comparison of two
      ;; empty sets: measured, a notes-dropping build passed all sixty
      ;; seeds before this record existed.
      (if (even? (rng 2))
          (list (list "malformdz" 1 '() (list 'put (list (cons 'ord "oops"))) "plain"))
          '())
      ;; and records that DO hang off the contested slot, which must not
      ;; survive it
      (let loop ((k 0) (out '()))
        (if (= k n)
            (reverse out)
            (loop (+ k 1)
                  (cons (list (string-append "plain" (number->string k) "0")
                              1 (list (cons WA 2)) (put (string-append "P" (number->string k)))
                              "gen")
                        out)))))))

;; TAGS ARE NOT IN THE BLOCK HASH EITHER. `state-hash` covers blocks; a
;; build that lost a tag across a rebuild would agree on every hash. The
;; same reasoning that put the note sets into the comparison puts these
;; there.
(define (tag-set r)
  (list-sort (lambda (a b) (string<? (car a) (car b)))
             (map (lambda (t) (cons (if (symbol? (car t)) (symbol->string (car t)) (car t))
                                    (cdr t)))
                  (state-tags r))))

;; SORTED ON THE WHOLE NOTE, not on its kind. Two notes of one kind --
;; two malformed records -- compare equal on the kind alone, so a sort
;; keyed there leaves them in arrival order and the comparison becomes
;; the thing it was meant to be independent of. Measured: identical
;; hashes, identical note contents, and a red row.
(define (note-set r)
  (list-sort (lambda (a b)
               (string<? (call-with-string-output-port (lambda (o) (write a o)))
                         (call-with-string-output-port (lambda (o) (write b o)))))
             (reduce-noted r)))

(define (fold-records records)
  (let ((r (reduce-empty)))
    (for-each (lambda (rec) (apply reduce-apply! r rec)) records)
    r))

;; ---- the rows --------------------------------------------------------------

(printf "== the state does not depend on the order the records arrive in ==\n")
;; CANONICAL ORDER IS THE REFERENCE AND NOT THE TRUTH. Both sides of
;; this comparison go through the same reduction; what differs is only
;; the order. A retained set that drops a record gives one answer when
;; the rebuild happens and another when it does not, and the orders that
;; trigger a rebuild are exactly the ones this varies over.
(define seeds 60)
(define order-mismatches
  (let loop ((seed 1) (out '()))
    (if (> seed seeds)
        (reverse out)
        (let* ((records (records-for seed))
               (canonical (fold-records records))
               (rng (make-rng (+ 7919 seed)))
               (shuffled (fold-records (shuffle rng records))))
          (loop (+ seed 1)
                (if (and (string=? (state-hash canonical) (state-hash shuffled))
                         ;; THE NOTES ARE COMPARED BESIDE THE HASH, because
                         ;; the hash does not cover them: it hashes blocks.
                         ;; A build that dropped an integrity observation
                         ;; when an unrelated plan arrived agreed on every
                         ;; hash while `check` changed its verdict -- so a
                         ;; corpus reading hashes alone was blind to it.
                         (equal? (note-set canonical) (note-set shuffled))
                         (equal? (tag-set canonical) (tag-set shuffled)))
                    out
                    (cons (list 'seed seed
                                (list 'canonical (state-hash canonical)
                                      (note-set canonical))
                                (list 'shuffled (state-hash shuffled)
                                      (note-set shuffled)))
                          out)))))))
(want "every seed agrees with itself in a different order" order-mismatches '())

;; AND THE SAME AGAIN THROUGH A SNAPSHOT. A snapshot carries the records
;; so that a reduction resumed from it can still fold them again; a
;; snapshot that carried only the state would resume into a reduction
;; that cannot answer for what arrives next. This is that claim, run
;; over the same seeds.
(printf "\n== a reduction resumed from a snapshot answers what one from scratch does ==\n")
(define resume-mismatches
  (let loop ((seed 1) (out '()))
    (if (> seed seeds)
        (reverse out)
        (let* ((records (records-for seed))
               (rng (make-rng (+ 104729 seed)))
               (order (shuffle rng records))
               (cut (max 1 (div (length order) 2)))
               (whole (fold-records order))
               ;; RESUMING MEANS RE-DELIVERING EVERYTHING ABOVE THE
               ;; APPLIED CUT, which is what the store does and what the
               ;; snapshot's cut is for. A record that had arrived and was
               ;; still waiting for its premises sits above that cut, so
               ;; it comes again -- the snapshot does not carry the
               ;; waiting set and does not need to. A first version of
               ;; this row fed only the records the first half had not
               ;; seen, and 34 of 60 seeds disagreed: it was resuming in a
               ;; way no caller resumes.
               (resumed
                 (let* ((first (fold-records (list-head order cut)))
                        (r (rows->state (state->rows first)))
                        (applied (reduce-applied-cut r)))
                   (for-each
                     (lambda (rec)
                       (let ((e (assoc (car rec) applied)))
                         (unless (and e (>= (cdr e) (cadr rec)))
                           (apply reduce-apply! r rec))))
                     order)
                   r)))
          (loop (+ seed 1)
                (if (and (string=? (state-hash whole) (state-hash resumed))
                         (equal? (note-set whole) (note-set resumed))
                         (equal? (tag-set whole) (tag-set resumed)))
                    out
                    (cons (list 'seed seed
                                (list 'whole (state-hash whole) (note-set whole))
                                (list 'resumed (state-hash resumed) (note-set resumed)))
                          out)))))))
(want "every seed agrees across a snapshot taken half way" resume-mismatches '())

;; CONTROL: THE CORPUS REALLY CONTAINS CONTESTED SLOTS. Without this the
;; two rows above are also passed by a corpus in which nothing is ever
;; gated -- and a rebuild that never happens agrees with everything.
(printf "\n== CONTROL: the corpus exercises what it claims to ==\n")
(define gated-seeds
  (let loop ((seed 1) (n 0))
    (if (> seed seeds)
        n
        (loop (+ seed 1)
              (if (pair? (reduce-noted (fold-records (records-for seed)))) (+ n 1) n)))))
(want "CONTROL: most seeds produce a gated record"
      (> gated-seeds (div seeds 3))
      #t)
(want "CONTROL: and not every seed does, or the corpus is one shape"
      (< gated-seeds seeds)
      #t)
;; AND A CONTROL FOR THE ROWS THEMSELVES: an independent block survives
;; the rebuild. Without one of these in the corpus a reduction that kept
;; only the records carrying a request passes every seed -- measured,
;; before this line existed.
(want "CONTROL: some seeds carry a record the interpreter refuses"
      (let loop ((seed 1) (n 0))
        (if (> seed seeds)
            (and (> n 0) (< n seeds))
            (loop (+ seed 1)
                  (if (exists (lambda (x) (eq? (car x) 'malformed-record))
                              (reduce-noted (fold-records (records-for seed))))
                      (+ n 1) n))))
      #t)
(want "CONTROL: blocks that belong to no request are in the state"
      (let ((r (fold-records (records-for 1))))
        (> (length (filter (lambda (b)
                             (let ((e (assq 'title (cadr (assq 'fields (cddr b))))))
                               (and e (char=? #\S (string-ref (car (car (cadr e))) 0)))))
                           (state-datum r)))
           0))
      #t)

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "q9 complete\n")
