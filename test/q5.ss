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

;; Resolutions, the fence, and the range test a plan's missing
;; sub-operations have to pass.
;;
;; A resolution is an operator saying what the store could not work out.
;; It is the one thing that can settle an identity whose evidence cannot
;; be read, which is why it is asked BEFORE the readability of the
;; evidence: below that rule it would be useless in exactly the case it
;; was written for.
;;
;; It is also the one place in this layer where a human statement enters,
;; so most of these rows are about what a resolution may NOT do. It may
;; not un-determine an execution someone already confirmed; it may not
;; promise anything about a living writer's future; it may not speak for
;; five sub-operations when the operator examined one; and two of them
;; that do not reference each other may not be silently ranked by
;; whichever the scan reached first.

(import (chezscheme) (theourgia request))

(define bad 0)
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))

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

(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (want-1 label (caught got) (caught expect))))))

(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))


(define W "w3kxxxxx")
(define OPW "opsxxxx0")
(define WHO "agent:claude")
(define AFTER (cons W 12))
(define ID (request-identity AFTER "req-1"))
(define FP (request-fingerprint WHO 'set (list "b" "t" "x") AFTER))
(define PLAN (cons W 13))

(define (ev seq sub payload . opts)
  (let ((placement (if (pair? opts) (car opts) 'valid-history)))
    (make-evidence (cons W seq)
                   (list WHO ID sub FP (if (eq? sub 'single) #f PLAN) AFTER)
                   '() payload placement #t '())))
(define (plan-ev entries)
  (make-evidence PLAN (list WHO ID 'plan FP #f AFTER) '()
                 (list 'plan "req-1" FP AFTER entries) 'valid-history #t '()))
(define two (list (cons 0 '(set "b" "t" "x")) (cons 1 '(set "c" "t" "y"))))
(define three (append two (list (cons 2 '(set "d" "t" "z")))))

;; A resolution as the design writes it. The operator's record sits in
;; the operator's own writer with the operator's own cursor -- it is a
;; record ABOUT this identity, not a record OF it.
(define (resolve sub verdict interval . opts)
  (let ((supersedes (if (pair? opts) (car opts) '())))
    (list 'resolve ID FP sub verdict interval (cons 'supersedes supersedes))))
(define (res-ev seq r)
  (make-evidence (cons OPW seq)
                 (list "ops:carol" (cons OPW "fix-7") 'single "opsfp" #f (cons OPW (- seq 1)))
                 '() r 'valid-history #t '()))
(define CLOSED (list W 10 20))
(define OPEN (list W 10 #f))

;; `resolution-state` is asked about ONE target: an identity and one
;; sub-operation. `all` is the whole request, which is the only thing a
;; single request or an empty plan has.
(define (rstate evidence n . opts)
  (resolution-state evidence n (if (pair? opts) (car opts) 'all)))

(define (decide evidence n . opts)
  (let ((intervals (if (pair? opts) (car opts) '())))
    (request-decision ID FP WHO AFTER evidence intervals n
                      (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) '()))))

(printf "== Q13: the shape a resolution has to have ==\n")
(want "the design's shape is accepted"
      (resolution? (resolve 0 'not-executed CLOSED))
      #t)
(want "and so is one that supersedes others"
      (resolution? (resolve 'all 'executed CLOSED (list (cons OPW 5))))
      #t)
(want "everything that is not that shape is refused"
      (map resolution?
           (list (list 'resolve ID FP 0 'not-executed CLOSED)
                 (list 'resolve ID FP 0 'maybe CLOSED '(supersedes))
                 (list 'resolve ID FP -1 'not-executed CLOSED '(supersedes))
                 (list 'resolve ID FP 0 'not-executed (list W 20 10) '(supersedes))
                 (list 'resolve ID FP 0 'not-executed CLOSED '(replaces))
                 (list 'resolve ID FP 0 'not-executed CLOSED (list 'supersedes 'x))))
      '(#f #f #f #f #f #f))
;; AN OPEN INTERVAL IS A LEGAL SHAPE AND AN ILLEGAL DETERMINATION. The
;; format can express it -- `executed` may use one -- so the refusal is
;; not the shape check's job and it must not be done there.
(want "an open interval is a shape the format can express"
      (resolution? (resolve 0 'not-executed OPEN))
      #t)

(printf "\n== Q13: what an operator determined is asked first ==\n")
;; A resolution settles an identity the store cannot settle. Below rule 1
;; it would never be reached in the case it exists for.
(want "an executed resolution answers over unreadable evidence"
      (decide (list (ev 14 'single '(set "b" "t" "x") 'quarantined)
                    (res-ev 5 (resolve 'all 'executed CLOSED)))
              #f)
      (list 'resolved-executed (cons OPW 5)))
(want "TWIN: the same evidence with no resolution is unknown"
      (decide (list (ev 14 'single '(set "b" "t" "x") 'quarantined)) #f)
      (list 'unknown (list 'quarantined (cons W 14))))
;; IT DOES NOT MANUFACTURE A REPLAY. The answer to a replay carries the
;; event id and the bindings the original execution produced, and a
;; determination that something ran recovers neither.
(want "and it is never reported as a replay"
      (car (decide (list (ev 14 'single '(set "b" "t" "x"))
                         (res-ev 5 (resolve 'all 'executed CLOSED)))
                   #f))
      'resolved-executed)
;; A RESOLUTION IS A RECORD ABOUT THE IDENTITY, NOT A RECORD OF IT. Its
;; actor is the operator's, with a different who, cursor and fingerprint,
;; so leaving it among the evidence would make the mismatch rule answer
;; `req-mismatch` for every identity anyone has ever resolved.
(want "a resolution is not compared against the request it resolves"
      (decide (list (ev 14 'single '(set "b" "t" "x"))
                    (res-ev 5 (resolve 0 'not-executed CLOSED)))
              #f)
      '(replay ("w3kxxxxx" . 14)))

(printf "\n== Q13': the fence stops a retraction, not an older determination ==\n")
;; The record either exists or it does not. Once an operator has
;; determined that it does, a statement that it does not is the one that
;; must be wrong -- so a `not-executed` that NAMES the confirmation in
;; its `supersedes`, which is the only way one record here can be said to
;; come after another, is invalid and the confirmation stands.
(want "a not-executed that names the confirmation it would replace is fenced"
      (rstate (list (res-ev 5 (resolve 'all 'executed CLOSED))
                    (res-ev 9 (resolve 'all 'not-executed CLOSED (list (cons OPW 5)))))
              #f)
      (list 'executed 'all CLOSED (cons OPW 5)))
;; AND IT REACHES NO FURTHER. A `not-executed` written earlier, which
;; knows nothing of the confirmation, has retracted nothing: it is a
;; second determination nobody has reconciled. Calling it fenced would
;; let the confirmation supply, after the fact, the supersession its
;; author never wrote -- and would turn an unreconciled pair into a
;; confident answer.
(want "TWIN: two determinations that do not reference each other are unreconciled"
      (rstate (list (res-ev 5 (resolve 'all 'not-executed CLOSED))
                    (res-ev 9 (resolve 'all 'executed CLOSED)))
              #f)
      (list 'incomparable (cons OPW 5) (cons OPW 9)))
(want "TWIN: and a confirmation that names the negative stands over it"
      (rstate (list (res-ev 5 (resolve 'all 'not-executed CLOSED))
                    (res-ev 9 (resolve 'all 'executed CLOSED (list (cons OPW 5)))))
              #f)
      (list 'executed 'all CLOSED (cons OPW 9)))
;; THE FENCE IS SCOPED TO ONE SUB-OPERATION. A confirmation about 0 says
;; nothing about 1.
(want "a confirmation on one sub-operation does not fence another"
      (list (rstate (list (res-ev 5 (resolve 0 'executed CLOSED))
                          (res-ev 9 (resolve 1 'not-executed CLOSED
                                             (list (cons OPW 5)))))
                    2 1)
            (rstate (list (res-ev 5 (resolve 0 'executed CLOSED))
                          (res-ev 9 (resolve 1 'not-executed CLOSED)))
                    2 0))
      (list (list 'not-executed 1 CLOSED (cons OPW 9))
            (list 'executed 0 CLOSED (cons OPW 5))))

(printf "\n== Q13': not-executed accepts closed intervals only ==\n")
;; The exemption says "this sub-operation did not run in this stretch".
;; A stretch with no top is a promise about a living writer's FUTURE, and
;; a true observation of the past cannot stand in for that.
(want "an open interval makes a not-executed invalid"
      (rstate (list (res-ev 9 (resolve 'all 'not-executed OPEN))) #f)
      (list 'invalid (list 'resolve-open-interval (cons OPW 9))))
(want "TWIN: executed may use an open interval"
      (rstate (list (res-ev 9 (resolve 'all 'executed OPEN))) #f)
      (list 'executed 'all OPEN (cons OPW 9)))
;; AN INVALID RESOLUTION IS RECORDED, NOT OBEYED, and it does NOT make
;; the identity unknown -- otherwise an operator could freeze a target
;; for ever by writing one sentence the format refuses.
(want "and the decision proceeds as though it had not been written"
      (decide (list (res-ev 9 (resolve 'all 'not-executed OPEN))) #f
              (list (list W 10 20)))
      (list 'unknown (list 'range-overlaps (list W 10 20))))
;; BUT THE STORE STILL HAS TO RECORD IT. A set holding one valid
;; resolution and one invalid one reports the valid one, and the invalid
;; one would otherwise be lost entirely -- so the reasons are asked for
;; separately from what stands.
(want "every invalid resolution is reported with its reason, valid ones beside it"
      (resolution-problems (list (res-ev 9 (resolve 'all 'not-executed OPEN))
                                 (res-ev 11 (resolve 0 'not-executed CLOSED)))
                           #f)
      (list (list 'resolve-open-interval (cons OPW 9))))
(want "TWIN: and nothing is reported when every one of them is valid"
      (resolution-problems (list (res-ev 11 (resolve 0 'not-executed CLOSED))) #f)
      '())

(printf "\n== Q13': one identity, one sub-operation ==\n")
;; `all` is a scope, not a shortcut: it is legal only where there is one
;; sub-operation to speak for. Over a plan of two it would be one
;; sentence standing in for two determinations the operator made once.
(want "all over a plan of two is out of scope"
      (rstate (list (res-ev 9 (resolve 'all 'not-executed CLOSED))) 2)
      (list 'invalid (list 'resolve-scope (cons OPW 9))))
(want "TWIN: all over a single request, and over an empty plan"
      (list (rstate (list (res-ev 9 (resolve 'all 'not-executed CLOSED))) #f)
            (rstate (list (res-ev 9 (resolve 'all 'not-executed CLOSED))) 0))
      (list (list 'not-executed 'all CLOSED (cons OPW 9))
            (list 'not-executed 'all CLOSED (cons OPW 9))))
(want "TWIN: a numbered sub-operation over a plan is in scope"
      (rstate (list (res-ev 9 (resolve 1 'not-executed CLOSED))) 2 1)
      (list 'not-executed 1 CLOSED (cons OPW 9)))
;; AND TWO DETERMINATIONS ABOUT DIFFERENT SUB-OPERATIONS HAVE NOT
;; CONTRADICTED EACH OTHER. A request-wide reading would report them as
;; an unreconciled pair and freeze both.
(want "two negatives on different sub-operations each stand"
      (let ((es (list (res-ev 5 (resolve 0 'not-executed CLOSED))
                      (res-ev 9 (resolve 1 'not-executed CLOSED)))))
        (list (rstate es 2 0) (rstate es 2 1)))
      (list (list 'not-executed 0 CLOSED (cons OPW 5))
            (list 'not-executed 1 CLOSED (cons OPW 9))))

(printf "\n== Q13': a marked resolution is not eligible ==\n")
;; Eligibility and order are different questions. `superseded` says this
;; record does not speak for this target at all; `supersedes` ranks the
;; records that do. And a marked one is worse here than anywhere else:
;; resolutions are taken out of the evidence before the readability rule
;; runs, so nothing downstream would ever see the mark -- while its
;; exemption would clear the very range test standing between a
;; completion and a second execution.
(define (marked-res seq r)
  (make-evidence (cons OPW seq)
                 (list "ops:carol" (cons OPW "fix-7") 'single "opsfp" #f (cons OPW (- seq 1)))
                 '() r 'valid-history #t '(superseded)))
(want "a marked not-executed does not clear the range test"
      (decide (list (plan-ev two) (ev 14 0 '(set "b" "t" "x"))
                    (marked-res 9 (resolve 1 'not-executed (list W 14 30))))
              2 (list (list W 14 30)))
      (list 'unknown (list 'range-overlaps (list W 14 30))))
(want "TWIN: the same resolution without the mark does"
      (decide (list (plan-ev two) (ev 14 0 '(set "b" "t" "x"))
                    (res-ev 9 (resolve 1 'not-executed (list W 14 30))))
              2 (list (list W 14 30)))
      '(complete (0)))

(printf "\n== Q13': two that do not reference each other are not a tie ==\n")
;; Choosing either would make the answer depend on which the scan reached
;; first. They sit on different writers as often as not, where the
;; sequence numbers do not compare at all.
(want "neither supersedes the other"
      (rstate (list (res-ev 5 (resolve 0 'not-executed CLOSED))
                    (res-ev 9 (resolve 0 'not-executed (list W 30 40))))
              2 0)
      (list 'incomparable (cons OPW 5) (cons OPW 9)))
(want "and the identity is unknown while that stands"
      (decide (list (plan-ev two) (ev 14 0 '(set "b" "t" "x"))
                    (res-ev 5 (resolve 1 'not-executed CLOSED))
                    (res-ev 9 (resolve 1 'not-executed (list W 30 40))))
              2 (list (list W 14 30)))
      (list 'unknown (list 'resolve-incomparable (cons OPW 5) (cons OPW 9))))
(want "one that names the other stands alone"
      (rstate (list (res-ev 5 (resolve 0 'not-executed CLOSED))
                    (res-ev 9 (resolve 0 'not-executed CLOSED (list (cons OPW 5)))))
              2 0)
      (list 'not-executed 0 CLOSED (cons OPW 9)))
(want "a third that names both settles it"
      (rstate (list (res-ev 5 (resolve 0 'not-executed CLOSED))
                    (res-ev 9 (resolve 0 'not-executed CLOSED))
                    (res-ev 11 (resolve 0 'not-executed CLOSED
                                        (list (cons OPW 5) (cons OPW 9)))))
              2 0)
      (list 'not-executed 0 CLOSED (cons OPW 11)))
;; A CYCLE IS NOT AN ORDER, and it leaves nothing standing rather than
;; whichever the loop happened to visit last.
(want "two that name each other leave nothing standing"
      (rstate (list (res-ev 5 (resolve 0 'not-executed CLOSED (list (cons OPW 9))))
                    (res-ev 9 (resolve 0 'not-executed CLOSED (list (cons OPW 5)))))
              2 0)
      '(incomparable))
;; ONE RECORD STANDING IS NOT THE SAME AS ONE STORY. The pair above name
;; only each other, so nothing outside the cycle names them and an
;; unrelated third record stands alone -- over a set it never reconciled.
;; What has to be true is that the standing record reaches every other
;; valid one through `supersedes`.
(want "a cycle beside an unrelated resolution is still unreconciled"
      (rstate (list (res-ev 5 (resolve 0 'not-executed CLOSED (list (cons OPW 9))))
                    (res-ev 9 (resolve 0 'not-executed CLOSED (list (cons OPW 5))))
                    (res-ev 11 (resolve 0 'not-executed (list W 30 40))))
              2 0)
      (list 'incomparable (cons OPW 5) (cons OPW 9) (cons OPW 11)))

(printf "\n== Q13: the exemption is contained, one way, and scoped ==\n")
;; "This sub-operation did not run anywhere in [10,20]" already covers
;; [12,15], so clearing the smaller stretch narrows the exemption rather
;; than widening the determination. The other direction is not available:
;; an operator who examined [10,20] has said nothing about [10,40].
(want "a stretch inside the determined one is cleared"
      (decide (list (res-ev 9 (resolve 'all 'not-executed CLOSED))) #f
              (list (list W 12 15)))
      '(execute))
(want "and the determined stretch itself is cleared"
      (decide (list (res-ev 9 (resolve 'all 'not-executed CLOSED))) #f
              (list CLOSED))
      '(execute))
(want "TWIN: a stretch reaching past it is not"
      (decide (list (res-ev 9 (resolve 'all 'not-executed CLOSED))) #f
              (list (list W 10 40)))
      (list 'unknown (list 'range-overlaps (list W 10 40))))
;; A STRETCH WITH NO TOP IS NEVER CLEARED. It is contained in nothing
;; closed, which is the same rule that makes an open interval illegal in
;; the resolution itself.
(want "TWIN: and a stretch with no top is never cleared"
      (decide (list (res-ev 9 (resolve 'all 'not-executed CLOSED))) #f
              (list (list W 10 #f)))
      (list 'unknown (list 'range-overlaps (list W 10 #f))))
;; AND IT IS SCOPED TO ITS SUB-OPERATION. Rule 2 asks about the whole
;; request, which only `all` speaks for.
(want "TWIN: a resolution for sub-operation 0 does not clear the whole request"
      (decide (list (res-ev 9 (resolve 0 'not-executed CLOSED))) #f
              (list CLOSED))
      (list 'unknown (list 'range-overlaps CLOSED)))

(printf "\n== Q16: a plan's missing sub-operations get the range test too ==\n")
;; Completing a plan writes records at indices believed empty, so "empty"
;; has to mean more than "I did not find anything". Their positions begin
;; after the last record this execution left -- the highest present index
;; -- and run upwards without a bound.
(want "a hole under an uncertain stretch is not a completion"
      (decide (list (plan-ev three) (ev 14 0 '(set "b" "t" "x"))) 3
              (list (list W 14 30)))
      (list 'unknown (list 'range-overlaps (list W 14 30))))
(want "TWIN: a stretch that ends below the last present record is clear"
      (decide (list (plan-ev three) (ev 14 0 '(set "b" "t" "x"))) 3
              (list (list W 2 13)))
      '(complete (0)))
;; AND THE EXEMPTION REACHES HERE, per missing index.
(want "a not-executed for the missing index clears it"
      (decide (list (plan-ev two) (ev 14 0 '(set "b" "t" "x"))
                    (res-ev 9 (resolve 1 'not-executed (list W 14 30))))
              2 (list (list W 14 30)))
      '(complete (0)))
(want "TWIN: the same resolution naming a different index does not"
      (decide (list (plan-ev three) (ev 14 0 '(set "b" "t" "x"))
                    (res-ev 9 (resolve 2 'not-executed (list W 14 30))))
              3 (list (list W 14 30)))
      (list 'unknown (list 'range-overlaps (list W 14 30))))
;; AN OPERATOR WHO CONFIRMED THAT A MISSING SUB-OPERATION RAN HAS SAID
;; THE ONE THING COMPLETION CANNOT ACT ON. Writing it again would be a
;; second execution; skipping it is not available either, because the
;; confirmation carries no event id and no bindings to put in its place.
(want "a confirmation on a missing sub-operation settles the request"
      (decide (list (plan-ev two) (ev 14 0 '(set "b" "t" "x"))
                    (res-ev 9 (resolve 1 'executed (list W 14 30))))
              2 '())
      (list 'resolved-executed (list 'sub 1) (list 'event (cons OPW 9))))
;; AND THE PLAN WITH NOTHING APPLIED IS THE CASE THE CURSOR IS EASIEST TO
;; LOSE IN. The plan record's own actor names no plan-event -- only the
;; sub-operations point back at it -- so a cursor read out of an actor
;; slot is #f exactly when there are no sub-operations, every missing
;; index skips its range test in silence, and a plan with nothing applied
;; is told to go ahead and apply all of it.
(want "a plan with no sub-operations yet, under an uncertain stretch"
      (decide (list (plan-ev two)) 2 (list (list W 13 30)))
      (list 'unknown (list 'range-overlaps (list W 13 30))))
(want "TWIN: the same plan with the stretch ending below its own event"
      (decide (list (plan-ev two)) 2 (list (list W 2 13)))
      '(complete ()))

;; AND AN ANCHOR THAT CANNOT BE RECOVERED IS NOT "NO OVERLAP". Here the
;; plan is declared by the request and the only record present is a
;; single one that belongs to no plan -- so there is neither a present
;; sub-operation nor a plan record to measure from. Answering `complete`
;; on the strength of having nothing to measure is the failure this whole
;; rule exists to prevent.
(want "a plan whose anchor is nowhere to be found"
      (decide (list (ev 14 'single '(set "b" "t" "x"))) 2 (list (list W 2 13)))
      '(unknown (no-cursor)))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "q5 complete\n")
