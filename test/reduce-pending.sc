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

;; THE REDUCTION'S PENDING QUEUE (reduce.sc, make-pending-queue).
;;
;; A record whose requirements have not all been applied waits. The log
;; delivers whole writers in name order, so a writer whose records depend on
;; a later-named writer has every record wait; releasing them used to cost a
;; pass over every waiting record per application. The queue files each
;; record under the one requirement it waits for, wakes it when that is
;; applied (or its gate lifted), and keeps the applicable records in a heap,
;; so the release costs the queue's bookkeeping.
;;
;; The rows pin two things. The COUNTS: pending-work-counter, a parameter a
;; fixture binds in process, is called with a kind and a count -- examine
;; (a record classified), scan (entries walked by a scan of the queue or of a
;; bucket's gates) and heap (a comparison in the ready heap); doubling the
;; waiting records adds a linear amount of each. A tree without the seam reads
;; #f there. And the ANSWERS, which the queue must not change: the order
;; records apply in (the trace), the already-pending refusal, the arrival
;; order reduce-pending answers, the rebuild's result, the clone's
;; independence, the count state-dump prints.
;;
;; Everything runs in process, through reduce-apply!, as reduce1 does.

(import (chezscheme) (theourgia reduce))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e)) (condition-message e) e)
                         (if (and (condition? e) (irritants-condition? e)) (condition-irritants e) '()))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) x)))))

;; ---- the instruments ---------------------------------------------------------

;; THE SEAM, found by name: a tree without it reads #f, and the count rows
;; are red there rather than the file failing to load.
(define counter (guard (e (#t #f)) (eval 'pending-work-counter (environment '(theourgia reduce)))))
;; -> (<examine> <scan> <heap>) counted while THUNK runs, or #f without the
;; seam; THUNK runs either way, so the rows beside a count still read a tree
;; without it
(define (work-of thunk)
  (if counter
      (let ((t (make-eq-hashtable)))
        (parameterize ((counter (lambda (kind n) (hashtable-update! t kind (lambda (v) (+ v n)) 0))))
          (thunk))
        (map (lambda (k) (hashtable-ref t k 0)) '(examine scan heap)))
      (begin (thunk) #f)))
(define (ms-of thunk)
  (let ((t0 (current-time 'time-monotonic)))
    (thunk)
    (let ((d (time-difference (current-time 'time-monotonic) t0)))
      (+ (* 1000 (time-second d)) (quotient (time-nanosecond d) 1000000)))))
;; -> #t when each of AT-2N minus AT-N is within the bound beside it
(define (within? at-n at-2n bounds)
  (and at-n at-2n (for-all (lambda (a b k) (<= (- b a) k)) at-n at-2n bounds)))
(define (log2-ceiling n) (let loop ((k 0) (p 1)) (if (>= p n) k (loop (+ k 1) (* p 2)))))

;; ---- the records ---------------------------------------------------------------

(define (section) '(put ((kind . section))))
(define (titled block title) (list 'set block 'title title))
(define (summary block text) (list 'set block 'summary text))
(define (events r) (reduce-trace r))
(define (pending-events r) (map (lambda (x) (cons (car x) (cadr x))) (reduce-pending r)))
(define (dump-pending r) (let ((p (assq 'pending (state-dump r)))) (and p (cdr p))))
(define (pad i width)
  (let ((s (number->string i))) (string-append (make-string (max 0 (- width (string-length s))) #\0) s)))

;; A plan of one insert, recorded as ORIGIN's 1, and its member: the shapes
;; test/admission-index.sc uses. A member delivered before its plan is gated
;; (pending-plan) until the plan arrives.
(define (intent title) (list 'insert 'root #f (list (cons 'kind 'section) (cons 'title title))))
(define (produced title)
  (list 'put (list (cons 'kind 'section) (cons 'title title) '(parent . root) '(ord . 1))))
(define (plan-actor origin req) (list "test" (cons origin req) 'plan "fp" #f (cons origin 0)))
(define (member-actor origin req i) (list "test" (cons origin req) i "fp" (cons origin 1) (cons origin 0)))
(define (plan! r origin req)
  (reduce-apply! r origin 1 '() (list 'plan req "fp" (cons origin 0) (list (cons 0 (intent "t0"))))
                 (plan-actor origin req)))
(define (member! r w seq deps origin req)
  (reduce-apply! r w seq deps (produced "t0") (member-actor origin req 0)))
;; Two records claiming one single request gate each other; when the first
;; was already applied, the second reverses it and the reduction is rebuilt
;; (test/relation-effects.sc's shape).
(define (claim-actor) (list "test" (cons "other000" "s") 'single "fp" #f (cons "other000" 0)))
(define (claims! r)
  (reduce-apply! r "claim000" 1 '() (produced "c") (claim-actor))
  (reduce-apply! r "claim001" 1 '() (produced "d") (claim-actor)))
;; An empty plan that consumes a version: applied, it completes at once
;; (test/consumption-index.sc CI-03).
(define (consuming-plan! r origin)
  (reduce-apply! r origin 1 '()
                 (list 'plan "p" "fp" (cons origin 0) '() (list 'consumes "dw" (list (list "dw.1" "v0" "h0" '()))))
                 (list "test" (cons origin "p") 'plan "fp" #f (cons origin 0))))

;; THE PEER SHAPE: writer "m0000000" writes a chain of N, and the peer writes
;; N records, its k-th depending on the chain's k-th. PEER-FIRST delivers the
;; peer whole before the chain (a peer whose name sorts first: every peer
;; record waits); PEER-LAST the chain first (nothing waits).
(define (peer-chain! r n peer-first?)
  (define (chain k) (reduce-apply! r "m0000000" k '() (if (= k 1) (section) (titled "m0000000.1" (format "m~a" k)))))
  (define (peer k) (reduce-apply! r "p0000000" k (list (cons "m0000000" k)) (summary "m0000000.1" (format "p~a" k))))
  (define (each f) (do ((k 1 (+ k 1))) ((> k n)) (f k)))
  (if peer-first? (begin (each peer) (each chain)) (begin (each chain) (each peer)))
  r)

;; THE FAN-OUT: a root and N dependents, each its own writer, all delivered
;; before the root, so one application makes N records ready at once.
;; INCREASING delivers the dependents in writer order (a sorted list inserted
;; in that order walks to its tail each time); otherwise in reverse.
(define (fan-out! r n increasing?)
  (let ((ws (map (lambda (i) (string-append "d" (pad i 7))) (iota n))))
    (for-each (lambda (w) (reduce-apply! r w 1 '(("r0000000" . 1)) (section)))
              (if increasing? ws (reverse ws)))
    (reduce-apply! r "r0000000" 1 '() (section))
    r))

;; ---- PEND-1 the count ----------------------------------------------------------

(define N 500)
(printf "== PEND-1: releasing the waiting records costs the queue's bookkeeping ==\n")
;; EXAMINATIONS: a peer record is examined at its arrival (filed under its
;; predecessor, or its dep for the first), when its predecessor is applied
;; (filed under its dep) and when its dep is applied (ready): three; a chain
;; record once. Four per pair, so 4N for N more pairs. SCAN: no record here
;; has a request identity, so no bucket's gates are scanned: the bound is the
;; brief's 3N. HEAP: the ready heap never holds two records here.
(let* ((ms-n 0) (ms-2n 0)
       (at-n (work-of (lambda () (set! ms-n (ms-of (lambda () (peer-chain! (reduce-empty) N #t)))))))
       (at-2n (work-of (lambda () (set! ms-2n (ms-of (lambda () (peer-chain! (reduce-empty) (* 2 N) #t))))))))
  (printf "NOTE PEND-1 peer first, ms at ~a and ~a records a side: ~a ~a; work (examine scan heap) ~s ~s\n"
          N (* 2 N) ms-n ms-2n at-n at-2n)
  (want (format "PEND-1 peer first: N = ~a more waiting pairs add at most 4N examinations, 3N scanned entries, 3N heap comparisons" N)
        (within? at-n at-2n (list (* 4 N) (* 3 N) (* 3 N)))
        #t))
(let* ((at-n (work-of (lambda () (peer-chain! (reduce-empty) N #f))))
       (at-2n (work-of (lambda () (peer-chain! (reduce-empty) (* 2 N) #f)))))
  (printf "NOTE PEND-1 peer last, work ~s ~s\n" at-n at-2n)
  (want (format "PEND-1 peer last: N = ~a more pairs add at most 4N, 3N, 3N" N)
        (within? at-n at-2n (list (* 4 N) (* 3 N) (* 3 N)))
        #t))

(printf "== PEND-1b: one application readies N records ==\n")
;; Each record is pushed once and popped once: a push compares at most once
;; per level, a pop twice, so 3 log2(2N) per record bounds the comparisons.
;; A dependent is examined at its arrival and at the root's application, and
;; a waiter's bucket is never walked: 4N and 3N bound the other two kinds.
(for-each
  (lambda (increasing?)
    (let* ((n-r (reduce-empty)) (n2-r (reduce-empty))
           (at-n (work-of (lambda () (fan-out! n-r N increasing?))))
           (at-2n (work-of (lambda () (fan-out! n2-r (* 2 N) increasing?)))))
      (printf "NOTE PEND-1b ~a, work ~s ~s\n" (if increasing? "increasing" "decreasing") at-n at-2n)
      (want (format "PEND-1b ~a writer order: N = ~a more dependents within 4N, 3N, 3N log2 2N, and the trace in writer order"
                    (if increasing? "increasing" "decreasing") N)
            (list (within? at-n at-2n (list (* 4 N) (* 3 N) (* 3 N (log2-ceiling (* 2 N)))))
                  (equal? (events n-r)
                          (cons '("r0000000" . 1) (map (lambda (i) (cons (string-append "d" (pad i 7)) 1)) (iota N)))))
            '(#t #t))))
  '(#t #f))

;; THE GATES' SIDE: N members, each of its own request whose plan never
;; arrives, so each is gated in its own bucket and the gate set grows to N.
;; Each arrival touches one bucket, whose own gates are compared before and
;; after its judgement: one scanned entry per arrival. A scan of the whole
;; gate set per arrival would be N^2/2.
(define (gated-many! r n)
  (do ((i 0 (+ i 1))) ((= i n) r)
    (reduce-apply! r (string-append "g" (pad i 7)) 1 '() (produced "t0") (member-actor (string-append "o" (pad i 7)) "q" 0))))
(let* ((at-n (work-of (lambda () (gated-many! (reduce-empty) N))))
       (at-2n (work-of (lambda () (gated-many! (reduce-empty) (* 2 N))))))
  (printf "NOTE PEND-1c gated, work ~s ~s\n" at-n at-2n)
  (want (format "PEND-1c N = ~a more gated members in their own buckets add at most 4N, 3N, 3N" N)
        (within? at-n at-2n (list (* 4 N) (* 3 N) (* 3 N)))
        #t))

;; ---- PEND-2 the order ----------------------------------------------------------

(printf "== PEND-2: the order records apply in ==\n")
;; test/reduce1.sc R3: b1 and d1 wait on z1, a1 on b1. A scheduler taking the
;; whole ready set at once answers z1 b1 d1 a1; a FIFO one differs too.
(want "PEND-2 a record made ready by an application is taken before one already waiting"
      (let ((r (reduce-empty)))
        (reduce-apply! r "b" 1 '(("z" . 1)) '(set "z.1" title "b"))
        (reduce-apply! r "d" 1 '(("z" . 1)) '(set "z.1" summary "d"))
        (reduce-apply! r "a" 1 '(("b" . 1)) '(set "z.1" note "a"))
        (reduce-apply! r "z" 1 '() '(put ((kind . section))))
        (events r))
      '(("z" . 1) ("b" . 1) ("a" . 1) ("d" . 1)))
;; A PLAN'S MEMBER delivered before the plan is gated, and depends on it; the
;; plan's arrival lifts the gate in place.
(want "PEND-2 a member gated until its plan, and a record depending on the plan: plan, member, record"
      (let ((r (reduce-empty)))
        (member! r "b0000000" 1 '(("a0000000" . 1)) "a0000000" "q")
        (reduce-apply! r "c0000000" 1 '(("a0000000" . 1)) (section))
        (plan! r "a0000000" "q")
        (list (events r) (reduce-gates r)))
      '((("a0000000" . 1) ("b0000000" . 1) ("c0000000" . 1)) ()))
;; FILING READS THE CURRENT STATE: a record woken by one requirement reads all
;; of them again, so a requirement met before (or while) it waited is seen.
(want "PEND-2 c1, then a1 depending on b1 and c1, then b1: a1 applies at b1"
      (let ((r (reduce-empty)))
        (reduce-apply! r "c" 1 '() (section))
        (reduce-apply! r "a" 1 '(("b" . 1) ("c" . 1)) (section))
        (reduce-apply! r "b" 1 '() (section))
        (events r))
      '(("c" . 1) ("b" . 1) ("a" . 1)))
(want "PEND-2 a1 depending on b1 and c1, then b1: a1 still waits, for c1; then c1: b1 c1 a1"
      (let ((r (reduce-empty)))
        (reduce-apply! r "a" 1 '(("b" . 1) ("c" . 1)) (section))
        (reduce-apply! r "b" 1 '() (section))
        (let ((mid (list (events r) (pending-events r))))
          (reduce-apply! r "c" 1 '() (section))
          (list mid (events r))))
      '(((("b" . 1)) (("a" . 1))) (("b" . 1) ("c" . 1) ("a" . 1))))
(want "PEND-2 a1 depending on b1 and c1, b1 depending on c1, then c1: c1 b1 a1"
      (let ((r (reduce-empty)))
        (reduce-apply! r "a" 1 '(("b" . 1) ("c" . 1)) (section))
        (reduce-apply! r "b" 1 '(("c" . 1)) (section))
        (reduce-apply! r "c" 1 '() (section))
        (events r))
      '(("c" . 1) ("b" . 1) ("a" . 1)))

;; ---- PEND-3 both delivery orders -----------------------------------------------

(printf "== PEND-3: both delivery orders ==\n")
(want "PEND-3 peer first and peer last: each order's own trace, one state, nothing pending"
      (let ((first (peer-chain! (reduce-empty) 3 #t)) (last (peer-chain! (reduce-empty) 3 #f)))
        (list (events first) (events last)
              (equal? (state-hash first) (state-hash last))
              (dump-pending first) (dump-pending last)))
      '((("m0000000" . 1) ("p0000000" . 1) ("m0000000" . 2) ("p0000000" . 2) ("m0000000" . 3) ("p0000000" . 3))
        (("m0000000" . 1) ("m0000000" . 2) ("m0000000" . 3) ("p0000000" . 1) ("p0000000" . 2) ("p0000000" . 3))
        #t 0 0))

;; ---- PEND-4 already pending ----------------------------------------------------

(printf "== PEND-4: a record delivered again ==\n")
(want "PEND-4 redelivered while pending: refused; applied, then redelivered: already applied"
      (let* ((r (reduce-empty))
             (a (reduce-apply! r "x" 2 '() (section)))
             (b (reduce-apply! r "x" 2 '() (section)))
             (c (reduce-apply! r "x" 1 '() (section)))
             (d (reduce-apply! r "x" 2 '() (section))))
        (list a b c d (events r) (dump-pending r)))
      '(accepted (refused already-pending) accepted (refused already-applied) (("x" . 1) ("x" . 2)) 0))

;; ---- PEND-5 gates --------------------------------------------------------------

(printf "== PEND-5: gated records ==\n")
(want "PEND-5 a gated member waits through 50 applications; it is never applied"
      (let ((r (reduce-empty)))
        (member! r "g0000000" 1 '() "planless" "q")
        (do ((k 1 (+ k 1))) ((> k 50)) (reduce-apply! r "h0000000" k '() (if (= k 1) (section) (titled "h0000000.1" (format "h~a" k)))))
        (list (length (events r)) (pending-events r) (dump-pending r)
              (and (assoc '("g0000000" . 1) (reduce-gates r)) #t)))
      '(50 (("g0000000" . 1)) 1 #t))
(want "PEND-5 a gate lifted in place: members before their plan, from the plan's writer, wait and the plan wakes them"
      (let ((r (reduce-empty)))
        (member! r "o0000000" 2 '() "o0000000" "q")
        (let ((before (list (pending-events r) (and (assoc '("o0000000" . 2) (reduce-gates r)) #t))))
          (plan! r "o0000000" "q")
          (list before (events r) (reduce-gates r) (dump-pending r))))
      '(((("o0000000" . 2)) #t) (("o0000000" . 1) ("o0000000" . 2)) () 0))
;; THE REBUILD: two records b1 then a1 survive it, and the two claims gate
;; each other. One drain after the refill applies a1 before b1; a drain per
;; refilled record applies them in the history's order, b1 a1.
(want "PEND-5 a rebuild with two survivors: trace a1 b1, the claims pending, the consumption and the notes rebuilt"
      (let ((r (reduce-empty)))
        (reduce-apply! r "b0000000" 1 '() (section))
        (consuming-plan! r "a0000000")
        (claims! r)
        (list (events r) (pending-events r) (dump-pending r)
              (state-consumed? r "dw" "v0")
              (filter (lambda (n) (eq? (car n) 'plan-conflict)) (reduce-noted r))))
      '((("a0000000" . 1) ("b0000000" . 1)) (("claim000" . 1) ("claim001" . 1)) 2 #t
        ((plan-conflict (event "claim000" 1)) (plan-conflict (event "claim001" 1)))))

;; ---- PEND-6 arrival order ------------------------------------------------------

(printf "== PEND-6: the order reduce-pending answers ==\n")
;; THREE CLASSES: waiting on a predecessor, gated, waiting on a dep.
(define (three-classes!)
  (let ((r (reduce-empty)))
    (reduce-apply! r "x0000000" 2 '() (section))
    (member! r "g0000000" 1 '() "planless" "q")
    (reduce-apply! r "y0000000" 1 '(("q0000000" . 1)) (section))
    r))
(want "PEND-6 waiting records of three classes answer in arrival order, each the four fields"
      (reduce-pending (three-classes!))
      (list (list "x0000000" 2 '() (section)) (list "g0000000" 1 '() (produced "t0"))
            (list "y0000000" 1 '(("q0000000" . 1)) (section))))
(want "PEND-6 after a rebuild: the history's order"
      (let ((r (three-classes!)))
        (claims! r)
        (pending-events r))
      '(("x0000000" . 2) ("g0000000" . 1) ("y0000000" . 1) ("claim000" . 1) ("claim001" . 1)))

;; ---- PEND-7 the clone ----------------------------------------------------------

(printf "== PEND-7: a clone's queue is its own ==\n")
;; The clone's arrivals continue the original's numbering; it wakes and
;; rebuilds its queue; the original's waiter is still there and wakes in it.
(want "PEND-7 the clone wakes and rebuilds; the original still holds x2, and x1 then applies x2 there"
      (let ((r (reduce-empty)))
        (reduce-apply! r "x0000000" 2 '() (section))
        (reduce-apply! r "y0000000" 2 '() (section))
        (let ((c (reduce-clone r)))
          (reduce-apply! c "z0000000" 2 '() (section))
          (let ((clone-pending (pending-events c)))
            (reduce-apply! c "x0000000" 1 '() (section))
            (let ((clone-trace (events c)))
              (claims! c)
              (let ((original-pending (pending-events r)))
                (reduce-apply! r "x0000000" 1 '() (section))
                (list clone-pending clone-trace (reduce-rehearsal? c) original-pending (events r) (pending-events r)))))))
      '((("x0000000" . 2) ("y0000000" . 2) ("z0000000" . 2))
        (("x0000000" . 1) ("x0000000" . 2))
        #t
        (("x0000000" . 2) ("y0000000" . 2))
        (("x0000000" . 1) ("x0000000" . 2))
        (("y0000000" . 2))))

;; THE GATED RECORDS ARE THE CLONE'S OWN: a gate lifted in the clone leaves
;; the original's gated record in place, and the original's own lift wakes it.
(want "PEND-7 a gate lifted in the clone; the original's member still gated, and its plan then applies it there"
      (let ((r (reduce-empty)))
        (member! r "m0000000" 1 '() "o0000000" "q")
        (let ((c (reduce-clone r)))
          (plan! c "o0000000" "q")
          (let ((clone-trace (events c)) (original-pending (pending-events r)))
            (plan! r "o0000000" "q")
            (list clone-trace original-pending (events r) (dump-pending r)))))
      '((("m0000000" . 1) ("o0000000" . 1)) (("m0000000" . 1)) (("m0000000" . 1) ("o0000000" . 1)) 0))

;; ---- PEND-8 boundaries ---------------------------------------------------------

(printf "== PEND-8: boundaries ==\n")
(want "PEND-8 a dep that never arrives: the record stays pending, nothing else is blocked, the dump counts it"
      (let ((r (reduce-empty)))
        (reduce-apply! r "r0000000" 1 '(("ghost000" . 1)) (section))
        (reduce-apply! r "s0000000" 1 '() (section))
        (list (events r) (pending-events r) (dump-pending r)))
      '((("s0000000" . 1)) (("r0000000" . 1)) 1))
(want "PEND-8 a dep (w . 0) is met by w's first event"
      (let ((r (reduce-empty)))
        (reduce-apply! r "y0000000" 1 '(("w0000000" . 0)) (section))
        (let ((before (pending-events r)))
          (reduce-apply! r "w0000000" 1 '() (section))
          (list before (events r))))
      '((("y0000000" . 1)) (("w0000000" . 1) ("y0000000" . 1))))
(want "PEND-8 a writer's seq 1 with no deps applies at once"
      (let ((r (reduce-empty)))
        (reduce-apply! r "x0000000" 1 '() (section))
        (list (events r) (dump-pending r)))
      '((("x0000000" . 1)) 0))
(want "PEND-8 x2 without x1 waits; x1 applies both"
      (let ((r (reduce-empty)))
        (reduce-apply! r "x0000000" 2 '() (section))
        (let ((before (pending-events r)))
          (reduce-apply! r "x0000000" 1 '() (section))
          (list before (events r))))
      '((("x0000000" . 2)) (("x0000000" . 1) ("x0000000" . 2))))

;; A CURSOR MOVED BACK: x0 needs only that x has an applied event, and sorts
;; before x2; applying it after x1 sets x's cursor to 0, so x2's predecessor
;; is no longer reached and x2 stays pending, as the whole-list drain left it.
(want "PEND-8 x0, x2, x1: x1 then x0 apply, and x2, its predecessor no longer reached, stays pending"
      (let ((r (reduce-empty)))
        (reduce-apply! r "x" 0 '() (section))
        (reduce-apply! r "x" 2 '() (section))
        (reduce-apply! r "x" 1 '() (section))
        (list (events r) (pending-events r) (dump-pending r)))
      '((("x" . 1) ("x" . 0)) (("x" . 2)) 1))

;; THE SAME EVENT PENDING TWICE: x0 applied after x1 moves x's cursor back, so
;; x1 is accepted again, and waits; the rebuild files both copies of x1, and
;; the one that applies leaves the waiting one pending.
(want "PEND-8 x1 delivered again after x0 moved the cursor back, then a rebuild: the waiting copy stays pending, and is refused again"
      (let ((r (reduce-empty)))
        (reduce-apply! r "x" 0 '() (section))
        (reduce-apply! r "x" 1 '() (section))
        (reduce-apply! r "x" 1 '(("ghost000" . 1)) (section))
        (claims! r)
        (list (events r) (pending-events r) (dump-pending r) (reduce-apply! r "x" 1 '(("ghost000" . 1)) (section))))
      '((("x" . 1) ("x" . 0)) (("x" . 1) ("claim000" . 1) ("claim001" . 1)) 3 (refused already-pending)))

;; OUTSIDE THE LOG'S ENVELOPE: wire.sc refuses a seq or a dep that is not an
;; exact integer >= 0, so only an in-process caller delivers these; the queue
;; still answers what the whole list answered.
(want "PEND-8 deps 1.0 and 3/2: met by w1 and by w2"
      (let ((r (reduce-empty)))
        (reduce-apply! r "a" 1 '(("w" . 1.0)) (section))
        (reduce-apply! r "b" 1 '(("w" . 3/2)) (section))
        (reduce-apply! r "w" 1 '() (section))
        (reduce-apply! r "w" 2 '() (section))
        (events r))
      '(("w" . 1) ("a" . 1) ("w" . 2) ("b" . 1)))
(want "PEND-8 negative seqs: w-2, w-1, z1 depending on (w . -1), then w1"
      (let ((r (reduce-empty)))
        (reduce-apply! r "w" -2 '() (section))
        (reduce-apply! r "w" -1 '() (section))
        (reduce-apply! r "z" 1 '(("w" . -1)) (section))
        (reduce-apply! r "w" 1 '() (section))
        (list (events r) (dump-pending r)))
      '((("w" . 1) ("w" . -2) ("w" . -1) ("z" . 1)) 0))
(want "PEND-8 x 1.0 while x1 is pending: already pending; z1 then applies x1"
      (let ((r (reduce-empty)))
        (reduce-apply! r "x" 1 '(("z" . 1)) (section))
        (let ((again (reduce-apply! r "x" 1.0 '(("z" . 1)) (section))))
          (reduce-apply! r "z" 1 '() (section))
          (list again (events r) (dump-pending r))))
      '((refused already-pending) (("z" . 1) ("x" . 1)) 0))

(printf "rows: ~a\n~a failures\nreduce-pending complete\n" rows bad)
(exit (if (zero? bad) 0 1))
