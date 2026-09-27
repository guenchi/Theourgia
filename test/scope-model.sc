;; Copyright 2018 - 2026 The Theourgia Authors
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

;; THE MUTATION RECORD'S SCOPES AGAINST A REFERENCE MODEL (F100a, rulings L and M).
;;
;; Review rounds kept finding one more shape of scope the hand-written
;; cells did not hold (the exit, the nesting, the actor, the record after
;; the outer scope, an empty outer scope, a scope re-entered through a
;; continuation, a handler that reads before unwinding, owners whose scopes
;; overlap). So the shapes are generated, and every one is checked
;; against a second, independent statement of the rule:
;;
;;   THE MODEL. Per key, the box installed now (or none). Entering a scope
;;   saves what is installed and installs the scope's box -- a fresh one,
;;   or one holding the entries handed in. Leaving it, by any exit,
;;   installs again what it saved. Re-entering it through a continuation
;;   saves what is installed at that moment and installs the SAME box
;;   again: that is what dynamic-wind's before-thunk does, and although no
;;   product path re-enters a scope, the model holds it (ruling L).
;;
;;   THE GENERATOR. Programs of writes, yields and scopes, nested 0 to 3
;;   deep. Each scope has an exit (a normal return, a raise caught
;;   outside, a continuation escape) that may leave one to three scopes at
;;   once; with or without one re-entry after the exit, through the last
;;   re-entry point taken inside it -- often in a deeper nest, so one
;;   continuation re-enters several scopes; with or without the entries
;;   handed in; and writes before, between and after scopes, so an outer
;;   scope is met empty, populated, or absent. A fixed seed and a fixed
;;   count; every program runs once in the process, and once in an actor of
;;   its own, three actors at a time with yield steps between, so owners'
;;   scopes overlap (ruling M).
;;
;;   THE CHECK. After every step -- each write, entry, yield and exit, the
;;   write after a re-entry, and inside the handler that catches a raise,
;;   before anything unwinds -- each owner's real (mutation-record) equals
;;   its model's installed box; at the end of a program the record is what
;;   it was before the program. Coverage rows say which combinations the
;;   generator produced and that the owners' scopes did overlap, and fail
;;   on any they did not.
;;
;;   STATED LIMITS. The owners are the actors of one scheduler: no two OS
;;   threads. Re-entry happens once per scope, straight after its exit. An
;;   actor stopped by the scheduler without unwinding is not generated (the
;;   ffi header states what happens to its record).
;;
;; The hand-written matrices in facade-ffi.sc and facade-record.sc stay as
;; the readable examples; this file is the one that goes red for a shape
;; nobody wrote a cell for.

(import (chezscheme) (theourgia ffi) (theourgia sched))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define dir (string-append scratch-base "/scope-model-" (number->string (get-process-id))))
(mkdir-p! dir)
(define path-n 0)
(define (fresh-path)
  (set! path-n (+ path-n 1))
  (string-append dir "/w-" (number->string path-n)))
(define tag-n 0)
(define (fresh-tag)
  (set! tag-n (+ tag-n 1))
  tag-n)

;; ONE FILE, WRITTEN THROUGH THE DOOR AND READ BACK (as the facades'
;; make-one!): any failure raises the one marker. run-safely turns it into
;; the program's mismatch, so it can never become a green row.
(define (bytes-of path) (utf8->string (entry-bytes path)))
(define (make-one! path)
  (let ((got (guard (e (#t (list 'raised (if (and (condition? e) (message-condition? e))
                                             (condition-message e)
                                             e))))
               (let ((fd (fd-open path '(write create))))
                 (write-all! fd (string->utf8 "m\n"))
                 (fd-close fd))
               (bytes-of path))))
    (unless (equal? got "m\n")
      (raise (list 'make-one!-failed path got)))))


;; ---- the generator ---------------------------------------------------------
;;
;; A linear congruential generator of its own, so the programs are the same
;; on every Chez and every run: the seed and the count are the file's, and
;; NOTES quotes them.
;;
;; A program is a list of steps: (write), (yield), or
;; (scope exit target reentry handoff body). `target` is how many scopes
;; the exit leaves at once: 1 is the scope itself, 2 its parent as well,
;; up to the scope's own nesting level; a normal return always leaves one.
(define program-seed 20260926)
(define program-count 500)
;; A GENERATOR IS A SEED AND WHETHER ITS RAISES CARRY A HANDLER PLAN.
;; The plan-less generator draws exactly the numbers it always drew, so the
;; 500 programs of SM-1..SM-4 are the ones they always were; the plans are a
;; second stream with a seed of its own.
(define (make-pick seed)
  (lambda (n)
    (set! seed (mod (+ (* seed 1103515245) 12345) 2147483648))
    (mod (quotient seed 65536) n)))
(define scope-exits '(normal raise escape))
(define (generator pick plans?)
  (define (gen-body level depth-left)
    (let ((n (pick 5)))
      (let loop ((i 0) (acc '()))
        (if (= i n)
            (reverse acc)
            (loop (+ i 1)
                  (cons (let ((r (pick 10)))
                          (cond ((and (> depth-left 0) (< r 5)) (gen-scope (+ level 1) depth-left))
                                ((< r 7) '(write))
                                (else '(yield))))
                        acc))))))
  ;; A HANDLER PLAN: the steps the catching handler takes before it leaves,
  ;; one to three, each an observation or a write (several observation
  ;; points inside one handler).
  (define (gen-plan)
    (let ((n (+ 1 (pick 3))))
      (let loop ((i 0) (acc '()))
        (if (= i n) (reverse acc) (loop (+ i 1) (cons (if (= 0 (pick 2)) 'check 'write) acc))))))
  (define (gen-scope level depth-left)
    (let* ((exit (list-ref scope-exits (pick 3)))
           (target (if (eq? exit 'normal) 1 (+ 1 (pick level))))
           (reentry (= 0 (pick 2)))
           (handoff (= 0 (pick 3)))
           (body (gen-body level (- depth-left 1))))
      (if (and plans? (eq? exit 'raise))
          (list 'scope exit target reentry handoff body (gen-plan))
          (list 'scope exit target reentry handoff body))))
  (lambda () (gen-body 0 3)))
(define (generate seed count plans?)
  (let ((gen (generator (make-pick seed) plans?)))
    (let loop ((i 0) (acc '()))
      (if (= i count) (reverse acc) (loop (+ i 1) (cons (gen) acc))))))
(define programs (generate program-seed program-count #f))

;; ---- one program against the model ---------------------------------------
;;
;; THE MODEL'S STATE for one owner (one key): the box installed now, and the
;; frames of the scopes open now, innermost first. A frame is
;; #(tag box saved kout): the scope's box, what its entry saved, and the
;; continuation that leaves it (every exit that targets it goes there).
;;
;; `yield!` is how a step gives the scheduler a chance to run another
;; owner; in the process it does nothing. `open-depth!` tells the caller
;; how many scopes this owner has open, for the interleaving features.
;;
;; Answers (mismatches features checks).
(define (run-program prog yield! open-depth! other-depth)
  (let ((installed #f)
        (frames '())
        (reentries '())
        (mismatches '())
        (features '())
        (checks 0))
    (define (frame-tag f) (vector-ref f 0))
    (define (frame-box f) (vector-ref f 1))
    (define (frame-saved f) (vector-ref f 2))
    (define (frame-kout f) (vector-ref f 3))
    (define (model-record)
      (if installed (reverse (unbox installed)) '()))
    (define (check! where)
      (set! checks (+ checks 1))
      (let ((real (mutation-record))
            (m (model-record)))
        (unless (equal? real m)
          (set! mismatches (cons (list where real m) mismatches)))))
    (define (feature! f)
      (unless (member f features) (set! features (cons f features))))
    ;; LEAVING UP TO AND INCLUDING frame F: every frame inside it is left
    ;; too, each installing what it saved, innermost first; what remains
    ;; installed is what F saved.
    (define (unwind-to! f)
      (let loop ()
        (let ((top (car frames)))
          (set! installed (frame-saved top))
          (set! frames (cdr frames))
          (unless (eq? top f) (loop)))))
    ;; A RE-ENTRY POINT, taken at every write inside a scope that will be
    ;; re-entered (the last one wins, so it is often inside a deeper nest),
    ;; with the frames open at that point.
    (define (capture-reentry!)
      (unless (null? reentries)
        (let ((snap frames))
          (call/cc
            (lambda (k)
              (for-each (lambda (r) (vector-set! r 1 k) (vector-set! r 2 snap)) reentries))))))
    (define (write-step!)
      (let ((p (fresh-path)))
        (make-one! p)
        (when installed
          (set-box! installed (cons (list 'write p) (cons (list 'create p) (unbox installed)))))
        (check! (list 'write p))
        (capture-reentry!)))
    ;; A WRITE IN A HANDLER: the raising scopes are all still installed, so it
    ;; lands in the innermost one's box. It takes no re-entry point: a
    ;; handler is left by its escape, never re-entered.
    (define (handler-write!)
      (let ((p (fresh-path)))
        (make-one! p)
        (when installed
          (set-box! installed (cons (list 'write p) (cons (list 'create p) (unbox installed)))))
        (check! (list 'handler-write p))))
    (define (yield-step!)
      (open-depth! (length frames))
      (let ((mine (length frames)) (theirs (other-depth)))
        (when (and (> mine 0) (> theirs 0)) (feature! '(interleaved open)))
        (when (and (> mine 1) (> theirs 1)) (feature! '(interleaved nested))))
      (yield!)
      (check! '(yield)))
    (define (run-body body)
      (let loop ((steps body) (after #f))
        (unless (null? steps)
          (let ((s (car steps)))
            (case (car s)
              ((write)
               (when after (feature! (cons 'write-after after)))
               (write-step!)
               (loop (cdr steps) #f))
              ((yield)
               (yield-step!)
               (loop (cdr steps) after))
              (else
               (loop (cdr steps) (run-scope s))))))))
    ;; A SCOPE. Entered once; its body runs; it leaves by its exit, which
    ;; may leave `target` scopes at once (a raise caught by that scope's
    ;; handler, or an escape through that scope's continuation). The handler
    ;; that catches a raise reads the record BEFORE anything unwinds (ruling
    ;; M): the scopes are still installed there. When this scope's own
    ;; continuation is reached and the program says so, it is re-entered
    ;; once, through the last re-entry point taken inside it, which may
    ;; re-enter several scopes at once. Answers (exit outer) for the write
    ;; that may follow it, or #f when its exit went past it.
    (define (run-scope s)
      (let* ((exit (list-ref s 1))
             (target (list-ref s 2))
             (reentry (list-ref s 3))
             (handoff (list-ref s 4))
             (body (list-ref s 5))
             (plan (if (> (length s) 6) (list-ref s 6) '()))
             (outer (cond ((not installed) 'none)
                          ((null? (unbox installed)) 'empty)
                          (else 'populated)))
             (initial (mutation-record))
             (box-m (box (if handoff (reverse (model-record)) '())))
             (tag (fresh-tag))
             (frame (vector tag box-m #f #f))
             (reentry-r (vector tag #f #f))
             (reentered #f)
             (level (+ 1 (length frames))))
        (feature! (list 'scope exit reentry outer))
        (feature! (list 'handoff handoff))
        (feature! (list 'depth level))
        (unless (eq? exit 'normal) (feature! (list 'target exit target)))
        (call/cc
          (lambda (kout)
            (vector-set! frame 3 kout)
            (vector-set! frame 2 installed)
            (set! installed box-m)
            (set! frames (cons frame frames))
            (when reentry (set! reentries (cons reentry-r reentries)))
            (with-exception-handler
              (lambda (e)
                (if (and (pair? e) (eq? (car e) 'scope-exit) (eqv? (cadr e) tag))
                    (begin
                      (check! (list 'handler tag))
                      ;; THE PLAN, if the program carries one: more
                      ;; observations, and writes, before anything unwinds.
                      ;; The features are recorded here, where the plan runs:
                      ;; a raise caught by an outer scope runs THAT scope's plan.
                      (unless (null? plan)
                        (feature! (list 'handler-points (length plan)))
                        (feature! (list 'handler-write (and (memq 'write plan) #t))))
                      (for-each (lambda (step)
                                  (if (eq? step 'write)
                                      (handler-write!)
                                      (check! (list 'handler-point tag))))
                                plan)
                      (kout 'raised))
                    (raise e)))
              (lambda ()
                (apply with-mutation-record
                       (lambda ()
                         (check! (list 'enter tag))
                         (run-body body)
                         (when (and reentry (not (vector-ref reentry-r 1)))
                           (let ((snap frames))
                             (call/cc (lambda (k) (vector-set! reentry-r 1 k) (vector-set! reentry-r 2 snap)))))
                         (if reentered
                             (write-step!)
                             (let ((f (list-ref frames (- target 1))))
                               (case exit
                                 ((normal) 'returned)
                                 ((raise) (raise (list 'scope-exit (frame-tag f))))
                                 ((escape) ((frame-kout f) 'escaped))))))
                       (if handoff (list initial) '()))))))
        ;; THIS SCOPE'S CONTINUATION: reached by its normal return, or by an
        ;; exit that targeted it from here or from deeper. Everything from
        ;; the innermost open frame out to this one has now been left.
        (unwind-to! frame)
        (set! reentries (remq reentry-r reentries))
        (check! (list 'exit tag exit (if reentered 'second 'first)))
        (when (and reentry (not reentered) (vector-ref reentry-r 1))
          (set! reentered #t)
          (let* ((snap (vector-ref reentry-r 2))
                 (back (reverse (filter (lambda (f) (not (memq f frames))) snap))))
            (feature! (list 'reenter (length back)))
            ;; RE-ENTRY RUNS EVERY RE-ENTERED SCOPE'S before-thunk, outermost
            ;; first: each saves what is installed then and installs its
            ;; SAME box again.
            (for-each (lambda (f)
                        (vector-set! f 2 installed)
                        (set! installed (frame-box f)))
                      back)
            (set! frames snap)
            (set! reentries (cons reentry-r reentries))
            ((vector-ref reentry-r 1) 'again)))
        (list exit outer)))
    (let ((pre (mutation-record)))
      (run-body prog)
      (open-depth! 0)
      (unless (equal? (mutation-record) pre)
        (set! mismatches (cons (list 'end (mutation-record) pre) mismatches))))
    (list (reverse mismatches) features checks)))

;; A PROGRAM THAT RAISES (a write that failed) answers the raise as its one
;; mismatch, so it fails its row instead of ending the file.
(define (run-safely prog yield! open-depth! other-depth)
  (guard (e (#t (list (list (list 'raised (if (and (condition? e) (message-condition? e))
                                              (condition-message e)
                                              e)))
                      '()
                      0)))
    (run-program prog yield! open-depth! other-depth)))

;; ---- what the generator produced -------------------------------------------
(define (combos . lists)
  (if (null? lists)
      '(())
      (apply append
        (map (lambda (x) (map (lambda (rest) (cons x rest)) (apply combos (cdr lists))))
             (car lists)))))
(define wanted-features
  (append
    (map (lambda (c) (cons 'scope c)) (combos scope-exits '(#t #f) '(none empty populated)))
    (map (lambda (c) (cons 'write-after c)) (combos scope-exits '(none empty populated)))
    (map (lambda (c) (cons 'target c)) (combos '(raise escape) '(1 2 3)))
    '((handoff #t) (handoff #f) (depth 1) (depth 2) (depth 3) (reenter 1) (reenter 2))))
(define actor-wanted-features
  (append wanted-features '((interleaved open) (interleaved nested))))
(define (missing-features wanted all)
  (filter (lambda (f) (not (member f all))) wanted))
(define (first-few xs) (if (> (length xs) 3) (list-head xs 3) xs))

;; ---- in the process --------------------------------------------------------
(define process-results
  (map (lambda (p) (run-safely p void (lambda (n) (void)) (lambda () 0))) programs))
(define process-checks (apply + (map caddr process-results)))
(define process-features (apply append (map cadr process-results)))
(printf "process: ~a programs, ~a checks, seed ~a\n" program-count process-checks program-seed)
(want "SM-1 generated scope programs in the process: the real record equals the model after every step"
      (first-few (apply append (map car process-results)))
      '())
(want "SM-2 the generator produced every combination of exit, re-entry and outer state, a write after each exit, exits across one to three scopes, and re-entries of one and two scopes"
      (missing-features wanted-features process-features)
      '())

;; ---- several observation points inside one handler --------------------------
;;
;; A second stream (its own seed) whose raises carry a handler plan: the
;; catching handler observes again, and writes, before it leaves. The model
;; is the one above: every raising scope is still installed in a handler,
;; so a write there lands in the innermost one's box, and the exit then
;; unwinds as always.
(define plan-seed 20260927)
(define plan-count 200)
(define plan-programs (generate plan-seed plan-count #t))
(define plan-results
  (map (lambda (p) (run-safely p void (lambda (n) (void)) (lambda () 0))) plan-programs))
(define plan-features (apply append (map cadr plan-results)))
(printf "handler plans: ~a programs, ~a checks, seed ~a\n" plan-count (apply + (map caddr plan-results)) plan-seed)
(want "SM-5 SAMPLE a raise whose handler observes, writes and observes again, before the exit: the record equals the model at each point"
      (car (run-safely '((write) (scope raise 1 #f #f ((write) (scope normal 1 #f #t ((write)))) (check write check)))
                       void (lambda (n) (void)) (lambda () 0)))
      '())
(want "SM-5 (PIN) generated programs whose handlers observe several times and write: the record equals the model after every step"
      (first-few (apply append (map car plan-results)))
      '())
(want "SM-5 the generator produced handlers of one, two and three points, with and without a write"
      (missing-features '((handler-points 1) (handler-points 2) (handler-points 3)
                          (handler-write #t) (handler-write #f))
                        plan-features)
      '())

;; ---- an owner in another process, modelled --------------------------------------
;;
;; Another process has an ffi of its own, and its record is not in this
;; table at all. Modelled without spawning: the key is switched to a foreign
;; one, a scope is opened under it and written to, then the key is switched
;; back and one of the programs above runs inside that open foreign scope.
;; Each side must not see the other: the program's record equals its model
;; (the foreign scope is not installed for it), the foreign record is what
;; its own writes made it, and both are closed afterwards. This runs before
;; the scheduler starts, which installs its own key thunk.
(define (run-under-foreign prog foreign-writes)
  (let ((kf (list 'foreign-process)) (kl (list 'this-process)))
    (mutation-set-self! (lambda () kf))
    (let ((inside
           (with-mutation-record
             (lambda ()
               (let loop ((i 0))
                 (when (< i foreign-writes) (make-one! (fresh-path)) (loop (+ i 1))))
               (let ((foreign-before (mutation-record)))
                 (mutation-set-self! (lambda () kl))
                 (let ((r (run-safely prog void (lambda (n) (void)) (lambda () 0))))
                   (mutation-set-self! (lambda () kf))
                   (list (car r) (length foreign-before) (equal? foreign-before (mutation-record)))))))))
      (let ((foreign-closed (null? (mutation-record))))
        (mutation-set-self! (lambda () kl))
        (append inside (list foreign-closed (null? (mutation-record))))))))

(want "SM-6 SAMPLE a program runs inside another process's open scope (two foreign writes): its record equals the model, the foreign record is untouched, both close"
      (run-under-foreign '((write) (scope normal 1 #f #t ((write)))) 2)
      '(() 4 #t #t #t))
(define foreign-results
  (map (lambda (p i) (run-under-foreign p (mod i 3)))
       (list-head programs 120) (iota 120)))
(want "SM-6 (PIN) the first 120 generated programs, each inside a foreign scope holding zero, one or two writes: every record equals its model, no foreign record moved, every scope closed"
      (list (first-few (apply append (map car foreign-results)))
            (for-all caddr foreign-results)
            (for-all cadddr foreign-results)
            (for-all (lambda (r) (list-ref r 4)) foreign-results))
      '(() #t #t #t))

;; ---- in actors: three programs at a time, interleaved ----------------------
(define (actor-yield!) (receive (after 1 'yielded)))
;; THE DEEPEST NESTING ANOTHER OWNER HAD OPEN at its last yield.
(define (deepest-other owner)
  (let-values (((ks vs) (hashtable-entries open-depths)))
    (let loop ((j 0) (m 0))
      (cond ((= j (vector-length ks)) m)
            ((eqv? (vector-ref ks j) owner) (loop (+ j 1) m))
            (else (loop (+ j 1) (max m (vector-ref vs j))))))))
;;
;; Each program runs in a fresh actor of its own (its own key), three at a
;; time; a (yield) step parks the actor for a millisecond, and the scheduler
;; also preempts on its timer, so the owners' scopes overlap. Each owner
;; checks its own record against its own model after every step.
;; ---- an actor killed inside its scopes -----------------------------------------
;;
;; A killed actor's after-thunks are discarded, not run (igropyr actor.sc,
;; @kill), so its scopes are never left: its entry in the record table is
;; let go only because the table is weak and the actor's pcb becomes
;; garbage. A weak pair watches the pcb. CONTROL: an actor killed with no
;; scope open is reclaimed at all, so the row about scopes measures the
;; table and not the scheduler. The owners still running are unaffected:
;; main's record and a fresh actor's scope read as the model says.
(define (killed-pcb-reclaimed? main depth writes)
  (let* ((p (spawn (lambda ()
                     (let open ((d depth))
                       (if (> d 0)
                           (with-mutation-record
                             (lambda ()
                               (let loop ((i 0)) (when (< i writes) (make-one! (fresh-path)) (loop (+ i 1))))
                               (open (- d 1))))
                           (begin (send main (list 'killed-ready))
                                  (receive (after 60000 'gone))))))))
         (wp (weak-cons p '())))
    (receive (after 5000 #f) (`(killed-ready) #t))
    (kill p 'scope-model)
    (set! p #f)
    (receive (after 50 'settled))
    (collect (collect-maximum-generation))
    (collect (collect-maximum-generation))
    (bwp-object? (car wp))))

(define (killed-actor-rows main)
  (want "SM-7 CONTROL an actor killed with no scope open is reclaimed"
        (killed-pcb-reclaimed? main 0 0)
        #t)
  (want "SM-7 SAMPLE an actor killed inside one scope holding a write is reclaimed: the record table does not keep its key"
        (killed-pcb-reclaimed? main 1 1)
        #t)
  (want "SM-7 (PIN) actors killed inside one, two and three nested scopes, with and without writes, are all reclaimed"
        (map (lambda (c) (list c (killed-pcb-reclaimed? main (car c) (cadr c))))
             '((1 0) (2 0) (3 0) (1 2) (2 2) (3 2)))
        (map (lambda (c) (list c #t)) '((1 0) (2 0) (3 0) (1 2) (2 2) (3 2))))
  (want "SM-7 after the kills main's record is empty and a fresh actor's scope reads its own writes only"
        (list (mutation-record)
              (let ((me self))
                (spawn (lambda ()
                         (send me (list 'fresh
                                        (with-mutation-record
                                          (lambda ()
                                            (let ((q (fresh-path)))
                                              (make-one! q)
                                              (equal? (mutation-record) (list (list 'create q) (list 'write q))))))))))
                (receive (after 5000 'no-answer) (`(fresh ,ok) ok))))
        '(() #t)))

(define group-size 3)
(define open-depths (make-eqv-hashtable))
(start-scheduler
  (lambda ()
    (let ((main self))
      (let loop ((ps programs) (i 0) (acc '()))
        (if (null? ps)
            (let ((results (reverse acc)))
              (printf "actors: ~a programs, ~a checks, ~a at a time\n" i (apply + (map caddr results)) group-size)
              (want "SM-3 the same programs in actors, three at a time and interleaved: each owner's record equals its model after every step"
                    (first-few (apply append (map car results)))
                    '())
              (want "SM-4 the interleaving happened: owners yielded with scopes open, and nested, while another owner had scopes open"
                    (missing-features actor-wanted-features (apply append (map cadr results)))
                    '())
              (killed-actor-rows main))
            (let* ((group (if (> (length ps) group-size) (list-head ps group-size) ps))
                   (ids (iota (length group))))
              (for-each
                (lambda (p id)
                  (let ((owner (+ i id)))
                    (spawn (lambda ()
                             (send main
                                   (list 'program owner
                                         (run-safely p
                                                     actor-yield!
                                                     (lambda (n) (hashtable-set! open-depths owner n))
                                                     (lambda () (deepest-other owner)))))))))
                group ids)
              ;; THE ANSWERS COME IN ANY ORDER; each is filed under its owner.
              (let* ((got (make-eqv-hashtable))
                     (_ (for-each
                          (lambda (id)
                            (receive (after 20000 (hashtable-set! got (list 'timeout id) #t))
                              (`(program ,o ,r) (hashtable-set! got o r))))
                          ids))
                     (answers
                      (map (lambda (id)
                             (hashtable-ref got (+ i id) (list (list (list 'no-answer (+ i id))) '() 0)))
                           ids)))
                (for-each (lambda (id) (hashtable-delete! open-depths (+ i id))) ids)
                (loop (list-tail ps (length group)) (+ i (length group)) (append (reverse answers) acc)))))))
      (system (string-append "rm -rf " dir))
      (printf "rows: ~a\n~a failures\nscope-model complete\n" rows bad)
      (exit (if (zero? bad) 0 1))))
