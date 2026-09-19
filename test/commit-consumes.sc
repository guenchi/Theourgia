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

;; A COMMIT SAYS, IN ITS OWN RECORD, WHICH DRAFTS IT TOOK.
;;
;; It used to say so in a file beside the drafts -- one immutable packet
;; per request, naming the envelopes that request had read. Two stores
;; had to be kept in step, and every read of a draft opened every packet
;; the writer had ever written to ask whether it had been replayed.
;;
;; THREE THINGS CHANGED TOGETHER, and they had to: a commit always
;; writes a plan, the plan carries a `consumes` list, and the reduction
;; keeps the index that list feeds. Landing the index without the list
;; would have made every earlier commit consume nothing, and every draft
;; a failed retirement had left behind would have come back as a draft.
;;
;; NOTE: THE ROWS HERE ASSERT THE REPRESENTATION, not only the behaviour.
;; The packet implementation answers "is this draft still a draft"
;; correctly too; what it cannot do is put the answer in the log.

(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce)
        (theourgia request) (theourgia trace) (theourgia ffi)
        (only (theourgia log) writer-directory)
        (only (theourgia store) store-evidence))

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

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/commit-consumes-" (number->string (get-process-id))))
(when (file-exists? root) (error 'commit-consumes "Use a fresh test root" root))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))
(define init (rpc-dispatch store '(init) "test"))
(define writer (cadr (assq 'writer (cdr init))))

;; KEY: THE WRITER IS NAMED HERE BECAUSE IT IS NO LONGER GUESSED. A draft
;; verb that was not told which writer it speaks for used to fall back to
;; this store's own local log writer, so two agents that never passed
;; `--writer` shared one draft space without either being told. The core
;; now refuses that call instead; naming the same writer the old fallback
;; would have chosen keeps every row below asking what it asked before.
;;
;; NEVER: AND ONLY WHERE IT WAS MISSING: a call that already names a writer is
;; naming it to make a point, and must keep the one it names.
(define draft-verbs '(write restore drafts discard commit))

(define (wants-writer? verb args)
  (or (memq verb draft-verbs)
      (and (eq? verb 'read)
           (or (member "--working" args) (member "--working-info" args)))))

(define (call . args)
  (rpc-dispatch store
                (if (and (wants-writer? (car args) (cdr args))
                         (not (member "--writer" (cdr args))))
                    (append args (list "--writer" writer))
                    args)
                "test"))
(define (insert title)
  (let ((a (call 'insert "--title" title "--text" "old")))
    (let ((ev (car (cadr (assq 'events (cdr a)))))) (block-id (car ev) (cdr ev)))))
(define A (insert "A"))
(define (state) (open-and-reduce store))

(define (envelope id)
  (let ((path (string-append (writer-directory store writer) "/working/" id)))
    (and (file-exists? path) #t)))

;; ---- CC-01 a commit of one block is a plan --------------------------------

(call 'write A "committed text")
(define saved (call 'read A "--working-info"))
(define version
  (let ((p (assq 'projection (cdr saved)))) (list-ref p 4)))
(define based-on
  (let ((p (assq 'projection (cdr saved)))) (list-ref p 5)))
(define cut
  (let ((p (assq 'projection (cdr saved)))) (list-ref p 6)))
(define cursor
  (string-append writer ":"
                 (number->string (cdr (assoc writer (reduce-applied-cut (state)))))))
;; KEY: THE VERSION IS PASSED, BECAUSE THE REQUEST CARRIES `--req`.
;; A commit that promises a retry has its identity taken over the
;; versions it consumes, and a retry from a new process cannot read them
;; from a draft that its own first attempt retired.
(want "CC-01 the commit succeeds"
      (rpc-ok? (call 'commit A "--req" "R1" "--cursor" cursor
                     "--working-version" version)) #t)

(define evidence (store-evidence store (cons writer "R1")))
(want "CC-01 one block is still written as a plan and one member"
      (map (lambda (e) (actor-sub (ev-actor e))) evidence) '(plan 0))

;; ---- CC-02 and the plan names what it took --------------------------------

(define plan-payload
  (ev-payload (find (lambda (e) (eq? 'plan (actor-sub (ev-actor e)))) evidence)))
(want "CC-02 the plan payload has a sixth element" (length plan-payload) 6)
(want "CC-02 which is a consumes list owned by this writer"
      (list (car (list-ref plan-payload 5)) (cadr (list-ref plan-payload 5)))
      (list 'consumes writer))
(want "CC-02 naming the block, the version, the baseline and the cut it read"
      (caddr (list-ref plan-payload 5))
      (list (list A version based-on cut)))

;; ---- CC-03 so the draft is consumed ---------------------------------------

(want "CC-03 the index says so" (state-consumed? (state) writer version) #t)
(want "CC-03 TWIN: and not for a version nobody committed"
      (state-consumed? (state) writer "0000") #f)
(want "CC-03 drafts lists nothing" (call 'drafts) '(ok (items)))

;; ---- CC-04 nothing is written beside the drafts ---------------------------

(want "CC-04 the packet directory does not exist"
      (file-exists? (string-append (writer-directory store writer) "/working-requests")) #f)

;; A COMMIT ADDS NO ENTRY TO THE WRITER'S DIRECTORY. The log grows; the
;; space beside the drafts does not.
(define (writer-entries)
  (length (filter (lambda (n) (not (member n '("." ".."))))
                  (directory-entries (writer-directory store writer)))))
(define entries-before (writer-entries))
(let loop ((n 0))
  (when (< n 5)
    (let ((b (insert (string-append "B" (number->string n)))))
      (call 'write b "text")
      (call 'commit b))
    (loop (+ n 1))))
(want "CC-04 five more commits add no entry beside the drafts"
      (writer-entries) entries-before)

;; ---- CC-06 a commit of several blocks names a version for each ----------
;;
;; A request that promises a retry must be able to rebuild its own
;; identity, and that identity is taken over the (block . version) pairs.
;; With more than one block there is more than one version, so the option
;; repeats -- `--working-version <block>=<version>` -- and a commit
;; naming exactly one block may still give the bare version.
;;
;; NOTE: WITHOUT THIS THE MULTI-BLOCK CASE WAS UNREPRESENTABLE: the single
;; `--working-version` was honoured only when one block was named, and a
;; two-block commit with `--req` was refused whatever the client sent.

(define M1 (insert "M1"))
(define M2 (insert "M2"))
(call 'write M1 "one")
(call 'write M2 "two")
(define (version-of id)
  (list-ref (assq 'projection (cdr (call 'read id "--working-info"))) 4))
(define vm1 (version-of M1))
(define vm2 (version-of M2))
(define (cursor-now)
  (string-append writer ":"
                 (number->string (cdr (assoc writer (reduce-applied-cut (state)))))))

(want "CC-06 naming only one of the two versions is refused"
      (let ((a (call 'commit M1 M2 "--req" "M" "--cursor" (cursor-now)
                     "--working-version" (string-append M1 "=" vm1))))
        (list (cadr a) (caddr a) (cadr (cadddr a))))
      (list 'bad-request 'req-needs-versions M2))

(want "CC-06 naming both of them commits"
      (rpc-ok? (call 'commit M1 M2 "--req" "M" "--cursor" (cursor-now)
                     "--working-version" (string-append M1 "=" vm1)
                     "--working-version" (string-append M2 "=" vm2)))
      #t)
(want "CC-06 and the plan consumes both, in block order"
      (map car (caddr (list-ref
                        (ev-payload (find (lambda (e) (eq? 'plan (actor-sub (ev-actor e))))
                                          (store-evidence store (cons writer "M"))))
                        5)))
      (list-sort string<? (list M1 M2)))
(want "CC-06 TWIN: both drafts are gone" (call 'drafts) '(ok (items)))

;; ---- CC-05 the lookup is bounded ------------------------------------------
;;
;; KEY: THE ROW READS A PROBE COUNT, NOT A TIME. `state-consumed?` puts one
;; trace event per plan that named the exact version it was asked about;
;; the packet implementation would have opened every packet the writer
;; ever wrote. A row that only checked the ANSWER is green for both.

(define (probes-for thunk)
  (let ((out (open-output-string)))
    (parameterize ((current-error-port out))
      (trace-enable! #t)
      (thunk)
      (trace-enable! #f))
    (let ((p (open-input-string (get-output-string out))))
      (let loop ((n 0))
        (let ((x (read p)))
          (if (eof-object? x) n
              (loop (+ n (if (and (pair? x) (eq? 'trace (car x))
                                  (eq? 'consumption-probe (cadr x)))
                             1 0)))))))))

(define (unrelated-commits! n)
  (let loop ((i 0))
    (when (< i n)
      (let ((b (insert (string-append "U" (number->string i)))))
        (call 'write b "u")
        (call 'commit b))
      (loop (+ i 1)))))

(define st1 (state))
(define probes-10 (probes-for (lambda () (state-consumed? st1 writer version))))
(unrelated-commits! 20)
(define st2 (state))
(define probes-30 (probes-for (lambda () (state-consumed? st2 writer version))))
(want "CC-05 the same question costs the same after twenty more commits"
      (list probes-10 probes-30) (list probes-10 probes-10))
(want "CC-05 TWIN: and it is really asking something -- the probe count is not zero"
      (> probes-10 0) #t)
(want "CC-05 the store really did grow"
      (> (length (state-datum st2)) (length (state-datum st1))) #t)

;; ---- CC-07 the reduction that DID the writing agrees with a replay ------
;;
;; NOTE: EVERY ROW ABOVE READS A REPLAY. `(state)` calls `open-and-reduce`,
;; which folds the records from disk -- so a build whose LIVE fold
;; disagrees with its replay passes all of them. That build existed: the
;; live write path called `reduce-apply!` without the record's actor, so
;; the reduction doing the writing never learned which plan a member
;; belonged to and completed nothing.
;;
;; The live reduction is not reachable from here, so what is asserted is
;; the property that made it disagree: the record on disk carries its
;; actor, and the actor names the plan. A fold that ignores it cannot
;; answer, and a fold that is not given it cannot either.

(define plan-member
  (find (lambda (e) (eqv? 0 (actor-sub (ev-actor e))))
        (store-evidence store (cons writer "R1"))))
(want "CC-07 the member record carries an actor" (and (ev-actor plan-member) #t) #t)
(want "CC-07 and that actor names the plan it belongs to"
      (actor-plan-event (ev-actor plan-member))
      (ev-event (find (lambda (e) (eq? 'plan (actor-sub (ev-actor e))))
                      (store-evidence store (cons writer "R1")))))
(want "CC-07 TWIN: and its index position"
      (actor-sub (ev-actor plan-member)) 0)

;; ---- CC-08 the fold that DOES the writing, asked directly ----------------
;;
;; NOTE: EVERY ROW ABOVE THIS ONE, CC-07 INCLUDED, READS A REPLAY. The
;; write path folds each record into a reduction of its own --
;; `with-store-write` allocates it (`reduce-empty`, store.ss) and calls
;; `reduce-apply!` with the record's ACTOR -- and a build that drops
;; that actor completes no plan in THAT reduction. It is the actor that
;; says which plan a member belongs to; without it the index gets a
;; member that belongs to nothing.
;;
;; NEVER: AND THE ONE I FIRST WROTE HERE WAS NOT A TEST OF IT. It turned the
;; resident cache on, committed, and compared the reduction before the
;; write with the one after, on the theory that the write folds into the
;; cached object. It does not: `with-store-write` builds its own, and
;; the `resident-hit` traced during a commit belongs to the read side.
;; The rows passed with both live `reduce-apply!` calls handed a bogus
;; actor -- they were about object lifetimes, not about consumption.
;;
;; KEY: THE REDUCTION IS HANDED TO THE CALLER. `proc` is called as
;; `(proc state view)`, and `state` IS the reduction being folded. No
;; verb in this tree keeps it -- which is why no fixture could see this
;; through `rpc-dispatch` -- so the fixture becomes that caller: it
;; keeps what it is given and asks the index afterwards. Same entry
;; point the commit path uses, same arguments in the same order.

(call 'write A "live fold text")
(define live-projection
  (assq 'projection (cdr (call 'read A "--working-info"))))
(define live-version (list-ref live-projection 4))
(define live-based-on (list-ref live-projection 5))
(define live-cut (list-ref live-projection 6))
(define live-state #f)
(define live-answers
  (with-store-write store
    (lambda (current view)
      (set! live-state current)
      (list (list 'set A 'src "live fold text")))
    "test"
    (make-write-request "test" 'commit (list A) "CC8"
                        (let ((p (assoc writer (reduce-applied-cut (state)))))
                          (if p p (cons writer 0))))
    (lambda (current) #f)
    #t
    (list 'consumes writer
          (list (list A live-version live-based-on live-cut)))))

(want "CC-08 the write went through"
      (for-all (lambda (a) (and (pair? a) (eq? 'ok (car a)))) live-answers) #t)
(want "CC-08 TWIN: and the fixture was handed the reduction"
      (and (reduction? live-state) #t) #t)
(want "CC-08 the fold that did the writing knows the draft was consumed"
      (state-consumed? live-state writer live-version) #t)

;; NEVER: AND A REPLAY AGREES. The two folds are different code paths over
;; the same records, and the defect this section exists for is exactly
;; them disagreeing -- so the row above is worth nothing without one
;; that says what the answer should have been.
(want "CC-08 TWIN: a fold built by reading says the same"
      (state-consumed? (state) writer live-version) #t)
(want "CC-08 TWIN: which really is a different reduction"
      (eq? live-state (state)) #f)

(printf "rows: ~a\n~a failures\ncommit-consumes complete\n" rows bad)
