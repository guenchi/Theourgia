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

;; Each limit branch of the supervisor, reached by a worker that would
;; have finished on its own.
;;
;; KEY: THE BUDGET IS THE CONTROLLED INPUT, NOT THE INSTRUMENT. The Python
;; fixture these rows replace reached the same two branches by replacing
;; the product's clock with one that jumped and its RSS reader with one
;; that returned a number no process had. That is the one thing this
;; tree's injection facility refuses to do -- `(theourgia ffi)` states
;; that a fault may change what a call DOES but may never lie about what
;; it did, because a forged reading forges the evidence a row reads.
;;
;; So these rows move the budget instead. A deadline of 1ms is really
;; past by the time the worker is ready; a budget of one mebibyte is
;; really below the resident size of any Chez process. The readings are
;; true, the workers are finite, and the branch is reached in under a
;; second rather than by waiting out a real limit.

(import (chezscheme))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))

;; NEVER: `want` IS A MACRO AND `caught` IS WHY. A procedural `want`
;; evaluates both arguments before the call, so a row whose expression
;; raises ENDS THE FILE -- the rows after it never run, and the ones
;; before it have already printed `ok`. The suite names fixtures that
;; lack this pair, and a new fixture has no business joining that list.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

;; SCRATCH PATHS LIVE UNDER THE RUNNER'S TWO ROOTS (F71): files and
;; directories under THEOURGIA_TEST_ROOT, socket paths under
;; THEOURGIA_TEST_SOCK, which is short enough for one. Run alone, without
;; them, a path falls back to /tmp as it always did.
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))

(define here (string-append scratch-base "/evalsup-" (number->string (get-process-id))))
(define store (string-append here "/store"))
(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))

(define (file-text path)
  (if (not (file-exists? path))
      ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))

(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

(define (quoted a)
  (string-append "'"
                 (apply string-append
                        (map (lambda (c) (if (char=? c #\') "'\\''" (string c)))
                             (string->list a)))
                 "'"))

(define (cli . args)
  (let ((out (string-append here "/out.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
              "scheme --script ../core.sc "
              (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
              "--store " store " --wire > " out " 2>&1"))
    (file-text out)))

(system (string-append "rm -rf " here "; mkdir -p " store))
(cli "init")

;; A worker that returns a value, and spends about a second EVALUATING,
;; so that the sampler gets many turns while it is alive.
;;
;; NOTE: THE COUNT IS NOT ARBITRARY AND A SMALLER ONE BREAKS THE ROW
;; SILENTLY. Most of a run's wall time is two Chez processes loading
;; this library from source; the sampler only exists between the
;; worker's `ready` and its answer. At 40 million iterations that window
;; is under one 50ms interval -- MEASURED: zero samples, total 1003ms,
;; indistinguishable from a run of `(+ 1 2)` -- so the row read green
;; while the budget was never once consulted. At 2 billion the window is
;; ~900ms and the supervisor takes 17 samples. A row about sampling has
;; to outlive the sampling interval, and the interval is a property of
;; the supervisor, not of the clock on the wall.
;; NEVER: TWO READINGS, NOT ONE (F66). A worker that never said ready answers
;; (error eval-worker-unavailable (reason no-ready)), and that is a different
;; fact from an answer that came back without naming its limit. Both used to
;; read UNNAMED, so a red row could not say which had happened.
;;
;; NEVER: THE CLAUSE IS COMPARED AS A DATUM, NOT FOUND AS A SUBSTRING (F104,
;; I5). The text "(limit 1)" was searched for in the whole output, so a
;; stdout that printed it -- the output of an ok answer is text inside it --
;; read named-the-limit for an answer that named no limit at all. The answer is read -- the first datum of the
;; output headed `ok` or `error`; stderr shares the output -- and the clause
;; is looked for among its clauses with equal?.
(define (answer-datum out)
  (guard (e (#t #f))
    (let ((port (open-string-input-port out)))
      (let loop ()
        (let ((d (read port)))
          (cond ((eof-object? d) #f)
                ((and (pair? d) (memq (car d) '(ok error))) d)
                (else (loop))))))))

(define (clauses-of d)
  (if (list? d) (filter pair? (cdr d)) '()))

(define (limit-reading out limit-clause)
  (let ((d (answer-datum out)))
    (cond ((member limit-clause (clauses-of d)) 'named-the-limit)
          ((and d (eq? (car d) 'error) (pair? (cdr d)) (eq? (cadr d) 'eval-worker-unavailable)
                (member '(reason no-ready) (clauses-of d)))
           'no-ready)
          (else 'UNNAMED))))

;; GUARD, green on the base: the substring the base looked for includes the
;; closing parenthesis, so "(limit 1)" was never found in "(limit 10)". The
;; row is kept so a later matcher that drops the parenthesis is red. The row
;; after it is the one the base fails.
(want "F104 GUARD (green on the base): a reading for (limit 1) is not satisfied by an answer naming (limit 10)"
      (limit-reading "(error eval-limit (resource time) (limit 10))\n" '(limit 1))
      'UNNAMED)

(want "F104 a reading for (limit 1) is not satisfied by an ok answer whose stdout prints (limit 1)"
      (limit-reading "(ok (values (3)) (stdout \"(limit 1)\") (stderr \"\"))\n" '(limit 1))
      'UNNAMED)

(want "F104 CONTROL TWIN: the answer that names (limit 1) reads named-the-limit, and no-ready reads no-ready"
      (list (limit-reading "(error eval-limit (resource time) (limit 1))\n" '(limit 1))
            (limit-reading "(theourgia machine-home \"/x\")\n(error eval-worker-unavailable (reason no-ready))\n" '(limit 1)))
      '(named-the-limit no-ready))

(define finite-slow "(let loop ((i 0)) (if (< i 2000000000) (loop (+ i 1)) 3))")

;; ---- the deadline branch ------------------------------------------------------
;;
;; NEVER: THE WORKER HERE WOULD HAVE ANSWERED. The row is about the
;; supervisor stopping it, so an infinite loop would not distinguish a
;; supervisor that enforces the deadline from one that merely never
;; returns.
(want "SUP-01 a deadline already past stops a worker that would have answered (the second reading is no-ready when the worker never said ready, UNNAMED when it answered without naming the limit)"
      (let ((out (cli "eval" "--timeout-ms" "1" "(+ 1 2)")))
        (list (if (contains? out "(resource time)") 'time (list 'said out))
              (limit-reading out '(limit 1))))
      '(time named-the-limit))

;; ---- the memory branch --------------------------------------------------------
;;
;; NEVER: ONE MEBIBYTE IS THE FLOOR THE ARGUMENT TABLE ALLOWS, and every Chez
;; process is tens of times larger, so the sample really is over budget.
;; NOTE: The worker must outlive one sampling interval, which is why it is
;; the slow finite one and not `(+ 1 2)`: a worker that exits first is
;; answered from its exit, and the row would pass or fail on a race.
(want "SUP-02 a budget below the worker's real size stops it at a sample (the second reading is no-ready when the worker never said ready, UNNAMED when it answered without naming the limit)"
      (let ((out (cli "eval" "--memory-bytes" "1048576" "--timeout-ms" "30000" finite-slow)))
        (list (if (contains? out "(resource memory)") 'memory (list 'said out))
              (limit-reading out '(limit 1048576))))
      '(memory named-the-limit))

;; ---- the twin -----------------------------------------------------------------
;;
;; NEVER: WITHOUT THIS ROW the two above are satisfied by a supervisor that
;; stops every worker it starts. The same two workers, with budgets they
;; fit inside, have to come back with their values -- which also says
;; that what decided the two rows above was the budget and not the
;; worker.
(want "SUP-03 TWIN: the same two workers inside their budgets return values"
      (let ((quick (cli "eval" "--timeout-ms" "30000" "(+ 1 2)"))
            (slow (cli "eval" "--memory-bytes" "2147483648" "--timeout-ms" "30000" finite-slow)))
        (list (if (contains? quick "(values (3))") 'quick-answered (list 'said quick))
              (if (contains? slow "(values (3))") 'slow-answered (list 'said slow))))
      '(quick-answered slow-answered))

(system (string-append "rm -rf " here))
(printf "rows: ~a\n~a failures\neval-supervisor complete\n" rows bad)
(exit (if (zero? bad) 0 1))
