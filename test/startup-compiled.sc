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

;; THE START-UP LINE, ON COMPILED OBJECTS. A user starts the command line
;; from objects, and from source a start measures the compiler (the
;; ruling of 2026-10-13), so the line is read here as it ships: this tree
;; and its base are each built with their own build.ss into this file's
;; scratch root, and `core.so no-such-verb` (or `core.sc` where a build
;; wrote no program object) is started from each object root in turn,
;; CHEZSCHEMELIBEXTS=.so, so nothing is compiled while it is timed.
;;
;;   SC-1  the step: three runs of forty interleaved pairs, base and tree;
;;         each run's ratio is p10(tree) / p10(base), and the median of the
;;         three is within the recorded step threshold
;;   SC-1 CONTROL  the same with the tree arm doubled (two starts per
;;         sample, the answer unchanged) is not green
;;   SC-2  the judge on fixed samples
;;   SC-3  the arms ran objects: an object root missing one library's
;;         object does not start
;;   SC-4  the starts interleaved, every start exited as no-such-verb exits
;;   SC-5  the cumulative line: the tree against the origin dd506ee, the
;;         first tip that shipped program objects, judged against 1%
;;
;; THE OPT-INS. THEOURGIA_BASE_LIBDIR names the base tree (a library root
;; holding its theourgia/ and igropyr/), as for the other fixtures that
;; compare with a base; unset, the file says so and is green.
;; THEOURGIA_STARTUP_ORIGIN_LIBDIR names the origin's tree, which the gate
;; archives beside the base; unset, SC-5 is a SKIP line naming it.
;;
;; BOTH ARMS SHARE THE PIN'S IGROPYR. Every root is built against the
;; igropyr/ beside it, which is the gate's pin for each, so a figure here
;; is theourgia's own start from one tree to the other. Against the origin
;; that is not quite the shipped dd506ee (whose igropyr may have differed);
;; the step row has the same property.
;;
;; THE TIMES ARE THE MONOTONIC CLOCK'S, in fractional milliseconds: one
;; millisecond is 2.5% of a 40 ms start, and the line is 1%.
;;
;; THE RECORD BELOW IS RECORDED, NEVER COMPUTED FROM THE RUN. The step
;; threshold comes from an A/A reading on a quiet machine (the base against
;; itself, three times; THEOURGIA_STARTUP_AA=1 prints it): 1 + max(0.01,
;; twice the largest deviation of a median from 1). Until it is recorded,
;; SC-1 reads `uncalibrated`, a FAIL.
;;
;; OVER THE CUMULATIVE LINE IS THE USER'S TO RULE. A start-up cost over the
;; line goes to the user, who accepts and records it, has it reduced and
;; re-gated, or sets a budget (2026-10-09); SC-5 is green within 1.01, or
;; within an accepted figure the record holds plus 0.01 for the drift
;; between sittings, and its red says that it is not a defect to fix on
;; sight. Nobody but the user writes an `accepted` entry.

(import (chezscheme))

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
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

;; ---- the record (recorded, never computed) -------------------------------------
(define startup-record
  '((step-threshold . #f)
    (aa-readings . #f)
    (taken . #f)
    ;; THE CUMULATIVE LINE: the 1% line of 2026-10-09 and 2026-10-13, from
    ;; the origin dd506ee (main's ruling of 2026-11-12, under the user's
    ;; delegation: the first tip that shipped program objects, so the line
    ;; compares shipped forms; the 2026-10-13 ruling named cd27dcf, whose
    ;; script form would give the row F293's one-time 28% as headroom).
    (origin . "dd506ee")
    (cumulative-line . #e1.01)
    (drift . #e0.01)
    ;; (accepted <ratio> <date> "<the user's words>"), written only on the
    ;; user's acceptance of a figure over the line
    (accepted . #f)
    ;; DERIVED, printed and never judged: cd27dcf against dd506ee read
    ;; 0.7195 on 2026-10-16 (archive/theourgia-startup-cumulative-2026-10-16-
    ;; dd506ee), so "since cd27dcf" is that figure times this file's SC-5.
    (derived (cd27dcf-vs-dd506ee 0.7195 "2026-10-16"))))
(define (record-ref key) (cdr (assq key startup-record)))

;; ---- places ---------------------------------------------------------------------
(define (dir-of path)
  (let loop ((i (- (string-length path) 1)))
    (cond ((< i 0) ".") ((char=? (string-ref path i) #\/) (substring path 0 i)) (else (loop (- i 1))))))
(define (non-empty name) (let ((v (getenv name))) (and v (> (string-length v) 0) v)))
(define (has-build? root) (file-exists? (string-append root "/theourgia/build.ss")))
;; THE TREE'S LIBRARY ROOT: the first root of CHEZSCHEMELIBDIRS holding a
;; theourgia/build.ss (env.sh sets it to the run's library root).
(define tree-root
  (let loop ((rs (let split ((s (or (getenv "CHEZSCHEMELIBDIRS") "")) (acc '()))
                   (let ((i (let find ((k 0)) (cond ((= k (string-length s)) #f) ((char=? (string-ref s k) #\:) k) (else (find (+ k 1)))))))
                     (if i (split (substring s (+ i 1) (string-length s)) (cons (substring s 0 i) acc))
                         (reverse (cons s acc)))))))
    (cond ((null? rs) #f) ((has-build? (car rs)) (car rs)) (else (loop (cdr rs))))))
(define base-root (non-empty "THEOURGIA_BASE_LIBDIR"))
(define origin-root (non-empty "THEOURGIA_STARTUP_ORIGIN_LIBDIR"))
(unless base-root
  (printf "SKIP: THEOURGIA_BASE_LIBDIR is unset: set it to a library root holding the base tree's theourgia/ and igropyr/ to read the compiled start-up line (an opt-in, not a failure)~%")
  (printf "rows: 0~%0 failures~%startup-compiled complete~%")
  (exit 0))
(unless (and tree-root (has-build? base-root))
  (printf "NOT A READING: the tree (~s) and THEOURGIA_BASE_LIBDIR (~s) must each be a library root holding theourgia/build.ss~%" tree-root base-root)
  (printf "startup-compiled complete~%")
  (exit 2))
(define pid-text (number->string (get-process-id)))
(define scratch
  (string-append (or (non-empty "THEOURGIA_TEST_ROOT") "/tmp") "/startup-compiled-" pid-text))
(define (sh command) (system command))
(sh (string-append "rm -rf '" scratch "' && mkdir -p '" scratch "'"))

;; ---- the builds ---------------------------------------------------------------
;; -> (rc objects): ROOT built with its own build.ss into OUT
(define (build! root out)
  (let ((rc (sh (string-append "cd '" root "/theourgia' && scheme --script build.ss '" root "' '" out
                               "' > '" out ".log' 2>&1 < /dev/null"))))
    (list rc (count-objects out))))
(define (count-objects dir)
  (let ((f (string-append scratch "/count")))
    (sh (string-append "find '" dir "' -name '*.so' 2>/dev/null | wc -l > '" f "'"))
    (call-with-input-file f read)))
(define tree-obj (string-append scratch "/obj-tree"))
(define base-obj (string-append scratch "/obj-base"))
(define origin-obj (string-append scratch "/obj-origin"))
(define tree-built (build! tree-root tree-obj))
(define base-built (build! base-root base-obj))

;; ---- a start, timed -------------------------------------------------------------
(define (now-ms)
  (let ((t (current-time 'time-monotonic)))
    (+ (* 1000.0 (time-second t)) (/ (time-nanosecond t) 1e6))))
(define (start-command obj)
  (string-append "cd '" obj "/theourgia' && CHEZSCHEMELIBDIRS='" obj "' CHEZSCHEMELIBEXTS=.so scheme "
                 (if (file-exists? (string-append obj "/theourgia/core.so")) "--program core.so" "--script core.sc")
                 " no-such-verb > /dev/null 2>&1 < /dev/null"))
;; -> (ms . rc) of COUNT starts of OBJ one after another
(define (timed obj count)
  (let ((t0 (now-ms)))
    (let loop ((k count) (rc #f))
      (if (zero? k)
          (cons (- (now-ms) t0) rc)
          (loop (- k 1) (sh (start-command obj)))))))

;; ---- the alternation ------------------------------------------------------------
;; One run: PAIRS pairs, base first in even pairs and tree first in odd ones
;; (so neither arm always follows the other); TREE-STARTS starts make one
;; tree sample (2 is the doubled control). -> (base-ms tree-ms rcs order)
(define (run-pairs a b pairs tree-starts)
  (let loop ((i 0) (as '()) (bs '()) (rcs '()) (order '()))
    (if (= i pairs)
        (list (reverse as) (reverse bs) (reverse rcs) (reverse order))
        (let* ((a-first (even? i))
               (x (if a-first (timed a 1) (timed b tree-starts)))
               (y (if a-first (timed b tree-starts) (timed a 1)))
               (ta (if a-first x y)) (tb (if a-first y x)))
          (loop (+ i 1) (cons (car ta) as) (cons (car tb) bs) (cons* (cdr tb) (cdr ta) rcs)
                (cons (if a-first 'ab 'ba) order))))))
;; -> three runs
(define (alternation a b pairs tree-starts)
  (list (run-pairs a b pairs tree-starts) (run-pairs a b pairs tree-starts) (run-pairs a b pairs tree-starts)))

;; ---- the judge (pure) -----------------------------------------------------------
;; p10 as the gate's script read it: the sorted samples' element at
;; floor(0.1 x (n - 1)).
(define (p10 xs)
  (let ((v (list-sort < xs))) (list-ref v (exact (floor (* 1/10 (- (length v) 1)))))))
(define (median3 xs) (cadr (list-sort < xs)))
;; RUNS is a list of (base-samples tree-samples); -> (median ratios)
(define (judge runs)
  (let ((ratios (map (lambda (r) (/ (p10 (cadr r)) (p10 (car r)))) runs)))
    (list (median3 ratios) ratios)))
;; green when the median is within THRESHOLD
(define (colour median threshold)
  (cond ((not threshold) 'uncalibrated) ((<= median threshold) 'green) (else 'red)))
;; the cumulative line: within the line, or within an accepted figure plus
;; the drift; otherwise over, and the user's to rule. ACCEPTED is the
;; record's entry without its key: (<ratio> <date> "<the user's words>"), or #f.
(define (cumulative-colour median line accepted drift)
  (cond ((<= median line) 'green)
        ((and accepted (<= median (+ (car accepted) drift))) 'green-accepted)
        (else 'over-the-line)))
(define (decimal x) (/ (round (* 10000 (inexact x))) 10000))
(define (print-run label run)
  (printf "~a: p10 base ~a ms tree ~a ms ratio ~a\n" label (decimal (p10 (car run))) (decimal (p10 (cadr run)))
          (decimal (/ (p10 (cadr run)) (p10 (car run))))))

;; =============================================================================
(printf "== SC-0: the builds ==\n")
(printf "tree ~a -> ~a objects; base ~a -> ~a objects\n" tree-root (cadr tree-built) base-root (cadr base-built))
(want "SC-0 the tree and its base each built with their own build.ss into objects"
      (list (car tree-built) (> (cadr tree-built) 0) (car base-built) (> (cadr base-built) 0))
      '(0 #t 0 #t))

(printf "== SC-2: the judge on fixed samples ==\n")
(define (n-of n x) (make-list n x))
(want "SC-2 the judge: each run's p10 ratio, the median of three; at the threshold green, just over it red, no threshold uncalibrated"
      (let ((runs (list (list (n-of 40 100) (n-of 40 101)) (list (n-of 40 100) (n-of 40 99)) (list (n-of 40 100) (n-of 40 102)))))
        (let ((j (judge runs)))
          (list (car j) (cadr j) (colour (car j) #e1.01) (colour (car j) #e1.0099) (colour (car j) #f))))
      (list 101/100 (list 101/100 99/100 102/100) 'green 'red 'uncalibrated))
(want "SC-2 p10 is the element at floor(0.1 x (n - 1)) of the sorted samples, so one fast outlier in forty is not the reading"
      (list (p10 (cons 1 (n-of 39 100))) (p10 (append (n-of 4 1) (n-of 36 100))) (p10 (append (n-of 5 1) (n-of 35 100))))
      (list 100 1 1))
(want "SC-2 the cumulative judge: within the line green; over it, green only within an accepted figure plus the drift; otherwise over the line"
      (list (cumulative-colour #e1.005 #e1.01 #f #e0.01)
            (cumulative-colour #e1.02 #e1.01 #f #e0.01)
            (cumulative-colour #e1.02 #e1.01 (list #e1.015 "d" "w") #e0.01)
            (cumulative-colour #e1.03 #e1.01 (list #e1.015 "d" "w") #e0.01))
      (list 'green 'over-the-line 'green-accepted 'over-the-line))

(printf "== SC-1: the step, the tree against its base, compiled ==\n")
(define step (alternation base-obj tree-obj 40 1))
(for-each (lambda (r k) (print-run (format "SC-1 run ~a" k) r)) step '(1 2 3))
(define step-judge (judge step))
(printf "SC-1 median ~a, threshold ~a\n" (decimal (car step-judge)) (record-ref 'step-threshold))
(want "SC-1 the median of three p10 ratios, tree over base, compiled, is within the recorded step threshold"
      (colour (car step-judge) (record-ref 'step-threshold))
      'green)
(define control (alternation base-obj tree-obj 10 2))
(define control-judge (judge control))
(printf "SC-1 CONTROL median ~a (the tree arm doubled)\n" (decimal (car control-judge)))
(want "SC-1 CONTROL: with the tree arm doubled (two starts per sample, the answer unchanged) the step is not green"
      (eq? (colour (car control-judge) (or (record-ref 'step-threshold) #e1.01)) 'green)
      #f)

(printf "== SC-3 and SC-4: what ran ==\n")
;; A COPY OF THE TREE'S OBJECT ROOT WITHOUT rpc.so, which core imports: a
;; start that read sources would still start; one that reads objects does
;; not find the library.
(define broken-obj (string-append scratch "/obj-broken"))
(sh (string-append "cp -R '" tree-obj "' '" broken-obj "' && rm -f '" broken-obj "/theourgia/rpc.so'"))
(define normal-rc (cdr (timed tree-obj 1)))
(define broken-rc (cdr (timed broken-obj 1)))
(want "SC-3 a start from the tree's objects exits as no-such-verb exits, and one from a copy missing rpc.so does not start: the arms run objects"
      (list normal-rc (= broken-rc normal-rc))
      (list 1 #f))
(want "SC-4 every timed start exited as no-such-verb exits, and the pairs alternated which arm went first"
      (let ((all (append (apply append (map caddr step)) (apply append (map caddr control))))
            (orders (apply append (map cadddr step))))
        (list (for-all (lambda (rc) (eqv? rc normal-rc)) all)
              (length (filter (lambda (o) (eq? o 'ab)) orders))
              (length (filter (lambda (o) (eq? o 'ba)) orders))))
      (list #t 60 60))

(printf "== SC-5: the cumulative line, from the origin ~a ==\n" (record-ref 'origin))
(if (not origin-root)
    (printf "SKIP SC-5: THEOURGIA_STARTUP_ORIGIN_LIBDIR is unset: set it to a library root holding the origin ~a's theourgia/ and igropyr/ (the gate archives it beside the base)\n" (record-ref 'origin))
    (let* ((built (build! origin-root origin-obj))
           (runs (alternation origin-obj tree-obj 40 1))
           (j (judge runs)))
      (for-each (lambda (r k) (print-run (format "SC-5 run ~a" k) r)) runs '(1 2 3))
      (printf "SC-5 median ~a against the line ~a; derived since cd27dcf ~a (cd27dcf against dd506ee ~a on ~a, times this)\n"
              (decimal (car j)) (record-ref 'cumulative-line)
              (decimal (* (cadr (assq 'cd27dcf-vs-dd506ee (record-ref 'derived))) (car j)))
              (cadr (assq 'cd27dcf-vs-dd506ee (record-ref 'derived)))
              (caddr (assq 'cd27dcf-vs-dd506ee (record-ref 'derived))))
      (want "SC-5 the origin built from its own build.ss"
            (list (car built) (> (cadr built) 0))
            '(0 #t))
      (want "SC-5 the tree against the origin dd506ee, compiled, is within the cumulative line (over it: the user rules, accept and record, reduce and re-gate, or a budget; not a defect to fix on sight)"
            (cumulative-colour (car j) (record-ref 'cumulative-line) (record-ref 'accepted) (record-ref 'drift))
            (if (record-ref 'accepted) 'green-accepted 'green))))

;; THE A/A READING, for the record: the base against itself, three times.
(when (non-empty "THEOURGIA_STARTUP_AA")
  (let* ((medians (map (lambda (k) (car (judge (alternation base-obj base-obj 40 1)))) '(1 2 3)))
         (deviation (apply max (map (lambda (m) (abs (- m 1))) medians))))
    (printf "A/A medians ~a; threshold 1 + max(0.01, 2 x ~a) = ~a\n"
            (map decimal medians) (decimal deviation) (decimal (+ 1 (max #e0.01 (* 2 deviation)))))))

(sh (string-append "rm -rf '" scratch "'"))
(printf "\n~a failures\nrows: ~a\nstartup-compiled complete\n" bad rows)
(exit (if (= bad 0) 0 1))
