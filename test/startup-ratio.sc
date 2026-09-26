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

;; F54: THE RELATIVE STARTUP INSTRUMENT (design core/briefs/f54-relative-
;; startup-instrument.md, v14; task core/briefs/f54-task.md).
;;
;; F0-2 compares the command line's start with an absolute budget in
;; milliseconds, and the same tree reads differently by the minute. This row
;; asserts a RATIO OF MINIMA instead: ten pairs, interleaved, of arm A (this
;; tree's `core.sc no-such-verb --wire`, as F0-2) and arm B (a checked-in
;; reference program over a checked-in reference library of the same kind
;; of work, test/f54-reference-main.ss and test/f54-reference.sc), and
;; min(A) / min(B) <= K. The statistics are one pure function, f54-judge,
;; pinned by fixed samples (C4); K is a RECORDED constant (C4b), never
;; recomputed from the run.
;;
;; A RED IS READ, NOT BELIEVED (design D2): the row prints both minima,
;; spreads and plateaus, and a red is confirmed by one alternation of the
;; previous tree before it is believed. The machine states under which the
;; row can colour an unchanged tree red, and those D4's rules turn into
;; `unreadable` only in their gross forms, are the design's D2 list.
;;
;; THE CALIBRATION RECORD is the `f54-calibration` form below, read as data
;; by C4b: r0 (the first quiet ratio on this machine), e-quiet (the largest
;; |r_i / r0 - 1| over the quiet ratios of the calibration runs), b (min(B)
;; of the first quiet run), K = r0 x (1 + max(0.15, 2 e-quiet)), e-load (the
;; largest |rR - 1| of the three C3 runs, information only), the reference
;; library's md5 and line count, arm A's closure line count at calibration
;; (information only), when, the machine, and the load before each run.
;; While any value K depends on is #f, or the reference library's md5 on
;; disk is not the recorded one, C1 answers `uncalibrated`, a FAIL: a changed
;; yardstick invalidates r0 (ruling Q8).
;;
;; AMENDMENT A-F54-1 (main session, 2026-09-26, to design v14). The design
;; widened K by the residual C3 measures UNDER LOAD; with the first
;; calibration that K (3.660) sat above every reading of the heavy tree
;; (3.58-3.65), so C2 could never go red. But the design already answers
;; load with the alternation rule, so K only has to absorb the noise of
;; QUIET runs: m = max(0.15, 2 e-quiet). K does not use e-load; a red under
;; load is confirmed by a run alone (the alternation rule). And C3 becomes
;; two comparisons per run, both of minima: the load is visible (rA >= 1.25),
;; and the ratio absorbs at least half of what the load does to the arm
;; (|rR - 1| <= (rA - 1) / 2). Its former fixed bound, |rR - 1| <= 0.10, is
;; withdrawn. The plateau and spread rules colour the QUIET run only; the
;; loaded pass is read by its minima, and its plateaus and spreads are
;; printed as information, never as a refusal.
;;
;; BEFORE CALIBRATION, FIVE ROWS ARE RED BY DESIGN, and each says
;; `uncalibrated` in its reading, so a run before the record is filled can be
;; told from a defect: C1 (the row), C2 (the heavy tree over K), C4b (K's
;; derivation on the recorded values), C5 (the reference's recorded size and
;; md5) and C5b's source md5. Any other red, or one of these five reading
;; something other than `uncalibrated`, is a defect. And with
;; THEOURGIA_F54_SKIP_LOAD=1 -- alone runs only; the suite never sets it --
;; C3's three rows read `load-not-granted`, a red: the loaded pass makes the
;; machine fully busy, and is run only on a grant.
;;
;; ENVIRONMENT READ: THEOURGIA_TEST_ROOT (where the copies go),
;; THEOURGIA_F54_SKIP_LOAD (above), CHEZSCHEMELIBDIRS (arm A's library
;; directories and the igropyr/ the tree copies link to). CHEZSCHEMELIBEXTS
;; is SET for every start the launcher makes, never inherited.
;;
;; THE CELLS. C1 the row; C2 a heavy copy of the tree (core.sc's import form
;; gains (theourgia store) and (theourgia daemon)) reads a ratio over K; C3
;; the load this file makes is visible, and the ratio absorbs at least half
;; of it (amendment A-F54-1); C4 the judge on fixed
;; samples; C4b K recorded and its derivation; C5 the reference's imports,
;; size and md5; C5b both arms read source even when the inherited
;; environment would load a planted compiled object; C6 the order of the
;; starts. At the end, no .so is under test/.

(import (chezscheme))

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

;; ---- the calibration record (read as data by C4b) ---------------------------
;;
;; Numbers are written exact (rationals, or #e...), so K's derivation and
;; every boundary compare exactly. As decimals: r0 2.6166 (662 / 253),
;; e-quiet 0.0409 (the c3-2 quiet pass, 650 / 259), K 3.0091, e-load 0.1994
;; (c3-1).
(define f54-calibration
  '((r0 . 662/253)
    (e-quiet . 3504/85729)
    (b . 253)
    (k . 331/110)
    (e-load . 1163/5832)
    (reference-md5 . "ce3c66061800322dcbaa4f981a76be1d")
    (reference-main-md5 . "519e60e21843efef108e04713f9dd96a")
    (reference-lines . 29012)
    (closure-lines . 28935)
    (taken . "2026-09-26 09:00-09:09 CEST, base 378d85b; the first set (08:52-08:56) overlapped another line's runs and was retaken")
    (machine . "Mac16,7, 14 logical cores (hw.ncpu), macOS 26.3.1, Chez Scheme 10.1.0")
    (load . "1-minute load before each run 4.38, 4.06, 4.58, 4.91; scheme processes 0, node processes 2")))
(define (cal-ref key) (cdr (assq key f54-calibration)))

;; ---- places ------------------------------------------------------------------
(define script-dir
  (let* ((argv0 (car (command-line)))
         (cut (let loop ((i (- (string-length argv0) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref argv0 i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring argv0 0 cut) ".")))
(define root (string-append script-dir "/.."))
(define test-dir script-dir)
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define scratch (string-append scratch-base "/f54-" (number->string (get-process-id))))
(define (sh cmd) (system cmd))
(define (sh-out cmd)
  (let* ((p (process cmd))
         (s (get-string-all (car p))))
    (close-port (car p))
    (close-port (cadr p))
    (if (eof-object? s) "" s)))
(define (first-line s)
  (let loop ((i 0))
    (cond ((= i (string-length s)) s)
          ((char=? (string-ref s i) #\newline) (substring s 0 i))
          (else (loop (+ i 1))))))
(define (md5-of path)
  (first-line (sh-out (string-append "md5 -q '" path "' 2>/dev/null || md5sum '" path "' | cut -c1-32"))))
(define (lines-of path)
  (let ((s (call-with-input-file path get-string-all)))
    (let loop ((i 0) (n 0))
      (if (= i (string-length s))
          n
          (loop (+ i 1) (if (char=? (string-ref s i) #\newline) (+ n 1) n))))))
(define (exists? path) (file-exists? path))
(define (decimal x) (/ (round (* 10000 (inexact x))) 10000.))
(sh (string-append "rm -rf " scratch " && mkdir -p " scratch))

;; THE LIBRARY DIRECTORY THAT HOLDS igropyr/, for the tree copies: the
;; first entry of CHEZSCHEMELIBDIRS with an igropyr/ in it.
(define igropyr-parent
  (let loop ((dirs (let split ((s (or (getenv "CHEZSCHEMELIBDIRS") "")) (acc '()))
                     (let ((i (let find ((j 0)) (cond ((= j (string-length s)) #f)
                                                      ((char=? (string-ref s j) #\:) j)
                                                      (else (find (+ j 1)))))))
                       (if i
                           (split (substring s (+ i 1) (string-length s)) (cons (substring s 0 i) acc))
                           (reverse (cons s acc)))))))
    (cond ((null? dirs) "/no-library-directory-holds-igropyr")
          ((file-directory? (string-append (car dirs) "/igropyr")) (car dirs))
          (else (loop (cdr dirs))))))

;; ---- the judge (D4), a pure function ----------------------------------------
;;
;; (f54-judge a b cal) -> (colour ratio minima spreads plateaus)
;; a, b: the two arms' samples in milliseconds; cal: an alist with k and b
;; (the recorded K and the recorded minimum of B). colour is green, red,
;; unreadable, or uncalibrated when k or b is #f. Every sample is kept.
(define (upper-median xs)
  (let ((v (list-sort < xs)))
    (list-ref v (div (length v) 2))))
(define (arm-min xs) (apply min xs))
(define (spread xs) (/ (upper-median xs) (arm-min xs)))
(define (plateau xs)
  (let ((m (arm-min xs)))
    (length (filter (lambda (x) (<= (* 10 x) (* 11 m))) xs))))
(define (drift-inside? min-b recorded-b)
  (and (>= (* 11 min-b) (* 10 recorded-b))
       (<= (* 10 min-b) (* 11 recorded-b))))
(define (f54-judge a b cal)
  (let* ((min-a (arm-min a))
         (min-b (arm-min b))
         (ratio (/ min-a min-b))
         (spreads (list (spread a) (spread b)))
         (plateaus (list (plateau a) (plateau b)))
         (k (cdr (assq 'k cal)))
         (recorded-b (cdr (assq 'b cal)))
         (colour (cond ((not (and k recorded-b)) 'uncalibrated)
                       ((not (and (<= (car spreads) 3/2) (<= (cadr spreads) 3/2)
                                  (>= (car plateaus) 3) (>= (cadr plateaus) 3)
                                  (drift-inside? min-b recorded-b)))
                        'unreadable)
                       ((<= ratio k) 'green)
                       (else 'red))))
    (list colour ratio (list min-a min-b) spreads plateaus)))
(define (derive-k r0 e-quiet) (* r0 (+ 1 (max 3/20 (* 2 e-quiet)))))
;; C3's two comparisons (amendment A-F54-1): the load is visible, and the
;; ratio absorbs at least half of what the load does to arm A.
(define (load-visible? ra) (>= ra 5/4))
(define (load-absorbed? rr ra) (<= (abs (- rr 1)) (/ (- ra 1) 2)))

;; ---- the launcher (D4, C5b, C6) -----------------------------------------------
;;
;; An arm is (library-directories . script-and-arguments). Each start is
;; timed around one `scheme --script`, and its environment is set HERE, for
;; both arms: CHEZSCHEMELIBEXTS the suite's source extensions only, so a
;; compiled object beside a source is never loaded whatever the caller's
;; environment says (C5b). The order is A B on even pairs and B A on odd
;; ones. THE TRACE IS WHAT HAPPENED, NOT WHAT WAS MEANT (F54 review r1):
;; each start appends its arm's label to `started` at the moment it starts,
;; and the trace file (C6) is that list, so a launcher that swapped a pair's
;; starts writes the swapped order. Answers the two arms' samples in pair
;; order and arm B's exit statuses.
(define source-exts ".sc::.sls::.scm")
(define started '())
(define (start-ms arm label)
  (set! started (cons label started))
  (let* ((t0 (real-time))
         (status (system (string-append "CHEZSCHEMELIBDIRS='" (car arm) "' CHEZSCHEMELIBEXTS='" source-exts
                                        "' scheme --script " (cdr arm) " > /dev/null 2>&1"))))
    (cons (- (real-time) t0) status)))
(define (run-pairs arm-a arm-b pairs trace-path)
  (set! started '())
  (let loop ((i 0) (a '()) (b '()) (b-status '()))
    (if (= i pairs)
        (begin
          (call-with-output-file trace-path
            (lambda (p) (for-each (lambda (s) (write s p) (newline p)) (reverse started)))
            'replace)
          (values (reverse a) (reverse b) (reverse b-status)))
        (if (even? i)
            (let* ((ra (start-ms arm-a 'A)) (rb (start-ms arm-b 'B)))
              (loop (+ i 1) (cons (car ra) a) (cons (car rb) b) (cons (cdr rb) b-status)))
            (let* ((rb (start-ms arm-b 'B)) (ra (start-ms arm-a 'A)))
              (loop (+ i 1) (cons (car ra) a) (cons (car rb) b) (cons (cdr rb) b-status)))))))
(define (expected-order pairs)
  (let loop ((i 0) (acc '()))
    (if (= i pairs) (reverse acc) (loop (+ i 1) (append (if (even? i) '(B A) '(A B)) acc)))))

(define arm-a (cons (getenv "CHEZSCHEMELIBDIRS") (string-append root "/core.sc no-such-verb --wire")))
(define arm-b (cons test-dir (string-append test-dir "/f54-reference-main.ss")))
(define reference-source (string-append test-dir "/f54-reference.sc"))
(define reference-md5-now (md5-of reference-source))
;; THE PROGRAM IS PINNED TOO (F54 review r2): C5 reads import forms, and a
;; program that kept its import but loaded a tree file by path would reach a
;; tree library through arm B; its md5 in the record makes any change to it
;; `uncalibrated`, as a changed library does.
(define reference-main (string-append test-dir "/f54-reference-main.ss"))
(define reference-main-md5-now (md5-of reference-main))
(define calibrated
  (and (cal-ref 'r0) (cal-ref 'e-quiet) (cal-ref 'b) (cal-ref 'k)
       (equal? (cal-ref 'reference-md5) reference-md5-now)
       (equal? (cal-ref 'reference-main-md5) reference-main-md5-now)))
(define judge-record
  (if calibrated f54-calibration (list (cons 'k #f) (cons 'b #f))))
(define (report label j)
  (printf "~a: colour ~a ratio ~a minima ~a spreads ~a plateaus ~a\n"
          label (car j) (decimal (cadr j)) (caddr j)
          (map decimal (cadddr j)) (list-ref j 4)))

;; ---- C1 and C6: the row -------------------------------------------------------
(define trace-path (string-append scratch "/order.trace"))
(define-values (quiet-a quiet-b quiet-b-status) (run-pairs arm-a arm-b 10 trace-path))
(define quiet (f54-judge quiet-a quiet-b judge-record))
(report "C1 quiet" quiet)
(printf "C1 samples A ~a B ~a\n" quiet-a quiet-b)
(unless calibrated
  (printf "C1 uncalibrated: r0 would be ~a (min A ~a / min B ~a); reference md5 on disk ~a, recorded ~a\n"
          (decimal (cadr quiet)) (car (caddr quiet)) (cadr (caddr quiet))
          reference-md5-now (cal-ref 'reference-md5)))
(unless (equal? (cal-ref 'reference-main-md5) reference-main-md5-now)
  (printf "C1 uncalibrated: the reference program's md5 on disk ~a, recorded ~a\n"
          reference-main-md5-now (cal-ref 'reference-main-md5)))
(want "C1 arm B's starts all exit 0 (the reference program ran)"
      (filter (lambda (s) (not (eqv? s 0))) quiet-b-status)
      '())
(want "C1 f54-judge on ten interleaved pairs answers green"
      (car quiet)
      'green)
(want "C6 the starts were interleaved, A B then B A, never all A then all B"
      (call-with-input-file trace-path
        (lambda (p) (let loop ((acc '())) (let ((x (read p))) (if (eof-object? x) (reverse acc) (loop (cons x acc)))))))
      (expected-order 10))

;; ---- C2: the heavy tree reads over K -----------------------------------------
;;
;; A copy of the tree (without test/) whose core.sc imports (theourgia store)
;; and (theourgia daemon) eagerly: R4's shape, the heavy libraries back in
;; the static closure. On 378d85b store is already in the closure (through
;; rpc and working): the growth is daemon's, 8 files and 13,488 lines over
;; a closure of 38 files and 28,935 lines (a walk of the import forms).
(define heavy-lib (string-append scratch "/heavy"))
(define heavy-tree (string-append heavy-lib "/theourgia"))
(define core-import-head "(import (chezscheme) (theourgia rpc) (theourgia arguments) (theourgia render)")
(define heavy
  (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e))))
    (sh (string-append "mkdir -p " heavy-tree " && (cd " root " && tar cf - --exclude=./test --exclude=./.git .) | (cd " heavy-tree " && tar xf -)"
                       " && ln -s " igropyr-parent "/igropyr " heavy-lib "/igropyr"))
    (let* ((core (string-append heavy-tree "/core.sc"))
           (text (call-with-input-file core get-string-all))
           (n (string-length core-import-head))
           (at (let find ((i 0))
                 (cond ((> (+ i n) (string-length text)) #f)
                       ((string=? (substring text i (+ i n)) core-import-head) i)
                       (else (find (+ i 1)))))))
      (unless at (raise 'core-import-form-not-found))
      (call-with-output-file core
        (lambda (p)
          (put-string p (substring text 0 (+ at n)))
          (put-string p " (theourgia store) (theourgia daemon)")
          (put-string p (substring text (+ at n) (string-length text))))
        'replace)
      (let-values (((a b s) (run-pairs (cons heavy-lib (string-append heavy-tree "/core.sc no-such-verb --wire"))
                                       arm-b 10 (string-append scratch "/heavy.trace"))))
        (f54-judge a b judge-record)))))
(when (pair? heavy) (unless (eq? (car heavy) 'RAISED) (report "C2 heavy" heavy)))
;; NOT GREEN AS WELL AS OVER K: the ratio is compared with the recorded K,
;; and the judge's own colour must not be green, so a judge that takes K from
;; anything but the record it was given (the run's own samples, say) is red
;; here -- the design's C4b mutant. Unreadable is not green: a noisy heavy
;; run is read, not coloured.
(want "C2 a copy whose core.sc imports store and daemon eagerly reads a ratio over K, and the judge does not colour it green"
      (cond ((not calibrated) 'uncalibrated)
            ((eq? (car heavy) 'RAISED) heavy)
            ((and (> (cadr heavy) (cal-ref 'k)) (not (eq? (car heavy) 'green))) 'over-k)
            (else (list 'ratio (decimal (cadr heavy)) 'k (decimal (cal-ref 'k)) 'colour (car heavy))))
      'over-k)

;; ---- C3: the residual under a CPU load this file makes -----------------------
;;
;; One busy loop per logical core (hw.ncpu), each a `scheme --script` started
;; here, recorded by pid, and stopped by pid on every exit path. The quiet
;; pass is C1's. The ratios compared are the ones f54-judge RETURNS, not
;; quotients computed here: a judge with a fixed denominator reads rR = rA,
;; and |rA - 1| is never within (rA - 1) / 2 once rA >= 1.25, so it is red.
;; The loaded pass's colour, plateaus and spreads are printed, not judged.
(define busy-count
  (string->number (first-line (sh-out "sysctl -n hw.ncpu 2>/dev/null || nproc"))))
(define busy-script (string-append scratch "/busy.ss"))
(define busy-pids (string-append scratch "/busy.pids"))
(call-with-output-file busy-script (lambda (p) (write '(let loop () (loop)) p)) 'replace)
(define (busy-pid-list)
  (if (exists? busy-pids)
      (call-with-input-file busy-pids
        (lambda (p) (let loop ((acc '())) (let ((x (read p))) (if (eof-object? x) (reverse acc) (loop (cons x acc)))))))
      '()))
(define (stop-busy!)
  (for-each (lambda (pid) (sh (string-append "kill " (number->string pid) " 2>/dev/null"))) (busy-pid-list)))
(define skip-load (equal? (getenv "THEOURGIA_F54_SKIP_LOAD") "1"))
(define c3-reading #f)
(define loaded
  (if skip-load
      'load-not-granted
  (dynamic-wind
    (lambda ()
      (sh (string-append "i=0; while [ $i -lt " (number->string busy-count) " ]; do"
                         " scheme --script " busy-script " > /dev/null 2>&1 & echo $! >> " busy-pids "; i=$((i+1)); done")))
    (lambda ()
      (let ((alive (length (filter (lambda (pid) (= 0 (sh (string-append "kill -0 " (number->string pid) " 2>/dev/null"))))
                                   (busy-pid-list)))))
        (let-values (((a b s) (run-pairs arm-a arm-b 10 (string-append scratch "/loaded.trace"))))
          (list alive a b (f54-judge a b judge-record)))))
    stop-busy!)))
(unless skip-load
  (let* ((loaded-judge (list-ref loaded 3))
         (r-a (/ (arm-min (list-ref loaded 1)) (arm-min quiet-a)))
         (r-r (/ (cadr loaded-judge) (cadr quiet))))
    (report "C3 loaded" loaded-judge)
    (printf "C3 busy loops ~a of ~a alive; rA ~a rR ~a e ~a\n"
            (car loaded) busy-count (decimal r-a) (decimal r-r) (decimal (abs (- r-r 1))))
    (set! c3-reading (list (car loaded) (load-visible? r-a) (load-absorbed? r-r r-a)))))
(want "C3 the load was running: one busy loop per core, all alive during the loaded pass"
      (if skip-load 'load-not-granted (car c3-reading))
      busy-count)
(want "C3 the load moved A (rA >= 1.25) and the ratio absorbed at least half of it (|rR - 1| <= (rA - 1) / 2)"
      (if skip-load 'load-not-granted (cdr c3-reading))
      '(#t #t))
(want "C3 every busy loop is stopped"
      (if skip-load
          'load-not-granted
          (filter (lambda (pid) (= 0 (sh (string-append "kill -0 " (number->string pid) " 2>/dev/null"))))
                  (busy-pid-list)))
      '())

;; ---- C4: the judge on fixed samples -------------------------------------------
(define (n-of n x) (make-list n x))
(define (cal k b) (list (cons 'r0 1) (cons 'e #e0.05) (cons 'k k) (cons 'b b)))
(define (colour-of a b c) (car (f54-judge a b c)))
(want "C4 K exactly at the ratio is green" (colour-of (n-of 10 120) (n-of 10 100) (cal #e1.20 100)) 'green)
(want "C4 K just under the ratio is red" (colour-of (n-of 10 120) (n-of 10 100) (cal #e1.1999 100)) 'red)
(want "C4 retention: four 100s and six 200s is unreadable (no sample dropped)"
      (colour-of (append (n-of 4 100) (n-of 6 200)) (n-of 10 100) (cal 3 100)) 'unreadable)
(want "C4 a median exactly 1.5 x the minimum answers a colour"
      (colour-of (append (n-of 5 100) (n-of 5 150)) (n-of 10 100) (cal 3 100)) 'green)
(want "C4 a median exactly 1.5 x the minimum answers a colour, for arm B too"
      (colour-of (n-of 10 100) (append (n-of 5 100) (n-of 5 150)) (cal 3 100)) 'green)
(want "C4 the uncalibrated branch: no K or recorded B answers uncalibrated, never a colour"
      (colour-of (n-of 10 100) (n-of 10 100) (list (cons 'k #f) (cons 'b #f)))
      'uncalibrated)
;; THE DENOMINATOR IS min(B), NOT THE RECORDED ONE (F54 review r1): B's
;; minimum 240 is inside the drift band around 253, and the quotient of the
;; minima 740/240 = 37/12 is over K; a denominator of 253 would read green.
(want "C4 the quotient's denominator is this run's min(B) when it differs from the recorded b: 740/240, red"
      (list-head (f54-judge (n-of 10 740) (n-of 10 240) (list (cons 'r0 1) (cons 'e-quiet 0) (cons 'k 331/110) (cons 'b 253))) 2)
      (list 'red 37/12))
(want "C4 the spreads returned are median / minimum per arm"
      (list-ref (f54-judge (append (n-of 4 100) (n-of 6 200)) (append (n-of 5 100) (n-of 5 150)) (cal 3 100)) 3)
      (list 2 3/2))
(want "C4 the quotient is of minima: three 100s and seven 140s over ten 100s is 1.00, green"
      (list-head (f54-judge (append (n-of 3 100) (n-of 7 140)) (n-of 10 100) (cal #e1.20 100)) 2)
      (list 'green 1))
(want "C4 the mirrored vector pins the denominator"
      (list-head (f54-judge (n-of 10 100) (append (n-of 3 100) (n-of 7 140)) (cal #e1.20 100)) 2)
      (list 'green 1))
(define (plateau-b tail) (colour-of (n-of 10 500) (append '(500) tail) (cal 3 500)))
(define (plateau-a tail) (colour-of (append '(500) tail) (n-of 10 500) (cal 3 500)))
(want "C4 plateau B: 500 and nine 650s is unreadable" (plateau-b (n-of 9 650)) 'unreadable)
(want "C4 plateau B: 500 540 549 answers a colour" (plateau-b (append '(540 549) (n-of 7 650))) 'green)
(want "C4 plateau B: 500 540 550 answers a colour (the window is inclusive)" (plateau-b (append '(540 550) (n-of 7 650))) 'green)
(want "C4 plateau B: 500 540 551 is unreadable" (plateau-b (append '(540 551) (n-of 7 650))) 'unreadable)
(want "C4 plateau A: 500 and nine 650s is unreadable" (plateau-a (n-of 9 650)) 'unreadable)
(want "C4 plateau A: 500 540 549 answers a colour" (plateau-a (append '(540 549) (n-of 7 650))) 'green)
(want "C4 plateau A: 500 540 550 answers a colour" (plateau-a (append '(540 550) (n-of 7 650))) 'green)
(want "C4 plateau A: 500 540 551 is unreadable" (plateau-a (append '(540 551) (n-of 7 650))) 'unreadable)
(define (drift minb rec) (colour-of (n-of 10 minb) (n-of 10 minb) (cal 3 rec)))
(want "C4 drift: 137 against recorded 150 is readable" (drift 137 150) 'green)
(want "C4 drift: 136 against recorded 150 is unreadable" (drift 136 150) 'unreadable)
(want "C4 drift: 165 against recorded 150 is readable" (drift 165 150) 'green)
(want "C4 drift: 166 against recorded 150 is unreadable" (drift 166 150) 'unreadable)
(want "C4 drift: 100 against recorded 110 (= 110 / 1.10) is readable" (drift 100 110) 'green)
(want "C4 drift: 99 against recorded 110 is unreadable" (drift 99 110) 'unreadable)
(want "C4 drift: 110 against recorded 100 (= 100 x 1.10) is readable" (drift 110 100) 'green)
(want "C4 drift: 111 against recorded 100 is unreadable" (drift 111 100) 'unreadable)
(want "C4 the upper median: four 100s, 140, five 160s is unreadable"
      (colour-of (append (n-of 4 100) '(140) (n-of 5 160)) (n-of 10 100) (cal #e1.15 100)) 'unreadable)
(want "C4 the upper median, mirrored for B"
      (colour-of (n-of 10 100) (append (n-of 4 100) '(140) (n-of 5 160)) (cal #e1.15 100)) 'unreadable)
(want "C4 the quotient of the two minima, not the minimum of paired quotients: 1.40, red"
      (list-head (f54-judge (append (n-of 3 140) (n-of 7 196))
                            (append (n-of 3 140) (n-of 3 100) (n-of 4 140))
                            (list (cons 'r0 1) (cons 'e #e0.10) (cons 'k #e1.20) (cons 'b 100)))
                 2)
      (list 'red 7/5))
(want "C4 C3's comparisons at their equality: rA 1.25 visible and 1.2499 not; at rA 1.5, rR 1.25 and 0.75 absorbed, 1.2501 and 0.7499 not"
      (list (load-visible? #e1.25) (load-visible? #e1.2499)
            (map (lambda (rr) (load-absorbed? rr #e1.5)) (list #e1.25 #e1.2501 #e0.75 #e0.7499)))
      '(#t #f (#t #f #t #f)))

;; ---- C4m: the judge against an independent model, on generated vectors -------
;;
;; F54 review r1 and r2 each found judge rules that no fixed vector pinned (the
;; denominator, the uncalibrated branch, arm B's noise edge; the precedence of
;; unreadable over red, the median's sort, arm B's retention). So the rules are
;; also checked the way scope-model.sc checks the scope rules: a SECOND judge,
;; written differently -- an insertion sort, loops that count, the rules as an
;; ordered list of tests in the design's D4 order -- and a seeded generator of
;; sample vectors, and the two must return the same whole value (colour ratio
;; minima spreads plateaus) for every vector. The C4 rows above stay as the
;; readable examples. (Main session, r3: the closing round for judge-rule
;; coverage; a further gap of that class is queue item F109.)
;;
;; THE GENERATOR: its own linear congruential generator, seed 20260926, 600
;; vectors, each arm's ten samples SHUFFLED. Boundaries it aims at: an arm's
;; minimum m a multiple of 10 (B's of 110), so the plateau edge 11m/10 and the
;; noise edge 3m/2 are exact integers and samples are drawn from m, 11m/10,
;; 11m/10 + 1, 3m/2, 3m/2 + 1, 2m and above; the recorded b at min(B), at the
;; two drift edges (min(B) x 10/11 and x 11/10) and one past each; K at the
;; ratio, 1/1000 under and over it, twice and half of it; K and b missing.
;; The coverage row names every outcome met, by the rule that decided it in
;; the model: uncalibrated, arm A's and arm B's spread, plateau, drift, red,
;; green, red hidden by unreadable, and on each arm a sample list whose sixth
;; in acquisition order is not its upper median, and samples at or above
;; twice the minimum.
(define (model-judge a b cal)
  (define (insert x sorted)
    (cond ((null? sorted) (list x))
          ((<= x (car sorted)) (cons x sorted))
          (else (cons (car sorted) (insert x (cdr sorted))))))
  (define (sort-up xs) (fold-left (lambda (acc x) (insert x acc)) '() xs))
  (define (count pred xs)
    (let loop ((xs xs) (n 0)) (if (null? xs) n (loop (cdr xs) (if (pred (car xs)) (+ n 1) n)))))
  (let* ((sa (sort-up a)) (sb (sort-up b))
         (ma (car sa)) (mb (car sb))
         (med-a (list-ref sa (quotient (length sa) 2)))
         (med-b (list-ref sb (quotient (length sb) 2)))
         (spread-a (/ med-a ma)) (spread-b (/ med-b mb))
         (plat-a (count (lambda (x) (<= x (* ma 11/10))) a))
         (plat-b (count (lambda (x) (<= x (* mb 11/10))) b))
         (k (cdr (assq 'k cal)))
         (rec (cdr (assq 'b cal)))
         (ratio (/ ma mb))
         (rules
          (list (cons (lambda () (not (and k rec))) 'uncalibrated)
                (cons (lambda () (> spread-a 3/2)) '(spread a))
                (cons (lambda () (> spread-b 3/2)) '(spread b))
                (cons (lambda () (< plat-a 3)) '(plateau a))
                (cons (lambda () (< plat-b 3)) '(plateau b))
                (cons (lambda () (or (< mb (/ rec 11/10)) (> mb (* rec 11/10)))) 'drift)
                (cons (lambda () (> ratio k)) 'red)
                (cons (lambda () #t) 'green)))
         (decided (cdr (find (lambda (r) ((car r))) rules)))
         (colour (cond ((eq? decided 'uncalibrated) 'uncalibrated)
                       ((memq decided '(red green)) decided)
                       (else 'unreadable))))
    (values (list colour ratio (list ma mb) (list spread-a spread-b) (list plat-a plat-b))
            decided)))
(define model-seed 20260926)
(define model-count 600)
(define model-state model-seed)
(define (model-pick n)
  (set! model-state (mod (+ (* model-state 1103515245) 12345) 2147483648))
  (mod (quotient model-state 65536) n))
(define (model-choose xs) (list-ref xs (model-pick (length xs))))
;; BY INDEX: exactly the one drawn element leaves the list, so equal samples
;; all stay (an earlier version removed every equal value, and arms came out
;; shorter than ten -- found by the paired-quotient mutant, F54 r3).
(define (model-shuffle xs)
  (let loop ((xs xs) (out '()))
    (if (null? xs)
        out
        (let ((i (model-pick (length xs))))
          (loop (append (list-head xs i) (list-tail xs (+ i 1))) (cons (list-ref xs i) out))))))
(define (model-arm m)
  (let* ((p (+ 1 (model-pick 10)))
         (near (let loop ((i 1) (acc '()))
                 (if (>= i p) acc
                     (loop (+ i 1) (cons (model-choose (list m (* m 11/10) (+ m (model-pick (quotient m 10))))) acc)))))
         (far (let loop ((i p) (acc '()))
                (if (>= i 10) acc
                    (loop (+ i 1) (cons (model-choose (list (+ (* m 11/10) 1) (* m 3/2) (+ (* m 3/2) 1) (* m 2) (+ (* m 2) (model-pick m)))) acc))))))
    (list->vector (cons m (append near far)))))
(define (model-vector)
  (let* ((ma (* 10 (+ 40 (model-pick 60))))
         (mb (* 110 (+ 2 (model-pick 4))))
         (a (model-shuffle (vector->list (model-arm ma))))
         (b (model-shuffle (vector->list (model-arm mb))))
         (ratio (/ ma mb))
         (rec (model-choose (list mb (* mb 10/11) (- (* mb 10/11) 1) (* mb 11/10) (+ (* mb 11/10) 1) #f)))
         (k (model-choose (list ratio (- ratio 1/1000) (+ ratio 1/1000) (* ratio 2) (/ ratio 2) #f))))
    (list a b (list (cons 'k k) (cons 'b rec)))))
(define model-vectors
  (let loop ((i 0) (acc '())) (if (= i model-count) (reverse acc) (loop (+ i 1) (cons (model-vector) acc)))))
(define (upper-median-in-order xs) (list-ref xs (quotient (length xs) 2)))
;; THE COVERAGE READS NOTHING UNDER TEST: its median is the model's
;; (an insertion sort), not the judge's `upper-median`, so a judge mutation
;; cannot blind the coverage row (F54 r3).
(define (model-upper-median xs)
  (let ((sorted (fold-left (lambda (acc x)
                             (let ins ((l acc))
                               (cond ((null? l) (list x))
                                     ((<= x (car l)) (cons x l))
                                     (else (cons (car l) (ins (cdr l)))))))
                           '() xs)))
    (list-ref sorted (quotient (length sorted) 2))))
(define model-disagreements '())
(define model-met '())
(define (model-note! key)
  (let ((e (assoc key model-met)))
    (if e (set-cdr! e (+ (cdr e) 1)) (set! model-met (cons (cons key 1) model-met)))))
(for-each
  (lambda (v)
    (let ((a (car v)) (b (cadr v)) (c (caddr v)))
      (let-values (((expected decided) (model-judge a b c)))
        (let ((got (f54-judge a b c)))
          (unless (equal? got expected)
            (set! model-disagreements (cons (list 'vector v 'judge got 'model expected) model-disagreements))))
        (model-note! decided)
        ;; MEMBER, NOT MEMQ: the outcomes are lists like (spread a), which eq?
        ;; never matches (F54 review r3: the count read drift's alone).
        (when (and (member decided '((spread a) (spread b) (plateau a) (plateau b) drift))
                   (cdr (assq 'k c)) (> (cadr expected) (cdr (assq 'k c))))
          (model-note! 'red-hidden-by-unreadable)
          (model-note! (list 'red-hidden-by decided)))
        (for-each
          (lambda (arm xs)
            (unless (= (upper-median-in-order xs) (model-upper-median xs)) (model-note! (list 'unsorted arm)))
            (when (exists (lambda (x) (>= x (* 2 (apply min xs)))) xs) (model-note! (list 'twice-min arm))))
          '(a b) (list a b)))))
  model-vectors)
(printf "C4m ~a vectors, seed ~a; outcomes met: ~s\n" model-count model-seed (reverse model-met))
(want "C4m every generated arm has ten samples"
      (filter (lambda (v) (not (and (= (length (car v)) 10) (= (length (cadr v)) 10)))) model-vectors)
      '())
(want "C4m the judge and an independent model return the same whole value for every generated vector"
      (let ((d (reverse model-disagreements))) (if (> (length d) 2) (list-head d 2) d))
      '())
(want "C4m the generator met every D4 outcome, the hidden red, and on each arm an unsorted list and samples at twice the minimum"
      (filter (lambda (k) (not (assoc k model-met)))
              '(uncalibrated (spread a) (spread b) (plateau a) (plateau b) drift red green
                red-hidden-by-unreadable (unsorted a) (unsorted b) (twice-min a) (twice-min b)))
      '())

;; ---- C4b: K is recorded, and derived from r0 and e ----------------------------
(define recorded-in-file
  (let* ((forms (call-with-input-file (string-append script-dir "/startup-ratio.sc")
                  (lambda (p) (let loop ((acc '())) (let ((x (read p))) (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))
         (form (find (lambda (f) (and (pair? f) (eq? (car f) 'define) (pair? (cdr f)) (eq? (cadr f) 'f54-calibration)))
                     forms)))
    (and form (cadr (caddr form)))))
(want "C4b the fixture file, read as data, holds the calibration record the row uses"
      (equal? recorded-in-file f54-calibration)
      #t)
(want "C4b K = r0 x (1 + max(0.15, 2 e-quiet)) on the recorded values, with the date and the machine recorded"
      (if calibrated
          (list (= (cal-ref 'k) (derive-k (cal-ref 'r0) (cal-ref 'e-quiet)))
                (string? (cal-ref 'taken)) (string? (cal-ref 'machine)))
          'uncalibrated)
      '(#t #t #t))
(want "C4b K from r0 and e-quiet = 0.10 is 1.20 r0" (derive-k #e1.3 #e0.10) (* #e1.20 #e1.3))

;; ---- C5: the reference imports nothing of the tree, transitively -----------------
;;
;; The import forms of the program and of every library they name that is
;; checked in beside them, read as data, recursively; no (theourgia ...) or
;; (igropyr ...) may appear. A NEGATIVE plants a wrapper importing
;; (theourgia ffi) and must be found.
(define (forms-of path)
  (call-with-input-file path
    (lambda (p) (let loop ((acc '())) (let ((x (read p))) (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))
(define (spec->name spec)
  (if (and (pair? spec) (memq (car spec) '(only prefix rename except for)))
      (spec->name (cadr spec))
      spec))
(define (import-names forms)
  (apply append
    (map (lambda (f)
           (cond ((and (pair? f) (eq? (car f) 'import)) (map spec->name (cdr f)))
                 ((and (pair? f) (eq? (car f) 'library))
                  (let ((clause (find (lambda (x) (and (pair? x) (eq? (car x) 'import))) (cddr f))))
                    (if clause (map spec->name (cdr clause)) '())))
                 (else '())))
         forms)))
;; EVERY SOURCE EXTENSION THE LAUNCHER PERMITS, in its order, read from the
;; same `source-exts` string (F54 review r1: a .sls library was reachable by
;; arm B and invisible here).
(define source-suffixes
  (let split ((s source-exts) (acc '()))
    (let ((i (let find ((j 0)) (cond ((>= (+ j 1) (string-length s)) #f)
                                     ((string=? (substring s j (+ j 2)) "::") j)
                                     (else (find (+ j 1)))))))
      (if i
          (split (substring s (+ i 2) (string-length s)) (cons (substring s 0 i) acc))
          (reverse (cons s acc))))))
(define (library-file dir name)
  (let ((stem (string-append dir (apply string-append (map (lambda (s) (string-append "/" (symbol->string s))) name)))))
    (find exists? (map (lambda (x) (string-append stem x)) source-suffixes))))
(define (walk-imports dir start)
  (let loop ((todo (list start)) (seen '()) (names '()))
    (if (null? todo)
        names
        (let ((f (car todo)))
          (if (member f seen)
              (loop (cdr todo) seen names)
              (let* ((ns (import-names (forms-of f)))
                     (files (filter values (map (lambda (n) (and (list? n) (library-file dir n))) ns))))
                (loop (append (cdr todo) files) (cons f seen) (append names ns))))))))
(define (tree-names names)
  (filter (lambda (n) (and (pair? n) (memq (car n) '(theourgia igropyr)))) names))
(want "C5 the reference program and library import nothing of theourgia or igropyr, transitively"
      (tree-names (walk-imports test-dir (string-append test-dir "/f54-reference-main.ss")))
      '())
(define c5-neg (string-append scratch "/c5-negative"))
(sh (string-append "mkdir -p " c5-neg " && cp " test-dir "/f54-reference-main.ss " reference-source " " c5-neg "/"))
(call-with-output-file (string-append c5-neg "/f54-wrapper.sc")
  (lambda (p) (write '(library (f54-wrapper) (export) (import (chezscheme) (theourgia ffi))) p))
  'replace)
(let* ((lib (string-append c5-neg "/f54-reference.sc"))
       (text (call-with-input-file lib get-string-all))
       (from "(import (chezscheme))")
       (n (string-length from))
       (at (let find ((i 0))
             (cond ((> (+ i n) (string-length text)) #f)
                   ((string=? (substring text i (+ i n)) from) i)
                   (else (find (+ i 1)))))))
  (call-with-output-file lib
    (lambda (p)
      (put-string p (substring text 0 at))
      (put-string p "(import (chezscheme) (f54-wrapper))")
      (put-string p (substring text (+ at n) (string-length text))))
    'replace))
(want "C5 NEGATIVE: a wrapper the reference imports, which imports (theourgia ffi), is found"
      (tree-names (walk-imports c5-neg (string-append c5-neg "/f54-reference-main.ss")))
      '((theourgia ffi)))
(sh (string-append "mv " c5-neg "/f54-wrapper.sc " c5-neg "/f54-wrapper.sls"))
(want "C5 NEGATIVE: the same wrapper as a .sls, which the launcher's extensions also reach, is found"
      (list source-suffixes (tree-names (walk-imports c5-neg (string-append c5-neg "/f54-reference-main.ss"))))
      (list '(".sc" ".sls" ".scm") '((theourgia ffi))))
(want "C5 the reference library's size and md5, and the reference program's md5, are the recorded ones"
      (if (and (cal-ref 'reference-lines) (cal-ref 'reference-md5) (cal-ref 'reference-main-md5))
          (list (lines-of reference-source) reference-md5-now reference-main-md5-now)
          'uncalibrated)
      (list (cal-ref 'reference-lines) (cal-ref 'reference-md5) (cal-ref 'reference-main-md5)))

;; ---- C5b: both arms read source, whatever the inherited environment ------------
;;
;; A compiled object that writes a marker file when its library is invoked is
;; planted beside a copy of each arm's source: the reference library's for
;; B, (theourgia arguments) -- imported by core.sc directly -- for A. With
;; compiled loading ENABLED the marker appears (the positive control); the
;; marker is removed; then the row's launcher runs from an environment in
;; which compiled loading is enabled, so only its own source-only override
;; can keep the object out, and the marker must stay absent. The copies are
;; under this run's scratch directory; nothing under test/ or the tree ever
;; holds a .so.
(define compiled-exts ".sc::.so")
(define (compile-into! variant object)
  (sh (string-append "echo '(compile-library \"" variant "\" \"" object "\")' | scheme -q > /dev/null 2>&1")))
(define (index-of text needle from)
  (let ((n (string-length needle)))
    (let find ((i from))
      (cond ((> (+ i n) (string-length text)) #f)
            ((string=? (substring text i (+ i n)) needle) i)
            (else (find (+ i 1)))))))
;; THE MARKER VARIANT: the library's import gains (chezscheme) under a
;; prefix -- (theourgia arguments) imports only (rnrs base) and (rnrs
;; lists), which have no file output -- and its body ends with a definition
;; that writes the marker when the library is invoked.
(define (with-marker source-text marker)
  (let* ((lib (index-of source-text "(library" 0))
         (imp (index-of source-text "(import" lib))
         (after-imp (+ imp (string-length "(import")))
         (cut (let loop ((i (- (string-length source-text) 1)))
                (if (char=? (string-ref source-text i) #\)) i (loop (- i 1))))))
    (string-append (substring source-text 0 after-imp)
                   " (prefix (chezscheme) f54cs:)"
                   (substring source-text after-imp cut)
                   "\n  (define f54-marker-written (let ((p (f54cs:open-output-file \"" marker "\" 'replace))) (f54cs:display \"loaded\" p) (f54cs:close-port p) #t)))\n")))
(define (write-text path text) (call-with-output-file path (lambda (p) (put-string p text)) 'replace))
(define (run-with-exts arm exts)
  (sh (string-append "CHEZSCHEMELIBDIRS='" (car arm) "' CHEZSCHEMELIBEXTS='" exts
                     "' scheme --script " (cdr arm) " > /dev/null 2>&1")))
(define (planted-case label source-dir lib-rel arm)
  (let* ((marker (string-append scratch "/" label ".marker"))
         (variant-dir (string-append scratch "/" label "-variant"))
         (variant (string-append variant-dir "/" lib-rel))
         (object (string-append source-dir "/" (substring lib-rel 0 (- (string-length lib-rel) 3)) ".so")))
    (sh (string-append "mkdir -p " variant-dir))
    (write-text variant (with-marker (call-with-input-file (string-append source-dir "/" lib-rel) get-string-all) marker))
    (compile-into! variant object)
    (run-with-exts arm compiled-exts)
    (let ((positive (exists? marker)))
      (sh (string-append "rm -f " marker))
      (let ((saved (getenv "CHEZSCHEMELIBEXTS")))
        (putenv "CHEZSCHEMELIBEXTS" compiled-exts)
        (run-pairs arm arm 1 (string-append scratch "/" label ".trace"))
        (putenv "CHEZSCHEMELIBEXTS" (or saved "")))
      (list (exists? object) positive (exists? marker)))))
(define c5b-b-dir (string-append scratch "/c5b-b"))
(sh (string-append "mkdir -p " c5b-b-dir " && cp " test-dir "/f54-reference-main.ss " reference-source " " c5b-b-dir "/"))
(want "C5b arm B: the planted object is there, loads with compiled loading enabled, and the launcher keeps it out"
      (planted-case "c5b-b" c5b-b-dir "f54-reference.sc" (cons c5b-b-dir (string-append c5b-b-dir "/f54-reference-main.ss")))
      '(#t #t #f))
(want "C5b arm B: the reference source is the one C5 recorded"
      (if (cal-ref 'reference-md5)
          (md5-of (string-append c5b-b-dir "/f54-reference.sc"))
          'uncalibrated)
      (cal-ref 'reference-md5))
(define c5b-a-lib (string-append scratch "/c5b-a"))
(define c5b-a-tree (string-append c5b-a-lib "/theourgia"))
(sh (string-append "mkdir -p " c5b-a-tree " && (cd " root " && tar cf - --exclude=./test --exclude=./.git .) | (cd " c5b-a-tree " && tar xf -)"
                   " && ln -s " igropyr-parent "/igropyr " c5b-a-lib "/igropyr"))
(want "C5b arm A: a planted theourgia/arguments.so is there, loads with compiled loading enabled, and the launcher keeps it out"
      (planted-case "c5b-a" c5b-a-tree "arguments.sc" (cons c5b-a-lib (string-append c5b-a-tree "/core.sc no-such-verb --wire")))
      '(#t #t #f))

;; ---- the end ---------------------------------------------------------------------
(sh (string-append "rm -rf " scratch))
(want "no .so is under test/"
      (string->number (first-line (sh-out (string-append "find " test-dir " -name '*.so' | wc -l | tr -d ' '"))))
      0)
(printf "rows: ~a\n~a failures\nstartup-ratio complete\n" rows bad)
(exit (if (zero? bad) 0 1))
