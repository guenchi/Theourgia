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

;; WHERE A WRITE REQUEST'S TIME GOES, read off the write path's timing seam
;; (timing-wrappers.sc, THEOURGIA_TIMING=1; store.sc prints the lines).
;;
;; An import is ONE write request (code-project.sc's import-code calls
;; with-store-write once with every intent), so its cost is read in two
;; parts: the request's phases once each, `(timing request <phase> <us>)`,
;; and each window of 100 intents' phases summed, `(timing intents <n>
;; ((<phase> <us>) ...))`. A cost that grows with the request shows as a
;; rising series.
;;
;; ALWAYS, AND CHEAP: with the variable unset a small import prints no
;; timing line (TC-0); set, it prints every request phase (TC-1). THE SWITCH
;; IS READ AT EXPANSION (timing-wrappers.sc's wrappers, as THEOURGIA_INJECT is): unset,
;; each wrapper expands to its bare expression, and store.sc and
;; code-project.sc reach the seam only through the wrappers (TC-E).
;;
;; AN OPT-IN FOR THE CURVE: with THEOURGIA_INSERT_COST=1, one synthetic import
;; of 3500 one-line files -- 7000 blocks, a file block and its code block
;; each -- prints the series at 1000, 3000 and 7000 intents and the ratio of
;; the last window to the tenth per phase. The rows assert only the shape;
;; the readings are the deliverable. The curve's stderr is also stamped line
;; by line as it arrives, so each window's wall time is printed beside the sum
;; of its phases (what no phase timed), and both files are kept under the test
;; root when the import completes and the root is removed.

(import (chezscheme) (theourgia rpc)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) reduce-applied-cut))

(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
(define-syntax tolerant
  (syntax-rules ()
    ((_ e) (guard (x (#t (list 'RAISED (if (and (condition? x) (message-condition? x)) (condition-message x) x)))) e))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (with-expected name expected (x) (want-1 name (tolerant got) x)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/insert-cost-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))

(define (file-text path)
  (if (file-exists? path) (let ((t (call-with-input-file path get-string-all))) (if (string? t) t "")) ""))
(define (lines-of text)
  (let loop ((p (open-input-string text)) (acc '()))
    (let ((l (get-line p))) (if (eof-object? l) (reverse acc) (loop p (cons l acc))))))
;; The (timing ...) lines of a child's stderr, as data.
(define (timing-lines path)
  (filter (lambda (d) (and (pair? d) (eq? (car d) 'timing)))
          (map (lambda (l) (guard (e (#t #f)) (read (open-input-string l)))) (lines-of (file-text path)))))
(define (write-files! dir n)
  (system (string-append "mkdir -p '" dir "'"))
  (let loop ((k 0))
    (when (< k n)
      (call-with-output-file (string-append dir "/f" (number->string k) ".js")
        (lambda (p) (put-string p (format "function f~a() { return ~a }\n" k k))) 'replace)
      (loop (+ k 1)))))
;; IMPORT IN A CHILD, so the switch, read once when the library loads, is the
;; child's: TIMING is "1" or #f (unset).
;; A TRACKED REQUEST carries its id and the cursor it was made at: the store's
;; writer and the last of its records applied (none yet on a fresh store: 0).
;; STAMPED, the child's stderr also goes through a filter that writes each
;; line, as it arrives, to <tag>.err.ts with the wall clock's seconds in front:
;; the window lines are flushed as they are printed, so the time between two
;; of them is that window's wall time, which the phases' sums are read against.
;; <tag>.err is the same lines unstamped, as without it.
(define stamp-filter
  (string-append "perl -MTime::HiRes=time -e '"
                 "open(R, \">\", shift) or die; open(T, \">\", shift) or die; "
                 "select((select(R), $| = 1)[0]); select((select(T), $| = 1)[0]); "
                 "while (<STDIN>) { print R $_; printf T \"%.6f %s\", time, $_ }'"))
(define (import-in-child! tag dir timing . stamped)
  (let* ((store (string-append root "/" tag "-store")) (err (string-append root "/" tag ".err"))
         (made (rpc-dispatch store '(init) "test"))
         (writer (cadr (assq 'writer (cdr made))))
         (seen (assoc writer (reduce-applied-cut (open-and-reduce store))))
         (cursor (string-append writer ":" (number->string (if seen (cdr seen) 0))))
         (child (string-append (if timing "THEOURGIA_TIMING=1 " "env -u THEOURGIA_TIMING ")
                               "THEOURGIA_LOCAL=1 scheme --script ../core.sc import-code '" dir "' --store '" store "' --req IC-" tag " --cursor '" cursor "' --wire")))
    (system (if (and (pair? stamped) (car stamped))
                (string-append "( " child " 2>&1 > '" root "/" tag ".out' < /dev/null ) | "
                               stamp-filter " '" err "' '" err ".ts'")
                (string-append child " > '" root "/" tag ".out' 2> '" err "' < /dev/null")))
    (list (file-text (string-append root "/" tag ".out")) (timing-lines err) (file-text err))))
(define (contains? s sub)
  (let ((n (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i n) (string-length s)) #f)
            ((string=? (substring s i (+ i n)) sub) #t)
            (else (loop (+ i 1)))))))
;; Every phase of an import's one request, each printed once. The import is
;; a TRACKED request (--req), as a client's is, so it takes the plan path.
;; `capture` and `packet` run before the write request, `total` from the
;; write's start to its end; `apply` includes the plan's three;
;; delivered-records and applied-records are counts, not times.
(define request-phases
  '(capture packet load-replay delivered-records applied-records intents verdict premises datum-src cursor rules
    plan-canonical plan-entries plan-append apply commit log-end total))
;; The eight steps of an intent; then intent (one-intent! whole), shape (the
;; loop's checks of the intent) and after (the caller's after-each), which with
;; refs are the loop's top level; and view, payload-check and frame, inside
;; one-intent! as check..receipt are.
(define intent-phase-names
  '(refs check resolve deps append reduce applied receipt intent shape after view payload-check frame))
(define top-level-phases '(refs shape after intent))
(define inside-intent-phases '(check resolve deps append reduce applied receipt view payload-check frame))
(define (request-lines lines phase)
  (filter (lambda (d) (and (= (length d) 4) (eq? (cadr d) 'request) (eq? (caddr d) phase))) lines))
(define (request-value lines phase)
  (let ((ls (request-lines lines phase))) (and (= (length ls) 1) (cadddr (car ls)))))
;; the answer's first datum, its head: ok, or what refused
(define (answer-head out)
  (let ((d (guard (e (#t #f)) (read (open-input-string out))))) (and (pair? d) (car d))))

;; THE WALL TIME AGAINST THE PHASES, from the stamped lines: each window's
;; wall time (its line's stamp less the previous window's) beside the sum of
;; its top-level phases (refs, shape, after, intent: the others are inside
;; intent), and what one-intent! spent outside its own phases, at 200, 1000,
;; 3000, 5000 and 7000 and over windows 200 to 7000; the time before the first
;; window (from the last request line printed before it) and after the last
;; (to the request's apply line). A reading, not a row.
(define (wall-against-phases ts-path)
  (let* ((stamped (filter (lambda (x) x)
                          (map (lambda (l)
                                 (guard (e (#t #f))
                                   (let* ((p (open-input-string l)) (t (read p)) (d (read p)))
                                     (and (real? t) (pair? d) (eq? (car d) 'timing) (cons t d)))))
                               (lines-of (file-text ts-path)))))
         (windows (filter (lambda (x) (and (>= (length x) 5) (eq? (caddr x) 'intents))) stamped))
         (at (lambda (n) (find (lambda (x) (= (cadddr x) n)) windows)))
         (ms (lambda (s) (exact (round (* 1000 s)))))
         (phase-ms (lambda (w names)
                     (exact (round (/ (apply + (map (lambda (ph) (let ((e (assq ph (car (cddddr w))))) (if e (cadr e) 0))) names))
                                      1000)))))
         (sum-ms (lambda (w) (phase-ms w top-level-phases)))
         (own-ms (lambda (w) (- (phase-ms w '(intent)) (phase-ms w inside-intent-phases))))
         (wall-ms (lambda (n) (let ((w (at n)) (v (at (- n 100)))) (and w v (ms (- (car w) (car v))))))))
    (printf "~%wall against phases, ms per window: window  wall  top-level phases  untimed  one-intent!'s own  shape  after  view  payload-check  frame~%")
    (for-each (lambda (n)
                (let ((w (at n)) (wall (wall-ms n)))
                  (when (and w wall)
                    (printf "  ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a~%" n wall (sum-ms w) (- wall (sum-ms w)) (own-ms w)
                            (phase-ms w '(shape)) (phase-ms w '(after)) (phase-ms w '(view))
                            (phase-ms w '(payload-check)) (phase-ms w '(frame))))))
              '(200 1000 3000 5000 7000))
    (let* ((ns (map cadddr windows))
           (ws (filter (lambda (n) (and (> n 100) (wall-ms n))) ns))
           (wall (apply + (map wall-ms ws)))
           (phases (apply + (map (lambda (n) (sum-ms (at n))) ws)))
           (first (and (pair? windows) (car windows)))
           (last (and (pair? windows) (car (reverse windows))))
           (before (and first (let ((rs (filter (lambda (x) (and (eq? (caddr x) 'request) (< (car x) (car first)))) stamped)))
                                (and (pair? rs) (ms (- (car first) (car (car (reverse rs)))))))))
           (apply-line (find (lambda (x) (and (eq? (caddr x) 'request) (eq? (cadddr x) 'apply))) stamped))
           (after (and last apply-line (ms (- (car apply-line) (car last))))))
      (printf "  windows 200..~a: wall ~a top-level phases ~a untimed ~a one-intent!'s own ~a~%" (if last (cadddr last) 'none)
              wall phases (- wall phases) (apply + (map (lambda (n) (own-ms (at n))) ws)))
      (printf "  before the first window ~a, after the last to apply ~a~%" before after))))

(define small (string-append root "/small"))
(write-files! small 3)

(let ((r (import-in-child! "unset" small #f)))
  (want "TC-0 with THEOURGIA_TIMING unset the import answers ok and its stderr holds no timing line"
        (list (answer-head (car r)) (contains? (caddr r) "(timing"))
        '(ok #f)))

(let* ((r (import-in-child! "set" small "1"))
       (lines (cadr r)))
  (want "TC-1 with THEOURGIA_TIMING=1 the import answers ok and prints every request phase exactly once, each a count of microseconds"
        (list (answer-head (car r))
              (map (lambda (ph) (let ((v (request-value lines ph))) (and (integer? v) (>= v 0)))) request-phases))
        (list 'ok (map (lambda (ph) #t) request-phases))))

;; THE EXPANSION ITSELF, in two readings. (a) In a child under each value of
;; the variable, each of timing-wrappers.sc's four wrappers is expanded around a plain
;; expression and the seam's entry point, timing-seam, counted in the printed
;; expansion: unset it must not occur (and timing-call is #f), set it must.
;; (Chez's `expand` of a whole library form returns no body to read, so the
;; wrappers are read where they expand.) (b) store.sc and code-project.sc,
;; read as data, name neither timing-seam nor (theourgia timing): they reach
;; the seam only through the wrappers, so an unset build of them holds none.
(define (wrapper-expansions tag timing)
  (let ((script (string-append root "/expand-" tag ".ss"))
        (out (string-append root "/expand-" tag ".out")))
    (call-with-output-file script
      (lambda (p)
        (write '(import (chezscheme) (only (theourgia timing-wrappers) timed intent-timed timing-call timing-quiet)) p)
        (write '(define (count-in text sub)
                  (let ((n (string-length sub)))
                    (let loop ((i 0) (k 0))
                      (cond ((> (+ i n) (string-length text)) k)
                            ((string=? (substring text i (+ i n)) sub) (loop (+ i 1) (+ k 1)))
                            (else (loop (+ i 1) k))))))
               p)
        (write '(write (list (map (lambda (f) (count-in (format "~s" (expand f)) "timing-seam"))
                                  '((timed 'x (+ 1 2)) (intent-timed 3 (+ 1 2))
                                    (timing-call delivered! (+ 1 2)) (timing-quiet (+ 1 2))))
                             (expand '(timing-call delivered! (+ 1 2)))))
               p))
      'replace)
    (system (string-append (if timing "THEOURGIA_TIMING=1 " "env -u THEOURGIA_TIMING ")
                           "scheme --script '" script "' > '" out "' 2>&1 < /dev/null"))
    (guard (e (#t (list 'UNREADABLE (file-text out)))) (read (open-input-string (file-text out))))))
;; whether X, a datum, holds the symbol SYM or the list L anywhere
(define (holds? x sym l)
  (let walk ((x x))
    (cond ((eq? x sym) #t)
          ((equal? x l) #t)
          ((pair? x) (or (walk (car x)) (walk (cdr x))))
          ((vector? x) (walk (vector->list x)))
          (else #f))))
(let ((unset (wrapper-expansions "unset" #f)) (set (wrapper-expansions "set" "1")))
  (want "TC-E unset, each timing wrapper expands to its bare expression (timing-call to #f) and store.sc and code-project.sc name the seam only through the wrappers; set, each wrapper calls the seam"
        (list unset
              (and (list? set) (pair? set) (list? (car set)) (map (lambda (n) (and (integer? n) (> n 0))) (car set)))
              (map (lambda (f) (holds? (call-with-input-file f read) 'timing-seam '(theourgia timing)))
                   '("../store.sc" "../code-project.sc")))
        '(((0 0 0 0) #f) (#t #t #t #t) (#f #f))))

(if (not (equal? (getenv "THEOURGIA_INSERT_COST") "1"))
    (printf "SKIP the curve: THEOURGIA_INSERT_COST is not 1 (one import of 7000 blocks; set it to read the series)~%")
    (let* ((big (string-append root "/big"))
           (files 3500))
      (write-files! big files)
      (let* ((r (import-in-child! "curve" big "1" #t))
             (lines (cadr r))
             (series (filter (lambda (d) (and (>= (length d) 4) (eq? (cadr d) 'intents))) lines))
             (window (lambda (n) (let ((w (find (lambda (d) (= (caddr d) n)) series))) (and w (cadddr w)))))
             (phase-us (lambda (n ph) (let* ((w (window n)) (e (and w (assq ph w)))) (and e (cadr e))))))
        (printf "~%request phases (us):~%")
        (for-each (lambda (ph) (printf "  ~a ~a~%" ph (request-value lines ph))) request-phases)
        (printf "~%per window of 100 intents (us): phase  at 1000  at 3000  at 7000  ratio 7000/1000~%")
        (for-each (lambda (ph)
                    (let ((a (phase-us 1000 ph)) (b (phase-us 3000 ph)) (c (phase-us 7000 ph)))
                      (printf "  ~a ~a ~a ~a ~a~%" ph a b c
                              (if (and (integer? a) (integer? c) (> a 0)) (exact->inexact (/ c a)) 'n/a))))
                  intent-phase-names)
        (want "TC-2 the curve: the import answers ok, the windows end at 100, 200, ... 7000, and each holds every intent phase once, a count of microseconds"
              (list (answer-head (car r))
                    (map caddr series)
                    (for-all (lambda (d)
                               (let ((w (cadddr d)))
                                 (and (list? w)
                                      (equal? (map car w) intent-phase-names)
                                      (for-all (lambda (e) (and (= (length e) 2) (integer? (cadr e)) (>= (cadr e) 0))) w))))
                             series))
              (list 'ok (map (lambda (k) (* 100 (+ k 1))) (iota 70)) #t))
        (wall-against-phases (string-append root "/curve.err.ts"))
        ;; KEPT: the import completes and the root goes, so both files are
        ;; copied beside it first, under the test root, named by this process.
        (let ((keep (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/insert-cost-curve-"
                                   (number->string (get-process-id)))))
          (system (string-append "cp '" root "/curve.err' '" keep ".err'; cp '" root "/curve.err.ts' '" keep ".err.ts'"))
          (printf "kept: ~a.err ~a.err.ts~%" keep keep)))))

(system (string-append "rm -rf '" root "'"))
(printf "~%~a failures~%rows: ~a~%insert-cost complete~%" bad rows)
(exit (if (= bad 0) 0 1))
