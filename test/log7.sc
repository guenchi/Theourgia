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

;; The publication point and the delivery boundary. Every row here exists
;; because a review found something the forty rows of log6 could not see:
;; they observe what delivery YIELDS, and these observe when it must
;; REFUSE, who may end the transaction, and what happens to the lock when
;; a callback does not return normally.
(import (chezscheme) (theourgia log) (theourgia wire) (theourgia ffi)
        (theourgia trace) (theourgia crc32))

;; THE WORK DIRECTORY IS DECIDED AT RUN TIME. Every fixture used to name
;; an absolute path under one session's scratchpad. That is green only
;; while that particular directory happens to still exist: tmp is swept,
;; and another machine has no such path at all -- so the whole suite
;; would go red for a reason with nothing to do with the code under test.
;; THEOURGIA_TEST_ROOT overrides the default; the pid keeps two runs, or
;; two fixtures, out of each other's way. Directories are left behind
;; deliberately, as evidence.
(define (test-dir name)
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/" name "-" (number->string (get-process-id)))))
    ;; A ROOT THAT DOES NOT SURVIVE THE ROUND TRIP IS REFUSED HERE. Trace
    ;; lines are written with display and read back as data, and paths go
    ;; into generated scripts and shell commands unquoted -- so a root
    ;; with a space or a bracket in it makes the crash device read no
    ;; events at all and rewrite nothing, which reads exactly like a tree
    ;; that needed no rewriting. Refusing is the one answer that cannot
    ;; be mistaken for success.
    (let loop ((i 0))
      (when (< i (string-length path))
        (let ((c (string-ref path i)))
          (unless (or (char-alphabetic? c) (char-numeric? c)
                      (memv c '(#\/ #\. #\- #\_)))
            (assertion-violation 'test-dir
              "THEOURGIA_TEST_ROOT may use only letters, digits, / . - and _"
              root)))
        (loop (+ i 1))))
    ;; THE SAME CHECK NOW GUARDS A REMOVAL, so it asks for two more
    ;; things a creation did not need: an absolute path, and no `..`
    ;; anywhere in it.
    (unless (and (> (string-length path) 0) (char=? #\/ (string-ref path 0)))
      (assertion-violation 'test-dir
        "THEOURGIA_TEST_ROOT must be an absolute path" root))
    (let loop ((i 0))
      (when (< (+ i 1) (string-length path))
        (when (and (char=? #\. (string-ref path i))
                   (char=? #\. (string-ref path (+ i 1))))
          (assertion-violation 'test-dir
            "THEOURGIA_TEST_ROOT may not contain .." root))
        (loop (+ i 1))))
    ;; AND THE DIRECTORY IS MADE FRESH, NOT ASSUMED FRESH. The name
    ;; carries the process id, which reads like a unique name and is not
    ;; one: the pid space wraps, the scratch root outlives the run, and a
    ;; directory left by an earlier run holding the same pid is handed to
    ;; this one already populated. Counted in the default root on
    ;; 2026-09-11: 4260 leftover directories over 1686 distinct pids, so
    ;; about one run in twenty inherited an older run's store. It showed
    ;; up once as a crash -- an init answering already-initialised to a
    ;; fixture that expected a store id -- and the crash is the harmless
    ;; form. The form that matters is an assertion passing against data
    ;; the run did not write. The sibling `-home` goes with it, because
    ;; the machine registry is keyed by store identity and a stale one
    ;; makes a fresh store look like a rollback.
    ;; AND THE LEAF IS NEVER THE ROOT. Removal only ever names
    ;; <root>/<name>-<pid>; a name that collapsed to nothing would aim it
    ;; at the scratch root itself, which holds every other run.
    (unless (and (> (string-length path) (+ 1 (string-length root)))
                 (string=? root (substring path 0 (string-length root)))
                 (char=? #\/ (string-ref path (string-length root))))
      (assertion-violation 'test-dir
        "the directory must lie strictly inside the root" (list root path)))
    ;; A CLEAN THAT FAILED MUST NOT READ AS A CLEAN THAT WORKED. If the
    ;; removal fails -- contents that cannot be unlinked, a busy mount --
    ;; `mkdir -p` then succeeds on the directory that is already there and
    ;; hands back exactly the populated directory this is here to
    ;; prevent. Both commands are checked, and a failure stops the run
    ;; rather than quietly weakening it.
    (let ((must! (lambda (command)
                   (let ((status (system command)))
                     (unless (eqv? 0 status)
                       (assertion-violation 'test-dir
                         "could not prepare the scratch directory"
                         (list command status)))))))
      (must! (string-append "rm -rf " path " " path "-home"))
      (must! (string-append "mkdir -p " path)))
    path))

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

(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) x))))))

(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))

(define d (test-dir "log7work"))
(define A "k3m9x2qa")
(define (rec seq deps payload)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" deps (storable-encode payload)))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (damage-crc! bv)
  ;; The seq field is inside the checksummed body, so flipping a digit
  ;; there makes the record's own CRC wrong while the frame stays intact.
  (bytevector-u8-set! bv 3 (if (= 48 (bytevector-u8-ref bv 3)) 49 48))
  bv)
(define (build!)
  (system (string-append "rm -rf " d " " d "-home; mkdir -p " d "/writers/" A " " d "/snap"))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"t\"))\n"))
  (file-ensure! (string-append d "/lock"))
  (put! (string-append d "/writers/" A "/owner.sexp") (string->utf8 "((machine \"m\"))\n")))
(define (seg! n . rs) (put! (string-append d "/writers/" A "/00000" (number->string n) ".sexp") (apply cat rs)))
(define (collect ls cut)
  (let ((seen '()))
    (let ((r (load-deliver! ls cut
               (lambda (w seg off seq ts actor deps payload)
                 (set! seen (cons seq seen))))))
      (list r (reverse seen)))))

(printf "== an error inside a segment stops the extent AT the last good record ==\n")
;; The torn branch was fixed for exactly this and its sibling was left
;; standing: record 1 is CRC-valid and contiguous, and reporting extent 0
;; threw it away. Pre-fix reading: end-seq 0 with ranges ().
(build!)
(seg! 1 (rec 1 '() '(put "a.1" ())) (damage-crc! (rec 2 '() '(put "a.2" ())))
        (rec 3 '() '(put "a.3" ())))
(let* ((ls (log-open d)) (p (load-prefix ls A)))
  (want "the record before the damage is still history" (discovery-end-seq p) 1)
  (want "and its segment range says so" (discovery-segment-ranges p) '((1 1 1)))
  (want "the error is retained, not swallowed"
        (map log-error-kind (discovery-integrity p)) '(crc))
  (want "delivery yields that record and the load stands"
        (list (collect ls '()) (load-outcome ls))
        (list (list 'delivered '(1)) 'open))
  (load-commit! ls))

(printf "== delivery must REACH the discovered boundary, not merely not raise ==\n")
;; The whole point of calling the scanner is its verdict. Damaged bytes in
;; a sealed segment, arriving after discovery, complete without raising
;; and deliver less than validation promised.
;; Pre-fix reading: (delivered (1)) and outcome committed -- record 2 was
;; skipped in silence and the load reported success.
(build!)
(seg! 1 (rec 1 '() '(put "a.1" ())))
(seg! 2 (rec 2 '() '(put "a.2" ())))
(seg! 3 (rec 3 '() '(put "a.3" ())))
(let ((ls (log-open d)))
  (want "discovery saw all three" (discovery-end-seq (load-prefix ls A)) 3)
  ;; sealed, and NOT the current segment, which is retained in memory
  (seg! 2 (damage-crc! (rec 2 '() '(put "a.2" ()))))
  (want "delivery refuses to call a short delivery a success"
        (collect ls '()) (list (list 'delivery-failed A) '(1)))
  (want "and the load is aborted"
        (let ((o (load-outcome ls))) (if (pair? o) (car o) o)) 'aborted))

(printf "== a reducer's return value may not truncate delivery ==\n")
;; The scanner stops on 'stop. A reducer that happens to return that
;; symbol would end the segment early -- and the scan COMPLETES, so it
;; reads as success. Pre-fix reading: (delivered (1)).
(build!)
(seg! 1 (rec 1 '() '(put "a.1" ())) (rec 2 '() '(put "a.2" ()))
        (rec 3 '() '(put "a.3" ())))
(let* ((ls (log-open d)) (seen '()))
  (let ((r (load-deliver! ls '()
             (lambda (w seg off seq ts actor deps payload)
               (set! seen (cons seq seen))
               'stop))))
    (want "every record in the extent still arrives" (list r (reverse seen))
          (list 'delivered '(1 2 3))))
  (load-commit! ls))

(printf "== the transaction boundary is enforced, not assumed ==\n")
;; Pre-fix reading: the commit SUCCEEDED and returned the staging state,
;; releasing the load's shared lock while the remaining segments were
;; still being read against files a writer was then free to replace.
(build!)
(seg! 1 (rec 1 '() '(put "a.1" ())))
(seg! 2 (rec 2 '() '(put "a.2" ())))
(let* ((ls (log-open d)) (inner 'never-ran))
  (guard (e (#t (want "committing from inside a delivery callback is refused"
                      (condition-violation? e) #t)))
    (load-deliver! ls '()
      (lambda (w seg off seq ts actor deps payload)
        (set! inner (guard (e (#t 'refused)) (load-commit! ls) 'committed))))
    (want "the inner commit did not take" inner 'refused))
  ;; A REFUSED INNER COMMIT DOES NOT KILL THE LOAD. My first version of
  ;; this row expected 'aborted and read 'open. The refusal is raised at
  ;; the callback, which caught it and returned normally, so delivery
  ;; finished -- and it is right that a rejected attempt to end the
  ;; transaction early leaves the transaction where it was. What the row
  ;; has to establish is that the publication point still belongs to the
  ;; owner afterwards.
  (want "the load is left open, not ended by the attempt"
        (load-outcome ls) 'open)
  (want "and its owner can still commit it"
        (begin (load-commit! ls) (load-outcome ls)) 'committed))

;; Pre-fix reading: 'committed -- a later commit overwrote the abort,
;; turning a discarded load into a published one.
(build!)
(seg! 1 (rec 1 '() '(put "a.1" ())))
(let ((ls (log-open d)))
  (load-abort! ls 'because)
  (want "a commit cannot overwrite an abort"
        (guard (e (#t 'refused)) (load-commit! ls) 'committed) 'refused)
  (want "and the outcome is still the abort"
        (load-outcome ls) '(aborted because)))

(printf "== an escape out of delivery aborts and releases the lock ==\n")
;; Pre-fix reading: outcome 'open with 0 unlock events -- the reader had
;; given up while an exclusive writer would still wait on its lock.
(build!)
(seg! 1 (rec 1 '() '(put "a.1" ())))
(seg! 2 (rec 2 '() '(put "a.2" ())))
(let ((p (open-output-string)) (ls #f))
  (parameterize ((current-error-port p))
    (trace-enable! #t)
    (set! ls (log-open d))
    (guard (e (#t (if #f #f)))
      (load-deliver! ls '()
        (lambda (w seg off seq ts actor deps payload)
          (raise 'the-reducer-gave-up))))
    (trace-enable! #f))
  (let ((s (get-output-string p)))
    (define (count-of needle)
      (let ((n (string-length s)) (m (string-length needle)))
        (let loop ((i 0) (k 0))
          (cond ((> (+ i m) n) k)
                ((string=? (substring s i (+ i m)) needle) (loop (+ i 1) (+ k 1)))
                (else (loop (+ i 1) k))))))
    (want "the lock is released on the way out" (count-of "(trace unlock") 1))
  (want "and the load is aborted, naming the escape"
        (load-outcome ls) '(aborted delivery-escaped)))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "log7 complete\n")
