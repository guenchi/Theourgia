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

;; WHAT THE SUITE SAYS ABOUT ITS OWN ROW BASELINE.
;;
;; `row-baseline-check.sh` compares three columns -- rows, lines, md5 --
;; against `rows-baseline.txt`; the md5 and the row count refuse, the line
;; count is a reading. (The row count once was a reading too, and the two
;; fixtures named in the defect below were the two stale entries that stayed
;; wrong because of it.) Its output is the
;; line a person quotes when they say a run was clean, and until this file
;; existed nothing measured what that line CLAIMED.
;;
;; THE DEFECT THIS FILE WAS WRITTEN FOR: the summary line read
;;
;;     row baseline: every listed fixture matches its recorded hash and counts
;;
;; directly above
;;
;;     row baseline, counts that differ in this environment: daemon-link-gate(...) q8(...)
;;
;; The code was right -- a differing count is a reading and does not refuse
;; -- but the sentence asserted a comparison it had not made, and it is the
;; sentence a reader believes. Two lines, each naming its own column, cannot
;; be read as covering one another.
;;
;; EVERY ROW BELOW RUNS THE REAL SCRIPT on a directory this file builds, so
;; a change to the wording has to change these readings. A row that restated
;; the expected sentence and compared it with a copy of itself would pass
;; over any script at all.

(import (chezscheme))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1))
             (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
;; The shared renderer, so a raise in a row here says what was raised
;; rather than only that something was.
(include "condition-render.ss")

(define-syntax caught
  (syntax-rules ()
    ((_ e0) (guard (e (#t (list 'RAISED (condition->text e)))) e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

;; THE SCRIPT UNDER TEST IS NAMED ONCE, and it is the one in this directory
;; -- not a copy. A fixture that measured its own copy would go on passing
;; after the real script changed.
(define checker
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1))))))
         (dir (if cut (substring self 0 cut) "."))
         (p (string-append dir "/row-baseline-check.sh"))
         ;; ABSOLUTE, BECAUSE EVERY CASE BELOW RUNS IT FROM ANOTHER
         ;; DIRECTORY. A relative name resolved against the fixture
         ;; directory and every case answered 127 -- "not found" -- which
         ;; is a number, and a row comparing numbers would have called it
         ;; a wrong exit code rather than a missing script.
         (abs (if (char=? #\/ (string-ref p 0))
                  p
                  (string-append (current-directory) "/" p))))
    (if (file-exists? abs)
        abs
        (assertion-violation 'row-baseline
          "row-baseline-check.sh is not beside this fixture" abs))))

;; THE SAME `test-dir` GUARD THE OTHER FIXTURES CARRY, and this is the
;; forty-sixth copy of it in this directory. It is copied rather than shared
;; because there is no place here to share it from; the duplication is a
;; known cost of that and not an accident. The guard earns its place in this
;; file specifically: the path below is pasted into shell commands.
(define here
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/row-baseline-" (number->string (get-process-id)))))
    (unless (and (> (string-length path) 0) (char=? #\/ (string-ref path 0)))
      (assertion-violation 'row-baseline "the test root must be an absolute path" root))
    (let loop ((i 0))
      (when (< i (string-length path))
        (let ((c (string-ref path i)))
          (unless (or (char-alphabetic? c) (char-numeric? c)
                      (memv c '(#\/ #\. #\- #\_)))
            (assertion-violation 'row-baseline
              "the test root may use only letters, digits, / . - and _" root)))
        (loop (+ i 1))))
    (let loop ((i 0))
      (when (< (+ i 1) (string-length path))
        (when (and (char=? #\. (string-ref path i)) (char=? #\. (string-ref path (+ i 1))))
          (assertion-violation 'row-baseline "the test root may not contain .." root))
        (loop (+ i 1))))
    path))

(define (put! path text)
  (call-with-output-file path (lambda (p) (display text p))))
(define (text-of path)
  (if (not (file-exists? path))
      ""
      (call-with-input-file path
        (lambda (p)
          (let loop ((out '()))
            (let ((c (read-char p)))
              (if (eof-object? c) (list->string (reverse out)) (loop (cons c out)))))))))

(define (lines-of text)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i (string-length text))
           (reverse (if (> i start) (cons (substring text start i) out) out)))
          ((char=? (string-ref text i) #\newline)
           (loop (+ i 1) (+ i 1) (cons (substring text start i) out)))
          (else (loop (+ i 1) start out)))))

(define (holds? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

;; ---- a directory the checker can be pointed at -----------------------------
;;
;; Each case builds a fixture directory and an output directory from
;; scratch. `md5 -q` is asked for the real hash rather than a made-up one,
;; because the checker compares against the file it finds and a wrong hash
;; here would make every case look like the stale case.
(define n-case 0)
(define (md5-of path)
  (let* ((tmp (string-append here "/md5.txt")))
    (system (string-append "md5 -q '" path "' > '" tmp "'"))
    (let ((t (text-of tmp)))
      (if (null? (lines-of t)) "" (car (lines-of t))))))

;; blocks: a list of (name rows lines out-text table-rows table-lines table-hash)
;; where a #f hash means "use the file's real hash".
(define (build! blocks)
  (set! n-case (+ n-case 1))
  (let* ((d (string-append here "/c" (number->string n-case)))
         (fx (string-append d "/fx"))
         (out (string-append d "/out")))
    (system (string-append "rm -rf '" d "'; mkdir -p '" fx "' '" out "'"))
    (for-each
      (lambda (b)
        (let* ((name (car b)) (out-text (cadddr b))
               (src (string-append fx "/" name ".sc")))
          (put! src (string-append ";; fixture " name "\n"))
          (when out-text (put! (string-append out "/" name ".out") out-text))))
      blocks)
    (let ((table
            (apply string-append
                   (map (lambda (b)
                          (let* ((name (car b))
                                 (tr (list-ref b 4)) (tl (list-ref b 5))
                                 (th (or (list-ref b 6) (md5-of (string-append fx "/" name ".sc")))))
                            (if (eq? tr 'unlisted)
                                ""
                                (string-append name " " tr " " tl " " th "\n"))))
                        blocks))))
      (put! (string-append fx "/rows-baseline.txt") table))
    (list fx out)))

;; Runs the real script in that directory and answers
;; (exit-code hash-line count-line other-lines rows-line all-lines).
(define (check dir-pair)
  (let* ((fx (car dir-pair)) (out (cadr dir-pair))
         (log (string-append fx "/log.txt"))
         (code (system (string-append "cd '" fx "' && sh '" checker "' '" out "' > '" log "' 2>&1")))
         (ls (lines-of (text-of log)))
         ;; A MISSING LINE ANSWERS WITH A SENTENCE, NOT WITH #f. Returning
         ;; #f made a row that asks a question ABOUT the line raise instead
         ;; of showing what the script actually printed, so the reading said
         ;; RAISED where it could have said which line was there.
         (pick (lambda (prefix)
                 (let loop ((l ls))
                   (cond ((null? l) (string-append "NO LINE BEGINNING " prefix))
                         ((holds? (car l) prefix) (car l))
                         (else (loop (cdr l))))))))
    (list code
          (pick "row baseline, hashes:")
          (pick "row baseline, counts:")
          (filter (lambda (l) (not (holds? l "row baseline, "))) ls)
          (pick "row baseline, rows:")
          ls)))

(printf "\n== a table that describes this directory ==\n")

(define clean
  (check (build! (list (list "alpha" 3 4 "ok one\nok two\nok three\nrows: 3\n" "3" "4" #f)
                       (list "beta"  2 3 "ok one\nok two\nrows: 2\n"           "2" "3" #f)))))

(want "RB-1 a table that matches answers zero and says so about the hashes"
      (list (car clean) (cadr clean))
      (list 0 "row baseline, hashes: every listed fixture is the version the table describes"))

(want "RB-2 and the counts get their own line, which says how many it compared"
      (caddr clean)
      "row baseline, counts: all 2 compared fixture(s) match their recorded row and line counts")

(want "RB-3 nothing else is printed"
      (cadddr clean)
      '())

(printf "\n== a ROW count that differs on the same file: a refusal ==\n")

;; The recorded row count is 3 and the run printed 2. Nothing about the FILE
;; changed, which is what makes this a count case and not a hash case. A row
;; is an assertion the file made; on the same file that number does not
;; depend on the machine, so a difference is a stale table or rows that
;; stopped running, and it refuses.
(define drifted
  (check (build! (list (list "alpha" 2 3 "ok one\nok two\nrows: 2\n" "3" "4" #f)
                       (list "beta"  2 3 "ok one\nok two\nrows: 2\n" "2" "3" #f)))))

(want "RB-4 a row count that differs from the table, the hash unchanged, refuses"
      (car drifted)
      1)

(want "RB-5 the rows line names the fixture and both row counts"
      (list-ref drifted 4)
      "row baseline, rows: these fixtures ran a different number of rows than the table records for the same file, which refuses: alpha(3->2)")

(printf "\n== a row count that differs for a fixture listed as KNOWN RED: a reading ==\n")

;; alpha is listed in a known-red file named by THEOURGIA_KNOWN_RED; beta is
;; not, and both ran fewer rows than the table records.
(define (with-known-red names thunk)
  (let ((f (string-append here "/known-red.txt")))
    (when (file-exists? f) (delete-file f))
    (put! f (apply string-append "# a comment line\n"
                   (map (lambda (n) (string-append n "\t^FAIL x\n")) names)))
    (putenv "THEOURGIA_KNOWN_RED" f)
    (let ((v (thunk))) (putenv "THEOURGIA_KNOWN_RED" "") v)))
(define known-only
  (with-known-red '("alpha")
    (lambda () (check (build! (list (list "alpha" 2 3 "ok one\nok two\nrows: 2\n" "3" "4" #f)
                                    (list "beta"  2 3 "ok one\nok two\nrows: 2\n" "2" "3" #f)))))))
(define known-and-not
  (with-known-red '("alpha")
    (lambda () (check (build! (list (list "alpha" 2 3 "ok one\nok two\nrows: 2\n" "3" "4" #f)
                                    (list "beta"  1 2 "ok one\nrows: 1\n" "2" "3" #f)))))))
(define (line-of run prefix)
  (let ((l (find (lambda (x) (holds? x prefix)) (list-ref run 5)))) (or l (string-append "NO LINE " prefix))))
(want "RB-4c a known-red fixture whose row count differs does not refuse, and its own line says it is a reading"
      (list (car known-only) (line-of known-only "row baseline, rows of known-red fixtures:") (list-ref known-only 4))
      (list 0 "row baseline, rows of known-red fixtures: these differ, which is a reading and not a refusal (they are known not to finish here): alpha(3->2)"
            "NO LINE BEGINNING row baseline, rows:"))
(want "RB-4c TWIN: in the same run a fixture NOT listed still refuses on its row count"
      (list (car known-and-not) (list-ref known-and-not 4))
      (list 1 "row baseline, rows: these fixtures ran a different number of rows than the table records for the same file, which refuses: beta(2->1)"))

(printf "\n== a LINE count that drifted: a reading, not a refusal ==\n")

;; Same rows, one more line: output whose length depends on the machine.
(define line-drift
  (check (build! (list (list "alpha" 3 5 "ok one\nok two\nok three\na note\nrows: 3\n" "3" "4" #f)
                       (list "beta"  2 3 "ok one\nok two\nrows: 2\n" "2" "3" #f)))))

(want "RB-4b a line count that differs, the rows the same, does not refuse, and the count line names it"
      (list (car line-drift) (caddr line-drift) (list-ref line-drift 4))
      (list 0 "row baseline, counts: these differ in line count in this environment, which is a reading and not a refusal: alpha(3/4->3/5)"
            "NO LINE BEGINNING row baseline, rows:"))

;; THE ROW THIS FILE EXISTS FOR.
;;
;; The old summary line said "matches its recorded hash and counts" in
;; exactly this situation -- one fixture's counts did not match, and the
;; sentence a reader quotes said they did. The hash line must say what it
;; compared and nothing more, so it may not contain the word at all.
(want "RB-6 and the hash line, in that same run, says NOTHING about counts"
      (list (cadr drifted)
            (holds? (cadr drifted) "count")
            (holds? (cadr drifted) "and counts"))
      (list "row baseline, hashes: every listed fixture is the version the table describes"
            #f #f))

(printf "\n== a hash that differs: a refusal ==\n")

(define stale
  (check (build! (list (list "alpha" 3 4 "ok one\nok two\nok three\nrows: 3\n" "3" "4"
                             "00000000000000000000000000000000")
                       (list "beta"  2 3 "ok one\nok two\nrows: 2\n" "2" "3" #f)))))

(want "RB-7 a recorded hash that is about another version refuses, and names it"
      (list (car stale) (cadr stale))
      (list 1 "row baseline, hashes: these fixtures are NOT the version the table describes (md5 differs): alpha"))

;; AND THE COUNT LINE STILL RUNS. The checks used to be in series, so a
;; refusal hid everything behind it; this row is what says they are not.
(want "RB-8 TWIN: and the count line is still printed under a refusal"
      (holds? (caddr stale) "row baseline, counts:")
      #t)

(printf "\n== a fixture that counts rows and is not in the table ==\n")

(define unlisted
  (check (build! (list (list "alpha" 3 4 "ok one\nok two\nok three\nrows: 3\n" 'unlisted "" #f)
                       (list "beta"  2 3 "ok one\nok two\nrows: 2\n" "2" "3" #f)))))

(want "RB-9 an unlisted fixture that counts rows refuses, and is named"
      (list (car unlisted)
            (exists (lambda (l) (holds? l "NO ROW BASELINE (the fixture counts rows")) (cadddr unlisted))
            (exists (lambda (l) (holds? l "alpha")) (cadddr unlisted)))
      (list 1 #t #t))

(want "RB-10 and the hash line says it only covered the LISTED ones"
      (cadr unlisted)
      "row baseline, hashes: every LISTED fixture is the version the table describes (the unlisted ones named above are compared against nothing)")

(printf "\n== a fixture with no output file is not compared, and does not drift ==\n")

;; A fixture the run did not produce output for has nothing to compare
;; against. It must not be counted as matching either, which is why RB-2
;; prints the number it compared rather than saying "all of them".
(define absent
  (check (build! (list (list "alpha" 3 4 #f "3" "4" #f)
                       (list "beta"  2 3 "ok one\nok two\nrows: 2\n" "2" "3" #f)))))

(want "RB-11 a fixture with no output is neither a match nor a drift"
      (list (car absent) (caddr absent))
      (list 0 "row baseline, counts: all 1 compared fixture(s) match their recorded row and line counts"))

(printf "\n== the table itself is missing ==\n")

(define no-table
  (let* ((d (string-append here "/notable"))
         (fx (string-append d "/fx")) (out (string-append d "/out")))
    (system (string-append "rm -rf '" d "'; mkdir -p '" fx "' '" out "'"))
    (check (list fx out))))

(want "RB-12 no table at all refuses and says so, rather than reading as a clean run"
      (list (car no-table)
            (exists (lambda (l) (holds? l "NO ROW BASELINE TABLE")) (cadddr no-table))
            (cadr no-table))
      (list 1 #t "NO LINE BEGINNING row baseline, hashes:"))

(printf "\n== called without an output directory ==\n")

(define no-arg
  (let* ((d (string-append here "/noarg"))
         (fx (string-append d "/fx")) (log (string-append d "/log.txt")))
    (system (string-append "rm -rf '" d "'; mkdir -p '" fx "'"))
    (let ((code (system (string-append "cd '" fx "' && sh '" checker "' > '" log "' 2>&1"))))
      (list code (lines-of (text-of log))))))

;; A DISTINCT EXIT CODE, because the caller has to tell "the baseline is
;; bad" from "I called this wrongly". `run-fixtures.sh` reads 2 and stops.
(want "RB-13 a call with no output directory is a usage error, not a baseline failure"
      (list (car no-arg)
            (exists (lambda (l) (holds? l "needs the output directory")) (cadr no-arg)))
      (list 2 #t))

(system (string-append "rm -rf '" here "'"))

(printf "rows: ~a\n~a failures\nrow-baseline complete\n" rows bad)
(exit (if (zero? bad) 0 1))
