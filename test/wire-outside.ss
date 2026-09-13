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

;; What this store writes, read back by a reader that is not ours.
;;
;; THE PROPOSITION NEEDS AN OUTSIDE READER, and that is the whole reason
;; this file exists separately. The rows here used to live in cli3 and
;; rpc1; when the codec was copied into this tree those files could have
;; kept their assertions by pointing at our own copy of the reader --
;; and the claim would have quietly changed from "what we emit, another
;; implementation can parse" to "what we emit, we can parse". The second
;; is weaker exactly where it matters: a refusal is addressed to a
;; program written by someone else.
;;
;; SO THIS FILE IMPORTS IGROPYR ON PURPOSE. It is red when the suite runs
;; against a library path with no igropyr on it, and that red is expected
;; and named in RUN.md. On the ordinary path it runs and must be green --
;; which means the proposition is checked on every ordinary run, not
;; whenever somebody remembers to check it.
(import (chezscheme) (theourgia rpc) (theourgia store) (theourgia reduce)
        (theourgia log) (theourgia ffi) (theourgia wire)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (igropyr sexpr) string->sexpr-extended))
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
;; answer apart -- `(assq 'cursor (cdr a))`, `(caddr ...)` -- and a
;; mutant that changes the answer's SHAPE makes the accessor raise while
;; the row's value is being computed, outside anything that was
;; catching. The file then ends where it stood.
;;
;; WHAT THAT COSTS IS NOT ONE ROW. Every row below the raise goes unrun,
;; and the runner sees no `FAIL` at all: the seeded-defect round reported
;; `0 FAIL, sentinel=False` and scored it as a crash rather than a kill,
;; for three separate defects that the store had in fact answered
;; correctly and visibly. The rows that would have caught them were
;; further down the file.
;;
;; SO THE GUARD GOES WHERE EVERY ROW PASSES THROUGH, and `got` is
;; evaluated inside it. This is a macro rather than a procedure for that
;; one reason: an argument is evaluated before the call, so a procedure
;; could not have guarded it.
;; BOTH HALVES, BECAUSE EITHER CAN RAISE. The first version of this
;; guarded `got` only, and a row whose EXPECTATION is derived from the
;; program's own answer -- `(cadr (cadr (datum-of init-run)))`, the store
;; id that the registry is then required to agree with -- raised while
;; the expectation was being built and ended the file just the same. Two
;; sides of one comparison, and only one of them was being asked whether
;; it could be computed.
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

;; THE PROGRAM UNDER TEST IS FOUND IN BOTH LAYOUTS IT LIVES IN. In a
;; delivery directory the fixture and cli.ss sit side by side; in the
;; repository the fixtures are under test/ and cli.ss is at the root.
;; Looking only beside itself, this fixture started no child at all in
;; the repository -- and every row then read the empty output of a
;; process that never ran.
;; AND IF NEITHER EXISTS IT SAYS SO AT ONCE, rather than letting each
;; row discover it separately.
(define cli
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1))))))
         (dir (if cut (substring self 0 cut) "."))
         (beside (string-append dir "/cli.ss"))
         (above (string-append dir "/../cli.ss")))
    (cond
      ((file-exists? beside) beside)
      ((file-exists? above) above)
      (else (assertion-violation 'cli3
              "cli.ss is neither beside this fixture nor one level up"
              (list beside above))))))

;; AND THE READING SAYS WHICH PROGRAM IT MEASURED. The locator is right
;; -- each layout has exactly one answer -- but the answer never appeared
;; in the output, so a copy of cli.ss sitting beside this fixture was
;; being tested instead of the working tree for a day before anyone
;; noticed, and every row read green the whole time. A run that names its
;; subject shows the drift on its first line.
(printf "cli3 testing ~a sha256 ~a\n"
        cli
        (bytevector->hex
          (sha256 (let ((b (call-with-port (open-file-input-port cli) get-bytevector-all)))
                    (if (eof-object? b) (make-bytevector 0) b)))))



(define scratch (test-dir "wire-outside"))
(define home (string-append scratch "/home"))

(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (slurp path)
  (and (file-exists? path)
       (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
         (if (eof-object? b) (make-bytevector 0) b))))
(define (text-of path) (let ((b (slurp path))) (if b (utf8->string b) "")))

(define n-store 0)
(define (fresh-store!)
  (set! n-store (+ n-store 1))
  (let ((d (string-append scratch "/s" (number->string n-store))))
    (system (string-append "rm -rf " d "; mkdir -p " d))
    (putenv "THEOURGIA_HOME" home)
    d))

(define out-path (string-append scratch "/out.txt"))
(define err-path (string-append scratch "/err.txt"))

;; ONE PLACE THAT RUNS THE PROGRAM, and it answers with the exit code and
;; the LINES it printed. A verb that prints one item per line is judged
;; line by line; collapsing the output into one datum would let a row
;; pass for output in a different order.
(define (run store . args)
  (let* ((cmd (string-append
                "env -u THEOURGIA_INJECT -u THEOURGIA_FAULT -u THEOURGIA_BARRIER "
                "THEOURGIA_HOME=" home " "
                "scheme --script " cli " "
                (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
                "--store " store " > " out-path " 2> " err-path))
         (code (system cmd))
         (text (text-of out-path)))
    (list code
          (let loop ((i 0) (start 0) (out '()))
            (cond
              ((>= i (string-length text)) (reverse out))
              ((char=? (string-ref text i) #\newline)
               (let ((line (substring text start i)))
                 (loop (+ i 1) (+ i 1)
                       (if (= 0 (string-length line))
                           out
                           (cons (guard (e (#t (list 'unreadable line)))
                                   (read (open-string-input-port line)))
                                 out)))))
              (else (loop (+ i 1) start out)))))))

(define (code-of r) (car r))
(define (lines-of r) (cadr r))

(define (init! d) (run d "init"))
(printf "\n== every refusal is readable by the reader it is sent to ==\n")
;; A REFUSAL IS ADDRESSED TO A PROGRAM, and that program accepts the
;; wire whitelist. While refusals carried the caller's intent back to
;; explain themselves, the ones that were RIGHT were the ones that could
;; not be read: the datum a refusal is about is exactly the datum the
;; wire layer will not write. `link a 1 b` came back spelling the
;; relation `\x31;`, and the client reported a transport error at that
;; byte instead of the reason.
;;
;; THE READER IS THE PRODUCT'S OWN, not a description of it. Asserting
;; "the answer looks wire-safe" would be this fixture writing down its
;; own copy of the whitelist; `string->sexpr-extended` is the procedure
;; a consumer actually uses, so a change to the whitelist reaches this
;; row without anyone remembering to update it.
(define (answer-text store . args)
  (apply run store args)
  (text-of out-path))
(define (reads-back? text)
  (guard (e (#t (list 'unreadable
                      (if (and (vector? e) (= 3 (vector-length e)))
                          (vector-ref e 1)
                          "raised"))))
    (let ((wire (string->sexpr-extended text))
          (chez (read (open-string-input-port text))))
      (equal? wire chez))))
(define dW (fresh-store!))
(init! dW)
(define wA (car (lines-of (run dW "insert" "--under" "root" "--title" "A"))))
(define idW (car (car (cadr (assq 'state (cdr wA))))))
;; CONTROL: AN ANSWER THAT IS NOT A REFUSAL ROUND-TRIPS, so a row below
;; that fails is saying something about refusals and not about the
;; reader or the harness.
;; ONE DATUM, BECAUSE THE READER TAKES ONE. `outline` prints a line per
;; block, so the wire reader correctly reported trailing data -- and the
;; row said the product was at fault when the row was.
(want "CONTROL: an ordinary answer is read the same by both readers"
      (reads-back? (answer-text dW "insert" "--under" "root" "--title" "C"))
      #t)
(for-each
  (lambda (case)
    (want (string-append "the refusal is readable: " (car case))
          (reads-back? (apply answer-text dW (cdr case)))
          #t))
  (list
    (cons "a relation that spells as a number"
          (list "link" idW "1" idW))
    (cons "a relation carrying a space"
          (list "link" idW "has part" idW))
    (cons "a field name that is not wire-safe"
          (list "set" idW "field name" "x"))
    (cons "an id position that is not an id"
          (list "del" "7"))
    (cons "too few arguments"
          (list "set" idW))))

(printf "\n== and an rpc answer, read by the same outside reader ==\n")
;; THE SAME QUESTION ONE LAYER DOWN. `rpc-dispatch` answers a caller
;; directly rather than through the command line, and its refusals travel
;; the same wire to the same kind of reader.
(define d1 (fresh-store!))
(init! d1)
;; THE READER IS THE PRODUCT'S OWN, so a change to the whitelist reaches
;; this row without anyone remembering to update a copy of it.
(want "and the whole answer is read the same by the wire reader"
      (let ((text (call-with-string-output-port
                    (lambda (port)
                      (write (rpc-dispatch d1 (list (string->symbol "show me") "x") "a")
                             port)))))
        (guard (e (#t (list 'unreadable
                            (if (and (vector? e) (= 3 (vector-length e)))
                                (vector-ref e 1) "raised"))))
          (equal? (string->sexpr-extended text)
                  (read (open-string-input-port text)))))
      #t)


(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "wire-outside complete\n")
