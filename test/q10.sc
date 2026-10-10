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

;; V9: the one raise that arrives with bytes already on the disk.
;;
;; `with-store-write` turns a raise into an answer by asking one
;; question -- were any bytes written -- and there used to be a second
;; place asking it, inside `commit-then`. The two did not agree: the
;; inner one said `(error unknown (interrupted ...))` and the outer one
;; says `(error unknown (execution-failed ...) (events ...))`. `unknown`
;; promises that a resend will find the records; the events are what a
;; caller looks for them by, and the inner arm did not carry them.
;;
;; WHY THIS FIXTURE BUILDS ITS OWN LIBRARY DIRECTORY. Every fault the
;; injector can fire lands inside `write-line!`, which catches
;; everything from the moment the written flag goes up -- so no input and
;; no fault reaches that arm. The one unguarded stretch between a durable
;; write and the answer is `reduce-apply!` on the record a caller did not
;; ask for by name: the batch receipt. Nothing makes the reducer raise
;; there, so this fixture makes one that does, in a copy, and measures
;; the answer the store gives over a receipt that is already on the disk.
;;
;; THE COPY IS TAKEN FROM THE LIBRARY PATH, NOT FROM THE WORKING TREE.
;; The mutation round mutates the pinned directory and then runs this
;; file; a copy taken from anywhere else would be measuring a version
;; nobody selected.
(import (chezscheme))

(define (test-dir name)
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/" name "-" (number->string (get-process-id)))))
    (let loop ((i 0))
      (when (< i (string-length path))
        (let ((c (string-ref path i)))
          (unless (or (char-alphabetic? c) (char-numeric? c)
                      (memv c '(#\/ #\. #\- #\_)))
            (assertion-violation 'test-dir
              "THEOURGIA_TEST_ROOT may use only letters, digits, / . - and _"
              root)))
        (loop (+ i 1))))
    (system (string-append "rm -rf " path "; mkdir -p " path))
    path))

(define scratch (test-dir "q10"))
(define home (string-append scratch "/home"))
(system (string-append "mkdir -p " home))

(define bad 0)
(define (want-1 label got expect)
  (if (equal? got expect)
      (printf "ok   ~a -> ~s\n" label got)
      (begin (set! bad (+ bad 1))
             (printf "FAIL ~a -> ~s   WANT ~s\n" label got expect))))

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


(define (file->string path)
  (call-with-port (open-file-input-port path (file-options)
                                        'block (native-transcoder))
    get-string-all))

(define (string->file! path text)
  (call-with-port (open-file-output-port path (file-options no-fail)
                                         'block (native-transcoder))
    (lambda (o) (put-string o text))))

;; ONE OCCURRENCE, AND IT REFUSES ANYTHING ELSE. A scaffold that silently
;; patched nothing would leave this fixture measuring the unscaffolded
;; store and reporting it as the scaffolded one -- green for a reason
;; that has nothing to do with the question. A refactor that moves the
;; anchor has to stop the fixture, not quietly widen it.
(define (replace-once path old new)
  (let* ((text (file->string path))
         (n (string-length text))
         (m (string-length old)))
    (let loop ((i 0) (hit #f))
      (cond
        ((> (+ i m) n)
         (if hit
             (string->file! path
               (string-append (substring text 0 hit) new
                              (substring text (+ hit m) n)))
             (assertion-violation 'replace-once "anchor matched nothing" path)))
        ((string=? (substring text i (+ i m)) old)
         (if hit
             (assertion-violation 'replace-once "anchor matched twice" path)
             (loop (+ i 1) i)))
        (else (loop (+ i 1) hit))))))

;; THE LIBRARY PATH IS A LIST, AND ONLY ITS FIRST ENTRY IS A DIRECTORY.
;; Read whole, `CHEZSCHEMELIBDIRS` may be `a:b`, and the copy below then
;; names a path that does not exist. That failure is silent: `cp` writes
;; to stderr and the fixture carries on, the child finds the sources
;; further along the path, and every row still answers -- including the
;; row whose job is to say the copy works.
(define pin
  (let* ((raw (getenv "CHEZSCHEMELIBDIRS"))
         (n (string-length raw)))
    (let loop ((i 0))
      (cond ((= i n) raw)
            ((char=? (string-ref raw i) #\:) (substring raw 0 i))
            (else (loop (+ i 1)))))))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
(define lib (string-append scratch "/lib"))
(system (string-append "mkdir -p " lib "/theourgia"))
;; ONLY THE STORE'S OWN SOURCES ARE COPIED. The dependencies stay where
;; they are and are found by putting the copy first on the path, so this
;; fixture never has a second opinion about what (igropyr ...) is.
(system (string-append "cp " pin "/theourgia/*.sc " lib "/theourgia/"))
;; AND THE COPY IS COUNTED BEFORE ANYTHING IS MEASURED AGAINST IT. A
;; fixture that scaffolds a copy and then measures the ORIGINAL reports
;; the store's own behaviour as the scaffolded one; nothing downstream
;; can tell those apart, because both answer.
(let ((n (let ((out (string-append scratch "/copied.txt")))
           (system (string-append "ls " lib "/theourgia/*.sc 2>/dev/null | wc -l > " out))
           (string->number
             (let ((t (file->string out)))
               (let loop ((i 0))
                 (cond ((= i (string-length t)) "0")
                       ((char-numeric? (string-ref t i))
                        (let scan ((j i))
                          (if (or (= j (string-length t))
                                  (not (char-numeric? (string-ref t j))))
                              (substring t i j)
                              (scan (+ j 1)))))
                       (else (loop (+ i 1))))))))))
  (unless (and n (> n 0))
    (assertion-violation 'q10 "the scaffold copied no sources" pin))
  (printf "scaffold: copied ~a sources from ~a\n" n pin))
;; THE COPY SHADOWS, THE ORIGINAL PATH STILL RESOLVES. The child gets
;; the copy first and then everything this fixture was given -- so
;; `(igropyr ...)` is found wherever the caller had it, and only
;; `(theourgia ...)` comes from the copy. Naming `pin` alone here
;; required the first entry to carry the dependencies too, which is an
;; assumption about the caller's path that nothing checks.
(define libdirs
  (string-append lib ":" (getenv "CHEZSCHEMELIBDIRS")))

(define probe (string-append scratch "/probe.sc"))
;; TWO PHASES IN ONE SCRIPT, BECAUSE THE FAULT IS ONE-SHOT. A request
;; needs a cursor that names a record this store has, so something must
;; be written before the batch can be sent -- and if that write happens
;; in the same process, its own commit spends the injected fault before
;; the batch ever reaches one. The phases are separate runs against the
;; same store: the first is never armed, the second may be.
(string->file! probe
  (string-append
    "(import (chezscheme) (theourgia store))\n"
    "(define d \"" scratch "/store\")\n"
    "(define phase (cadr (command-line)))\n"
    "(define a '(insert root #f ((kind . section) (title . \"A\"))))\n"
    "(define b '(insert root #f ((kind . section) (title . \"B\"))))\n"
    "(if (string=? phase \"setup\")\n"
    "    (begin\n"
    "      (system (string-append \"mkdir -p \" d))\n"
    "      (store-init! d)\n"
    "      (let* ((r (car (with-store-write d (lambda (st v) (list a)) \"t\")))\n"
    "             (ev (car (cadr (assq 'events (cdr r))))))\n"
    "        (write ev) (newline)))\n"
    "    (let ((ev (cons (caddr (command-line))\n"
    "                    (string->number (cadddr (command-line))))))\n"
    "      (write (with-store-write d (lambda (st v) (list a b)) \"t\"\n"
    "               (make-write-request \"t\" 'insert (list \"root\" \"AB\") \"r-1\" ev)))\n"
    "      (newline)))\n"))

;; THE READER SAYS WHAT IT IS LOOKING FOR. The library prints a
;; `(theourgia machine-home ...)` banner before anything else, and a
;; reader that takes the first datum that happens to be a pair reads the
;; banner as the answer -- silently, because a banner IS a pair. Each
;; phase names the shape it expects instead.
(define (run-child dirs phase args fault want?)
  (let ((out (string-append scratch "/probe.out")))
    (system (string-append "rm -f " out))
    (system (string-append
              (if fault
                  (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT='" fault "' ")
                  "env -u THEOURGIA_INJECT -u THEOURGIA_FAULT ")
              "env -u THEOURGIA_BARRIER "
              "THEOURGIA_HOME=" home
              " CHEZSCHEMELIBDIRS=" dirs
              " CHEZSCHEMELIBEXTS='" exts "'"
              " perl -e 'alarm 120; exec @ARGV' scheme --script " probe
              " " phase " " args
              " > " out " 2>&1"))
    (let ((text (file->string out)))
      (guard (e (#t (list 'unreadable text)))
        (let ((in (open-string-input-port text)))
          (let loop ((d (read in)))
            (cond ((eof-object? d) (list 'no-answer text))
                  ((want? d) d)
                  (else (loop (read in))))))))))

;; THE SETUP RUN IS NEVER ARMED, so the cursor it returns is a real one
;; and the fault is still unspent when the batch runs.
;; WHICH LIBRARIES THE CHILD RUNS AGAINST IS AN ARGUMENT, so a row can
;; ask the same question of the scaffolded copy and of the store itself.
;; A twin that could only reach the scaffold would be comparing two
;; readings of one thing.
(define (run-probe/dirs dirs . fault)
  (system (string-append "rm -rf " scratch "/store"))
  (let ((ev (run-child dirs "setup" "" #f
                       (lambda (d) (and (pair? d) (string? (car d))
                                        (integer? (cdr d)))))))
    (if (not (and (pair? ev) (string? (car ev))))
        (list (list 'setup-failed ev))
        (run-child dirs "batch"
                   (string-append (car ev) " " (number->string (cdr ev)))
                   (if (pair? fault) (car fault) #f)
                   (lambda (d) (and (pair? d) (pair? (car d))))))))

(define (run-probe . fault) (apply run-probe/dirs libdirs fault))

;; THE HEADS OF A BATCH'S ANSWERS, which is all the control row needs.
(define (heads answer)
  (if (and (list? answer) (for-all pair? answer))
      (map car answer)
      (list 'not-a-batch answer)))

(printf "\n== a receipt that is on the disk is answered with its events ==\n")
;; CONTROL: THE COPY IS A WORKING STORE. Without this row every row below
;; also passes against a library directory that was copied wrong, where
;; nothing runs and nothing is written.
(want "CONTROL: unscaffolded, the batch runs and both items are answered"
      (heads (run-probe))
      '(ok ok))

;; THE SCAFFOLD. `reduce-apply!` is the first thing to run after the
;; receipt is durable, and nothing guards it between there and the
;; answer. Made to raise, it puts the store in the one state this arm
;; exists to describe: bytes written, work not finished.
(replace-once (string-append lib "/theourgia/reduce.sc")
  "  (define (reduce-apply! r writer seq deps payload . rest)\n    (let ((have (assoc writer (reduction-applied r))))"
  (string-append
    "  (define (reduce-apply! r writer seq deps payload . rest)\n"
    "    (when (and (pair? payload) (eq? (car payload) 'batch))\n"
    "      (assertion-violation 'reduce-apply! \"scaffolded reducer failure on a receipt\" payload))\n"
    "    (let ((have (assoc writer (reduction-applied r))))"))

(define scaffolded (run-probe))

(want "the answer is one answer, and it is the word for an outcome in doubt"
      (and (list? scaffolded) (= 1 (length scaffolded))
           (pair? (car scaffolded))
           (list (car (car scaffolded)) (cadr (car scaffolded))))
      '(error unknown))
;; THE ROW THAT KEEPS THE SECOND SUPPLIER OUT. An arm that answers
;; `unknown` without the events is telling a caller to come back and
;; look for records it has not named. This row fails the moment such an
;; arm is put back.
(want "and it names the records a resend will find"
      (let* ((one (and (pair? scaffolded) (car scaffolded)))
             (evs (and (pair? one) (assq 'events (cddr one)))))
        (and evs (list? (cadr evs)) (> (length (cadr evs)) 0)
             (for-all (lambda (e) (and (pair? e) (string? (car e))
                                       (integer? (cdr e))))
                      (cadr evs))))
      #t)
;; AND THE REASON IS THE ONE THE SCAFFOLD RAISED, not a word chosen by
;; whichever handler caught it first. Two handlers used different words
;; for this; a row that accepted either would not have noticed.
(want "the reason carries the failure the store actually had"
      (let* ((one (car scaffolded))
             (r (assq 'execution-failed (cddr one))))
        (and r (cadr r)))
      "scaffolded reducer failure on a receipt")

(printf "\n== when the log cannot be flushed, that is what the answer says ==\n")
;; THE GUARD ROUND THE THUNK IS WHAT LETS THE COMMIT RUN AT ALL. Take it
;; away and a raise leaves `commit-then` before `session-commit!` is
;; reached, so the request is answered without the barrier this store
;; runs before it answers anything.
;;
;; AND AFTER THE SECOND HANDLER WAS REMOVED, THIS IS THE ONLY PLACE THE
;; DIFFERENCE SHOWS. While `commit-then` had an arm of its own, a raise
;; with bytes on the disk was answered there and the two versions
;; disagreed about every such raise. Now the enclosing guard answers
;; them all the same way -- correctly -- and the versions part company
;; only when the raise and a failing barrier arrive together. That
;; window is narrower because of a change made in this batch, which is
;; the reason this row exists rather than being left to the next one.
;; THE FAULT NAMES A PATH, because `fsync-fail` is path-scoped; the
;; segment is what the commit barrier flushes.
(define flush-fault "fsync-fail@commit:file=000001")
(define barred (run-probe flush-fault))
(want "a raise over a log that will not flush is answered as a failed barrier"
      (let ((one (and (pair? barred) (car barred))))
        (and (pair? one)
             (list (car one) (cadr one)
                   (and (pair? (cddr one)) (car (caddr one))))))
      '(error unknown commit-barrier-failed))
;; TWIN: THE SAME RAISE WITHOUT THE FAULT IS A DIFFERENT ANSWER, which
;; is what says the word above came from the barrier and not from the
;; raise. Measured first, because the obvious twin is the wrong one: the
;; same FAULT without the raise also answers `commit-barrier-failed`, so
;; a row written that way compares two readings of one thing and would
;; have passed against anything.
(want "TWIN: the same raise with no fault is not answered as a barrier failure"
      (let* ((noflt (run-probe))
             (one (and (pair? noflt) (car noflt))))
        (and (pair? one)
             (not (and (pair? (cddr one))
                       (pair? (caddr one))
                       (eq? 'commit-barrier-failed (car (caddr one)))))))
      #t)

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "q10 complete\n")
