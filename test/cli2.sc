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

;; The publish verb: the local end of sync, and the lock it waits on.
;;
;; Every rule about whether a segment may be installed lives in the log
;; layer, and log18 and log19 are where those rules are judged. What is
;; left for the command is narrow and none of it is obvious: it has to
;; turn an answer into an exit code a shell can read, refuse an argument
;; before converting it, tell "no such file" from "refused", and WAIT
;; when another process holds the store rather than reporting a failure a
;; sync client would have to re-send to understand.
;;
;; THE WAITING ROW STARTS ITS CHILD ASYNCHRONOUSLY, and it must: the
;; parent holds the lock for the whole of that row, so a synchronous
;; start would deadlock the fixture rather than measure it. The holder
;; parks at a barrier the product announces before it blocks, so nothing
;; here is synchronised by elapsed time -- `sleep` appears only as the
;; interval between looks at a file another process is writing.

(import (chezscheme) (theourgia store) (theourgia reduce) (theourgia log)
        (theourgia ffi) (theourgia wire)
        (only (theourgia digest) sha256 bytevector->hex))

;; THE RANGE A SEGMENT HOLDS, READ OUT OF THE SEGMENT. A manifest entry
;; declares first and last sequence beside the hash. A fixture that
;; declared them from memory would be asserting its own arithmetic
;; rather than what it actually wrote, and the product's own check for a
;; manifest that contradicts its bytes would then be measuring the
;; fixture.
(define (segment-seqs bytes)
  (let ((text (utf8->string bytes)))
    (let loop ((i 0) (start 0) (seqs '()))
      (cond
        ((>= i (string-length text)) (reverse seqs))
        ((char=? (string-ref text i) #\newline)
         (let ((r (decode-line (string->utf8 (substring text start (+ i 1))))))
           (loop (+ i 1) (+ i 1)
                 (if (and (pair? r) (eq? (car r) 'ok)) (cons (cadr r) seqs) seqs))))
        (else (loop (+ i 1) start seqs))))))

(define (manifest-entry n hash bytes)
  (let ((seqs (segment-seqs bytes)))
    (if (null? seqs)
        (list n hash 1 1)
        (list n hash (apply min seqs) (apply max seqs)))))

(define (manifest-entry-text n hash bytes)
  (let ((e (manifest-entry n hash bytes)))
    (string-append "(" (number->string n) " \"" hash "\" "
                   (number->string (caddr e)) " " (number->string (cadddr e)) ")")))

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
;; delivery directory the fixture and cli.sc sit side by side; in the
;; repository the fixtures are under test/ and cli.sc is at the root.
;; Looking only beside itself, this fixture started no child at all in
;; the repository -- and every row then read the empty output of a
;; process that never ran.
;; AND IF NEITHER EXISTS IT SAYS SO AT ONCE, rather than letting each
;; row discover it separately.
;; THE PROGRAM UNDER TEST IS FOUND IN BOTH LAYOUTS IT LIVES IN. In a
;; delivery directory the fixture and cli.sc sit side by side; in the
;; repository the fixtures are under test/ and cli.sc is at the root.
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
         (beside (string-append dir "/cli.sc"))
         (above (string-append dir "/../cli.sc")))
    (cond
      ((file-exists? beside) beside)
      ((file-exists? above) above)
      (else (assertion-violation 'cli2
              "cli.sc is neither beside this fixture nor one level up"
              (list beside above))))))

;; AND THE READING SAYS WHICH PROGRAM IT MEASURED. The locator is right
;; -- each layout has exactly one answer -- but the answer never appeared
;; in the output, so a copy of cli.sc sitting beside this fixture was
;; being tested instead of the working tree for a day before anyone
;; noticed, and every row read green the whole time. A run that names its
;; subject shows the drift on its first line.
(printf "cli1 testing ~a sha256 ~a\n"
        cli
        (bytevector->hex
          (sha256 (let ((b (call-with-port (open-file-input-port cli) get-bytevector-all)))
                    (if (eof-object? b) (make-bytevector 0) b)))))


(define scratch (test-dir "cli2"))
(define W "wwwlocl0")
(define M "mirrorz9")
(define home (string-append scratch "/home"))
(define d (string-append scratch "/store"))

(define (rec seq payload)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" '() (storable-encode payload)))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (recs lo hi)
  (apply cat (let loop ((i lo))
               (if (> i hi) '() (cons (rec i (list 'put (list (cons 'kind 'section)))) (loop (+ i 1)))))))
(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (slurp path)
  (and (file-exists? path)
       (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
         (if (eof-object? b) (make-bytevector 0) b))))
(define (text-of path) (let ((b (slurp path))) (if b (utf8->string b) "")))

(define (build!)
  (system (string-append "rm -rf " d " " home "; mkdir -p "
                         d "/writers/" W " " d "/writers/" M " " d "/snap " home))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"c2\"))\n"))
  (call-with-port (open-file-output-port (string-append d "/lock") (file-options no-fail))
    (lambda (p) #f))
  (put! (string-append d "/writers/" W "/owner.sexp") (string->utf8 "((machine \"m\"))\n"))
  (putenv "THEOURGIA_HOME" home)
  (let ((k (instance-install! d))) (owner-install! d W k))
  (put! (string-append d "/writers/" W "/" (segment-file-name 1)) (make-bytevector 0)))

(define cand (string-append scratch "/cand.bin"))
(define out-path (string-append scratch "/out.txt"))
(define err-path (string-append scratch "/err.txt"))

(define (run . args)
  (let* ((cmd (string-append
                "env -u THEOURGIA_INJECT -u THEOURGIA_FAULT -u THEOURGIA_BARRIER "
                "THEOURGIA_HOME=" home " "
                "scheme --script " cli " publish "
                (apply string-append (map (lambda (a) (string-append "'" a "' ")) args))
                "--store " d " > " out-path " 2> " err-path))
         (code (system cmd)))
    ;; A CHILD THAT NEVER RAN IS A READING, NOT AN EXCEPTION: an empty
    ;; stdout reaches `read` as end-of-file, and a row that took the cadr
    ;; of that would die where a caller cannot tell a failed assertion
    ;; from a program that did not start.
    (let ((text (text-of out-path)))
      (list code
            (if (= 0 (string-length text))
                (list 'no-answer (text-of err-path))
                (guard (e (#t (list 'unreadable text)))
                  (read (open-string-input-port text))))))))

(printf "== C1: the verb answers with the log layer's own words ==\n")
(want "a candidate for a fresh writer is published, and the shell reads zero"
      (begin (build!) (put! cand (recs 1 3)) (run M "1" cand))
      (list 0 '(ok (published 1))))
(want "the same candidate again is finished, and still zero"
      (run M "1" cand)
      (list 0 '(ok (idempotent 1))))
;; THE KEPT PATH IS PART OF THE ANSWER AND IS NOT ASSERTED WHOLE. A
;; refused candidate's bytes are kept under a content-addressed name, and
;; a sync client wants to be told where -- but the name is generated, so
;; the row asserts the reason exactly and the path by what it must be:
;; a file that is really there.
(define (reason answer)
  (let ((a (and (pair? answer) (eq? (car answer) 'error) (cadr answer))))
    (if (and (pair? a) (pair? (cdr a)) (pair? (cadr a)) (eq? 'kept (car (cadr a))))
        (car a)
        a)))
(define (kept-path answer)
  (let ((a (and (pair? answer) (eq? (car answer) 'error) (cadr answer))))
    (and (pair? a) (pair? (cdr a)) (pair? (cadr a)) (eq? 'kept (car (cadr a)))
         (cadr (cadr a)))))
(want "a candidate that leaves a hole is refused, and the shell reads one"
      (begin (put! (string-append scratch "/gap.bin") (recs 20 21))
             (let ((r (run M "5" (string-append scratch "/gap.bin"))))
               (list (car r) (reason (cadr r))
                     (and (kept-path (cadr r)) (file-exists? (kept-path (cadr r)))))))
      (list 1 '(segment-layout-conflict
                 (gap-before-candidate (history-ends 3) (candidate-starts 20)))
            #t))
;; A FILE THAT IS NOT THERE IS NOT A REFUSAL. A sync client that could
;; not tell them apart would re-send bytes it never read.
(want "a candidate file that does not exist says so, and names the path"
      (run M "2" (string-append scratch "/absent.bin"))
      (list 1 (list 'error 'no-candidate (list 'path (string-append scratch "/absent.bin")))))

(printf "\n== C2: the arguments are checked by shape before they are used ==\n")
;; `string->number' implements the whole numeric syntax, and
;; `#e1e99999999' is a request to build an exact integer of ten billion
;; digits: it does not refuse, it allocates until the machine is gone,
;; and no check placed after it ever runs. The row is timed, because the
;; failure it guards against is not a wrong answer but no answer.
(define (timed-run . args)
  (let* ((t0 (current-time 'time-monotonic))
         (r (apply run args))
         (t1 (current-time 'time-monotonic)))
    (list (car r) (cadr r) (< (- (time-second t1) (time-second t0)) 20))))
(want "a segment written as an exact power of ten is refused at once"
      (timed-run M "#e1e99999999" cand)
      (list 1 '(usage (publish <writer> <segment> <file> (<sha256>))) #t))
(want "so is one that is not digits at all, and one that is zero"
      (list (cadr (run M "abc" cand)) (cadr (run M "0" cand)))
      (list '(usage (publish <writer> <segment> <file> (<sha256>)))
            '(usage (publish <writer> <segment> <file> (<sha256>)))))
(want "TWIN: an ordinary segment number is not refused"
      (begin (build!) (put! cand (recs 1 3)) (car (run M "1" cand)))
      0)

(printf "\n== C3: the hash is the sender's declaration ==\n")
;; Computing it here from the same bytes would compare a number with
;; itself. Given one, it is checked; given none, the bytes on disk are
;; taken as their own.
(want "a declared hash that is not these bytes' hash is refused"
      (begin (build!) (put! cand (recs 1 3))
             (run M "1" cand "0000000000000000000000000000000000000000000000000000000000000000"))
      (list 1 '(error invalid-candidate sha-mismatch)))
(want "TWIN: the sender's own hash, correctly declared, publishes"
      (run M "1" cand (bytevector->hex (sha256 (slurp cand))))
      (list 0 '(ok (published 1))))

(printf "\n== C4: it waits for the store rather than failing ==\n")
;; A SECOND PROCESS PUBLISHING, OR A READER HOLDING THE SHARED LOCK,
;; MAKES THIS BLOCK. A sync client told "busy" would have to re-send to
;; find out whether it was refused or merely queued, and those are
;; different facts about its bytes.
;;
;; THE HOLDER PARKS AT A BARRIER THE PRODUCT ANNOUNCES BEFORE IT BLOCKS,
;; so the row waits for an event rather than for a duration; `sleep`
;; appears only as the interval between looks at a file another process
;; is writing. And the publisher is started ASYNCHRONOUSLY: the holder
;; has the lock for the whole of this row, so a synchronous start would
;; hang the fixture instead of measuring it.
;; THE WINDOW IS THE PATTERN'S OWN LENGTH. Written with a hard-coded
;; width it compared a 28-character pattern against 30-character windows,
;; which `string=?` answers #f for without complaining -- so the search
;; never matched, and a child that had parked correctly was reported as
;; a child that never parked.
(define (has-substring? text pattern)
  (let ((n (string-length text)) (m (string-length pattern)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring text i (+ i m)) pattern) #t)
            (else (loop (+ i 1)))))))

(define fifo (string-append scratch "/fifo"))
(define trace (string-append scratch "/trace"))
(define holder (string-append scratch "/holder.sc"))
(define pub-out (string-append scratch "/pub.out"))
(define pub-done (string-append scratch "/pub.done"))

(define (write-holder!)
  (put! holder
        (string->utf8
          (string-append
            "#!chezscheme\n(import (chezscheme) (theourgia log) (theourgia ffi))\n"
            "(putenv \"THEOURGIA_HOME\" \"" home "\")\n"
            "(let ((s (log-begin \"" d "\" (lambda args 'applied))))\n"
            "  (let ((v (session-view s)))\n"
            "    (when v\n"
            "      (session-append! s (make-frame (view-revision v) (view-epoch v)\n"
            "                                     (view-writer v) (view-expect-seq v)\n"
            "                                     \"agent:claude\" '() '(put \"w.1\" ())))))\n"
            "  (log-end! s))\n"))))

(define (waited-for-the-lock . flags)
  (system (string-append "rm -f " fifo " " trace " " pub-out " " pub-done))
  (system (string-append "mkfifo " fifo))
  (write-holder!)
  (put! cand (recs 1 3))
  ;; the holder takes the store and parks inside it
  (system (string-append "THEOURGIA_INJECT=on THEOURGIA_TRACE=1 "
                         "THEOURGIA_BARRIER=before-append:" fifo " "
                         "scheme --script " holder " > /dev/null 2> " trace
                         " & echo $! > " scratch "/holder.pid"))
  (let ((parked (let loop ((n 0))
                  (cond ((> n 400) #f)
                        ((has-substring? (text-of trace) "(trace barrier before-append") #t)
                        (else (system "sleep 0.05") (loop (+ n 1)))))))
    (if (not parked)
        (begin (system (string-append "kill -9 $(cat " scratch "/holder.pid) 2>/dev/null"))
               'holder-never-parked)
        (begin
          ;; asynchronous, because this process cannot wait for it yet
          (system (string-append
                    "( env -u THEOURGIA_FAULT -u THEOURGIA_BARRIER "
                    (if (null? flags) "-u THEOURGIA_INJECT " (car flags))
                    "THEOURGIA_HOME=" home " scheme --script " cli " publish "
                    M " 1 " cand " --store " d " > " pub-out " 2>/dev/null"
                    "; echo done > " pub-done " ) &"))
          (system "sleep 1")
          (let ((blocked (not (file-exists? pub-done))))
            (system (string-append "printf x > " fifo))
            (let ((finished (let loop ((n 0))
                              (cond ((> n 400) #f)
                                    ((file-exists? pub-done) #t)
                                    (else (system "sleep 0.05") (loop (+ n 1)))))))
              (list blocked finished
                    (if finished
                        (guard (e (#t (list 'unreadable (text-of pub-out))))
                          (read (open-string-input-port (text-of pub-out))))
                        'no-answer))))))))

(want "a publish started while another process holds the store waits, then completes"
      (begin (build!) (waited-for-the-lock))
      (list #t #t '(ok (published 1))))
;; AND THE LOCK IS WHAT MAKES IT WAIT. A lock is invisible to every
;; assertion about the answers that come back, so the row above would
;; read the same for a publish that merely takes a second. Run again
;; with the product's lock removed and the same publish is NOT blocked:
;; that is the reading the row is distinguishing itself from.
(want "CONTROL: with the product's lock removed, the same publish is not blocked"
      (let ((r (begin (build!)
                      (waited-for-the-lock "THEOURGIA_INJECT=on THEOURGIA_NOFLOCK=1 "))))
        (car r))
      #f)
;; TWIN: THE SAME PUBLISH WITH NOBODY HOLDING THE STORE does not wait --
;; otherwise the row above would pass for a publish that always blocks
;; for a second and then runs.
(want "TWIN: with nothing holding the store it does not wait at all"
      (begin (build!) (put! cand (recs 1 3))
             (let* ((t0 (current-time 'time-monotonic))
                    (r (run M "1" cand))
                    (t1 (current-time 'time-monotonic)))
               (list (car r) (cadr r) (< (- (time-second t1) (time-second t0)) 5))))
      (list 0 '(ok (published 1)) #t))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "cli2 complete\n")
