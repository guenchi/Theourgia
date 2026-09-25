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

;; Request identity in the record, and the verbs that keep books.
;;
;; A REQUEST'S IDENTITY TRAVELS IN THE RECORD IT WROTE. Who asked, which
;; request it was, which sub-operation this is, the fingerprint of what
;; was asked, the plan it belongs to and the cursor it was written
;; against are all in the actor -- so any one record answers "was this
;; request executed", and a reader never infers the origin from the
;; physical writer. That inference is wrong for every record a successor
;; writer carries after an adopt, which is exactly when the question is
;; being asked.
;;
;; AND THE SHAPE IS CHECKED, NOT THE LENGTH. Two slots changed meaning
;; when the actor grew from five to six: a bare request id became the
;; pair `(origin . req-id)`, and `plan-event-id` was inserted before
;; `after`. A length test accepts an actor whose fields have all shifted
;; by one -- right size, every field reading its neighbour.
;;
;; BOOKKEEPING VERBS ARE UNDERSTOOD AND CHANGE NOTHING. They advance the
;; cursor and carry premises; they touch no block. A verb from outside
;; the table is not silently nothing: until now an older build reading a
;; newer store applied such a record, counted it as applied, and reported
;; a state missing it without saying so.

(import (chezscheme) (theourgia request) (theourgia store) (theourgia reduce) (theourgia log)
        (theourgia ffi) (theourgia wire)
        (only (theourgia crc32) crc32-string-hex)
        (only (theourgia reduce) caller-payload-reason)
        (only (theourgia project) block-text)
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
;; makes the accessor raise while the row's value is being computed --
;; outside anything that was catching. The file then ends where it
;; stood, every row below it goes unrun, and the runner sees no `FAIL`
;; at all: the round scored three such defects as crashes with no
;; failures, for answers the store had in fact got right and said
;; plainly.
;;
;; IT IS A MACRO FOR ONE REASON: an argument is evaluated before the
;; call, so a procedure could not have guarded `got`.
;;
;; IT DOES NOT COVER EVERYTHING. Top-level definitions between rows are
;; still outside it, and a raise there still ends the file.
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
;; delivery directory the fixture and core.sc sit side by side; in the
;; repository the fixtures are under test/ and core.sc is at the root.
;; Looking only beside itself, this fixture started no child at all in
;; the repository -- and every row then read the empty output of a
;; process that never ran.
;; AND IF NEITHER EXISTS IT SAYS SO AT ONCE, rather than letting each
;; row discover it separately.

;; THE PROGRAM UNDER TEST IS FOUND IN BOTH LAYOUTS IT LIVES IN. In a
;; delivery directory the fixture and core.sc sit side by side; in the
;; repository the fixtures are under test/ and core.sc is at the root.
;; Looking only beside itself, this fixture started no child at all in
;; the repository -- and every row then read the empty output of a
;; process that never ran.
;; AND IF NEITHER EXISTS IT SAYS SO AT ONCE, rather than letting each
;; row discover it separately.
;; THE PROGRAM UNDER TEST IS FOUND IN BOTH LAYOUTS IT LIVES IN. In a
;; delivery directory the fixture and core.sc sit side by side; in the
;; repository the fixtures are under test/ and core.sc is at the root.
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
         (beside (string-append dir "/core.sc"))
         (above (string-append dir "/../core.sc")))
    (cond
      ((file-exists? beside) beside)
      ((file-exists? above) above)
      (else (assertion-violation 'q1
              "core.sc is neither beside this fixture nor one level up"
              (list beside above))))))

;; AND THE READING SAYS WHICH PROGRAM IT MEASURED. The locator is right
;; -- each layout has exactly one answer -- but the answer never appeared
;; in the output, so a copy of core.sc sitting beside this fixture was
;; being tested instead of the working tree for a day before anyone
;; noticed, and every row read green the whole time. A run that names its
;; subject shows the drift on its first line.
(printf "q1 testing ~a sha256 ~a\n"
        cli
        (bytevector->hex
          (sha256 (let ((b (call-with-port (open-file-input-port cli) get-bytevector-all)))
                    (if (eof-object? b) (make-bytevector 0) b)))))




(define scratch (test-dir "q1"))
(define home (string-append scratch "/home"))
(define out-path (string-append scratch "/out.txt"))
(define err-path (string-append scratch "/err.txt"))

(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (slurp path)
  (and (file-exists? path)
       (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
         (if (eof-object? b) (make-bytevector 0) b))))
(define (text-of path) (let ((b (slurp path))) (if b (utf8->string b) "")))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))

(define n-store 0)
(define (fresh-store!)
  (set! n-store (+ n-store 1))
  (let ((d (string-append scratch "/s" (number->string n-store))))
    (system (string-append "rm -rf " d "; mkdir -p " d))
    (putenv "THEOURGIA_HOME" home)
    d))
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
                       (if (= 0 (string-length line)) out
                           (cons (guard (e (#t (list 'unreadable line)))
                                   (read (open-string-input-port line)))
                                 out)))))
              (else (loop (+ i 1) start out)))))))
(define (code-of r) (car r))
(define (lines-of r) (cadr r))

(printf "== T1: the actor carries a whole identity, checked by shape ==\n")
(define (accepts? actor)
  (not (guard (e (#t #t)) (encode-record 1 1 actor '() (storable-encode '(a))) #f)))
(define good (list "agent:claude" (cons "w" "req-1") 0 "fp" (cons "w" 2) (cons "w" 3)))
(want "a six element request actor is accepted, and reads back whole"
      (let ((r (decode-line (encode-record 1 1 good '() (storable-encode '(a))))))
        (list (car r) (list-ref r 3)))
      (list 'ok good))
(want "a plain name is still an actor"
      (accepts? "agent:claude") #t)
;; THE ROW A LENGTH CHECK PASSES. This is the old five-slot actor with
;; one field appended: the right number of elements, every field after
;; the first reading the one beside it.
(want "six elements in the old order are refused"
      (accepts? (list "agent:claude" "req-1" 'single "fp" #f (cons "w" 3)))
      #f)
(want "and so is the five element actor it came from"
      (accepts? (list "agent:claude" "req-1" 0 "fp" (cons "w" 3)))
      #f)
(want "each slot is checked: identity, sub, fingerprint, plan, after"
      (list (accepts? (list "a" "req-1" 0 "fp" #f (cons "w" 3)))
            (accepts? (list "a" (cons "w" "r") 'other "fp" #f (cons "w" 3)))
            (accepts? (list "a" (cons "w" "r") 0 42 #f (cons "w" 3)))
            (accepts? (list "a" (cons "w" "r") 0 "fp" "not-an-event" (cons "w" 3)))
            (accepts? (list "a" (cons "w" "r") 0 "fp" #f #f)))
      (list #f #f #f #f #f))
(want "TWIN: the shapes a request really uses are all accepted"
      (list (accepts? good)
            (accepts? (list "a" (cons "w" "r") 'single "fp" #f (cons "w" 1)))
            (accepts? (list "a" (cons "w" "r") 'plan "fp" #f (cons "w" 1)))
            (accepts? (list "a" (cons "w" (list 'batch "b1" 2)) 2 "fp" (cons "w" 1) (cons "w" 3))))
      (list #t #t #t #t))

(printf "\n== T2: bookkeeping verbs, and verbs this build does not know ==\n")
;; A BOOKKEEPING RECORD ADVANCES THE CURSOR AND CHANGES NO BLOCK. The
;; state hash is taken over the blocks alone, so this holds by
;; construction -- and the row is here because it is the property the
;; rest of the request machinery will lean on, not because the code
;; looks like it.
(define (rec seq payload)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" '() (storable-encode payload)))
;; A RECORD FRAMED BY HAND, BECAUSE `encode-record` REFUSES TO MAKE THIS
;; ONE. It checks its dependencies and raises on `(7)` -- correctly, that
;; is the write path defending itself. The record this row needs is one
;; that arrived from somewhere that did not check, so it is assembled the
;; way the format says and given a correct CRC. Only the dependency list
;; is wrong: the frame is right, the checksum is right, the payload is a
;; proper `put`.
(define (raw-record seq deps-text payload-text)
  (let* ((text (string-append "(" (number->string seq) " "
                              (number->string (+ 1757300000000 seq))
                              " \"peer\" " deps-text " " payload-text ")"))
         (hex (crc32-string-hex text)))
    (string->utf8 (string-append hex " " text "\n"))))
(define (mirror-file name . recs)
  (let ((p (string-append scratch "/" name)))
    (put! p (apply cat recs))
    p))
(define (state-of d) (open-and-reduce d))
;; A POISON RECORD IS PLANTED, NOT PUBLISHED. Publish refuses a record
;; the reader would refuse, so it can no longer be the way these rows put
;; one in a store; they write the mirror's one segment and its manifest
;; entry directly, as bytes arriving from somewhere that did not check.
;; `planted?` reads both back, and every row that plants asks it first:
;; a row about how the store treats a poison record says nothing if the
;; record never got there.
(define (plant-mirror! d writer bytes)
  (let ((dir (writer-directory d writer)))
    (system (string-append "mkdir -p " dir))
    (put! (string-append dir "/" (segment-file-name 1)) bytes)
    (write-manifest! d writer (list (list 1 (segment-sha bytes) 1 1)))))
(define (planted? d writer bytes)
  (and (equal? (slurp (string-append (writer-directory d writer) "/" (segment-file-name 1)))
               bytes)
       (equal? (read-manifest d writer) (list (list 1 (segment-sha bytes) 1 1)))))

(define d1 (fresh-store!))
(run d1 "init")
(run d1 "insert" "--under" "root" "--title" "A")
(define hash-before (state-hash (state-of d1)))
(define bookkeeping
  (mirror-file "book.bin"
               (rec 1 '(plan "r1" "fp" (("w" . 1)) ()))
               (rec 2 '(batch "b1" "fp" (("w" . 1)) ()))
               (rec 3 '(resolve ("w" . "r1") "fp" all executed (("w" . 1) ("w" . 2)) (supersedes)))))
(want "CONTROL: three bookkeeping records publish"
      (car (lines-of (run d1 "publish" "mirrorzz" "1" bookkeeping)))
      '(ok (published 1)))
(want "they advance that writer's cursor"
      (cdr (assoc "mirrorzz" (reduce-applied-cut (state-of d1))))
      3)
(want "and they change no block, so the state hash is what it was"
      (state-hash (state-of d1))
      hash-before)
(want "nor are they reported as verbs this build does not know"
      (lines-of (run d1 "conflicts"))
      '())

;; A VERB FROM OUTSIDE THE TABLE IS AN OBSERVATION, NOT A NO-OP. Applied
;; silently, it would leave an older build reporting a state missing
;; those records without saying anything was missing.
(define d2 (fresh-store!))
(run d2 "init")
(want "CONTROL: the record publishes and is delivered"
      (car (lines-of (run d2 "publish" "mirrorzz" "1"
                          (mirror-file "unknown.bin" (rec 1 '(frobnicate "x"))))))
      '(ok (published 1)))
(want "an unrecognised verb is reported, naming the record and the verb"
      (lines-of (run d2 "conflicts"))
      (list (list 'unknown-verb (list 'event "mirrorzz" 1) (list 'verb 'frobnicate))))
;; A PAYLOAD THAT IS NOT A FORM IS NOT AN UNKNOWN VERB. It used to be
;; reported as `(verb malformed)`, which names a verb no one wrote; it is
;; now `malformed-record` with the reason, alongside every other record
;; this reducer cannot apply. The two answers are kept apart on purpose:
;; an unknown verb is a record from a NEWER build and wants an upgrade, a
;; malformed record is broken and wants repair.
;; A BROKEN ENVELOPE IS AN INTEGRITY ERROR, NOT A REDUCER NOTE. The
;; reducer answers for records it cannot APPLY; a record whose envelope
;; is malformed never reaches it, because the scheduler reads the
;; envelope's fields before any of that. So this one is named by `check`
;; against its writer, with the segment, the offset and the reason -- and
;; the store's verdict is `damaged`, which is the word an operator acts
;; on.
(want "a record whose payload is not a form is an integrity error, named and located"
      (let ((d (fresh-store!))
            (poison (raw-record 1 "()" "hello")))
        (run d "init")
        (plant-mirror! d "mirrorzz" poison)
        (if (not (planted? d "mirrorzz" poison))
            'not-planted
            (let* ((c (car (lines-of (run d "check"))))
                   (writers (cadr (assq 'writers (cdr c))))
                   (mirror (assoc "mirrorzz" writers)))
              (list (cadr (assq 'integrity (cdr mirror)))
                    (cadr (assq 'verdict (cdr c)))))))
      (list (list (list 'frame (list 'segment 1) (list 'offset 0)
                        (list 'reason 'payload-not-a-form)))
            'damaged))
;; AND THE CASE THAT ACTUALLY ENDED A READ: malformed DEPENDENCIES. The
;; scheduler reads both halves of every dependency before the reducer is
;; consulted at all, so `deps` of `(7)` raised `7 is not a pair` inside
;; scheduling -- past the CRC, past the frame check, and before any of
;; the reducer's own care about records it cannot apply. The payload
;; above is fine; only the envelope is wrong.
(want "a record whose dependencies are malformed is an integrity error, not a raise"
      (let ((d (fresh-store!))
            (poison (raw-record 1 "(7)" "(put ((title . \"Bad deps\")))")))
        (run d "init")
        (plant-mirror! d "mirrorzz" poison)
        (if (not (planted? d "mirrorzz" poison))
            'not-planted
            (let* ((c (car (lines-of (run d "check"))))
                   (writers (cadr (assq 'writers (cdr c))))
                   (mirror (assoc "mirrorzz" writers)))
              (list (cadr (assq 'integrity (cdr mirror)))
                    (length (lines-of (run d "outline")))))))
      (list (list (list 'frame (list 'segment 1) (list 'offset 0)
                        (list 'reason 'deps-malformed)))
            0))
;; TWIN: WELL-FORMED DEPENDENCIES STILL SCHEDULE. Without this the row
;; above is also passed by a build that calls every dependency list
;; malformed -- which would stop every batch in the product.
;; TWIN: A DEPENDENCY THAT IS REALLY SCHEDULED, not one that names the
;; record just before it on its own writer. `mirrorzz:2` depending on
;; `mirrorzz:1` is satisfied by the writer's own order, so it would be
;; delivered even by a store that ignored dependencies entirely. A
;; dependency ACROSS writers has to wait for the other writer's record
;; and become deliverable when it arrives -- that is the behaviour the
;; scheduler exists for, and the row below is the one that needs it.
;; A POSITION VALUE FROM ELSEWHERE. The field collection's SHAPE was
;; checked and its `parent` and `ord` VALUES were not, so a record
;; carrying `(ord . "oops")` applied cleanly, left no note, and then the
;; store raised the moment it had to sort siblings. The caller's
;; reserved-name rule cannot reach this: it stops a caller writing `ord`
;; at all, and says nothing about a record that arrived from another
;; machine.
;;
;; A SIBLING IS REQUIRED, so that sorting actually has to compare the two
;; values. With one child there is nothing to order and the bad value is
;; never looked at.
(for-each
  (lambda (case)
    (want (string-append "a foreign record with " (car case) " is noted, and the store still sorts")
          (let ((d (fresh-store!)))
            (run d "init")
            (run d "publish" "mirrorzz" "1"
                 (mirror-file (string-append "pos-" (caddr case) ".bin")
                              (rec 1 '(put ((kind . section) (title . "Sibling")
                                            (parent . root) (ord . 0))))
                              (rec 2 '(put ((kind . section) (title . "Second")
                                            (parent . root) (ord . 1))))
                              (rec 3 (cadr case))))
            (let* ((notes (lines-of (run d "conflicts")))
                   (ls (lines-of (run d "outline"))))
              (list (length notes)
                    (and (pair? notes) (car (car notes)))
                    (and (pair? ls) (pair? (car ls)) (eq? (car (car ls)) 'error)))))
          (list 1 'malformed-record #f)))
  (list (list "a string ord" '(put ((kind . section) (parent . root) (ord . "oops"))) "ord")
        (list "a numeric parent" '(put ((kind . section) (parent . 7) (ord . 1))) "parent")
        ;; THE MOVE NEEDS A SECOND BLOCK TO BE SORTED AGAINST. This moved
        ;; the fixture's only block, so nothing ever compared its ord and
        ;; "the store still sorts" was a claim the row could not support
        ;; -- the same way `(0 . 1)` survived in read1 unnoticed.
        (list "a move to a bad ord" '(move "mirrorzz.2" root "oops") "move")))
;; TWIN: and a well-formed position from the same writer still applies,
;; so the rows above are about the values and not about refusing foreign
;; records.
(want "TWIN: a foreign record with a good position still applies"
      (let ((d (fresh-store!)))
        (run d "init")
        (run d "publish" "mirrorzz" "1"
             (mirror-file "pos-good.bin"
                          (rec 1 '(put ((kind . section) (title . "One") (parent . root) (ord . 0))))
                          (rec 2 '(put ((kind . section) (title . "Two") (parent . root) (ord . 1))))))
        (list (length (lines-of (run d "conflicts")))
              (length (lines-of (run d "outline")))))
      (list 0 2))
;; A RECORD THIS BUILD WOULD NOT WRITE, AND STILL HAS TO SHOW. The write
;; path refuses a title carrying a line terminator; a record from another
;; machine, or from a build that had no such rule, can hold one. The
;; listing put it through unchanged and then had MORE ROWS THAN BLOCKS --
;; the extra row looking exactly like a real one, with a well-formed id
;; no block has.
;;
;; THE ESCAPE IS THE LISTING'S ALONE. `read` answers the field as stored,
;; because a caller asking for the value wants the value; only the
;; line-per-block rendering has a reason to rewrite it. A row that
;; checked the escape in BOTH would be requiring the store to lie to the
;; caller.
(want "a foreign title with a line terminator is one row, escaped, and read keeps it"
      (let* ((d (fresh-store!)))
        (run d "init")
        (run d "insert" "--under" "root" "--title" "Real")
        (run d "publish" "mirrorzz" "1"
             (mirror-file "phantom.bin"
                          (rec 1 '(put ((kind . section)
                                        (title . "Fake\n- zzzzzzzz.9  Phantom")
                                        (parent . root) (ord . 5))))))
        (let* ((rows (lines-of (run d "outline")))
               (blocks (length (state-outline (open-and-reduce d))))
               (raw (let* ((rd (car (lines-of (run d "read" "mirrorzz.1"))))
                           (fs (cdr (assq 'fields (cadr rd)))))
                      (cdr (assq 'title fs)))))
          (list (length rows)
                blocks
                ;; THE RENDERED TEXT CARRIES THE ESCAPE. Asking the
                ;; PARSED lines whether one of them is the phantom is
                ;; meaningless: every outline line reads as the symbol
                ;; `-`, so such a check matches every row and says
                ;; nothing. What the row-count equality above already
                ;; proves is that no extra row exists; this says the
                ;; character was rewritten rather than dropped.
                (let ((t (text-of out-path)))
                  (let loop ((i 0))
                    (cond ((> (+ i 2) (string-length t)) 'no-escape)
                          ((string=? (substring t i (+ i 2)) "\\n") 'escaped)
                          (else (loop (+ i 1))))))
                ;; and the value the caller asked for still has the real
                ;; character in it
                (let loop ((i 0))
                  (cond ((= i (string-length raw)) 'escaped-in-read)
                        ((char=? (string-ref raw i) #\newline) 'raw-kept)
                        (else (loop (+ i 1)))))
                (length (lines-of (run d "conflicts"))))))
      (list 2 2 'escaped 'raw-kept 0))

;; THE WRITE PATH AND THE WIRE WRITER MUST ANSWER THE SAME QUESTION, and
;; the write path asks by CALLING the writer's own predicate rather than
;; restating its character rules. Two copies of "which symbols may be
;; written bare" drift, and the drift only shows once a record is on
;; disk: `("#%sym" "...")` on disk is what a writer that encoded a symbol
;; and a reader that could not apply it leave behind.
;;
;; THE ROW FEEDS ONE INPUT TO BOTH SIDES. `a-b` must be accepted by each
;; and `a b` refused by each; a row that only checked the store would
;; pass a store with its own private, slowly diverging rule.
(define (writer-emits-bare? sym)
  (guard (e (#t #f)) (begin (sexpr->string-extended (list sym)) #t)))
(define (store-accepts-symbol? sym)
  (not (caller-payload-reason (list 'set "x.1" sym "v"))))
(for-each
  (lambda (case)
    (want (string-append "the store and the wire writer agree about |" (car case) "|")
          (let ((sym (string->symbol (car case))))
            (list (and (writer-emits-bare? sym) #t)
                  (store-accepts-symbol? sym)))
          (list (cdr case) (cdr case))))
  (list (cons "a-b" #t) (cons "a b" #f)
        (cons "title" #t) (cons "a(b" #f)
        (cons "a.b" #t) (cons "" #f)))

;; A MOVE NAMES BOTH COORDINATES, so #f is a WRONG VALUE there -- while
;; in a `put` the same #f means the coordinate was omitted and the
;; interpreter fills it in. One predicate written for both skipped the
;; #f, and `(move <id> root #f)` applied, left no note, and broke sorting
;; the moment a sibling existed.
(for-each
  (lambda (case)
    (want (string-append "a foreign " (car case) " is noted, and the store still sorts")
          (let ((d (fresh-store!)))
            (run d "init")
            (run d "publish" "mirrorzz" "1"
                 (mirror-file (string-append "mv-" (caddr case) ".bin")
                              (rec 1 '(put ((kind . section) (title . "One")
                                            (parent . root) (ord . 0))))
                              (rec 2 '(put ((kind . section) (title . "Two")
                                            (parent . root) (ord . 1))))
                              (rec 3 (cadr case))))
            (let ((notes (lines-of (run d "conflicts")))
                  (ls (lines-of (run d "outline"))))
              (list (length notes)
                    (and (pair? notes) (car (car notes)))
                    (and (pair? ls) (pair? (car ls)) (eq? (car (car ls)) 'error))
                    (length ls))))
          (list 1 'malformed-record #f 2)))
  (list (list "move with #f for its ord" '(move "mirrorzz.2" root #f) "ord")
        (list "move with #f for its parent" '(move "mirrorzz.2" #f 0) "parent")))
;; TWIN: AND A PUT MAY STILL OMIT EITHER COORDINATE, which is the case
;; the shared predicate was written for and must keep working.
(want "TWIN: a put that omits its position still applies"
      (let ((d (fresh-store!)))
        (run d "init")
        (run d "publish" "mirrorzz" "1"
             (mirror-file "mv-omit.bin"
                          (rec 1 '(put ((kind . section) (title . "Only"))))))
        (list (length (lines-of (run d "conflicts")))
              (length (lines-of (run d "outline")))))
      (list 0 1))

;; A FIELD THE MARKDOWN READER APPENDS MUST BE TEXT, OR THE READER SAYS
;; SO RATHER THAN FALLING OVER. `src`, `heading-src` and `front` go
;; straight into `string-append`; a record carrying a number in one of
;; them was applied happily and then answered `(error internal ...)` when
;; read. A field can also be UNSETTLED -- two concurrent writes leave a
;; conflict rather than a value, which is ordinary correct data and is
;; likewise not a string.
(want "a foreign record whose src is not text is still readable"
      (let ((d (fresh-store!)))
        (run d "init")
        (run d "publish" "mirrorzz" "1"
             (mirror-file "srcnum.bin"
                          (rec 1 '(put ((kind . section) (title . "T") (src . 7)
                                        (parent . root) (ord . 0))))))
        ;; THE EXIT CODE IS THE READING. A field that is not text now
        ;; contributes nothing, so the rendered document is EMPTY and
        ;; there are no lines to look at -- while an `(error internal
        ;; ...)` would print one. Asking for the first line raised on
        ;; the success case, which is the row failing on the answer it
        ;; exists to see.
        (let ((r (run d "read" "mirrorzz.1" "--md")))
          (list (code-of r) (length (lines-of r)))))
      (list 0 0))
;; TWIN: AND A NEIGHBOUR WITH REAL TEXT STILL RENDERS IT, so the row
;; above is about surviving a field it cannot use rather than about
;; rendering nothing at all.
(want "TWIN: a record whose src is text still renders it"
      (let ((d (fresh-store!)))
        (run d "init")
        (run d "publish" "mirrorzz" "1"
             (mirror-file "srctext.bin"
                          (rec 1 '(put ((kind . section) (title . "T") (src . "hello\n")
                                        (parent . root) (ord . 0))))))
        (let ((r (run d "read" "mirrorzz.1" "--md")))
          (list (code-of r) (length (lines-of r)))))
      (list 0 1))
;; AND A DOCUMENT'S OWN BYTES INCLUDE ITS FRONT MATTER, which is what the
;; README says a file-level block's body is. The reader answered only
;; what sits above the first heading.
(want "a document read on its own carries its front matter"
      (let ((d (fresh-store!)))
        (run d "init")
        (run d "publish" "mirrorzz" "1"
             (mirror-file "docfront.bin"
                          (rec 1 '(put ((kind . doc) (title . "D") (path . "d.md")
                                        (front . "---\ny: 1\n---\n") (src . "body\n")
                                        (parent . root) (ord . 0))))))
        ;; THE RENDERED BYTES, not parsed lines. `read --md` prints the
        ;; document itself, so reading its output as data turns `---`
        ;; into a symbol and loses the thing being asserted.
        (begin (run d "read" "mirrorzz.1" "--md") (text-of out-path)))
      "---\ny: 1\n---\nbody\n")

;; A SNAPSHOT WRITTEN BEFORE A RULE EXISTED, READ BY A BUILD THAT HAS IT.
;; The reducer now refuses a `level` it cannot use, so no new record can
;; carry one -- which leaves exactly one way for a store to be holding
;; one already: rows written by an older build. `rows->state` does not
;; validate, and it must not, because those rows are this store's own
;; history rather than an incoming record.
;;
;; SO THE READER HAS TO SURVIVE IT. `level` is used as a string length
;; when a heading is rendered; a stringy one raised, which made every
;; Markdown read of that block fail. Refusing to write one and surviving
;; one that exists are two different obligations, and only the first has
;; a write path to enforce it.
;;
;; THE ROWS ARE REAL ONES WITH ONE VALUE SWAPPED. Hand-written rows are a
;; guess at an internal shape -- my first attempt at this row built a
;; field structure that does not exist and raised for that reason
;; instead, which read exactly like the defect.
(want "a snapshot carrying a level this build would refuse still renders"
      (let* ((r0 (reduce-empty))
             (ignored (reduce-apply! r0 "w" 1 '()
                        '(put ((kind . section) (title . "T") (level . 2)
                               (parent . root) (ord . 0)))))
             (rows (state->rows r0))
             (swap (lambda (self x)
                     (cond ((and (pair? x) (equal? (car x) 2)) (cons "2" (cdr x)))
                           ((pair? x) (cons (self self (car x)) (self self (cdr x))))
                           (else x))))
             (bad (swap swap rows))
             (r (rows->state bad)))
        (list (guard (e (#t 'raised)) (begin (block-text r "w.1" #f) 'rendered))
              ;; and the level really is the unusable one, so the row is
              ;; about tolerance rather than about a value it liked
              (let* ((b (state-read r "w.1"))
                     (fields (cdr (assq 'fields b)))
                     (lv (assq 'level fields)))
                (and lv (cdr lv)))))
      (list 'rendered "2"))

;; TWIN: AND AN ORD THE STORE ITSELF PRODUCES. `ord-between` answers
;; exact RATIONALS as well as integers -- inserting between 0 and 1 gives
;; 1/2 -- so a check written for integers alone would refuse the store's
;; own arithmetic. The first version of this rule was narrower than the
;; values it was judging, and a fixture crafting `(0 . 1)` was what
;; showed it.
(want "TWIN: a rational ord, which is what an insert between two siblings produces"
      (let ((d (fresh-store!)))
        (run d "init")
        (run d "publish" "mirrorzz" "1"
             (mirror-file "pos-rational.bin"
                          (rec 1 '(put ((kind . section) (title . "One") (parent . root) (ord . 0))))
                          (rec 2 '(put ((kind . section) (title . "Two") (parent . root) (ord . 1))))
                          (rec 3 '(put ((kind . section) (title . "Mid") (parent . root) (ord . 1/2))))))
        (list (length (lines-of (run d "conflicts")))
              (length (lines-of (run d "outline")))))
      (list 0 3))

(want "TWIN: a cross-writer dependency waits, then is delivered when its premise arrives"
      (let ((d (fresh-store!)))
        (run d "init")
        ;; the dependent record arrives FIRST and cannot be applied yet
        (run d "publish" "peerbbbb" "1"
             (mirror-file "dep-second.bin"
                          (encode-record 1 1757300000002 "peer" (list (cons "mirrorzz" 1))
                                         (storable-encode '(put ((kind . section) (title . "Needs One")))))))
        (let ((waiting (length (lines-of (run d "outline")))))
          ;; now its premise arrives on the other writer
          (run d "publish" "mirrorzz" "1"
               (mirror-file "dep-first.bin"
                            (rec 1 '(put ((kind . section) (title . "One"))))))
          (list waiting (length (lines-of (run d "outline"))))))
      (list 0 2))

;; AND THE STORE IS STILL A STORE. The record is refused; the writer that
;; sent it is not, and neither is this store's own history.
;; AND THE READING IS THE OUTLINE ITSELF, not its line count. `outline`
;; answering `(error ...)` is also one line, so a count alone is passed
;; by a store that has stopped working -- the exact condition this row
;; exists to rule out.
(want "and the store still reads and still writes with such a record present"
      (let ((d (fresh-store!))
            (poison (raw-record 1 "()" "hello")))
        (run d "init")
        (run d "insert" "--under" "root" "--title" "Mine")
        (plant-mirror! d "mirrorzz" poison)
        (if (not (planted? d "mirrorzz" poison))
            'not-planted
            (let* ((wrote (car (lines-of (run d "insert" "--under" "root" "--title" "After"))))
                   (ls (lines-of (run d "outline"))))
              (list (and (pair? wrote) (car wrote))
                    (length ls)
                    (and (pair? (car ls)) (eq? (car (car ls)) 'error))))))
      (list 'ok 2 #f))
;; AND A SNAPSHOT DOES NOT ERASE THE OBSERVATION. The notes are about
;; records the snapshot's cut COVERS, and replay skips everything
;; covered -- so leaving them out of the rows meant taking a snapshot
;; quietly made a damaged record stop being reported. The store looked
;; healthier for having been snapshotted.
;; AND THE SNAPSHOT HAS TO HAVE BEEN TAKEN, AND USED. Discarding the
;; snapshot's own answer let a failed or refused snapshot pass this row,
;; and a reader that always replays from empty would pass it too --
;; neither would ever exercise the rows. So the row asserts the snapshot
;; was written, and that the reopened state is seeded from a cut that
;; COVERS the noted event: if it were not covered, replay would deliver
;; the record again and the note would be produced afresh rather than
;; restored.
(want "a note survives a snapshot and a reopen, and the snapshot was used"
      (let ((d (fresh-store!)))
        (run d "init")
        (run d "insert" "--under" "root" "--title" "Mine")
        (run d "publish" "mirrorzz" "1" (mirror-file "unknown2.bin" (rec 1 '(frobnicate "x"))))
        (let* ((before (lines-of (run d "conflicts")))
               ;; THE INSTRUMENT'S FIRST READING. `redelivered` is #f at
               ;; the end, and #f is also what a trace that never carries
               ;; this shape would answer -- a broken check and a working
               ;; snapshot are the same value. Before the snapshot exists
               ;; the event must BE in the trace, or the reading below
               ;; means nothing.
               (seen-before
                 (and (memp (lambda (e)
                              (and (pair? e) (equal? (car e) "mirrorzz")
                                   (equal? (cdr e) 1)))
                            (reduce-trace (open-and-reduce d)))
                      #t))
               (snap (car (lines-of (run d "snapshot"))))
               (cut (and (pair? snap) (eq? (car snap) 'ok)
                         (cadr (assq 'cut (cdr snap)))))
               (covers (and cut
                            (let ((e (assoc "mirrorzz" cut)))
                              (and e (>= (cdr e) 1)))))
               ;; AND THE REOPENED READ MUST NOT HAVE DELIVERED IT. A
               ;; written snapshot with a covering cut is still passed by
               ;; a reader that ignores snapshots and replays from empty:
               ;; the note would be produced afresh and look identical.
               ;; The trace says which records the read actually
               ;; delivered, so the covered event appearing in it means
               ;; the snapshot was written and then not used.
               (trace (reduce-trace (open-and-reduce d)))
               (redelivered
                 (and (memp (lambda (e)
                              (and (pair? e) (equal? (car e) "mirrorzz")
                                   (equal? (cdr e) 1)))
                            trace)
                      #t)))
          (list before seen-before (and (pair? snap) (car snap)) covers redelivered
                (lines-of (run d "conflicts")))))
      (let ((note (list (list 'unknown-verb (list 'event "mirrorzz" 1)
                              (list 'verb 'frobnicate)))))
        (list note #t 'ok #t #f note)))
;; TWIN: AND AN UNKNOWN VERB IS STILL AN UNKNOWN VERB. Without this the
;; row above is also passed by a build that calls everything it cannot
;; apply malformed -- which would tell an operator to repair records that
;; only need a newer binary.
;; AND A RECORD THAT IS POISONED RATHER THAN MERELY UNKNOWN. `(put (7))`
;; is a `put` this build DOES know, carrying a field collection its
;; reducer cannot walk -- the exact shape that once made a store
;; permanently unreadable, because the write path appended before it
;; reduced.
;;
;; THE WRITE PATH NOW REFUSES IT, WHICH IS WHY THIS ROW COMES IN THROUGH
;; `publish`. That is not a contrivance: it is how a record from another
;; machine, or from a build whose write path did not check, actually
;; arrives. A store must be able to READ what it would not have written,
;; or the first foreign record ends it.
;; AND "STILL READS" READS THE LINE, not its count. An outline answering
;; `(error ...)` is also exactly one line -- the same weakness that was
;; corrected two rows below and NOT here, because I fixed the row I was
;; looking at and did not search for its family.
(want "a record this reducer cannot apply is reported, and the store still reads"
      (let ((d (fresh-store!)))
        (run d "init")
        (run d "insert" "--under" "root" "--title" "Good")
        (run d "publish" "mirrorzz" "1" (mirror-file "poison.bin" (rec 1 '(put (7)))))
        (let ((ls (lines-of (run d "outline"))))
          (list (lines-of (run d "conflicts"))
                (length ls)
                (and (pair? (car ls)) (eq? (car (car ls)) 'error)))))
      (list (list (list 'malformed-record (list 'event "mirrorzz" 1)
                        (list 'reason 'field-entry-not-a-pair)))
            1 #f))
;; AND IT IS STILL WRITABLE AFTERWARDS, which is the half the defect
;; actually took away: the store answered an internal error to every
;; later write, not only to the read.
(want "and a store holding one is still writable"
      (let ((d (fresh-store!)))
        (run d "init")
        (run d "publish" "mirrorzz" "1" (mirror-file "poison2.bin" (rec 1 '(put (7)))))
        (let ((wrote (car (lines-of (run d "insert" "--under" "root" "--title" "After")))))
          (list (and (pair? wrote) (car wrote)) (length (lines-of (run d "outline"))))))
      (list 'ok 1))
(want "TWIN: a well-formed record with a verb this build lacks is still unknown-verb"
      (let ((d (fresh-store!)))
        (run d "init")
        (run d "publish" "mirrorzz" "1" (mirror-file "newverb.bin" (rec 1 '(frobnicate "x"))))
        (lines-of (run d "conflicts")))
      (list (list 'unknown-verb (list 'event "mirrorzz" 1) (list 'verb 'frobnicate))))

;; Q15': A BOOKKEEPING RECORD STILL RELEASES WHAT WAITED ON IT. An
;; implementation that suppressed every effect while ingesting one would
;; leave the operation that depended on it pending for ever, and the row
;; above -- where nothing changed -- would pass just the same.
(define d3 (fresh-store!))
(run d3 "init")
(define blocked
  (mirror-file "blocked.bin"
               (encode-record 1 1757300001000 "agent:claude" '(("mirrorzz" . 2))
                              (storable-encode '(put ((kind . section) (title . "waited")))))))
;; THE WRITER NAME IS EIGHT CHARACTERS OF [0-9a-z]. A shorter one is not
;; a writer this store will ever read, which is a trap worth naming
;; here: the publish below would answer `published` for a name like
;; "mirrora" and the records would never be delivered to anything.
(want "CONTROL: an operation whose premise has not arrived is pending"
      (begin (run d3 "publish" "mirrorab" "1" blocked)
             (map car (lines-of (run d3 "conflicts"))))
      '(pending))
(want "and delivering the bookkeeping record it waited on applies it"
      (begin (run d3 "publish" "mirrorzz" "1"
                  (mirror-file "release.bin"
                               (rec 1 '(plan "r1" "fp" (("w" . 1)) ()))
                               (rec 2 '(plan "r2" "fp" (("w" . 1)) ()))))
             (list (lines-of (run d3 "conflicts"))
                   (length (lines-of (run d3 "log")))))
      (list '() 3))

(printf "\n== T3: the fingerprint covers who asked, what, and of what ==\n")
;; A RETRY IS THE SAME IDENTITY AND THE SAME FINGERPRINT. The same
;; identity with a different fingerprint is a client reusing an id for a
;; different request -- a mistake to report, not a history to rebuild.
;;
;; THE DIGEST IS OVER A CANONICAL SERIALISATION, not over the fields
;; compared one at a time. A field-by-field comparison invites one that
;; forgets a field, and the field it forgets is the one nobody thought
;; of -- which is the same field the client changed.
;;
;; THE FIXED VECTOR BELOW WAS NOT COPIED OUT OF THIS IMPLEMENTATION. The
;; canonical string was printed, and an independent sha256 (python's
;; hashlib, outside this process and this language) was taken over the
;; same bytes; the two agreed. An expectation read back from the code it
;; checks is a statement that the code equals itself.
(define fixed-canonical
  "(\"agent:claude\" set \"b\" \"title\" \"x\" (\"w3k\" . 412))")
(want "the canonical form is the request's words in order"
      (sexpr->string-extended (list "agent:claude" 'set "b" "title" "x" (cons "w3k" 412)))
      fixed-canonical)
(want "and its fingerprint is this exact digest"
      (request-fingerprint "agent:claude" 'set (list "b" "title" "x") (cons "w3k" 412))
      "9b16cdbd2325c04118e1d09f8288946fc3e81b84ec9fc9004a4241413df83521")
;; EACH PART OF THE REQUEST MOVES IT. A fingerprint that forgot the verb
;; would make `(set b "x")` and `(del b)` the same request.
(define (fp who verb args after) (request-fingerprint who verb args after))
(define base-fp (fp "agent:claude" 'set (list "b" "title" "x") (cons "w3k" 412)))
(want "the argument, the verb, who asked and the cursor each change it"
      (list (equal? base-fp (fp "agent:claude" 'set (list "b" "title" "y") (cons "w3k" 412)))
            (equal? base-fp (fp "agent:claude" 'del (list "b" "title" "x") (cons "w3k" 412)))
            (equal? base-fp (fp "agent:other"  'set (list "b" "title" "x") (cons "w3k" 412)))
            (equal? base-fp (fp "agent:claude" 'set (list "b" "title" "x") (cons "w3k" 413))))
      (list #f #f #f #f))
(want "TWIN: the same request asked twice has the same fingerprint"
      (equal? base-fp (fp "agent:claude" 'set (list "b" "title" "x") (cons "w3k" 412)))
      #t)
;; THE ORIGIN IS READ FROM THE CURSOR THE REQUEST NAMED, never from
;; whichever writer holds the record now.
(want "the identity is the cursor's writer and the request id"
      (request-identity (cons "w3k" 412) "req-1")
      (cons "w3k" "req-1"))
(want "a request id is 1 to 64 of [A-Za-z0-9._-], or a batch item"
      (list (req-id-ok? "req-1") (req-id-ok? "a.b_c-D9")
            (req-id-ok? (list 'batch "b1" 0))
            (req-id-ok? "") (req-id-ok? "has space") (req-id-ok? "has/slash")
            (req-id-ok? (make-string 65 #\a)))
      (list #t #t #t #f #f #f #f))
(want "TWIN: sixty-four characters is still a request id"
      (req-id-ok? (make-string 64 #\a))
      #t)

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "q1 complete\n")
