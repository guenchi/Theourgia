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

;; log-publish!: what may be installed, and where it may sit.
;;
;; Section 9.6 answers two questions that look like one. The first is
;; whether these bytes and the bytes already here can be reconciled --
;; identical, a prefix, a disagreement, a repair. The second is whether
;; the result READS: a writer's segments are walked in number order and a
;; reader stops at the first sequence that does not continue the last, so
;; a candidate that is perfectly formed and agrees with everything can
;; still be unpublishable because of where it would sit.
;;
;; THE SECOND QUESTION HAS THREE SIDES AND EACH WAS LEARNED SEPARATELY.
;; Forward -- does what comes after still join on -- was written first.
;; Backward was missing entirely, so a free segment number read as "a new
;; segment" and a writer whose history ended at 2 published a candidate
;; of 20-21, leaving 3 through 19 owned by nobody. Backward then turned
;; out to have two sides of its own: a candidate that starts too late
;; leaves a hole, and one that starts too early repeats sequences the
;; reader has already passed. Each says which two numbers disagree,
;; because the three ask for different repairs.
;;
;; AND THE COORDINATES COME FROM DECLARATIONS. A file in the directory is
;; not history; the manifest says which segments this writer published
;; and what range each holds, and the retirement record says where a
;; retained prefix ends. Reading the extent out of the bytes instead made
;; a killed install -- a segment written but never listed -- supply a
;; history no reader could see, and made a damaged neighbour erase the
;; only number the layout decision needed.
;;
;; Every refusal row here is followed by the passing twin that shows it
;; is refusing the case and not the shape of the case.

(import (chezscheme) (theourgia log) (theourgia wire) (theourgia ffi)
        (only (igropyr crypto) sha256 bytevector->hex))

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
(define (want label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))

(define scratch (test-dir "log18"))
(define W "wwwlocl0")
(define M "mirrorz9")
(define n-store 0)

(define (rec seq payload)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" '() (storable-encode payload)))
(define (recs lo hi)
  (apply cat (let loop ((i lo))
               (if (> i hi) '() (cons (rec i (list 'put (list (cons 'kind 'section)))) (loop (+ i 1)))))))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (slurp path)
  (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
    (if (eof-object? b) (make-bytevector 0) b)))
(define (sha-of b) (bytevector->hex (sha256 b)))

;; A STORE PER ROW, and its machine home is a SIBLING of the store rather
;; than a directory inside it: a registry under the store is refused, and
;; when it was allowed it silently disabled rollback detection.
(define (fresh!)
  (set! n-store (+ n-store 1))
  (let ((d (string-append scratch "/s" (number->string n-store))))
    (system (string-append "rm -rf " d " " d "-home; mkdir -p "
                           d "/writers/" W " " d "/writers/" M " " d "/snap"))
    (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"p9\"))\n"))
    (file-ensure! (string-append d "/lock"))
    (put! (string-append d "/writers/" W "/owner.sexp") (string->utf8 "((machine \"m\"))\n"))
    (putenv "THEOURGIA_HOME" (string-append d "-home"))
    (let ((k (instance-install! d))) (owner-install! d W k))
    (put! (string-append d "/writers/" W "/" (segment-file-name 1)) (make-bytevector 0))
    d))

(define (mpath d n) (string-append d "/writers/" M "/" (segment-file-name n)))
(define (plant! d n bytes) (put! (mpath d n) bytes))
(define (pub d seg bytes) (log-publish! d M seg bytes (sha-of bytes)))
;; A KEPT CANDIDATE'S ANSWER CARRIES THE PATH IT WAS KEPT AT, which is a
;; generated name and not what these rows are about. Only that shape is
;; trimmed -- every other answer is asserted whole, so a row cannot pass
;; by losing the part that distinguishes it.
(define (why a)
  (if (and (pair? a) (pair? (cdr a)) (pair? (cadr a)) (eq? 'kept (car (cadr a))))
      (car a)
      a))
(define (listed? d n)
  (let ((m (read-manifest d M))) (and m (if (assv n m) #t #f))))
(define (present? d n) (file-exists? (mpath d n)))
(define (damage bytes at)
  (let ((o (bytevector-copy bytes)))
    (bytevector-u8-set! o at (if (= 97 (bytevector-u8-ref o at)) 98 97))
    o))

(printf "== P1: the ten outcomes of a comparison ==\n")
(want "a brand new segment is installed" (why (pub (fresh!) 1 (recs 1 2))) '(published 1))
(want "the same bytes again are finished, not re-installed"
      (let ((d (fresh!))) (pub d 1 (recs 1 2)) (why (pub d 1 (recs 1 2)))) '(idempotent 1))
(want "a candidate shorter than what is here is incomplete"
      (let ((d (fresh!))) (pub d 1 (recs 1 2)) (why (pub d 1 (recs 1 1)))) '(incomplete 1))
(want "a candidate longer than a sealed segment is unmergeable"
      (let ((d (fresh!))) (pub d 1 (recs 1 2)) (why (pub d 1 (recs 1 3))))
      'newer-history-unmergeable)
(want "a record that disagrees forks the history at its sequence"
      (let ((d (fresh!)))
        (pub d 1 (recs 1 2))
        (why (pub d 1 (cat (rec 1 '(put ((kind . section)))) (rec 2 '(put ((kind . other))))))))
      '(divergence (fork 2)))
(want "a declared hash that is not the bytes' hash is refused before anything else"
      (why (log-publish! (fresh!) M 1 (recs 1 2) "deadbeef"))
      '(error invalid-candidate sha-mismatch))
;; THE ANSWER NAMES THE BYTES IT DISPLACED. The sender is the one party
;; that may want them, and deriving the name on its side would be a
;; second supplier of it.
(want "a damaged local segment the candidate covers is repaired, and the answer says where the old bytes went"
      (let* ((d (fresh!))
             (broken (damage (recs 1 3) 80))
             (answer (begin (plant! d 1 broken) (pub d 1 (recs 1 3))))
             (path (cadr (assq 'evidence (cddr answer)))))
        (list (car answer) (cadr answer)
              (file-exists? path)
              (equal? (slurp path) broken)))
      (list 'repaired 1 #t #t))
;; TWO REPAIRS LEAVE TWO FILES. A name that collided would lose the
;; first set of bytes to the second repair, which is the one moment they
;; were worth keeping.
(want "a second repair keeps its own evidence beside the first"
      (let* ((d (fresh!))
             (first (damage (recs 1 3) 80))
             (a1 (begin (plant! d 1 first) (pub d 1 (recs 1 3))))
             (second (damage (recs 1 3) 90))
             (a2 (begin (plant! d 1 second) (pub d 1 (recs 1 3))))
             (p1 (cadr (assq 'evidence (cddr a1))))
             (p2 (cadr (assq 'evidence (cddr a2)))))
        (list (equal? p1 p2) (file-exists? p1) (file-exists? p2)
              (equal? (slurp p1) first) (equal? (slurp p2) second)))
      (list #f #t #t #t #t))
;; ABSENCE IS NOT EVIDENCE OF DISAGREEMENT. Local 1 and 3 against a
;; candidate of 1 and 2 says nothing about 3, and installing would
;; destroy it.
(want "a candidate that omits a valid local record is refused, and names it"
      (let ((d (fresh!))) (plant! d 1 (damage (recs 1 3) 80)) (why (pub d 1 (recs 1 2))))
      '(refused insufficient-coverage (seq 3)))
(want "a candidate that overlaps this segment but starts elsewhere cannot be compared"
      (let ((d (fresh!))) (pub d 1 (recs 1 2)) (why (pub d 1 (recs 2 3))))
      '(segment-layout-conflict not-start-aligned))
(want "a segment number already holding something else is occupied"
      (let ((d (fresh!))) (pub d 1 (recs 1 2)) (pub d 2 (recs 3 4)) (why (pub d 2 (recs 5 6))))
      '(segment-layout-conflict target-occupied))

(printf "\n== P2: history is what the store declares, not what is on disk ==\n")
;; THE HOLE THIS SECTION EXISTS FOR. Before it, a free segment number and
;; a candidate that agreed with nothing in particular were enough to
;; publish: local history ending at 2 accepted a candidate of 20-21 and
;; answered (published 5), with 3 through 19 unreachable for good --
;; segment numbers only increase, so nothing can ever be inserted to
;; carry them.
(want "a candidate that leaves a hole is refused, and the reason names the hole"
      (let ((d (fresh!)))
        (pub d 1 (recs 1 2))
        (list (why (pub d 5 (recs 20 21))) (present? d 5) (listed? d 5)))
      (list '(segment-layout-conflict (gap-before-candidate (history-ends 2) (candidate-starts 20)))
            #f #f))
(want "TWIN: the same free segment number, continuing the history, is published"
      (let ((d (fresh!))) (pub d 1 (recs 1 2)) (why (pub d 5 (recs 3 4))))
      '(published 5))
;; A GAP OF ONE IS A GAP. Every other row here leaves a wide hole, and a
;; comparison off by one accepts them all while letting exactly this case
;; through -- the smallest hole is the one a reader still stops at.
(want "a hole of a single sequence is refused like any other"
      (let ((d (fresh!))) (pub d 1 (recs 1 2)) (why (pub d 5 (recs 4 5))))
      '(segment-layout-conflict (gap-before-candidate (history-ends 2) (candidate-starts 4))))
(want "a candidate that starts before the history ends repeats what a reader passed"
      (let ((d (fresh!)))
        (pub d 1 (recs 1 5))
        (list (why (pub d 2 (recs 3 7))) (present? d 2) (listed? d 2)))
      (list '(segment-layout-conflict (overlaps-preceding (history-ends 5) (candidate-starts 3)))
            #f #f))
(want "TWIN: the same segment number, starting where the history ends, is published"
      (let ((d (fresh!))) (pub d 1 (recs 1 5)) (why (pub d 2 (recs 6 10))))
      '(published 2))
;; A FILE IS NOT HISTORY. These three differ only in what the store has
;; SAID about the segment below, and the bytes are identical in all
;; three.
(want "an unlisted file below the candidate is an orphan and declares nothing"
      (let ((d (fresh!))) (plant! d 1 (recs 1 10)) (why (pub d 2 (recs 11 20))))
      '(segment-layout-conflict (gap-before-candidate (history-ends 0) (candidate-starts 11))))
(want "a listed segment below declares its range even with its bytes damaged"
      (let ((d (fresh!)))
        (pub d 1 (recs 1 10))
        (plant! d 1 (damage (recs 1 10) 80))
        (why (pub d 2 (recs 11 20))))
      '(published 2))
(want "and the damaged segment still says where history ends"
      (let ((d (fresh!)))
        (pub d 1 (recs 1 10))
        (plant! d 1 (damage (recs 1 10) 80))
        (why (pub d 2 (recs 50 60))))
      '(segment-layout-conflict (gap-before-candidate (history-ends 10) (candidate-starts 50))))
;; A KILLED INSTALL LEAVES EXACTLY THAT ORPHAN: bytes written, manifest
;; never reached. Reading the extent from files would let it supply a
;; history no reader can see.
(want "a segment written but never listed supplies no history"
      (let ((d (fresh!))) (plant! d 1 (recs 1 2)) (why (pub d 5 (recs 3 4))))
      '(segment-layout-conflict (gap-before-candidate (history-ends 0) (candidate-starts 3))))
(want "TWIN: with nothing at all below, a first segment starting at 1 is published"
      (why (pub (fresh!) 1 (recs 1 5))) '(published 1))
(want "and one that does not start at 1 is not"
      (why (pub (fresh!) 5 (recs 20 21)))
      '(segment-layout-conflict (gap-before-candidate (history-ends 0) (candidate-starts 20))))

(printf "\n== P3: a hole in the declarations can still be filled ==\n")
;; THE END OF THE TRAVERSABLE PREFIX, NOT THE HIGHEST DECLARED SEQUENCE.
;; They differ only when the declarations already hold a hole -- and
;; there the difference decides whether the hole can ever be repaired.
(define (declare! d . entries)
  (put! (string-append d "/writers/" M "/published.sexp")
        (string->utf8
          (string-append "("
            (apply string-append
              (map (lambda (e)
                     (let ((n (car e)) (b (cadr e)))
                       (plant! d n b)
                       (string-append "(" (number->string n) " \"" (sha-of b) "\" "
                                      (number->string (caddr e)) " "
                                      (number->string (cadddr e)) ")")))
                   entries))
            ")\n"))))
(want "with 1 and 3 declared and 2 free, the candidate that fills 2 is published"
      (let ((d (fresh!)))
        (declare! d (list 1 (recs 1 10) 1 10) (list 3 (recs 20 30) 20 30))
        (why (pub d 2 (recs 11 19))))
      '(published 2))
;; AN ORPHAN DOES NOT STOP THE WALK EITHER. It is not history, so it is
;; not a break in history: skipping it is the same rule as not counting
;; it, seen from the other side. Reading the segments from the directory
;; rather than from the manifest makes an unlisted file below a listed
;; one hide everything above it.
(want "an orphan file below a listed segment does not hide what it declares"
      (let ((d (fresh!)))
        (declare! d (list 2 (recs 1 10) 1 10))
        (plant! d 1 (recs 90 95))
        (why (pub d 3 (recs 11 12))))
      '(published 3))
(want "and beyond the hole the history still ends where the reader stops"
      (let ((d (fresh!)))
        (declare! d (list 1 (recs 1 10) 1 10) (list 3 (recs 20 30) 20 30))
        (why (pub d 4 (recs 31 40))))
      '(segment-layout-conflict (gap-before-candidate (history-ends 10) (candidate-starts 31))))

(printf "\n== P4: and the result has to read forward too ==\n")
(want "a candidate that does not meet the segment above it is refused"
      (let ((d (fresh!)))
        (pub d 1 (recs 1 5)) (pub d 2 (recs 6 10))
        (why (pub d 1 (recs 1 8))))
      '(segment-layout-conflict would-not-read-through))
(want "TWIN: the same segment, meeting the one above it exactly, is finished"
      (let ((d (fresh!)))
        (pub d 1 (recs 1 5)) (pub d 2 (recs 6 10))
        (why (pub d 1 (recs 1 5))))
      '(idempotent 1))

(printf "\n== P5: a declaration that contradicts its bytes is not history ==\n")
;; WITH THE HASH MATCHING, these bytes are the ones the manifest names,
;; so a range that disagrees is the manifest contradicting itself -- and
;; the declaration is what every layout decision is made from. It is
;; excluded exactly as a hash mismatch is, because in both cases the
;; store cannot say what this segment is.
(define (writer-extent d w)
  (let* ((ls (log-open d))
         (p (load-prefix ls w))
         (ends (if p (discovery-end-seq p) 'no-prefix))
         (kinds (map (lambda (e) (log-error-kind (cdr e))) (load-integrity ls))))
    (load-abort! ls 'probe)
    (list ends kinds)))
(want "a hand-edited last sequence takes the segment out of the extent, and is named"
      (let ((d (fresh!)))
        (pub d 1 (recs 1 5))
        (declare! d (list 1 (recs 1 5) 1 9))
        (writer-extent d M))
      (list 0 '(manifest-range)))
(want "TWIN: the declaration agreeing with the bytes leaves the extent whole"
      (let ((d (fresh!)))
        (pub d 1 (recs 1 5))
        (writer-extent d M))
      (list 5 '()))

(printf "\n== P6: divergence is looked for across the whole history ==\n")
;; NOT JUST THIS FILE. A disagreement can fall in a neighbouring segment,
;; and a check confined to the target would install a candidate that
;; contradicts a record the store already delivered.
(define (published-two!)
  (let ((d (fresh!))) (pub d 1 (recs 1 5)) (pub d 2 (recs 6 10)) d))
(define (altered lo hi at)
  (apply cat (let loop ((i lo))
               (cond ((> i hi) '())
                     ((= i at) (cons (rec i '(put ((kind . other)))) (loop (+ i 1))))
                     (else (cons (rec i (list 'put (list (cons 'kind 'section)))) (loop (+ i 1))))))))
(want "a candidate for segment 2 that disagrees with a record held in segment 1"
      (let ((d (fresh!)))
        (pub d 1 (recs 1 5))
        (why (pub d 2 (altered 5 10 5))))
      '(divergence (fork 5)))
;; AND IT IS ASKED BEFORE THE LAYOUT QUESTION. That same candidate also
;; starts one sequence below where the history ends, so the layout would
;; refuse it as an overlap. A disagreement about a record says something
;; more specific than "this cannot sit here", and it is the answer that
;; tells the sender what to do, so it is the one reported.
(want "CONTROL: the same layout with no disagreement is an overlap"
      (let ((d (fresh!)))
        (pub d 1 (recs 1 5))
        (why (pub d 2 (recs 5 10))))
      '(segment-layout-conflict (overlaps-preceding (history-ends 5) (candidate-starts 5))))
(want "a candidate that disagrees within its own segment forks there too"
      (let ((d (published-two!))) (why (pub d 1 (altered 1 5 3)))) '(divergence (fork 3)))
;; THE FORK IS MONOTONE AND TAKES THE MINIMUM. An existing fork at eight
;; and a new disagreement at three must move to three; a later one at
;; nine must not move it back up, because that would bring three through
;; eight back to life.
(want "a second quarantine takes the minimum, and a third does not raise it"
      (let ((d (published-two!)))
        (list (why (pub d 2 (altered 6 10 8)))
              (why (pub d 1 (altered 1 5 3)))
              (why (pub d 2 (altered 6 10 9)))))
      (list '(divergence (fork 8)) '(divergence (fork 3)) '(divergence (fork 3))))

(printf "\n== P7: a retired prefix declares history no manifest lists ==\n")
(define (retire! d segment offset seq)
  (put! (string-append d "/writers/" M "/retired.sexp")
        (string->utf8 (string-append "((format 1) (tx \"t1\") (prefix "
                                     (number->string segment) " "
                                     (number->string offset) " "
                                     (number->string seq) "))\n"))))
(want "the retired segment is the one segment a candidate may extend"
      (let* ((d (fresh!)) (tail (recs 6 8)))
        (pub d 1 (recs 1 5))
        (plant! d 2 tail)
        (retire! d 2 (bytevector-length tail) 8)
        (let ((answer (pub d 2 (recs 6 10))))
          (list (car answer) (cadr answer)
                (and (assq 'evidence (cddr answer)) #t)
                (listed? d 2))))
      (list 'extended 2 #t #t))
(want "and the segment above a retired prefix continues from what it declares"
      (let* ((d (fresh!)) (tail (recs 6 8)))
        (pub d 1 (recs 1 5))
        (plant! d 2 tail)
        (retire! d 2 (bytevector-length tail) 8)
        (why (pub d 3 (recs 9 12))))
      '(published 3))
(want "TWIN: above a retired prefix, a candidate that leaves a hole is still refused"
      (let* ((d (fresh!)) (tail (recs 6 8)))
        (pub d 1 (recs 1 5))
        (plant! d 2 tail)
        (retire! d 2 (bytevector-length tail) 8)
        (why (pub d 3 (recs 20 22))))
      '(segment-layout-conflict (gap-before-candidate (history-ends 8) (candidate-starts 20))))

(printf "\n== P8: killed between the bytes and the manifest ==\n")
;; IDEMPOTENT MEANS FINISHED, and finished includes being in the manifest
;; with this hash. A killed install leaves bytes that match and no
;; manifest entry; answering `idempotent' there would make "done" mean
;; two different things and leave the segment unpublished for good.
(want "bytes already in place and unlisted are carried through to the manifest"
      (let ((d (fresh!)))
        (plant! d 1 (recs 1 2))
        (list (why (pub d 1 (recs 1 2))) (listed? d 1)))
      (list '(published 1) #t))
(want "and only then is the same candidate finished"
      (let ((d (fresh!)))
        (plant! d 1 (recs 1 2))
        (pub d 1 (recs 1 2))
        (why (pub d 1 (recs 1 2))))
      '(idempotent 1))
(want "the retired-prefix case is carried through the same way"
      (let* ((d (fresh!)) (whole (recs 6 10)))
        (pub d 1 (recs 1 5))
        (plant! d 2 whole)
        (retire! d 2 (bytevector-length (recs 6 8)) 8)
        (list (why (pub d 2 whole)) (listed? d 2)))
      (list '(published 2) #t))

(printf "\n== P9: a manifest this store cannot read stops the publish ==\n")
;; EVERY DECISION BELOW IT IS MADE FROM THE MANIFEST -- which segments
;; exist, what range each holds, whether this candidate is already
;; published -- so carrying on would be deciding from an absence mistaken
;; for an emptiness. It refuses in the ANSWER rather than by raising:
;; the caller is a sync client with a candidate in hand and needs to be
;; told to come back, not handed an exception.
;;
;; Nothing is written at all, the candidate included: keeping evidence
;; against a writer whose manifest is unreadable adds a file to a
;; directory nobody can currently reason about.
(define (manifest-path-of d) (string-append d "/writers/" M "/published.sexp"))
(define (published-once!)
  (let ((d (fresh!)))
    (pub d 1 (recs 1 5))
    d))
(define (publish-with-manifest broken)
  (let* ((d (published-once!))
         (path (manifest-path-of d)))
    (broken path)
    (let* ((answer (why (pub d 2 (recs 6 9))))
           (restored (begin (system (string-append "chmod 644 " path)) #t))
           (bytes (slurp path)))
      (list answer
            (present? d 2)
            (file-is-directory? (string-append d "/writers/" M "/incoming"))
            (and bytes (> (bytevector-length bytes) 0))))))

;; The path in the answer is a generated scratch path, so the rows
;; assert what it must BE -- the writer's manifest -- rather than its
;; text, and assert the reason whole.
(define (names-manifest? answer)
  (let ((p (cadr (assq 'path (cddr answer)))))
    (and (string? p)
         (let ((suffix "/published.sexp"))
           (and (>= (string-length p) (string-length suffix))
                (string=? (substring p (- (string-length p) (string-length suffix))
                                     (string-length p))
                          suffix))))))
(define (reason-of answer) (cadr (assq 'reason (cddr answer))))

(want "a truncated manifest refuses the publish, and nothing is written"
      (let* ((d (published-once!))
             (path (manifest-path-of d))
             (before (slurp path)))
        (put! path (string->utf8 "((1 \"aa\" 1"))
        (let ((answer (pub d 2 (recs 6 9))))
          (list (car answer) (cadr answer) (names-manifest? answer) (reason-of answer)
                (present? d 2)
                (file-is-directory? (string-append d "/writers/" M "/incoming")))))
      (list 'refused 'manifest-unreadable #t "unparseable" #f #f))
(want "and an unreadable one answers the same way, with the reason the system gave"
      (let* ((d (published-once!))
             (path (manifest-path-of d)))
        (system (string-append "chmod 000 " path))
        (let ((answer (pub d 2 (recs 6 9))))
          (system (string-append "chmod 644 " path))
          (list (car answer) (cadr answer) (names-manifest? answer) (reason-of answer)
                (present? d 2))))
      (list 'refused 'manifest-unreadable #t "Permission denied" #f))
(want "TWIN: with the manifest readable the same candidate publishes"
      (let ((d (published-once!))) (why (pub d 2 (recs 6 9))))
      '(published 2))

(printf "\n== P10: segment numbers need not be dense, sequences must be ==\n")
;; TWO DIFFERENT COORDINATES, and only one of them has to be contiguous.
;; A reader walks segments in number order and records in sequence order,
;; so a gap between segment NUMBERS costs nothing -- there is no record
;; in it to miss -- while a gap between SEQUENCES is history nobody
;; holds. A publisher that had to fill segment numbers densely would
;; have to know what its peer had already used.
(want "a candidate continuing the sequence may take any free segment number"
      (let ((d (fresh!)))
        (pub d 1 (recs 1 2))
        (list (why (pub d 7 (recs 3 3))) (listed? d 7)))
      (list '(published 7) #t))
(want "and a reader reads straight through the gap in the numbering"
      (let ((d (fresh!)))
        (pub d 1 (recs 1 2))
        (pub d 7 (recs 3 3))
        (let* ((ls (log-open d)) (p (load-prefix ls M)))
          (let ((end (if p (discovery-end-seq p) 'no-prefix)))
            (load-abort! ls 'probe)
            end)))
      3)
;; THE TWIN THAT MUST STILL BE REFUSED: the same free number, a sequence
;; that does not continue.
(want "TWIN: a free number does not excuse a gap in the sequence"
      (let ((d (fresh!)))
        (pub d 1 (recs 1 2))
        (why (pub d 7 (recs 4 4))))
      '(segment-layout-conflict (gap-before-candidate (history-ends 2) (candidate-starts 4))))

(printf "\n== P11: a name no reader will ever look at is not a writer ==\n")
;; Writer ids are eight characters of [0-9a-z] and the reader filters the
;; directory by exactly that. A segment published under any other name is
;; durable, listed in a manifest, and delivered to nothing -- while the
;; sender is told `published` and deletes its own copy on the strength of
;; it. Durable and unreachable is the inverse of what an answer promises.
;;
;; The check asks the SAME predicate the reader uses; a second opinion
;; about what a writer id is would put the two back where they started.
(define (publish-as name)
  (let ((d (fresh!)))
    (list (why (log-publish! d name 1 (recs 1 2) (sha-of (recs 1 2))))
          (file-is-directory? (string-append d "/writers/" name)))))
(want "a name that is too short is refused, and nothing is created"
      (publish-as "mirrora")
      (list '(refused invalid-writer (writer "mirrora")) #f))
(want "too long, and out of the character set, likewise"
      (list (car (publish-as "mirrorabcd")) (car (publish-as "MIRRORZZ")))
      (list '(refused invalid-writer (writer "mirrorabcd"))
            '(refused invalid-writer (writer "MIRRORZZ"))))
(want "TWIN: eight characters of the right kind publish"
      (publish-as "mirrorzz")
      (list '(published 1) #t))
;; AND THE READER AGREES, which is the property the refusal exists to
;; keep: a name this store would publish is a name it will also read.
(want "the published writer is one the reader lists"
      (let ((d (fresh!)))
        (pub d 1 (recs 1 2))
        (let ((ls (log-open d)))
          (let ((names (load-writers ls)))
            (load-abort! ls 'probe)
            (and (member M names) #t))))
      #t)

(printf "\n~a failures\n" bad)
;; A run that did not reach here is not a pass. The runner requires this
;; line AND a zero failure count: they are two propositions.
(printf "log18 complete\n")
