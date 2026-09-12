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

;; R8, the disk path: the same records read off a store through
;; open-and-reduce and fed straight to the reducer must give the same
;; state. Everything the reducer is asserted on elsewhere is asserted
;; there against a record list; this file is the one that shows the
;; record list and the log agree.
(import (chezscheme) (theourgia log) (theourgia ffi) (theourgia trace)
        (theourgia reduce) (theourgia store) (theourgia wire)
        (only (igropyr crypto) sha256 bytevector->hex))

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


(define d (test-dir "reduce2work"))
(define W "wwwx7q2a")
(define V "vvvy8r3b")
(define U "uuuz9s4c")
(define (id w n) (string-append w "." (number->string n 36)))

;; ONE CORPUS, TWO CONSUMERS. The bytes on disk and the list fed to the
;; reducer are derived from this and nothing else -- two hand-written
;; copies would drift, and the drift would read as a defect in whichever
;; of the two paths was edited second.
(define corpus
  (list
    (list W 1 '() (list 'put (list (cons 'kind 'section) (cons 'title "root-x"))))
    (list W 2 '() (list 'put (list (cons 'kind 'section) (cons 'title "root-y"))))
    (list V 1 (list (cons W 1)) (list 'set (id W 1) 'title "v-said"))
    (list V 2 (list (cons W 2)) (list 'move (id W 1) (id W 2) 1))
    (list U 1 (list (cons W 2)) (list 'link (id W 1) 'explains (id W 2)))
    (list U 2 (list (cons V 1)) (list 'link (id W 1) 'explains (id W 2)))
    (list W 3 (list (cons U 1)) (list 'unlink (id W 1) 'explains (id W 2)))
    (list W 4 (list (cons V 2)) (list 'tag "t" (list (cons W 2))))
    (list V 3 '() (list 'del (id W 2)))))

(define (writers-of) (list W V U))
(define (records-of w) (filter (lambda (e) (string=? (car e) w)) corpus))

(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs)
          o
          (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                 (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (bytes-of e)
  (encode-record (cadr e) (+ 1757300000000 (cadr e)) "agent:claude"
                 (caddr e) (storable-encode (cadddr e))))

;; ONLY THE LOCAL WRITER GETS AN owner.sexp, and every other writer gets
;; a manifest: that is what makes a mirrored writer's segment count.
;; Without the manifest the segment file is on disk and simply ignored,
;; and the state comes out missing a whole writer -- which reads as a
;; reduction defect rather than as a fixture that never published.
(define (build-at! dir)
  (system (string-append "rm -rf " dir " " dir "-home; mkdir -p " dir "/writers/" W
                         " " dir "/writers/" V " " dir "/writers/" U " " dir "/snap"))
  (put! (string-append dir "/meta.sexp") (string->utf8 "((format 1) (store-id \"t\"))\n"))
  (call-with-port (open-file-output-port (string-append dir "/lock") (file-options no-fail))
    (lambda (p) (if #f #f)))
  (put! (string-append dir "/writers/" W "/owner.sexp") (string->utf8 "((machine \"m\"))\n"))
  (putenv "THEOURGIA_HOME" (string-append dir "-home"))
  (let ((n (instance-install! dir))) (owner-install! dir W n))
  (for-each
    (lambda (w)
      (let ((seg (apply cat (map bytes-of (records-of w)))))
        (put! (string-append dir "/writers/" w "/" (segment-file-name 1)) seg)
        (unless (string=? w W)
          (write-manifest! dir w (list (manifest-entry 1 (bytevector->hex (sha256 seg)) seg))))))
    (writers-of)))
(define (build!) (build-at! d))

(define (in-memory records)
  (let ((r (reduce-empty)))
    (for-each (lambda (e) (reduce-apply! r (car e) (cadr e) (caddr e) (cadddr e))) records)
    r))

;; THE LOG'S ORDER IS PER WRITER, NOT THE ORDER THE CORPUS IS WRITTEN
;; IN. Feeding the corpus list to the reducer and the log to
;; open-and-reduce are therefore two different arrival orders of the
;; same records, which is the property being asserted.
(define (by-writer)
  (apply append (map records-of (writers-of))))

(build!)
(define disk (open-and-reduce d))
(define memo (in-memory corpus))
(define per-writer (in-memory (by-writer)))

(printf "== R8: the log and the record list agree ==\n")
(want "CONTROL: every record in the corpus was applied, none left pending"
      (list (length (reduce-trace disk)) (length (reduce-pending disk)))
      (list 9 0))
(want "the state read off the store equals the state fed to the reducer"
      (list (equal? (state-datum disk) (state-datum memo))
            (equal? (state-hash disk) (state-hash memo)))
      (list #t #t))
(want "and the per-writer arrival order the log imposes changes nothing"
      (equal? (state-datum disk) (state-datum per-writer))
      #t)
(want "the state itself, so the two paths agreeing is not two paths agreeing on nothing"
      (list (map cadr (state-datum disk))
            (let* ((b (car (state-datum disk)))
                   (fs (cadr (assq 'fields (cddr b)))))
              (cadr (assq 'title fs))))
      (list (list (id W 1) (id W 2))
            (list (cons "v-said" (cons V 1)))))
(want "the deleted block is not visible and the surviving one is"
      (list (and (state-read disk (id W 2)) #t)
            (cadr (assq 'deleted (cddr (cadr (state-datum disk)))))
            (and (state-read disk (id W 1)) #t))
      (list #t #t #t))
;; W.3's unlink saw U.1's link and not U.2's -- U.2 names V.1, which is
;; concurrent with it -- so the edge stays, carried by the event the
;; unlink never saw. One logical edge however many events made it.
(want "an unlink removes only the link events its own past covers"
      (let ((b (car (state-datum disk))))
        (cadr (assq 'edges (cddr b))))
      (list (cons 'explains (id W 2))))

(printf "== R8: rows round-trip on a state that came off disk ==\n")
(want "state to rows and back gives the same state and the same hash"
      (let ((again (rows->state (state->rows disk))))
        (list (equal? (state-datum disk) (state-datum again))
              (equal? (state-hash disk) (state-hash again))))
      (list #t #t))

(printf "== R9: reading the store at a cut ==\n")
;; A CUT IS REFUSED BY THE SAME RULE WHEREVER IT IS ASKED. open-and-reduce
;; does not re-decide usability; it asks cut-usable? of the whole
;; history and hands back the verdict, so a cut that outline refuses and
;; a cut the store refuses cannot come apart.
(want "a cut naming a record the store does not have is refused"
      (open-and-reduce d (list (cons W 1) (cons "qqqq0000" 5)))
      '(unusable not-received))
(want "a cut that omits a premise of what it names is refused"
      (open-and-reduce d (list (cons W 1) (cons V 1) (cons U 2)))
      '(unusable not-closed))
;; U.2 names V.1, and V.1 names W.1 -- so a cut holding U.2 must hold
;; both. This one does, and it stops before everything W did afterwards.
(want "a legal earlier cut gives exactly the state at that moment"
      (let ((early (open-and-reduce d (list (cons W 2) (cons V 1) (cons U 2))))
            (expected (in-memory
                        (filter (lambda (e)
                                  (let ((c (assoc (car e) (list (cons W 2) (cons V 1) (cons U 2)))))
                                    (and c (<= (cadr e) (cdr c)))))
                                corpus))))
        (list (equal? (state-datum early) (state-datum expected))
              (equal? (state-hash early) (state-hash expected))))
      (list #t #t))
(want "and what happened after the cut is not in it"
      (let* ((early (open-and-reduce d (list (cons W 2) (cons V 1) (cons U 2))))
             (b (car (state-datum early))))
        (list (length (reduce-trace early))
              (cadr (assq 'deleted (cddr (cadr (state-datum early)))))
              (cadr (assq 'edges (cddr b)))))
      (list 5 #f (list (cons 'explains (id W 2)))))
(want "CONTROL: the full state differs from the state at that cut"
      (equal? (state-hash disk)
              (state-hash (open-and-reduce d (list (cons W 2) (cons V 1) (cons U 2)))))
      #f)

(printf "== R8: resuming from a snapshot ==\n")
;; THE SNAPSHOT IS WRITTEN AT A MID CUT and the store is then opened
;; again. `disk` above was read before this file existed, so it is the
;; full-replay reference the resumed state has to match.
(define (traced thunk)
  (let ((p (open-output-string)))
    (parameterize ((current-error-port p))
      (trace-enable! #t)
      (guard (e (#t (trace-enable! #f) (raise e))) (thunk))
      (trace-enable! #f))
    (get-output-string p)))
(define (lines-of text)
  (let ((n (string-length text)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((>= i n) (reverse (if (> i start) (cons (substring text start i) acc) acc)))
            ((char=? (string-ref text i) #\newline)
             (loop (+ i 1) (+ i 1) (cons (substring text start i) acc)))
            (else (loop (+ i 1) start acc))))))
(define (has-substring? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) sub) #t)
            (else (loop (+ i 1)))))))
(define mid-cut (list (cons W 2) (cons V 1) (cons U 2)))
(snapshot-write! (string-append d "/snap/" (segment-file-name 1))
                 mid-cut (state->rows (open-and-reduce d mid-cut)))
(define resumed #f)
(define resume-trace (traced (lambda () (set! resumed (open-and-reduce d)))))
(want "the resumed state equals the full replay"
      (list (equal? (state-datum resumed) (state-datum disk))
            (equal? (state-hash resumed) (state-hash disk)))
      (list #t #t))
;; THIS ROW SAYS THE SNAPSHOT FILE PARSED, NOT THAT THE RESUME USED IT.
;; The load selects and reads a snapshot when the store is opened,
;; whichever way the caller then seeds -- a version that threw the rows
;; away and replayed everything still emits this event. The two rows
;; after it are what catch that.
(want "the snapshot was read"
      (let ((ls (filter (lambda (l) (has-substring? l "(trace snapshot-read"))
                        (lines-of resume-trace))))
        (list (> (length ls) 0)
              (has-substring? (if (null? ls) "" (car ls)) "/snap/000001.sexp")))
      (list #t #t))
;; AND REPLAY STARTED AFTER THE CUT. rows->state restores the applied
;; cut but not the trace, so the resumed reduction's trace holds exactly
;; the records the snapshot did not already account for.
(want "only the records after the cut were replayed"
      (list (length (reduce-trace resumed))
            (list-sort (lambda (a b) (if (string=? (car a) (car b))
                                         (< (cdr a) (cdr b))
                                         (string<? (car a) (car b))))
                       (reduce-trace resumed)))
      (list 4 (list (cons V 2) (cons V 3) (cons W 3) (cons W 4))))
;; A READER TAKES THE SHARED LOCK. Holding the exclusive one would lock
;; writers out of the store for the length of a read, and nothing about
;; the state that comes back would look any different.
(want "and the store lock was taken shared, not exclusive"
      (let ((ls (filter (lambda (l) (has-substring? l "(trace flock")) (lines-of resume-trace))))
        (list (> (length ls) 0)
              (for-all (lambda (l) (has-substring? l "shared")) ls)
              (exists (lambda (l) (has-substring? l "exclusive")) ls)))
      (list #t #t #f))

;; AND THE SNAPSHOT SAVED WORK, which is the only reason to have one.
;; The trace row above catches a resume that ignored the snapshot
;; outright; it does NOT catch one that seeds from the snapshot and then
;; replays the log from the beginning anyway, because re-delivered
;; records are refused as already applied and never reach the trace.
;; Records parsed is what separates them: 20 against 22 here.
;;
;; BOTH READINGS ROSE BY FOUR when the manifest began declaring each
;; segment's first and last sequence. Opening now decodes the first and
;; last line of every listed segment to check that declaration against
;; the bytes, which is two parse events per listed segment and two
;; listed segments in each of these stores. The gap between the two
;; readings is what this row is about, and it did not move; the absolute
;; numbers are re-measured rather than kept, because an expectation that
;; survives a change in what the product does is measuring nothing.
;; THE BASELINE IS MEASURED, NOT WRITTEN DOWN. Reading at a cut that
;; covers everything replays from the beginning by construction, so it
;; is the same store, the same records and no snapshot -- a constant
;; would have to be re-derived by hand every time the corpus changes.
(define (parse-count thunk)
  (length (filter (lambda (l) (has-substring? l "(trace parse"))
                  (lines-of (traced thunk)))))
;; A TWIN STORE HOLDING THE SAME CORPUS AND NO SNAPSHOT. Reading at a
;; cut would have been the cheaper baseline, but that path deliberately
;; replays twice -- once to judge the cut and once to build the state --
;; so its reading is two passes and would sit above a resume that
;; re-read everything, which is the case this row exists to catch.
(define d2 (test-dir "reduce2twin"))
(build-at! d2)
(putenv "THEOURGIA_HOME" (string-append d "-home"))
(want "CONTROL: the twin store holds the same state and has no snapshot"
      (let ((twin (open-and-reduce d2)))
        (list (equal? (state-datum twin) (state-datum disk))
              (length (reduce-trace twin))
              (length (directory-list (string-append d2 "/snap")))))
      (list #t 9 0))
(want "resuming parses fewer records than reading the same corpus cold"
      (let ((resume (parse-count (lambda () (open-and-reduce d))))
            (cold (parse-count (lambda () (open-and-reduce d2)))))
        (list (< resume cold) resume cold))
      (list #t 20 22))

(printf "== the applied frontier of a read stops before what is pending ==\n")
;; RULING: THE READ PATH'S APPLIED FRONTIER IS THE REDUCTION'S OWN CUT.
;; A store holding a record whose premise is not in it -- here U names a
;; writer the store has never had -- must come back with that record
;; pending and the frontier stopping short of it, not with a frontier
;; that counts records nobody could apply.
;; THE CALLBACK'S ANSWER CANNOT DISAGREE WITH THE FRONTIER because both
;; are read from `reduce-applied-cut`: the answer is "is this record in
;; the cut yet", and the frontier is the cut. That is the reason to
;; prefer it to a second cursor kept alongside -- there is no second
;; cursor to drift.
(define dp (test-dir "reduce2pending"))
(build-at! dp)
;; U's segment is replaced: two records whose premise is a writer that
;; does not exist in this store at all.
(let ((seg (apply cat (map bytes-of
                           (list (list U 1 (list (cons "qqqqqqqq" 4))
                                       (list 'set (id W 1) 'title "never"))
                                 (list U 2 '()
                                       (list 'set (id W 1) 'summary "never either")))))))
  (put! (string-append dp "/writers/" U "/" (segment-file-name 1)) seg)
  (write-manifest! dp U (list (manifest-entry 1 (bytevector->hex (sha256 seg)) seg))))
(putenv "THEOURGIA_HOME" (string-append d "-home"))
(define stuck (open-and-reduce dp))
;; AND IT CASCADES: W.3 names U.1 as its premise, so W.3 and W.4 are
;; stuck behind U's pair. Four records pending, not two -- a frontier
;; that stopped only at the record with the missing premise, and carried
;; on past the ones waiting on IT, would report having applied a record
;; built on a state it never reached.
(want "the unapplicable records are pending, and so is everything behind them"
      (list (length (reduce-pending stuck))
            (assoc U (reduce-applied-cut stuck)))
      (list 4 #f))
(want "CONTROL: the frontier stops at the last record that could be applied"
      (list-sort (lambda (a b) (string<? (car a) (car b))) (reduce-applied-cut stuck))
      (list (cons V 3) (cons W 2)))
;; AND THE EFFECT OF THE PENDING RECORDS IS NOT IN THE STATE. A frontier
;; that stopped correctly while the write had already been applied would
;; be a frontier that describes a state nobody can reproduce.
(want "and nothing they would have written is visible"
      (let* ((b (car (state-datum stuck)))
             (fs (cadr (assq 'fields (cddr b)))))
        (list (and (assq 'title fs) #t)
              (map (lambda (c) (car c)) (cadr (assq 'title fs)))
              (and (assq 'summary fs) #t)))
      (list #t (list "v-said") #f))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "reduce2 complete\n")
