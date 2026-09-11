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

;; P2, P5, P8, P9 at the library level: with-store-write called from
;; Scheme rather than through a process. The process-level half of the
;; same criteria belongs to the CLI fixture.
(import (chezscheme) (theourgia log) (theourgia ffi) (theourgia trace)
        (theourgia reduce) (theourgia store) (theourgia wire)
        (only (igropyr crypto) sha256 bytevector->hex))

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
    (system (string-append "mkdir -p " path))
    path))

(define bad 0)
(define (want label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))

(define W "wwwx7q2a")
(define V "vvvy8r3b")
(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (slurp path)
  (guard (e (#t 'no-such-file))
    (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
      (if (eof-object? b) (make-bytevector 0) b))))
(define (wpath dir w n) (string-append dir "/writers/" w "/" (segment-file-name n)))

;; A STORE WITH THE LAYOUT init WOULD HAVE LEFT: the first segment file
;; exists and is empty. Without it a writer has no append target at all
;; and every write is refused before it reserves -- which is a correct
;; refusal about a store nobody has initialised, not a defect in the
;; thing under test.
(define (fresh! dir . mirror-records)
  (fresh-as! dir W V (if (null? mirror-records) '() (car mirror-records))))

;; THE LOCAL WRITER'S NAME IS AN ARGUMENT because one case needs the
;; block's creator to sort ABOVE everything the local writer goes on to
;; do: an implementation that used the highest event-id as the version
;; would then see no change and let a stale write through, and with the
;; creator sorting below it that implementation passes.
(define (fresh-as! dir local mirror mirror-records)
  (let ((W local) (V mirror))
  (system (string-append "rm -rf " dir "; mkdir -p " dir "/writers/" W
                         " " dir "/writers/" V " " dir "/snap"))
  (put! (string-append dir "/meta.sexp") (string->utf8 "((format 1) (store-id \"t\"))\n"))
  (call-with-port (open-file-output-port (string-append dir "/lock") (file-options no-fail))
    (lambda (p) (if #f #f)))
  (put! (string-append dir "/writers/" W "/owner.sexp") (string->utf8 "((machine \"m\"))\n"))
  (putenv "THEOURGIA_HOME" (string-append dir "/home"))
  (let ((n (instance-install! dir))) (owner-install! dir W n))
  (put! (wpath dir W 1) (make-bytevector 0))
  (unless (null? mirror-records)
    (let ((seg (let* ((bs (map (lambda (e)
                                 (encode-record (car e) (+ 1757300000000 (car e))
                                                "agent:claude" (cadr e)
                                                (storable-encode (caddr e))))
                               mirror-records))
                      (n (apply + (map bytevector-length bs)))
                      (o (make-bytevector n)))
                 (let loop ((bs bs) (i 0))
                   (if (null? bs)
                       o
                       (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs))))))))))
      (put! (wpath dir V 1) seg)
      (write-manifest! dir V (list (cons 1 (bytevector->hex (sha256 seg)))))))
  dir))

;; THE DEPS AS THEY WENT TO DISK, not as the answer reported them. A
;; library that computed the right deps and wrote different ones would
;; be invisible to any assertion that reads its own return value.
;; decode-line, not a bare parse: the line carries a CRC field and a
;; frame, and parsing the whole line as a datum fails on the checksum.
(define (record-n dir w n)
  (let* ((bytes (slurp (wpath dir w 1)))
         (text (utf8->string bytes))
         (len (string-length text)))
    (let loop ((i 0) (start 0) (k 0))
      (cond
        ((>= i len) 'no-such-record)
        ((char=? (string-ref text i) #\newline)
         (if (= k n)
             (decode-line (string->utf8 (substring text start (+ i 1))))
             (loop (+ i 1) (+ i 1) (+ k 1))))
        (else (loop (+ i 1) start k))))))
(define (deps-of dir w n)
  (let ((r (record-n dir w n)))
    (if (and (pair? r) (eq? (car r) 'ok)) (list-ref r 4) r)))

(define (w! dir . intents) (with-store-write dir (lambda (state view) intents)))
(define (one dir intent) (car (w! dir intent)))
(define (state-of dir) (open-and-reduce dir))
(define (field dir id name)
  (let* ((b (state-read (state-of dir) id))
         (fs (and b (cdr (assq 'fields b)))))
    (and fs (let ((e (assq name fs))) (and e (cdr e))))))
(define (ord-of dir id)
  (let loop ((rows (state-outline (state-of dir))))
    (cond ((null? rows) 'not-placed)
          ((equal? (caddr (car rows)) id) (cadr (car rows)))
          (else (loop (cdr rows))))))
(define (parent-of dir id)
  (let loop ((rows (state-outline (state-of dir))))
    (cond ((null? rows) 'not-placed)
          ((equal? (caddr (car rows)) id) (car (car rows)))
          (else (loop (cdr rows))))))
(define (watermark dir)
  (let ((b (slurp (registry-path))))
    (if (bytevector? b) (utf8->string b) b)))

(printf "== P2: insert commits one fact ==\n")
(define d (test-dir "store1p2"))
(fresh! d)
;; THE WRITE RUNS BEFORE THE EXPECTATION IS BUILT, and it is a define
;; rather than an argument because Chez does not specify the order in
;; which a call evaluates its arguments: with both inline, the expected
;; block-hash was read off a store the insert had not reached yet.
(define first-answer (one d '(insert root #f ((kind . section) (title . "a")))))
(want "the answer names the record, the block and the cursor"
      first-answer
      (list 'ok (list 'events (list (cons W 1)))
            (list 'state (list (cons (string-append W ".1")
                                     (block-hash (state-of d) (string-append W ".1")))))
            (list 'cursor (cons W 1))
            (list 'replay #f)))
(want "the id is derived from the sequence, and the first ord is 0"
      (list (map cadr (state-datum (state-of d))) (ord-of d (string-append W ".1")))
      (list (list (string-append W ".1")) 0))
(want "a second sibling appended after it gets ord 1"
      (begin (one d (list 'insert 'root (string-append W ".1") '((kind . section) (title . "b"))))
             (list (ord-of d (string-append W ".2")) (parent-of d (string-append W ".2"))))
      (list 1 'root))
(want "and one placed under the first is that block's child at ord 0"
      (begin (one d (list 'insert (string-append W ".1") #f '((kind . section) (title . "c"))))
             (list (parent-of d (string-append W ".3")) (ord-of d (string-append W ".3"))))
      (list (string-append W ".1") 0))
;; STERN-BROCOT, NOT THE MIDPOINT. Between ord 0 and ord 1 the smallest
;; denominator is 1/2; the midpoint rule gives 1/2 here too, so the row
;; that tells them apart is in reduce1 -- what this one pins is that
;; insert asks for a place BETWEEN the named sibling and the next, and
;; that the outline then reads a, new, b.
(want "inserting after the first of two siblings lands between them"
      (begin (one d (list 'insert 'root (string-append W ".1") '((kind . section) (title . "mid"))))
             (list (ord-of d (string-append W ".4"))
                   (map caddr (filter (lambda (row) (eq? (car row) 'root))
                                      (state-outline (state-of d))))))
      (list 1/2 (list (string-append W ".1") (string-append W ".4") (string-append W ".2"))))
;; ONLY THE AFFECTED BLOCK IS REPORTED. A state report listing every
;; block would be correct about all of them and useless as a token.
(want "the state report names the block the write touched and no other"
      (map car (cadr (assq 'state (cdr (one d (list 'set (string-append W ".1") 'title "x"))))))
      (list (string-append W ".1")))

(printf "== P2: a refused write moves nothing ==\n")
(define before-bytes (slurp (wpath d W 1)))
(define before-mark (watermark d))
(want "an unknown parent is refused, and it names the nearest ids"
      (let ((a (one d '(insert "nosuch.9" #f ((kind . section) (title . "z"))))))
        (list (car a) (cadr a) (caddr a) (car (cadddr a))))
      (list 'error 'unknown-id "nosuch.9" 'nearest))
(want "the log is byte-for-byte what it was, and so is the registry"
      (list (equal? before-bytes (slurp (wpath d W 1)))
            (equal? before-mark (watermark d)))
      (list #t #t))

(printf "== P2: ids are base36 of the real sequence ==\n")
(define d36 (test-dir "store1p36"))
(fresh! d36)
(one d36 '(insert root #f ((kind . section) (title . "root"))))
;; THIRTY-FOUR NON-PUT EVENTS, so the next insert lands on sequence 36.
;; Base36 and decimal agree up to nine; the sequence that tells them
;; apart is 36, which is "10" in base36 and would be "36" in decimal.
(let loop ((n 0))
  (when (< n 34)
    (one d36 (list 'set (string-append W ".1") 'title (number->string n)))
    (loop (+ n 1))))
(want "CONTROL: the next sequence really is 36"
      (cdr (assoc W (reduce-applied-cut (state-of d36))))
      35)
(define late-answer (one d36 '(insert root #f ((kind . section) (title . "late")))))
(want "the block made by event 36 is called .10, not .36"
      (list (cadr (assq 'state (cdr late-answer)))
            (cadr (assq 'events (cdr late-answer))))
      (list (list (cons (string-append W ".10")
                        (block-hash (state-of d36) (string-append W ".10"))))
            (list (cons W 36))))

(printf "== P2: deps come from the reduction ==\n")
(define dm (test-dir "store1mirror"))
(fresh! dm (list (list 1 '() (list 'put (list (cons 'kind 'section) (cons 'title "mirror"))))
                 (list 2 '() (list 'set (string-append V ".1") 'title "mirror2"))))
(want "CONTROL: the mirrored writer's blocks are visible"
      (map cadr (state-datum (state-of dm)))
      (list (string-append V ".1")))
;; A write in a store that holds another writer's history has to declare
;; what it had seen of that writer, or a reader cannot tell whether this
;; block was placed with knowledge of it.
(want "a write under a mirrored block declares that writer's last sequence"
      (begin (one dm (list 'insert (string-append V ".1") #f '((kind . section) (title . "mine"))))
             (list (parent-of dm (string-append W ".1"))
                   (deps-of dm W 0)))
      (list (string-append V ".1") (list (cons V 2))))
;; AND WITH A MIRROR PRESENT IT KEEPS THE MIRROR AND DROPS ONLY ITSELF.
;; THIS ROW HAS TO RUN WHILE dm IS THE CURRENT STORE. fresh! points
;; THEOURGIA_HOME at the store it is making, so a write aimed at an
;; earlier store after a later fresh! is judged against the wrong
;; machine registry and refused -- and the reading for that is a missing
;; record, which looks like the write never being attempted.
(want "the second record in a mirrored store names the mirror and not itself"
      (begin (one dm (list 'set (string-append V ".1") 'title "touched"))
             (deps-of dm W 1))
      (list (cons V 2)))

(define dlocal (test-dir "store1local"))
(fresh! dlocal)
(want "CONTROL: with only the local writer the deps are empty"
      (begin (one dlocal '(insert root #f ((kind . section) (title . "solo"))))
             (deps-of dlocal W 0))
      '())
;; THE WRITER'S SECOND RECORD, WHERE ITS OWN ENTRY EXISTS TO BE WRONGLY
;; INCLUDED. On the first record the applied cut is empty, so deps come
;; out () whether or not the writer filters itself -- both rows above
;; are green for a version that names its own last sequence. Here the
;; cut holds (W . 1) and a version that does not filter writes it into
;; the record, declaring as a premise the one event that is always a
;; premise.
(want "a writer does not declare itself as its own premise"
      (begin (one dlocal (list 'set (string-append W ".1") 'title "again"))
             (list (cdr (assoc W (reduce-applied-cut (state-of dlocal))))
                   (deps-of dlocal W 1)))
      (list 2 '()))


(printf "== P3: every verb is asserted by its effect, not by its answer ==\n")
;; A COMMAND THAT ANSWERS ok AND DOES NOTHING PASSES EVERY ASSERTION
;; THAT READS ONLY THE ANSWER. Each verb below is read back out of a
;; freshly opened store -- a new reduction built from the log, not the
;; one the write ran against.
(define dv (test-dir "store1verbs"))
(fresh! dv)
(define B1 (string-append W ".1"))
(define B2 (string-append W ".2"))
(one dv '(insert root #f ((kind . section) (title . "one"))))
(one dv '(insert root #f ((kind . section) (title . "two"))))
(want "set puts the value where a later read finds it"
      (begin (one dv (list 'set B1 'summary "written"))
             (field dv B1 'summary))
      "written")
;; THE TWO-ARGUMENT set REMOVES THE FIELD. Absent is a value the codec
;; carries, not a missing entry, so the read has to come back with no
;; summary rather than with an empty one.
(want "a two-argument set makes the field absent"
      (begin (one dv (list 'set B1 'summary))
             (list (field dv B1 'summary)
                   (map car (cdr (assq 'fields (state-read (state-of dv) B1))))))
      (list #f (list 'kind 'title)))
(want "move puts the block under the parent it names"
      (begin (one dv (list 'move B2 B1 #f))
             (list (parent-of dv B2) (ord-of dv B2)))
      (list B1 0))
(want "link is visible as an edge on the block it starts from"
      (begin (one dv (list 'link B1 'explains B2))
             (cdr (assq 'edges (state-read (state-of dv) B1))))
      (list (cons 'explains B2)))
(want "and unlink takes it away again"
      (begin (one dv (list 'unlink B1 'explains B2))
             (cdr (assq 'edges (state-read (state-of dv) B1))))
      '())
;; A DELETED BLOCK IS STILL READABLE AND SAYS SO; the outline stops
;; listing it. Reporting it as unknown would lose the difference between
;; "deleted" and "never existed", which is the difference a caller needs
;; to tell a mistake from a race.
(want "del hides the block from the outline but read still answers for it"
      (begin (one dv (list 'del B2))
             (list (cdr (assq 'deleted (state-read (state-of dv) B2)))
                   (map caddr (state-outline (state-of dv)))))
      (list #t (list B1)))

(printf "== P5 / R10(a-c): writing against a token ==\n")
;; THE CREATOR SORTS ABOVE THE WRITER THAT CHANGES THE BLOCK. Z made the
;; block; A changes it. (Z . 1) stays the highest event-id in the store
;; through every write below, so a version scheme that reported the
;; highest event-id would report the same value before and after A's
;; change and admit a write that must be refused.
(define A "aaaq1w2e")
(define Z "zzzp9o8i")
(define dt (test-dir "store1token"))
(fresh-as! dt A Z
           (list (list 1 '() (list 'put (list (cons 'kind 'section)
                                              (cons 'title "made-by-z"))))))
(define X (string-append Z ".1"))
(define (wt . intents) (with-store-write dt (lambda (state view) intents)))
(define (one-t intent) (car (wt intent)))
(want "CONTROL: the block exists and its creator is the highest event-id"
      (list (and (state-read (state-of dt) X) #t)
            (car (list-sort (lambda (a b) (if (string=? (car a) (car b))
                                              (> (cdr a) (cdr b))
                                              (string>? (car a) (car b))))
                            (reduce-applied-cut (state-of dt)))))
      (list #t (cons Z 1)))
(define token-before (block-hash (state-of dt) X))
(one-t (list 'set X 'summary "a-changed-it"))
(want "CONTROL: the highest event-id is unchanged by that write"
      (car (list-sort (lambda (a b) (if (string=? (car a) (car b))
                                        (> (cdr a) (cdr b))
                                        (string>? (car a) (car b))))
                      (reduce-applied-cut (state-of dt))))
      (cons Z 1))
(define count-before (length (reduce-trace (state-of dt))))
(define mark-before (watermark dt))
;; (a) THE STALE TOKEN IS REFUSED, and refused before anything is
;; reserved -- the registry water mark is what says "before", because a
;; reservation moves it whether or not the bytes are ever written.
(want "a write carrying the old token is refused and says what the token is now"
      (one-t (list 'expect token-before (list 'set X 'summary "stale")))
      (list 'error 'changed (list 'current (block-hash (state-of dt) X))))
(want "and nothing was reserved: no record, and the water mark stands"
      (list (= count-before (length (reduce-trace (state-of dt))))
            (equal? mark-before (watermark dt)))
      (list #t #t))
;; (c) AN UNRELATED BLOCK MOVING DOES NOT INVALIDATE THIS ONE'S TOKEN.
(define token-now (block-hash (state-of dt) X))
(one-t '(insert root #f ((kind . section) (title . "somewhere else"))))
(want "a change to another block leaves this token usable"
      (list (equal? token-now (block-hash (state-of dt) X))
            (car (one-t (list 'expect token-now (list 'set X 'summary "accepted")))))
      (list #t 'ok))
(want "and the value the accepted write carried is what reads back"
      (field dt X 'summary)
      "accepted")
;; (b) A DELETED BLOCK REFUSES WITH ITS OWN REASON, not with "changed":
;; the caller that asked for "unchanged" needs to know the block is gone
;; rather than that it differs.
(define token-live (block-hash (state-of dt) X))
(one-t (list 'del X))
(want "a write against a deleted block says deleted"
      (one-t (list 'expect token-live (list 'set X 'summary "too late")))
      (list 'error 'deleted X))

(printf "== applying a write locally drains what was waiting on it ==\n")
;; A RECORD ALREADY IN THE STORE CAN BE WAITING FOR THE ONE ABOUT TO BE
;; WRITTEN. The mirror's only record names (W . 1) as its premise, and
;; W has written nothing yet, so it sits pending through log-begin. The
;; local insert then CREATES (W . 1) -- and applying it has to drain the
;; mirror's record too, or the state this session goes on to answer from
;; is not the state a replay produces.
(define dd (test-dir "store1drain"))
(fresh-as! dd W V
           (list (list 1 (list (cons W 1)) (list 'set (string-append W ".1") 'title "from-v"))))
(want "CONTROL: the mirror's record is waiting, and its effect is nowhere"
      (let ((st (state-of dd)))
        (list (length (reduce-pending st))
              (assoc V (reduce-applied-cut st))
              (map cadr (state-datum st))))
      (list 1 #f '()))
(define in-session (vector #f))
(define drain-answer
  (with-store-write dd
    (lambda (state view)
      (vector-set! in-session 0 state)
      '((insert root #f ((kind . section) (title . "mine")))))))
;; NOT (cdr (assoc …)): when the drain does not happen there is no
;; entry for V at all, and cdr of #f ends the file with an exception
;; instead of a FAIL row -- a run with no failure count reads like a
;; run that was never made.
(define (reached cut w)
  (let ((e (assoc w cut))) (if e (cdr e) 'not-applied)))
(want "the write commits and the waiting record goes with it"
      (list (car (car drain-answer))
            (length (reduce-pending (vector-ref in-session 0)))
            (reached (reduce-applied-cut (vector-ref in-session 0)) V))
      (list 'ok 0 1))
;; AND THE SESSION'S STATE IS THE STATE A REPLAY GIVES. Skipping the
;; drain leaves the title as the insert wrote it, and the next process
;; to open the store reads something else.
(want "the in-session state and a fresh replay agree, title and all"
      (let ((fresh (state-of dd)))
        (list (equal? (state-datum (vector-ref in-session 0)) (state-datum fresh))
              (field dd (string-append W ".1") 'title)))
      (list #t "from-v"))

(printf "== an append whose outcome is unknown does not answer 'refused' ==\n")
;; ONLY ONE OF THE FIVE OUTCOMES MEANS THE LOG IS UNTOUCHED. A caller
;; told "this failed" will not look for the record; if the bytes are in
;; fact durable, the next replay hands it a record its own answer said
;; did not exist. fsync failing after the write is exactly that case.
;; A CHILD PROCESS, because THEOURGIA_INJECT is an expansion-time gate:
;; arming it here would arm it for every append in this file.
(define df (test-dir "store1fault"))
(fresh! df)
(define child-path (string-append df "/child.ss"))
(define child-out (string-append df "/child.out"))
(put! child-path
      (string->utf8
        (string-append
          "#!r6rs\n(import (chezscheme) (theourgia log) (theourgia ffi)\n"
          "        (theourgia store) (theourgia reduce))\n"
          "(putenv \"THEOURGIA_HOME\" \"" df "/home\")\n"
          "(define res\n"
          "  (guard (e (#t (list (list 'raised))))\n"
          "    (with-store-write \"" df "\"\n"
          "      (lambda (st v) '((insert root #f ((kind . section) (title . \"faulted\"))))))))\n"
          "(printf \"~s\\n\" (car res))\n")))
(define faulted-answer
  (begin
    (system (string-append "THEOURGIA_INJECT=on THEOURGIA_FAULT=fsync-fail@commit:file=000001.sexp "
                           "scheme --script " child-path " > " child-out " 2>/dev/null"))
    (let ((text (let ((b (slurp child-out))) (if (bytevector? b) (utf8->string b) ""))))
      (guard (e (#t (list 'unreadable text)))
        (read (open-string-input-port text))))))
(want "the answer names the outcome and says it is indeterminate, not refused"
      (list (car faulted-answer) (cadr faulted-answer) (caddr faulted-answer))
      (list 'error 'indeterminate 'written-fsync-failed))
;; AND THE RECORD IS THERE. This is what makes the wording matter: had
;; the answer said refused, the caller would not have looked, and the
;; block below would be a block nobody believes was written.
(want "and a plain replay finds the record the caller was not told about"
      (list (length (reduce-trace (state-of df)))
            (map cadr (state-datum (state-of df))))
      (list 1 (list (string-append W ".1"))))

(printf "== P9: a view that has gone stale inside one process ==\n")
;; ONE PROCESS, NOT TWO. P4 covers two processes racing for the lock;
;; this is the case where the same session prepares a frame, commits
;; something else, and then offers the prepared frame -- the sequence it
;; reserved is taken and the append must refuse before reserving again.
(define ds (test-dir "store1stale"))
(fresh! ds)
(define stale-result
  (with-store-write ds
    (lambda (state view)
      ;; the frame this intent will become was prepared against `view`
      (list '(insert root #f ((kind . section) (title . "first")))
            '(insert root #f ((kind . section) (title . "second")))))))
(want "CONTROL: two intents in one session both commit, in order"
      (map car stale-result)
      (list 'ok 'ok))
(want "and they took consecutive sequences"
      (map (lambda (a) (cadr (assq 'events (cdr a)))) stale-result)
      (list (list (cons W 1)) (list (cons W 2))))
(define ds2 (test-dir "store1stale2"))
(fresh! ds2)
;; A VIEW HELD ACROSS A COMMIT NAMES A SEQUENCE THAT IS GONE. The view
;; is captured, the session then commits something else on the same
;; sequence, and the held view still says 1 -- which is what a caller
;; who prepared a frame from it would offer.
(define held-view (vector #f))
(with-store-write ds2
  (lambda (state view)
    (vector-set! held-view 0 view)
    '((insert root #f ((kind . section) (title . "takes the sequence"))))))
(want "the held view still names the sequence the commit consumed"
      (list (view-expect-seq (vector-ref held-view 0))
            (length (reduce-trace (state-of ds2)))
            (cdr (assoc W (reduce-applied-cut (state-of ds2)))))
      (list 1 1 1))

(printf "== which ids a refusal suggests ==\n")
;; THE SUGGESTIONS ARE PART OF THE ANSWER, and nothing so far has said
;; what makes one id nearer than another. Every row that reads them has
;; been happy with any three ids at all, because the stores those rows
;; used had at most three blocks.
;; THE RULE: longest shared prefix first, ties broken lexicographically,
;; at most three. Ids share a long prefix exactly when they come from
;; the same writer, which is what makes the prefix the useful measure
;; here -- a typo in a block id is almost always a typo in the tail.
(define dn (test-dir "store1nearest"))
(fresh! dn)
(let loop ((n 0))
  (when (< n 6)
    (one dn (list 'insert 'root #f (list (cons 'kind 'section)
                                         (cons 'title (number->string n)))))
    (loop (+ n 1))))
(want "CONTROL: there are more blocks than a refusal will name"
      (length (state-datum (state-of dn)))
      6)
;; A NEAR MISS ON THE TAIL: the ids are <writer>.1 .. <writer>.6, and a
;; request for <writer>.7 shares the whole writer with all of them. The
;; tie is then lexicographic, so the first three by id come back.
(want "an id differing only in its tail is answered with the lowest three"
      (let ((a (one dn (list 'set (string-append W ".7") 'title "x"))))
        (list (car a) (cadr a) (cadr (cadddr a))))
      (list 'error 'unknown-id
            (list (string-append W ".1") (string-append W ".2") (string-append W ".3"))))
;; AND AN ID SHARING NOTHING STILL GETS AN ANSWER rather than an empty
;; list: the caller asked for help, and "no id is close" is less useful
;; than "here is what this store holds".
(want "an id from another writer entirely still gets three suggestions"
      (let ((a (one dn '(set "zzzzzzzz.1" title "x"))))
        (list (car a) (length (cadr (cadddr a)))))
      (list 'error 3))
;; AT MOST THREE, whatever the store holds. A refusal that listed every
;; id would be a refusal an agent has to parse before it can act.
(want "never more than three, however many blocks there are"
      (let ((a (one dn (list 'set (string-append W ".9") 'title "x"))))
        (length (cadr (cadddr a))))
      3)
;; THE ROWS ABOVE DO NOT SEPARATE "longest shared prefix" FROM PLAIN
;; ALPHABETICAL ORDER, because every id in that store came from one
;; writer and so shares its whole prefix. Here two writers are present
;; and the mirror's name sorts FIRST -- so an implementation that only
;; sorted would suggest the mirror's blocks for a typo in a local id.
(define dn2 (test-dir "store1nearest2"))
(fresh-as! dn2 W V
           (list (list 1 '() (list 'put (list (cons 'kind 'section) (cons 'title "m1"))))
                 (list 2 '() (list 'put (list (cons 'kind 'section) (cons 'title "m2"))))
                 (list 3 '() (list 'put (list (cons 'kind 'section) (cons 'title "m3"))))))
(let loop ((n 0))
  (when (< n 3)
    (one dn2 (list 'insert 'root #f (list (cons 'kind 'section)
                                          (cons 'title (number->string n)))))
    (loop (+ n 1))))
(want "CONTROL: the mirror's ids sort before the local writer's"
      (list (string<? (string-append V ".1") (string-append W ".1"))
            (length (state-datum (state-of dn2))))
      (list #t 6))
(want "a typo in a local id is answered with local ids, not the alphabetically first"
      (let ((a (one dn2 (list 'set (string-append W ".9") 'title "x"))))
        (cadr (cadddr a)))
      (list (string-append W ".1") (string-append W ".2") (string-append W ".3")))

(printf "\n~a failures\n" bad)
(printf "store1 complete\n")
