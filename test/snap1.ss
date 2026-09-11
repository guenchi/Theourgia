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

;; I1: generating a snapshot (section 4.5', L7' and L8).
;; The reading side -- selecting a snapshot, refusing a broken one,
;; replaying from its cut -- is log6, log7 and reduce2. This is the
;; writing side: what goes in one, and when the log layer refuses to
;; write it at all.
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

(define W "wwwlocl0")
(define V "vvvmirrr")
(define scratch (test-dir "snap1"))
(define (put! p bv)
  (call-with-port (open-file-output-port p (file-options no-fail))
    (lambda (o) (put-bytevector o bv))))
(define (slurp p)
  (guard (e (#t 'no-such-file))
    (let ((b (call-with-port (open-file-input-port p) get-bytevector-all)))
      (if (eof-object? b) "" (utf8->string b)))))
(define (rec seq deps payload)
  (encode-record seq (+ 1757300000000 seq) "t" deps (storable-encode payload)))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs)
          o
          (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                 (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))

(define case-n 0)
(define (fresh! local-records mirror-records)
  (set! case-n (+ case-n 1))
  (let ((d (string-append scratch "/s" (number->string case-n))))
    (system (string-append "rm -rf " d "; mkdir -p " d "/writers/" W
                           " " d "/writers/" V " " d "/snap"))
    (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"t\"))\n"))
    (call-with-port (open-file-output-port (string-append d "/lock") (file-options no-fail))
      (lambda (p) (if #f #f)))
    (put! (string-append d "/writers/" W "/owner.sexp") (string->utf8 "((machine \"m\"))\n"))
    (putenv "THEOURGIA_HOME" (string-append d "/home"))
    (let ((n (instance-install! d))) (owner-install! d W n))
    (put! (string-append d "/writers/" W "/" (segment-file-name 1))
          (if (null? local-records) (make-bytevector 0) (apply cat local-records)))
    (unless (null? mirror-records)
      (let ((seg (apply cat mirror-records)))
        (put! (string-append d "/writers/" V "/" (segment-file-name 1)) seg)
        (write-manifest! d V (list (cons 1 (bytevector->hex (sha256 seg)))))))
    d))

(define (snap-files d)
  (let ((dir (string-append d "/snap")))
    (list-sort string<? (filter (lambda (n) (segment-file-number n)) (directory-list dir)))))

(printf "== a snapshot is written, and it is the reduction that was frozen ==\n")
(define d1
  (fresh! (list (rec 1 '() (list 'put (list (cons 'kind 'section) (cons 'title "one"))))
                (rec 2 '() (list 'set (string-append W ".1") 'title "two")))
          '()))
(define answer1 (store-snapshot! d1 "t"))
(want "it reports where it wrote and at what cut"
      (list (car answer1)
            (caddr answer1)
            (snap-files d1))
      (list 'written (list (cons W 2)) '("000001.sexp")))
(want "the cut is the reduction's applied cursor, not a file scan"
      (caddr answer1)
      (reduce-applied-cut (open-and-reduce d1)))
;; THE FILE IS WHOLE: section 4.5' says a snapshot without a correct end
;; line is discarded entirely, so the one just written has to have one.
(want "the file ends with an end line the reader accepts"
      (let-values (((cut rows) (snapshot-read (string-append d1 "/snap/000001.sexp"))))
        (list (and cut #t) (equal? cut (caddr answer1)) (> (length rows) 0)))
      (list #t #t #t))

(printf "== L7'(1'): the cut stops at what was applied, not at what was read ==\n")
;; THE MIRROR'S ONLY RECORD NAMES A PREMISE NOBODY HAS. It is on disk, it
;; is published, a scan of its log reports it -- and the reduction has
;; not applied it, so it is not in the cut and its effect is not in the
;; rows. An implementation that took the last sequence each writer's
;; file reaches would put it in both.
(define d2
  (fresh! (list (rec 1 '() (list 'put (list (cons 'kind 'section) (cons 'title "local"))))
                (rec 2 '() (list 'set (string-append W ".1") 'title "local2")))
          (list (rec 1 (list (cons "cccabsnt" 1))
                     (list 'set (string-append W ".1") 'title "from-the-mirror")))))
(want "CONTROL: the mirror's record is on disk, published, and pending"
      (let* ((st (open-and-reduce d2))
             (ls (log-open d2))
             (scanned (let ((p (load-prefix ls V)))
                        (load-commit! ls)
                        (and p (discovery-end-seq p)))))
        (list scanned (length (reduce-pending st)) (assoc V (reduce-applied-cut st))))
      (list 1 1 #f))
(define answer2 (store-snapshot! d2 "t"))
(want "the cut names only the writer whose records were applied"
      (caddr answer2)
      (list (cons W 2)))
(want "and the pending record's effect is not in the rows"
      (let-values (((cut rows) (snapshot-read (string-append d2 "/snap/000001.sexp"))))
        (let loop ((rs rows))
          (cond ((null? rs) 'not-present)
                ((and (eq? (car (car rs)) 'block))
                 (let* ((body (caddr (car rs)))
                        (fs (cdr (assq 'fields body)))
                        (title (assq 'title fs)))
                   (if title (map car (cdr title)) (loop (cdr rs)))))
                (else (loop (cdr rs))))))
      '("local2"))

(printf "== ahead of the durable frontier is refused, never trimmed ==\n")
;; TRIMMING WOULD PAIR A STATE COMPUTED OVER ONE SET OF RECORDS WITH A
;; CUT NAMING A SMALLER ONE -- a snapshot that is internally a lie and
;; that nothing downstream could detect. The reduction layer recomputes
;; at a smaller cut instead.
(define d3
  (fresh! (list (rec 1 '() (list 'put (list (cons 'kind 'section) (cons 'title "one")))))
          '()))
(define (snapshot-with d cut rows)
  (let* ((state (reduce-empty))
         (s (log-begin d (lambda args 'applied))))
    (let ((answer (guard (e (#t (log-end! s) (raise e)))
                    (session-snapshot! s (list (session-view s) cut rows)))))
      (log-end! s)
      answer)))
(want "CONTROL: a cut at the durable frontier is written"
      (car (snapshot-with d3 (list (cons W 1)) '()))
      'written)
(want "one sequence beyond it is refused, and the refusal says by how much"
      (snapshot-with d3 (list (cons W 2)) '())
      (list 'refused 'cut-ahead (list 'writer W) (list 'cut 2) (list 'durable 1)))
(want "a writer the store has never heard of is refused the same way"
      (snapshot-with d3 (list (cons "zzzzzzzz" 1)) '())
      (list 'refused 'cut-ahead (list 'writer "zzzzzzzz") (list 'cut 1) (list 'durable 0)))
;; AND A VIEW THAT HAS MOVED ON IS REFUSED. The envelope is one freeze;
;; a view taken before something changed does not describe the state the
;; rows were computed from.
;; AFTER AN APPEND THE SESSION IS WAITING TO BE TOLD THE RECORD WAS
;; APPLIED, and until then there is no view at all. That is its own
;; refusal and it names itself: answering "no local writer" for a
;; session that has one and is merely unconfirmed sends the caller
;; looking for the wrong thing.
(want "between an append and its confirmation there is no view, and the reason says so"
      (let* ((s (log-begin d3 (lambda args 'applied)))
             (v (session-view s)))
        (let ((answer
                (guard (e (#t (log-end! s) (raise e)))
                  (let ((r (session-append!
                             s (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                           (view-expect-seq v) "t" '()
                                           (list 'set (string-append W ".1") 'title "x")))))
                    (list (car r)
                          (session-snapshot! s (list v (list (cons W 1)) '())))))))
          (log-end! s)
          answer))
      (list 'committed (list 'refused 'not-ready)))
;; AND ONCE IT IS CONFIRMED, THE OLD VIEW IS STALE. The envelope is one
;; freeze: a view taken before the append does not describe the state
;; the rows would have been computed from.
(want "a view from before the append is refused as stale once a new one exists"
      (let* ((s (log-begin d3 (lambda args 'applied)))
             (v (session-view s)))
        (let ((answer
                (guard (e (#t (log-end! s) (raise e)))
                  (let ((r (session-append!
                             s (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                           (view-expect-seq v) "t" '()
                                           (list 'set (string-append W ".1") 'title "y")))))
                    (session-applied! s (session-epoch s) (list (cons W (cadr r))))
                    (list (car r)
                          (and (session-view s) #t)
                          (session-snapshot! s (list v (list (cons W 1)) '())))))))
          (log-end! s)
          answer))
      (list 'committed #t (list 'refused 'stale-view)))

;; A SNAPSHOT TAKEN AFTER THIS SESSION'S OWN WRITE REACHES THAT WRITE.
;; The durable frontier is not what the load found when the session
;; opened: `next-seq` advances only when an append comes back committed,
;; which is to say fsynced, so one less than it is the highest sequence
;; on disk. Reading the frontier from the load alone stops at the
;; session's opening and refuses every snapshot taken after a write --
;; and every row above still passes, because none of them writes first.
(want "a cut reaching this session's own committed append is written"
      (let* ((s (log-begin d3 (lambda args 'applied)))
             (v (session-view s)))
        (let ((answer
                (guard (e (#t (log-end! s) (raise e)))
                  (let ((r (session-append!
                             s (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                           (view-expect-seq v) "t" '()
                                           (list 'set (string-append W ".1") 'title "z")))))
                    (session-applied! s (session-epoch s) (list (cons W (cadr r))))
                    (list (car r)
                          (cadr r)
                          (car (session-snapshot!
                                 s (list (session-view s)
                                         (list (cons W (cadr r))) '()))))))))
          (log-end! s)
          answer))
      (list 'committed 4 'written))

(printf "== each snapshot is a new file, so the older one survives ==\n")
;; SELECTION TRIES THE HIGHEST NUMBER FIRST AND FALLS BACK. Writing in
;; place would destroy the fallback that section 4.5' depends on when
;; the newest turns out to be unusable.
(define d4
  (fresh! (list (rec 1 '() (list 'put (list (cons 'kind 'section) (cons 'title "one")))))
          '()))
(want "a second snapshot is written beside the first, not over it"
      (begin (store-snapshot! d4 "t")
             (store-snapshot! d4 "t")
             (snap-files d4))
      '("000001.sexp" "000002.sexp"))
(want "and the older one is still readable on its own"
      (let-values (((cut rows) (snapshot-read (string-append d4 "/snap/000001.sexp"))))
        (and cut #t))
      #t)

(printf "== L8: a snapshot never reaches past what was fsynced ==\n")
;; THE CRITERION ALLOWS TWO OUTCOMES and requires the case to say which
;; happened: either generating the snapshot is refused, or its cut stops
;; at the last sequence that was actually fsynced. What it forbids is a
;; cut that reaches past one.
;; A CHILD PROCESS, because THEOURGIA_INJECT is an expansion-time gate.
;; THE STORE IS MADE THE WAY A USER MAKES ONE, and its first record is
;; written through the ordinary path. Hand-building the segment gave a
;; store whose append never reached its fsync at all under the fault --
;; the snapshot's cut was then 1 because nothing had been appended, not
;; because the failed record was excluded, and the rows below were green
;; without measuring anything. A fault that consumes itself somewhere
;; else leaves every assertion about it true and empty.
(define d5 (string-append scratch "/s-l8"))
(system (string-append "rm -rf " d5 "; mkdir -p " d5))
(putenv "THEOURGIA_HOME" (string-append d5 "/home"))
(store-init! d5)
(with-store-write d5
  (lambda (st v) '((insert root #f ((kind . section) (title . "durable"))))) "t")
(define W5 (car (store-writers d5)))
(define child (string-append scratch "/l8.ss"))
;; THE SNAPSHOT IS ASKED FOR IN THE SAME SESSION AS THE FAILED APPEND.
;; Asking in a LATER session measures something else: opening a session
;; runs the takeover barrier, which fsyncs the segment and so makes the
;; half-written record durable after all -- the cut then legitimately
;; names it, and the row reads as a violation when nothing is wrong.
;; Within the session, the sequence was never fsynced, and the envelope
;; that names it must be refused.
(put! child
      (string->utf8
        (string-append
          "#!r6rs\n(import (chezscheme) (theourgia log) (theourgia ffi) (theourgia reduce))\n"
          "(putenv \"THEOURGIA_HOME\" \"" d5 "/home\")\n"
          "(define out '())\n"
          "(define (note! x) (set! out (cons x out)))\n"
          "(guard (e (#t (note! (list 'raised (format \"~s\" e)))))\n"
          "  (let* ((s (log-begin \"" d5 "\" (lambda args 'applied)))\n"
          "         (v (session-view s)))\n"
          "    (note! (list 'before-append (view-expect-seq v)))\n"
          "    (let ((r (session-append!\n"
          "               s (make-frame (view-revision v) (view-epoch v) (view-writer v)\n"
          "                             (view-expect-seq v) \"t\" '()\n"
          "                             '(put ((kind . section) (title . \"doomed\")))))))\n"
          "      (note! (list 'append (car r)))\n"
          "      (note! (list 'snapshot-at-2\n"
          "                   (session-snapshot! s (list (session-view s)\n"
          "                                              (list (cons \"" W5 "\" 2)) '()))))\n"
          "      (note! (list 'snapshot-at-1\n"
          "                   (session-snapshot! s (list (session-view s)\n"
          "                                              (list (cons \"" W5 "\" 1)) '()))))\n"
          "      (log-end! s))))\n"
          "(write (reverse out)) (newline)\n")))
(define l8
  (begin
    (system (string-append "THEOURGIA_INJECT=on "
                           "THEOURGIA_FAULT=fsync-fail@commit:file=000001.sexp "
                           "scheme --script " child " > " scratch "/l8.out 2>/dev/null"))
    (let ((text (slurp (string-append scratch "/l8.out"))))
      (guard (e (#t (list (list 'unreadable text))))
        (let ((x (read (open-string-input-port text))))
          (if (eof-object? x) (list (list 'child-produced-nothing text)) x))))))
(define (l8-part name)
  (let loop ((xs l8))
    (cond ((null? xs) (list 'missing name))
          ((eq? (car (car xs)) name) (cadr (car xs)))
          (else (loop (cdr xs))))))
(want "CONTROL: the append really did fail its fsync"
      (list (l8-part 'before-append) (l8-part 'append))
      (list 2 'written-fsync-failed))
;; L8 FIRST BRANCH: generating is refused, and the refusal says the cut
;; reaches past what this session can vouch for.
(want "a cut naming the sequence whose fsync failed is refused"
      (l8-part 'snapshot-at-2)
      (list 'refused 'cut-ahead (list 'writer W5) (list 'cut 2) (list 'durable 1)))
;; AND THE LAST SEQUENCE THAT DID SUCCEED IS STILL WRITABLE. Without
;; this the row above is satisfied by a build that refuses every
;; snapshot after any failure at all.
(want "CONTROL: a cut at the last successful sequence is still written"
      (car (l8-part 'snapshot-at-1))
      'written)

(printf "== L7: the remaining two ways a snapshot is discarded ==\n")
;; THE BAD SNAPSHOT CARRIES A GHOST BLOCK -- a block no record ever
;; created. If any part of it were adopted the ghost would be in the
;; state, so "is the ghost there" separates "discarded entirely" from
;; "partly believed". Section 4.5' says not partly.
(define (snap-at! d n cut rows)
  (snapshot-write! (string-append d "/snap/" (segment-file-name n)) cut rows))
(define (adopted d)
  (let ((ls (log-open d)))
    (let ((r (list (load-snapshot-cut ls) (load-snapshot-reason ls))))
      (load-commit! ls)
      r)))
(define (ghost-visible? d)
  (let loop ((bs (state-datum (open-and-reduce d))))
    (cond ((null? bs) #f)
          ((string=? (cadr (car bs)) "ghost") #t)
          (else (loop (cdr bs))))))

;; (a) THE END LINE IS PRESENT AND ITS CHECKSUM IS WRONG. The cut is
;; fully supported by the log, so nothing but the checksum can reject
;; it -- which is what makes this different from the cut-beyond-log
;; case log6 covers.
(define d6
  (fresh! (list (rec 1 '() (list 'put (list (cons 'kind 'section) (cons 'title "one"))))
                (rec 2 '() (list 'set (string-append W ".1") 'title "two")))
          '()))
(snap-at! d6 1 (list (cons W 2)) '((block "ghost" ((tomb . #f) (fields) (position)))))
(want "CONTROL: as written, that snapshot is adopted and the ghost is in the state"
      (list (car (adopted d6)) (ghost-visible? d6))
      (list (list (cons W 2)) #t))
(want "one flipped byte in the end line discards the whole file"
      (let* ((p (string-append d6 "/snap/000001.sexp"))
             (text (slurp p)))
        ;; the end line is the last one; corrupt a digit of its checksum
        (put! p (string->utf8
                  (let loop ((i (- (string-length text) 2)))
                    (cond
                      ((char=? (string-ref text i) #\newline)
                       (string-append (substring text 0 (+ i 1))
                                      "(end \"00000000\")\n"))
                      (else (loop (- i 1)))))))
        (list (car (adopted d6)) (cadr (adopted d6)) (ghost-visible? d6)))
      (list #f 'crc #f))
(want "and the state is exactly what the log alone gives"
      (let* ((st (open-and-reduce d6))
             (b (car (state-datum st))))
        (list (map cadr (state-datum st))
              (cadr (assq 'title (cadr (assq 'fields (cddr b)))))))
      (list (list (string-append W ".1")) (list (cons "two" (cons W 2)))))

;; (b) THE SEGMENT THE CUT NEEDS IS GONE. The snapshot itself is whole;
;; what fails is the coverage check -- the log can no longer show that
;; the records the cut names ever existed.
(define d7
  (fresh! (list (rec 1 '() (list 'put (list (cons 'kind 'section) (cons 'title "one")))))
          (list (rec 1 '() (list 'put (list (cons 'kind 'section) (cons 'title "mirror")))))))
(snap-at! d7 1 (list (cons W 1) (cons V 1))
          '((block "ghost" ((tomb . #f) (fields) (position)))))
(want "CONTROL: with both segments present the snapshot is adopted"
      (list (car (adopted d7)) (ghost-visible? d7))
      (list (list (cons W 1) (cons V 1)) #t))
(want "removing the segment the cut names discards the snapshot whole"
      (begin
        (system (string-append "rm -f " d7 "/writers/" V "/000001.sexp"))
        (list (car (adopted d7)) (cadr (adopted d7)) (ghost-visible? d7)))
      (list #f 'unsupported-cut #f))

(printf "\n~a failures\n" bad)
(printf "snap1 complete\n")
