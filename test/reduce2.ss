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
  (system (string-append "rm -rf " dir "; mkdir -p " dir "/writers/" W
                         " " dir "/writers/" V " " dir "/writers/" U " " dir "/snap"))
  (put! (string-append dir "/meta.sexp") (string->utf8 "((format 1) (store-id \"t\"))\n"))
  (call-with-port (open-file-output-port (string-append dir "/lock") (file-options no-fail))
    (lambda (p) (if #f #f)))
  (put! (string-append dir "/writers/" W "/owner.sexp") (string->utf8 "((machine \"m\"))\n"))
  (putenv "THEOURGIA_HOME" (string-append dir "/home"))
  (let ((n (instance-install! dir))) (owner-install! dir W n))
  (for-each
    (lambda (w)
      (let ((seg (apply cat (map bytes-of (records-of w)))))
        (put! (string-append dir "/writers/" w "/" (segment-file-name 1)) seg)
        (unless (string=? w W)
          (write-manifest! dir w (list (cons 1 (bytevector->hex (sha256 seg))))))))
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
;; Records parsed is what separates them: 16 against 18 here.
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
(putenv "THEOURGIA_HOME" (string-append d "/home"))
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
      (list #t 16 18))

(printf "\n~a failures\n" bad)
(printf "reduce2 complete\n")
