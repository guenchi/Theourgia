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


;; `uncertain.sexp` is a cache, and this is what that costs and buys.
;;
;; An uncertain interval is a stretch of a writer's history the store
;; cannot vouch for. It is the thing standing between "this request did
;; not run" and "this request did not run anywhere I can see", so losing
;; one is how a client is told to execute something a second time.
;;
;; The file holding them is therefore written for speed and read for
;; nothing that is not also derivable. THE TEST OF THAT CLAIM IS TO TAKE
;; IT AWAY: delete it, corrupt it, write half of it, and see whether the
;; intervals a caller gets back change. They must not -- and each of
;; those states must be reported, because a store that quietly rebuilt
;; what it lost would look identical to one that never lost anything.
;;
;; THE RECONCILIATION IS NOT SYMMETRIC, and that is the subtle half. An
;; entry the records imply and the cache lacks is the cache being wrong.
;; An entry the cache holds and the records no longer imply is the
;; ordinary result of a repair -- the tail was mended, the records are
;; clean again -- and it must stay, because a request whose positions
;; touch that stretch still cannot be told apart from one that ran. So
;; the set only ever grows.

(import (chezscheme) (theourgia log) (theourgia ffi) (theourgia wire)
        (only (theourgia digest) sha256 bytevector->hex))

(define (test-dir name)
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/" name "-" (number->string (get-process-id)))))
    ;; A ROOT THAT DOES NOT SURVIVE THE ROUND TRIP IS REFUSED HERE. Paths
    ;; go into generated scripts and shell commands unquoted, so a root
    ;; with a space or a bracket in it makes the child read nothing at
    ;; all -- which reads exactly like a child that ran and found
    ;; nothing wrong.
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
    ;; AND THE LEAF IS NEVER THE ROOT. Removal only ever names
    ;; <root>/<name>-<pid>; a name that collapsed to nothing would aim
    ;; it at the scratch root itself, which holds every other run.
    (unless (and (> (string-length path) (+ 1 (string-length root)))
                 (string=? root (substring path 0 (string-length root)))
                 (char=? #\/ (string-ref path (string-length root))))
      (assertion-violation 'test-dir
        "the directory must lie strictly inside the root" (list root path)))
    ;; AND THE DIRECTORY IS MADE FRESH, NOT ASSUMED FRESH. The name
    ;; carries the process id, which reads like a unique name and is not
    ;; one: the pid space wraps and the scratch root outlives the run, so
    ;; a directory left by an earlier run holding the same pid is handed
    ;; to this one already populated. Both commands are checked, because
    ;; a removal that failed leaves `mkdir -p` succeeding on exactly the
    ;; populated directory this is here to prevent.
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


(define scratch (test-dir "q6"))
(define W "wwwlocl0")
(define M "mirrorz9")
(define d (string-append scratch "/store"))
(define home (string-append scratch "/home"))
(define child (string-append scratch "/child.sc"))
(define child-out (string-append scratch "/child.out"))
(define cand (string-append scratch "/cand.bin"))

(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))
(define (slurp path)
  (and (file-exists? path)
       (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
         (if (eof-object? b) (make-bytevector 0) b))))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                              (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (rec seq payload)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" '() (storable-encode payload)))
(define bytes (cat (rec 1 '(put ((kind . section))))
                   (rec 2 '(put ((kind . section))))))
(define sha (bytevector->hex (sha256 bytes)))

;; A STORE WITH ONE LOCAL WRITER AND NOTHING WRONG WITH IT. Each row
;; below damages it deliberately and says which damage it made, so that
;; "nothing is uncertain" is never the answer left over from a fixture
;; that failed to break anything.
(define (build!)
  (let ((status (system (string-append "rm -rf " d " " home "; mkdir -p "
                                       d "/writers/" W " " d "/writers/" M " "
                                       d "/snap " home))))
    (unless (eqv? 0 status)
      (assertion-violation 'build! "could not build the store" status)))
  (put! (string-append d "/meta.sexp") (string->utf8 "((format 1) (store-id \"q3\"))\n"))
  (put! (string-append d "/lock") (make-bytevector 0))
  (putenv "THEOURGIA_HOME" home)
  (let ((k (instance-install! d))) (owner-install! d W k))
  (store-register! d))

(define (first-publish!) (log-publish! d M 1 bytes sha))


;; A TORN TAIL IS A PARTIAL LINE AT THE END OF A SEGMENT: bytes written,
;; the newline that closes the record never reached. Nothing about this
;; is a fixture trick -- it is the residue of a crash part way through an
;; append, and the one shape the format is designed to recover from.
(define (torn-segment!)
  (let ((whole (cat (rec 1 '(put ((kind . section)))) (rec 2 '(put ((kind . section)))))))
    (put! (string-append d "/writers/" W "/" (segment-file-name 1))
          (cat whole (string->utf8 "(3 175730")))))

(define (repair-tail!)
  (put! (string-append d "/writers/" W "/" (segment-file-name 1))
        (cat (rec 1 '(put ((kind . section)))) (rec 2 '(put ((kind . section)))))))

(define (cache! text)
  (put! (string-append d "/writers/" W "/uncertain.sexp") (string->utf8 text)))
(define (drop-cache!)
  (delete-file (string-append d "/writers/" W "/uncertain.sexp")))

(define TORN (list W 2 #f))
(define REPAIRED (list W 40 60))

(printf "== Q14: what the records themselves say ==\n")
(build!)
(torn-segment!)
(want "a torn tail puts everything above the last readable record in doubt"
      (uncertain-derived d W)
      (list TORN))
;; TWIN: and a writer whose records are whole has nothing uncertain about
;; it. Without this the row above would pass for an implementation that
;; answered "everything is uncertain" to every question.
(want "TWIN: the same writer with its tail intact"
      (begin (build!) (uncertain-derived d W))
      '())

(printf "\n== Q14: taking the cache away ==\n")
(build!)
(torn-segment!)
;; THE ORDINARY STATE OF A HEALTHY WRITER IS NO FILE AT ALL, so an absent
;; cache is only a problem when there is something to cache. Reporting it
;; otherwise would make every healthy writer look damaged.
(want "no cache and nothing to cache is not a complaint"
      (begin (build!) (uncertain-load d W))
      '(() #f))
(build!)
(torn-segment!)
(want "no cache, with something to cache, answers from the records and says so"
      (uncertain-load d W)
      (list (list TORN) (list 'uncertain-cache 'absent)))
(want "with the cache written, the same intervals and no complaint"
      (begin (uncertain-write! d W (list TORN) 'commit)
             (uncertain-load d W))
      (list (list TORN) #f))
;; THE DELETION CELL. The intervals a caller gets back are the same ones;
;; only the report differs. That is the whole claim the word "cache" is
;; making.
(want "deleting it changes the report and not the answer"
      (begin (drop-cache!) (uncertain-load d W))
      (list (list TORN) (list 'uncertain-cache 'absent)))
;; THE CORRUPTION CELL.
(want "corrupting it changes the report and not the answer"
      (begin (cache! "((this is not") (uncertain-load d W))
      (list (list TORN) (list 'uncertain-cache 'unreadable)))
;; A FILE THAT PARSES INTO THE WRONG SHAPE IS UNREADABLE, NOT EMPTY. An
;; empty list is a positive claim that nothing is uncertain, and reading
;; a wrong shape as that claim is the most dangerous available answer.
(want "and so does one that parses into something else"
      (begin (cache! "((fork 3))") (uncertain-load d W))
      (list (list TORN) (list 'uncertain-cache 'unreadable)))
(want "TWIN: an empty cache is a claim, and it is checked against the records"
      (begin (cache! "()") (uncertain-load d W))
      (list (list TORN) (list 'uncertain-cache 'stale (list TORN))))

(printf "\n== Q14: the reconciliation is a union, and it is not symmetric ==\n")
;; An entry the records imply and the cache lacks is the cache being
;; wrong: the records win and the difference is named.
(want "a cache missing what the records imply is stale, and the records win"
      (begin (cache! "((\"wwwlocl0\" 40 60))") (uncertain-load d W))
      (list (list REPAIRED TORN) (list 'uncertain-cache 'stale (list TORN))))
;; AN ENTRY THE RECORDS NO LONGER IMPLY IS A REPAIR, NOT AN ERROR. The
;; tail was mended and the records are clean there again -- while the
;; fact that the store was once unsure about that stretch is exactly what
;; must survive, because a request whose positions touch it still cannot
;; be told apart from one that ran.
(want "a cache holding more than the records imply is kept whole"
      (begin (build!)
             (cache! "((\"wwwlocl0\" 40 60))")
             (uncertain-load d W))
      (list (list REPAIRED) #f))
(want "and a repair does not take the entry away"
      (begin (build!)
             (torn-segment!)
             (uncertain-write! d W (list REPAIRED TORN) 'commit)
             ;; the repair itself: the partial line is removed and the
             ;; records read to the end again. The cache is untouched.
             (repair-tail!)
             (list (uncertain-derived d W) (uncertain-load d W)))
      (list '() (list (list REPAIRED TORN) #f)))

(printf "\n== Q14: a writer whose beginning cannot be read ==\n")
;; A directory with neither an owner nor a manifest is an interrupted
;; publication: this store cannot say what the writer's history is, so
;; the whole of it is in doubt rather than none of it.
(want "an unreadable writer puts its whole domain in doubt"
      (begin (build!)
             (delete-file (string-append d "/writers/" W "/owner.sexp"))
             (uncertain-derived d W))
      (list (list W 0 #f)))

(printf "\n== Q14: what may be written ==\n")
(want "an entry that is not an interval is refused by name"
      (guard (e ((assertion-violation? e) (list (condition-who e) (condition-message e)))
                (#t (list 'some-other-condition)))
        (uncertain-write! d W (list (list W 5)) 'commit))
      '(uncertain-write! "every entry must be (writer low high)"))

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "q6 complete\n")
