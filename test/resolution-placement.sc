;; Copyright 2018 - 2026 guenchi
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;     http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; WHERE A RESOLUTION WAS FOUND (ledger F478; request.sc resolution-evidence).
;;
;; A `not-executed` resolution clears the range test for its target
;; (request-decision, rule 2's exempt), and resolutions are taken out of the
;; evidence before rule 1's placement and delivery checks. resolution-evidence
;; leaves out a resolution marked superseded or plan-mismatch, and nothing
;; else: so one the store cannot vouch for -- a sync candidate in incoming/,
;; a frame at or above its writer's fork, one not delivered, one in damaged/
;; -- clears the test as one in delivered history does.
;;
;;   RP-0  control: no resolution: unknown, range-overlaps
;;   RP-1  control: the resolution in valid history and delivered: execute
;;   RP-2  its frame only in incoming/, kept as a sync candidate
;;   RP-3  its frame at its writer's fork (quarantine.sexp)
;;   RP-4  in valid history and not delivered (a cut taken before it)
;;   RP-5  its frame in damaged/
;;   RP-6  control of the existing filter: delivered and marked superseded
;;
;; RP-2 to RP-5 assert only that the answer is NOT execute, and print it: the
;; design's question (ignore such a resolution, or answer unknown with a named
;; reason) is not decided here.
;;
;; THE STORE, built by hand as test/store-memory.sc builds its own: writer W
;; (local) holds five records and an uncertain.sexp naming the closed stretch
;; (W 2 5); the request R ("R", its cursor (W . 3)) has no evidence of its own.
;; THE RESOLUTION lives on a second writer W2, a mirror (its published.sexp
;; lists its segment), at sequence 3, written by an operator's request actor:
;; (resolve (W . "R") "fp" all not-executed (W 0 10) (supersedes)). There is no
;; verb that writes a resolution, so its frame is encoded by encode-record,
;; checksum and all. A fork on W2 opens a stretch on W2 only, which R's range
;; does not touch; that is why the resolution is not on W.
(import (chezscheme) (theourgia log) (theourgia wire)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (theourgia store) open-and-reduce make-write-request request-verdict)
        (only (theourgia reduce) reduce-applied-cut reduce-gates)
        (only (theourgia evidence-index) index-forget-memory! indexed-records))

(define bad 0)
(define rows-run 0)
(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e)
                             e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect)
     (begin (set! rows-run (+ rows-run 1))
            (with-expected label expect (x) (want-1 label (caught got) (caught x)))))))

(define root
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (string-append (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")
                   "/resolution-placement-" (number->string (get-process-id)))))
(system (string-append "rm -rf '" root "' && mkdir -p '" root "'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))

(define W "wwww1111")
(define W2 "mmmm2222")
(define (put! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p bv))))
(define (cat . bs)
  (let* ((n (apply + (map bytevector-length bs))) (o (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) o
          (begin (bytevector-copy! (car bs) 0 o i (bytevector-length (car bs)))
                 (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (put-record seq title)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" '()
                 (storable-encode (list 'put (list (cons 'kind 'section) (cons 'title title))))))
(define resolution (list 'resolve (cons W "R") "fp" 'all 'not-executed (list W 0 10) '(supersedes)))
(define operator (list "operator" (cons W2 "OP1") 'single "fp-op" #f (cons W2 2)))
(define resolution-frame
  (encode-record 3 (+ 1757300000000 3) operator '() (storable-encode resolution)))
(define without (cat (put-record 1 "m1") (put-record 2 "m2")))
(define with (cat without resolution-frame))
(define (writer-dir st w) (string-append st "/writers/" w))
;; W2's segment and its manifest, with the resolution or without it
(define (mirror! st segment last)
  (put! (string-append (writer-dir st W2) "/" (segment-file-name 1)) segment)
  (put! (string-append (writer-dir st W2) "/published.sexp")
        (string->utf8 (format "((1 ~s 1 ~a))\n" (bytevector->hex (sha256 segment)) last))))
;; -> a fresh store NAME, W2 holding the resolution in its segment when WITH-RES?
(define (build! name with-res?)
  (let ((st (string-append root "/" name)))
    (system (string-append "mkdir -p '" (writer-dir st W) "' '" (writer-dir st W2) "' '" st "/snap'"))
    (put! (string-append st "/meta.sexp") (string->utf8 "((format 1) (store-id \"t\"))\n"))
    (put! (string-append st "/lock") (make-bytevector 0))
    (put! (string-append (writer-dir st W) "/owner.sexp") (string->utf8 "((machine \"m\"))\n"))
    (let ((n (instance-install! st))) (owner-install! st W n))
    (put! (string-append (writer-dir st W) "/" (segment-file-name 1))
          (apply cat (map (lambda (k) (put-record k (format "w~a" k))) '(1 2 3 4 5))))
    (put! (string-append (writer-dir st W) "/uncertain.sexp") (string->utf8 (format "((~s 2 5))\n" W)))
    (if with-res? (mirror! st with 3) (mirror! st without 2))
    st))
(define request (make-write-request "test" 'insert (list "--title" "x") "R" (cons W 3)))
;; -> the verdict for R on ST, the index forgotten; CUT and GATES the store's
;; own unless given
(define (verdict st . given)
  (index-forget-memory!)
  (let* ((state (and (null? given) (open-and-reduce st)))
         (cut (if (pair? given) (car given) (reduce-applied-cut state)))
         (gates (if (pair? given) (cadr given) (reduce-gates state))))
    (request-verdict st request cut #f gates)))
;; WHERE THE INDEX FILED THE RESOLUTION, printed beside each row's answer so
;; the row shows it reached the placement it names
(define (placements st)
  (index-forget-memory!)
  (map (lambda (r) (list (car r) (list-ref r 4))) (indexed-records st (cons W "R"))))
(define (shape v)
  (cond ((not (pair? v)) (list 'not-a-verdict v))
        ((and (eq? (car v) 'unknown) (pair? (cdr v)) (pair? (cadr v))) (list 'unknown (car (cadr v))))
        (else (list (car v)))))
(define (not-execute? v) (not (equal? (shape v) '(execute))))

;; ---- the controls ----------------------------------------------------------------
(define rp0 (verdict (build! "rp0" #f)))
(printf "RP-0 ~s\n" rp0)
(want "RP-0 CONTROL: no resolution: unknown, range-overlaps (W 2 5)"
      rp0 (list 'unknown (list 'range-overlaps (list W 2 5))))
(define rp1 (verdict (build! "rp1" #t)))
(printf "RP-1 ~s; placed ~s\n" rp1 (placements (string-append root "/rp1")))
(want "RP-1 CONTROL: the resolution in valid history and delivered: execute"
      (shape rp1) '(execute))

;; ---- where the store cannot vouch for it -------------------------------------------
;; RP-2: W2's segment without it; its frame kept as a sync candidate in
;; incoming/, <segment>.<sha>.seg with its .ok marker (log.sc keep-incoming!)
(define rp2
  (let* ((st (build! "rp2" #f))
         (dir (string-append (writer-dir st W2) "/incoming"))
         (name (string-append (segment-file-name 2) "." (bytevector->hex (sha256 resolution-frame)))))
    (system (string-append "mkdir -p '" dir "'"))
    (put! (string-append dir "/" name ".seg") resolution-frame)
    (put! (string-append dir "/" name ".ok") (make-bytevector 0))
    (verdict st)))
(printf "RP-2 ~s; placed ~s\n" rp2 (placements (string-append root "/rp2")))
(want "RP-2 the resolution's frame only in incoming/ (a sync candidate): not execute" (not-execute? rp2) #t)
;; RP-3: in W2's segment, and W2's fork at its sequence
(define rp3
  (let ((st (build! "rp3" #t)))
    (put! (string-append (writer-dir st W2) "/quarantine.sexp") (string->utf8 "((format 1) (fork 3))\n"))
    (verdict st)))
(printf "RP-3 ~s; placed ~s\n" rp3 (placements (string-append root "/rp3")))
(want "RP-3 the resolution's frame at its writer's fork: not execute" (not-execute? rp3) #t)
;; RP-4: the cut and gates taken while W2 did not yet hold it; then its frame
;; written into W2's segment (and its manifest); the verdict asked with that cut
(define rp4
  (let* ((st (build! "rp4" #f))
         (state (open-and-reduce st))
         (cut (reduce-applied-cut state))
         (gates (reduce-gates state)))
    (mirror! st with 3)
    (list cut (verdict st cut gates))))
(printf "RP-4 cut ~s ~s; placed ~s\n" (car rp4) (cadr rp4) (placements (string-append root "/rp4")))
(want "RP-4 the resolution in valid history, not delivered (the cut stops at W2's 2): not execute"
      (list (assoc W2 (car rp4)) (not-execute? (cadr rp4))) (list (cons W2 2) #t))
;; RP-5: W2's segment without it; its frame in damaged/
(define rp5
  (let* ((st (build! "rp5" #f)) (dir (string-append (writer-dir st W2) "/damaged")))
    (system (string-append "mkdir -p '" dir "'"))
    (put! (string-append dir "/broken") resolution-frame)
    (verdict st)))
(printf "RP-5 ~s; placed ~s\n" rp5 (placements (string-append root "/rp5")))
(want "RP-5 the resolution's frame in damaged/: not execute" (not-execute? rp5) #t)

;; ---- the existing filter -------------------------------------------------------------
;; RP-6: delivered, and marked superseded: resolution-evidence leaves it out
(define rp6
  (let* ((st (build! "rp6" #t)) (state (open-and-reduce st)))
    (verdict st (reduce-applied-cut state) (cons (cons (cons W2 3) 'superseded) (reduce-gates state)))))
(printf "RP-6 ~s\n" rp6)
(want "RP-6 CONTROL of the existing filter: delivered and marked superseded: unknown, range-overlaps"
      rp6 (list 'unknown (list 'range-overlaps (list W 2 5))))

(printf "\n~a failures\nrows: ~a\nresolution-placement complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
