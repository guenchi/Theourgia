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

;; THE MEMORY OF A LOAD. A load asks a memory for the three pure
;; computations on a segment's bytes -- the scan, the digest and the edge
;; sequences -- and a memory answers what those answer for the same bytes,
;; from what it remembers when the bytes are the ones it remembers. These
;; rows hold it to that on stores small enough to count:
;;
;;   SM-1  a cold read decodes each record once, and a manifest-listed
;;         segment of n records n + 2 times (its first and last lines are
;;         decoded again for the range the manifest declares); the
;;         callers of discover-prefix that are not loads remember nothing
;;   SM-3  bytes that changed between two loads handed one memory are
;;         answered as a load with no memory answers them: a middle
;;         segment damaged, a listed segment replaced at the same length;
;;         and bytes that grew are decoded only past what was remembered
;;   a load lets go of its own memory when its delivery ends, and leaves a
;;         memory its caller handed in with the caller
;;   SM-6  past its bound a memory scans a segment at every ask, and the
;;         answer is the same
;;
;; The counts are read from the trace: one `parse` per line decoded, and
;; one `memory` event per segment a load's memory scans, naming the phase,
;; the segment, how it was answered and how many frames it handed on.
(import (chezscheme) (theourgia log) (theourgia trace) (theourgia reduce)
        (theourgia store) (theourgia wire) (theourgia degeneracy)
        (only (theourgia digest) sha256 bytevector->hex))

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
    (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
(define (test-dir name)
  (let ((path (string-append root "/" name "-" (number->string (get-process-id)))))
    (unless (eqv? 0 (system (string-append "rm -rf '" path "' '" path "-home' && mkdir -p '" path "'")))
      (assertion-violation 'test-dir "could not prepare the scratch directory" path))
    path))

(define W "wwwm4k2a")
(define M "mmmn5r7b")

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
(define (record w seq)
  (encode-record seq (+ 1757300000000 seq) "agent:claude" '()
                 (storable-encode (list 'put (list (cons 'kind 'section)
                                                   (cons 'title (format "~a-~a" w seq)))))))
(define (records w from to)
  (apply cat (map (lambda (k) (record w k)) (iota* from to))))
(define (iota* from to)
  (if (> from to) '() (cons from (iota* (+ from 1) to))))
(define (segment-path dir w n) (string-append dir "/writers/" w "/" (segment-file-name n)))
(define (segment-bytes dir w n)
  (call-with-port (open-file-input-port (segment-path dir w n)) get-bytevector-all))

;; THE LOCAL WRITER W HOLDS 8 RECORDS IN THREE SEGMENTS (1-3, 4-6, 7-8);
;; with MIRROR?, the mirrored writer M holds 4 in one segment that its
;; manifest lists, with the segment's hash and the range it holds.
(define (build! dir mirror?)
  (system (string-append "rm -rf '" dir "' '" dir "-home'; mkdir -p '" dir "/writers/" W "' '"
                         dir "/writers/" M "' '" dir "/snap'"))
  (put! (string-append dir "/meta.sexp") (string->utf8 "((format 1) (store-id \"t\"))\n"))
  (put! (string-append dir "/lock") (make-bytevector 0))
  (put! (string-append dir "/writers/" W "/owner.sexp") (string->utf8 "((machine \"m\"))\n"))
  (putenv "THEOURGIA_HOME" (string-append dir "-home"))
  (let ((n (instance-install! dir))) (owner-install! dir W n))
  (put! (segment-path dir W 1) (records W 1 3))
  (put! (segment-path dir W 2) (records W 4 6))
  (put! (segment-path dir W 3) (records W 7 8))
  (if mirror?
      (let ((seg (records M 1 4)))
        (put! (segment-path dir M 1) seg)
        (write-manifest! dir M (list (list 1 (bytevector->hex (sha256 seg)) 1 4))))
      (system (string-append "rm -rf '" dir "/writers/" M "'")))
  dir)

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
(define (has-prefix? s p)
  (and (>= (string-length s) (string-length p))
       (string=? (substring s 0 (string-length p)) p)))
(define (parses text)
  (length (filter (lambda (l) (has-prefix? l "(trace parse")) (lines-of text))))
;; the memory events of a trace, each as (phase writer segment how frames)
(define (memory-events text)
  (map (lambda (l)
         (let ((d (read (open-string-input-port l))))
           (let ((s (caddr d)))
             (list (car s) (symbol->string (cadr s)) (caddr s) (cadddr s) (cadddr d)))))
       (filter (lambda (l) (has-prefix? l "(trace memory ")) (lines-of text))))

;; A whole load: the records it delivered, (writer . seq) in order.
(define (load! dir memory)
  (let* ((ls (if memory (log-open-with-memory dir memory) (log-open dir)))
         (got '()))
    (load-deliver! ls '() (lambda (w seg off seq ts actor deps payload)
                            (set! got (cons (cons w seq) got))))
    (load-commit! ls)
    (reverse got)))
(define (all-of w from to) (map (lambda (k) (cons w k)) (iota* from to)))

;; A RECORD OPENED THROUGH ITS TYPE, every field of the type and its
;; parents, inside pairs and vectors: equal? compares records by identity,
;; so a discovery is compared as what it holds.
(define (opened x)
  (cond
    ((pair? x) (cons (opened (car x)) (opened (cdr x))))
    ((vector? x) (vector-map opened x))
    ((record? x)
     (let walk ((rtd (record-rtd x)) (fields '()))
       (if (not rtd)
           (cons* 'record (record-type-name (record-rtd x)) fields)
           (walk (record-type-parent rtd)
                 (append (let loop ((i 0) (acc '()))
                           (if (= i (vector-length (record-type-field-names rtd)))
                               (reverse acc)
                               (loop (+ i 1)
                                     (cons (list (vector-ref (record-type-field-names rtd) i)
                                                 (opened ((record-accessor rtd i) x)))
                                           acc))))
                         fields)))))
    (else x)))

;; what a discovery says about where a writer stops and why
(define (extent p)
  (list (discovery-end-seq p)
        (map (lambda (e) (list (log-error-kind e) (log-error-segment e) (log-error-offset e)))
             (discovery-integrity p))))

;; =============================================================================
(printf "== SM-1: a cold read decodes each record once ==\n")
(define local (build! (test-dir "segmem-local") #f))
(define twin (build! (test-dir "segmem-twin") #t))

(let* ((got #f)
       (text (traced (lambda () (set! got (load! local #f))))))
  (want "SM-1 CONTROL: the cold read of the local store delivers its 8 records in order"
        got (all-of W 1 8))
  (want "SM-1 a cold read of a store with no listed segment parses each record once (8 records; base 16)"
        (parses text) 8)
  (want "SM-1 its memory events: validation scans each segment, delivery replays it, handing on each segment's records"
        (memory-events text)
        (list (list 'validation W 1 'scanned 3) (list 'validation W 2 'scanned 3)
              (list 'validation W 3 'scanned 2)
              (list 'delivery W 1 'replayed 3) (list 'delivery W 2 'replayed 3)
              (list 'delivery W 3 'replayed 2))))

(let* ((got #f)
       (text (traced (lambda () (set! got (load! twin #f))))))
  (want "SM-1 CONTROL: the cold read of the twin delivers M's 4 records and W's 8"
        (list (filter (lambda (e) (string=? (car e) M)) got)
              (filter (lambda (e) (string=? (car e) W)) got))
        (list (all-of M 1 4) (all-of W 1 8)))
  (want "SM-1 with one listed segment of 4 records, that segment parses 4 + 2 (base 10), the local writer 8 (base 16)"
        (parses text) 14)
  (want "SM-1 the listed segment is scanned once, by validation, and replayed by delivery"
        (filter (lambda (e) (string=? (cadr e) M)) (memory-events text))
        (list (list 'validation M 1 'scanned 4) (list 'delivery M 1 'replayed 4))))

;; THE CALLERS OF discover-prefix THAT ARE NOT LOADS are handed the memory
;; that remembers nothing: each call decodes what it decoded before, and
;; no memory event is traced.
(let ((text (traced (lambda ()
                      (discover-prefix twin W 'shared)
                      (discover-prefix twin W 'shared)
                      (discover-prefix twin M 'shared)))))
  (want "SM-1 discover-prefix outside a load remembers nothing: two calls on W parse 8 each, one on M 4 + 2, no memory event"
        (list (parses text) (length (memory-events text)))
        (list 22 0)))

;; =============================================================================
(printf "== SM-3: what changed between two loads is answered as a load with no memory answers it ==\n")
;; A MIDDLE SEGMENT DAMAGED BETWEEN TWO DISCOVERIES HANDED ONE MEMORY: the
;; memory remembers it undamaged, and the second must cut W where a
;; discovery with no memory cuts it. One byte of record 5's title is
;; changed, the length kept, so the file reads, its checksum fails at
;; record 5, and W stops at 4.
(define (flip-at! path i)
  (let ((bv (call-with-port (open-file-input-port path) get-bytevector-all)))
    (bytevector-u8-set! bv i (if (= 65 (bytevector-u8-ref bv i)) 66 65))
    (put! path bv)))
(define (title-offset bv w seq)
  (let ((needle (string->utf8 (format "~a-~a" w seq))))
    (let loop ((i 0))
      (cond ((> (+ i (bytevector-length needle)) (bytevector-length bv)) #f)
            ((let same ((k 0))
               (or (= k (bytevector-length needle))
                   (and (= (bytevector-u8-ref bv (+ i k)) (bytevector-u8-ref needle k))
                        (same (+ k 1)))))
             i)
            (else (loop (+ i 1)))))))

(let* ((dir (build! (test-dir "segmem-damage") #f))
       (memory (make-load-memory))
       (before (extent (discover-prefix dir W 'shared memory)))
       (path (segment-path dir W 2))
       (_ (flip-at! path (title-offset (call-with-port (open-file-input-port path) get-bytevector-all) W 5)))
       (after (discover-prefix dir W 'shared memory))
       (base (discover-prefix dir W 'shared)))
  (want "SM-3 CONTROL: undamaged, W reaches 8 with no note"
        before (list 8 '()))
  (want "SM-3 a middle segment damaged between two discoveries handed one memory is cut where a discovery with no memory cuts it, the whole discovery equal"
        (list (equal? (opened after) (opened base)) (extent after))
        (list #t (list 4 (list (list 'crc 2 (bytevector-length (record W 4))))))))

;; A LISTED SEGMENT REPLACED AT THE SAME LENGTH: the manifest still names
;; the old bytes' hash, so the segment is excluded, and a digest answered
;; from what the memory remembers would wrongly agree with the manifest.
(let* ((dir (build! (test-dir "segmem-replace") #t))
       (memory (make-load-memory))
       (before (extent (discover-prefix dir M 'shared memory)))
       (path (segment-path dir M 1))
       (_ (flip-at! path (title-offset (call-with-port (open-file-input-port path) get-bytevector-all) M 2)))
       (after (discover-prefix dir M 'shared memory))
       (base (discover-prefix dir M 'shared)))
  (want "SM-3 CONTROL: as published, M reaches 4 with no note"
        before (list 4 '()))
  (want "SM-3 a listed segment replaced at its length between two discoveries handed one memory fails the manifest's hash as with no memory, the whole discovery equal"
        (list (equal? (opened after) (opened base)) (extent after))
        (list #t (list 0 (list (list 'manifest-hash 1 0))))))

;; BYTES THAT GREW: W's highest segment gains record 9 between two loads
;; handed one memory. The second decodes only the appended line, and
;; delivers what a load with no memory delivers.
(let* ((dir (build! (test-dir "segmem-grow") #f))
       (memory (make-load-memory))
       (first (load! dir memory))
       (_ (put! (segment-path dir W 3) (cat (records W 7 8) (record W 9))))
       (second #f)
       (text (traced (lambda () (set! second (load! dir memory))))))
  (want "SM-3 CONTROL: the first load delivered 8 records"
        first (all-of W 1 8))
  (want "SM-3 a highest segment grown between two loads handed one memory delivers what a load with no memory delivers"
        (list (equal? second (load! dir #f)) (length second))
        (list #t 9))
  (want "SM-3 and decodes only the appended line: the sealed segments are replayed, the grown one extended"
        (list (parses text) (memory-events text))
        (list 1 (list (list 'validation W 1 'replayed 3) (list 'validation W 2 'replayed 3)
                      (list 'validation W 3 'extended 3)
                      (list 'delivery W 1 'replayed 3) (list 'delivery W 2 'replayed 3)
                      (list 'delivery W 3 'replayed 3)))))

;; A LISTED SEGMENT REPUBLISHED between two loads handed one memory, with
;; new bytes (record 5 appended), a new digest and a new range in the
;; manifest: accepted as a load with no memory accepts it. The digest is
;; the new bytes', never the remembered one.
(define (publish! dir w seg)
  (write-manifest! dir w (list (list 1 (bytevector->hex (sha256 seg)) 1 (segment-records seg)))))
(define (segment-records bv)
  (let loop ((i 0) (n 0))
    (cond ((= i (bytevector-length bv)) n)
          ((= 10 (bytevector-u8-ref bv i)) (loop (+ i 1) (+ n 1)))
          (else (loop (+ i 1) n)))))
(let* ((dir (build! (test-dir "segmem-republish") #t))
       (memory (make-load-memory))
       (first (load! dir memory))
       (_ (let ((seg (records M 1 5))) (put! (segment-path dir M 1) seg) (publish! dir M seg)))
       (second #f)
       (text (traced (lambda () (set! second (load! dir memory))))))
  (want "SM-3 a listed segment republished with new bytes, digest and range between two loads handed one memory is accepted as with no memory"
        (list (equal? second (load! dir #f))
              (filter (lambda (e) (string=? (car e) M)) second)
              (filter (lambda (e) (string=? (cadr e) M)) (memory-events text)))
        (list #t (all-of M 1 5)
              (list (list 'validation M 1 'extended 5) (list 'delivery M 1 'replayed 5)))))

;; AN ENTRY WITH A DIGEST AND NO TRANSCRIPT: a manifest naming another hash
;; ends the segment before its scan, so the memory holds the bytes and
;; their digest only. The manifest corrected, the next load scans the
;; segment and delivers it.
(let* ((dir (build! (test-dir "segmem-corrected") #t))
       (memory (make-load-memory))
       (seg (segment-bytes dir M 1))
       (_ (write-manifest! dir M (list (list 1 (bytevector->hex (sha256 (records W 1 1))) 1 4))))
       (refused (extent (discover-prefix dir M 'shared memory)))
       (_ (publish! dir M seg))
       (got #f)
       (text (traced (lambda () (set! got (load! dir memory))))))
  (want "SM-3 an entry a hash test ended before its scan is scanned by the next load once the manifest is corrected, and delivered"
        (list refused
              (filter (lambda (e) (string=? (car e) M)) got)
              (filter (lambda (e) (string=? (cadr e) M)) (memory-events text)))
        (list (list 0 (list (list 'manifest-hash 1 0)))
              (all-of M 1 4)
              (list (list 'validation M 1 'scanned 4) (list 'delivery M 1 'replayed 4)))))

;; A HIGHEST SEGMENT SHORTENED between two loads handed one memory: the
;; shorter bytes are not the remembered ones and do not begin a longer
;; remembered copy, so the segment is scanned again.
(let* ((dir (build! (test-dir "segmem-shorten") #f))
       (memory (make-load-memory))
       (first (load! dir memory))
       (_ (put! (segment-path dir W 3) (records W 7 7)))
       (second #f)
       (text (traced (lambda () (set! second (load! dir memory))))))
  (want "SM-3 a highest segment shortened between two loads handed one memory delivers what a load with no memory delivers, and is scanned again"
        (list (equal? second (load! dir #f)) (length second)
              (filter (lambda (e) (eqv? (caddr e) 3)) (memory-events text)))
        (list #t 7 (list (list 'validation W 3 'scanned 1) (list 'delivery W 3 'replayed 1)))))

;; THE SAME REMEMBERED BYTES UNDER OTHER ARGUMENTS, at the level of a load:
;; a delivery from a cut (each segment's expected sequence and the records
;; it hands on are delivery's, not the first load's), and a retirement
;; marker added between two discoveries (a ceiling stops the replay where
;; it stops a scan).
(let* ((dir (build! (test-dir "segmem-cut") #f))
       (memory (make-load-memory))
       (_ (load! dir memory))
       (from-cut (lambda (memory)
                   (let ((ls (if memory (log-open-with-memory dir memory) (log-open dir))) (got '()))
                     (load-deliver! ls (list (cons W 4))
                                    (lambda (w seg off seq ts actor deps payload)
                                      (set! got (cons (cons w seq) got))))
                     (load-commit! ls)
                     (reverse got))))
       (got #f)
       (text (traced (lambda () (set! got (from-cut memory))))))
  (want "SM-3 a delivery from a cut, after a whole load with one memory, delivers what one with no memory delivers, from the cut's segment, replayed"
        (list (equal? got (from-cut #f)) got
              (filter (lambda (e) (eq? (car e) 'delivery)) (memory-events text)))
        (list #t (all-of W 5 8)
              (list (list 'delivery W 2 'replayed 3) (list 'delivery W 3 'replayed 2)))))
(let* ((dir (build! (test-dir "segmem-retire") #f))
       (memory (make-load-memory))
       (_ (discover-prefix dir W 'shared memory))
       (at (+ (bytevector-length (record W 4)) (bytevector-length (record W 5))))
       (_ (put! (string-append dir "/writers/" W "/retired.sexp")
                (string->utf8 (format "((prefix 2 ~a 5))\n" at))))
       (after (discover-prefix dir W 'shared memory))
       (base (discover-prefix dir W 'shared)))
  (want "SM-3 a retirement marker added between two discoveries handed one memory stops W where a discovery with no memory stops it, the whole discovery equal"
        (list (equal? (opened after) (opened base)) (discovery-end-seq after))
        (list #t 5)))

;; =============================================================================
(printf "== a load lets go of its memory when its delivery ends ==\n")
;; A SESSION THAT IS KEPT must not keep every segment's bytes and frames:
;; the load's own memory is dropped when its delivery ends, and a memory
;; a caller handed in stays with the caller, not emptied.
(define local-bytes
  (apply + (map (lambda (n) (bytevector-length (segment-bytes local W n))) '(1 2 3))))
(let* ((ls (log-open local))
       (during (load-remembered-bytes ls)))
  (load-deliver! ls '() (lambda args (if #f #f)))
  (let ((after (load-remembered-bytes ls)))
    (load-commit! ls)
    (want "a load remembers its segments' bytes until its delivery ends, and none after"
          (list during after)
          (list local-bytes 0))))
(let* ((memory (make-load-memory))
       (ls (log-open-with-memory local memory)))
  (load-deliver! ls '() (lambda args (if #f #f)))
  (load-commit! ls)
  (want "a memory handed to a load is let go by the load and kept, not emptied, by its caller"
        (list (load-remembered-bytes ls) (memory-remembered-bytes memory))
        (list 0 local-bytes)))

;; A LOAD THAT IS NEVER DELIVERED lets go when it ends: committed after
;; reading its prefixes, or aborted.
(let* ((committed (log-open local))
       (before (load-remembered-bytes committed)))
  (load-commit! committed)
  (let ((aborted (log-open local)))
    (load-abort! aborted 'not-delivered)
    (want "a load committed without a delivery, and one aborted, remember nothing once they end"
          (list before (load-remembered-bytes committed) (load-remembered-bytes aborted))
          (list local-bytes 0 0))))

;; A DELIVERY ABANDONED AS A KILL ABANDONS IT. igropyr discards a killed
;; process's winders without running them, so a load whose delivery is
;; killed stays delivering and its after-thunk never lets go. The seam does
;; the same in this process: during a session's redelivery (a reload, which
;; a mirror's new segment between two appends causes), the callback empties
;; the winder list and leaves by a continuation taken outside. The session's
;; end then lets the memory go, whatever the load's outcome.
(let* ((dir (build! (test-dir "segmem-killed") #t))
       (kill? #f)
       (escape #f)
       (s (log-begin dir (lambda (w seg off seq . rest)
                           (when (and kill? (string=? w M) (= seq 5))
                             (($primitive $current-winders) '())
                             (escape 'killed))
                           'applied)))
       (append-one! (lambda (payload)
                      (let ((v (session-view s)))
                        (session-append! s (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                                       (view-expect-seq v) "agent:claude" '() payload)))))
       (first (append-one! '(put "w.9" ())))
       ;; the reducer confirms the append, so the session offers a view again
       (_ (session-applied! s (session-epoch s) (list (cons W 9))))
       (seg2 (record M 5))
       ;; WRITTEN AS ANOTHER PROCESS'S PUBLICATION LEAVES IT: this process's
       ;; session is the store's active operation, so log-publish! here is refused.
       (_ (begin
            (put! (segment-path dir M 2) seg2)
            (put! (string-append dir "/writers/" M "/published.sexp")
                  (string->utf8 (format "~s\n" (list (list 1 (bytevector->hex (sha256 (records M 1 4))) 1 4)
                                                      (list 2 (bytevector->hex (sha256 seg2)) 5 5)))))))
       (left (call/cc (lambda (k) (set! escape k) (set! kill? #t) (append-one! '(put "w.10" ())))))
       (ls (session-load s))
       (during (list left (load-outcome ls) (> (load-remembered-bytes ls) 0))))
  (log-end! s)
  (want "a session whose load was left delivering, as a killed delivery leaves it, lets its memory go when the session ends"
        (list during (load-remembered-bytes ls))
        (list (list 'killed 'delivering #t) 0)))

;; =============================================================================
(printf "== SM-6: past its bound, a memory scans at every ask ==\n")
;; THE BOUND COUNTS REMEMBERED BYTES. With room for W's first two segments
;; only, the third is answered for each ask and not kept: validation and
;; delivery each scan it, and the load answers what a load with no memory
;; answers.
(let* ((limit (+ (bytevector-length (segment-bytes local W 1))
                 (bytevector-length (segment-bytes local W 2))))
       (memory (make-load-memory limit))
       (got #f)
       (text (traced (lambda () (set! got (load! local memory))))))
  (want "SM-6 a load past its memory's bound delivers what a load with no memory delivers"
        got (load! local #f))
  (want "SM-6 the segments within the bound are scanned once and replayed; the one past it is scanned at each ask"
        (list (parses text) (memory-events text))
        (list 10 (list (list 'validation W 1 'scanned 3) (list 'validation W 2 'scanned 3)
                       (list 'validation W 3 'scanned 2)
                       (list 'delivery W 1 'replayed 3) (list 'delivery W 2 'replayed 3)
                       (list 'delivery W 3 'scanned 2))))
  (want "SM-6 the memory holds no more than its bound: the two segments that fit"
        (memory-remembered-bytes memory) limit))

;; A REPLACED ENTRY GIVES ITS BYTES BACK. The bound is the store's bytes
;; once W's highest segment has grown by record 9; the grown segment fits
;; only if the bytes it replaces are counted out first.
(let* ((dir (build! (test-dir "segmem-bound-grow") #f))
       (before (apply + (map (lambda (n) (bytevector-length (segment-bytes dir W n))) '(1 2 3))))
       (memory (make-load-memory (+ before (bytevector-length (record W 9))))))
  (load! dir memory)
  (put! (segment-path dir W 3) (cat (records W 7 8) (record W 9)))
  (load! dir memory)
  (let ((text (traced (lambda () (load! dir memory)))))
    (want "SM-6 a segment that grew replaces its entry within the bound: the third load replays it"
          (list (memory-remembered-bytes memory)
                (filter (lambda (e) (eqv? (caddr e) 3)) (memory-events text)))
          (list (+ before (bytevector-length (record W 9)))
                (list (list 'validation W 3 'replayed 3) (list 'delivery W 3 'replayed 3))))))

;; =============================================================================
(printf "== what a load delivers is shared with its memory ==\n")
;; NEVER A CONTRACT. A replay hands on the memory's own objects, so a
;; consumer that changes one changes what every later replay of that entry
;; delivers. This row shows the hazard without the degeneracy switch; under
;; the switch the first load raises at the consumer's call (the second
;; fixed line), the second load is not made, and the row is red by design.
(define (actor-of-1 dir memory change?)
  (let ((ls (log-open-with-memory dir memory)) (got #f))
    (load-deliver! ls '() (lambda (w seg off seq ts actor deps payload)
                            (when (= seq 1)
                              (set! got (string-copy actor))
                              (when change? (string-set! actor 0 #\X)))))
    (load-commit! ls)
    got))
(let* ((dir (build! (test-dir "segmem-shared") #f))
       (memory (make-load-memory))
       (first (caught (actor-of-1 dir memory #t)))
       (second (if (string? first) (caught (actor-of-1 dir memory #f)) 'not-loaded)))
  (want "NEVER (the hazard, not a contract): a consumer that changes the actor it is handed changes what a later load with the same memory delivers"
        (list first second)
        (list "agent:claude" "Xgent:claude"))
  (want "under the degeneracy switch, a consumer that changes a value it was delivered is named as that, never as a replay that differs"
        (if (degeneracy-active) first 'switch-off)
        (if (degeneracy-active)
            (list 'RAISED "degeneracy: a consumer changed a value it was delivered")
            'switch-off)))

(printf "\n~a failures\nrows: ~a\nsegment-memory complete\n" bad rows-run)
(exit (if (= bad 0) 0 1))
