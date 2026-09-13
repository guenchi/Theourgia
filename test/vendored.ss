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

;; Z2: the copied platform layer, and whether it is still what was copied.
;;
;; A file that carries its own digest proves nothing: edit the body, edit
;; the header, and the file agrees with itself again. So the digest lives
;; in `vendored-sources.txt`, outside the copy, and this reads the bytes
;; back out of `ffi.ss` to compare. Editing the copy does not edit the
;; table; that asymmetry is the whole mechanism.
;;
;; THIS NEEDS NOTHING OUTSIDE THE REPOSITORY. Checking the copy against
;; the ORIGINAL needs igropyr reachable and lives in the comparison class,
;; where absence is reported as not-run rather than passed. The question
;; here is narrower and always answerable: has this copy been changed.
(import (chezscheme) (theourgia wire))

(define (test-dir name)
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/" name "-" (number->string (get-process-id)))))
    (system (string-append "rm -rf " path "; mkdir -p " path))
    path))

(define scratch (test-dir "vendored"))
(define bad 0)
(define rows-run 0)

(define (want-1 label got expect)
  (let ((ok (equal? got expect)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expect)))))

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

;; THE SOURCE IS FOUND IN BOTH LAYOUTS. Beside this file in a delivery,
;; one level up in the repository -- the same rule the other fixtures use.
(define (locate name)
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1))))))
         (dir (if cut (substring self 0 cut) "."))
         (beside (string-append dir "/" name))
         (above (string-append dir "/../" name)))
    (cond ((file-exists? beside) beside)
          ((file-exists? above) above)
          (else (assertion-violation 'vendored
                  "neither beside this fixture nor one level up" name)))))

(define (slurp path)
  (call-with-port (open-file-input-port path) get-bytevector-all))

(define (text-of path)
  (utf8->string (slurp path)))



;; THREE SEGMENTS, ASKED SEPARATELY. Checking one and reporting for all is
;; how a check goes absent without going quiet: it keeps passing, about a
;; file nobody is looking at. Each copy is read out of its own file, its
;; own digest compared, its own forms counted, its own names matched
;; against the ones its own header names.
(define (table-rows)
  (let* ((t (text-of (locate "vendored-sources.txt")))
         (n (string-length t)))
    (let loop ((i 0) (start 0) (out '()))
      (cond
        ((>= i n) (reverse out))
        ((char=? (string-ref t i) #\newline)
         (let ((line (substring t start i)))
           (loop (+ i 1) (+ i 1)
                 (if (or (= 0 (string-length line))
                         (char=? (string-ref line 0) #\#))
                     out (cons line out)))))
        (else (loop (+ i 1) start out))))))

(define (fields line)
  (let ((n (string-length line)))
    (let loop ((i 0) (start 0) (out '()))
      (cond
        ((>= i n) (reverse (if (> i start) (cons (substring line start i) out) out)))
        ((char=? (string-ref line i) #\space)
         (loop (+ i 1) (+ i 1)
               (if (> i start) (cons (substring line start i) out) out)))
        (else (loop (+ i 1) start out))))))

(define (table-row-for file)
  (let loop ((rs (table-rows)))
    (cond ((null? rs) #f)
          ((let ((f (fields (car rs))))
             (and (>= (length f) 3) (string=? (car f) "segment")
                  (string=? (cadr f) file) f)))
          (else (loop (cdr rs))))))

(define (md5-of-string str)
  (let ((f (string-append scratch "/seg.txt")))
    (call-with-port (open-file-output-port f (file-options no-fail))
      (lambda (o) (put-bytevector o (string->utf8 str))))
    (let ((out (string-append scratch "/seg.md5")))
      (system (string-append "md5 -q " f " > " out))
      (let* ((t (text-of out)) (n (string-length t)))
        (let trim ((k n))
          (if (and (> k 0) (memv (string-ref t (- k 1)) '(#\newline #\return)))
              (trim (- k 1))
              (substring t 0 k)))))))

(define copies '("ffi.ss" "wire.ss" "digest.ss"))

(define begin-marker ";; BEGIN COPIED FROM IGROPYR")
(define end-marker ";; END COPIED FROM IGROPYR")

(define (all-positions hay needle)
  (let ((hn (string-length hay)) (nn (string-length needle)))
    (let loop ((i 0) (out '()))
      (cond ((> (+ i nn) hn) (reverse out))
            ((string=? (substring hay i (+ i nn)) needle)
             (loop (+ i 1) (cons i out)))
            (else (loop (+ i 1) out))))))

(define (segment-of file)
  (let* ((t (text-of (locate file)))
         (bs (all-positions t begin-marker))
         (es (all-positions t end-marker)))
    (list bs es
          (if (and (= 1 (length bs)) (= 1 (length es)) (< (car bs) (car es)))
              (substring t (car bs) (+ (car es) (string-length end-marker)))
              ""))))

(define (segment-forms seg)
  (let ((p (open-string-input-port seg)))
    (let loop ((out '()))
      (let ((d (read p)))
        (if (eof-object? d) (reverse out) (loop (cons d out)))))))

(define (defined-names forms)
  (let loop ((fs forms) (out '()))
    (cond ((null? fs) (reverse out))
          ((and (pair? (car fs)) (eq? 'define (car (car fs))))
           (let ((t (cadr (car fs))))
             (loop (cdr fs) (cons (if (pair? t) (car t) t) out))))
          (else (loop (cdr fs) out)))))

;; THE NAMES THE HEADER CLAIMS, read out of the header itself. The header
;; is inside the segment, so it cannot be edited to agree with a changed
;; body without moving the digest checked above it.
(define (declared-names seg)
  (let* ((key "extracted   ")
         (i (let loop ((k 0))
              (cond ((> (+ k (string-length key)) (string-length seg)) #f)
                    ((string=? (substring seg k (+ k (string-length key))) key) k)
                    (else (loop (+ k 1)))))))
    (and i
         (let* ((rest (substring seg (+ i (string-length key)) (string-length seg)))
                (stop (let loop ((k 0))
                        (cond ((>= (+ k 4) (string-length rest)) (string-length rest))
                              ((and (char=? (string-ref rest k) #\newline)
                                    (char=? (string-ref rest (+ k 1)) #\space)
                                    (char=? (string-ref rest (+ k 2)) #\space)
                                    (char=? (string-ref rest (+ k 3)) #\;)
                                    (char=? (string-ref rest (+ k 4)) #\;)
                                    (or (>= (+ k 6) (string-length rest))
                                        (char=? (string-ref rest (+ k 5)) #\newline)))
                               k)
                              (else (loop (+ k 1))))))
                (head (substring rest 0 stop)))
           (let loop ((k 0) (start 0) (out '()))
             (cond
               ((>= k (string-length head))
                (reverse (if (> k start) (cons (substring head start k) out) out)))
               ((memv (string-ref head k) '(#\space #\newline #\, #\;))
                (loop (+ k 1) (+ k 1)
                      (if (> k start) (cons (substring head start k) out) out)))
               (else (loop (+ k 1) start out))))))))

(for-each
  (lambda (file)
    (let* ((sg (segment-of file))
           (bs (car sg)) (es (cadr sg)) (seg (caddr sg)))
      (printf "\n-- ~a\n" file)
      (want (string-append file ": the begin marker appears exactly once")
            (length bs) 1)
      (want (string-append file ": the end marker appears exactly once")
            (length es) 1)
      (want (string-append file ": the copy hashes to what the table recorded")
            (let ((row (table-row-for file)))
              (and row (string=? (md5-of-string seg) (caddr row))))
            #t)
      (want (string-append file ": the names defined are the names the header lists")
            (let* ((declared (declared-names seg))
                   (defined (map symbol->string (defined-names (segment-forms seg)))))
              (and declared
                   (list (length declared)
                         (and (= (length declared) (length defined))
                              (for-all (lambda (n) (member n defined)) declared)
                              #t))))
            (let ((declared (declared-names seg)))
              (list (and declared (length declared)) #t)))))
  copies)

(printf "\n== the loader is given this library's candidates, not upstream's ==\n")
;; UPSTREAM'S `shared-object-candidates` WAS NOT COPIED, and the reason is
;; that it names OpenSSL. What this library loads is libc, and it passes
;; its own list at the call site. That distinction lives in a comment
;; beside the copy, and a comment does not stop the next person restoring
;; the function that looks missing -- so it is asserted here.
(define ffi-text (text-of (locate "ffi.ss")))
(define ffi-ends (cadr (segment-of "ffi.ss")))

(define libc-candidates
  (let* ((t ffi-text)
         (k (let loop ((ps (all-positions t "(load-first-shared-object!")))
              (cond ((null? ps) #f)
                    ((> (car ps) (car ffi-ends)) (car ps))
                    (else (loop (cdr ps)))))))
    (and k
         (let* ((rest (substring t k (min (string-length t) (+ k 220)))))
           rest))))

(want "the call site passes a libc list"
      (and libc-candidates
           (let ((has (lambda (s)
                        (let loop ((i 0))
                          (cond ((> (+ i (string-length s)) (string-length libc-candidates)) #f)
                                ((string=? (substring libc-candidates i (+ i (string-length s))) s) #t)
                                (else (loop (+ i 1))))))))
             (and (has "libc.dylib") (has "libc.so") #t)))
      #t)
;; AND NOT OPENSSL'S. This is the row that goes red if `shared-object-candidates`
;; is restored and wired in: the names would change from libc to crypto.
(want "and names no OpenSSL library"
      (and libc-candidates
           (let ((has (lambda (s)
                        (let loop ((i 0))
                          (cond ((> (+ i (string-length s)) (string-length libc-candidates)) #f)
                                ((string=? (substring libc-candidates i (+ i (string-length s))) s) #t)
                                (else (loop (+ i 1))))))))
             (and (not (has "libcrypto")) (not (has "openssl")) #t)))
      #t)

(printf "\n== base64 is an algorithm, so it is checked as one ==\n")
;; THE COPIED DIGEST CELLS CANNOT DO THIS. A fixture that hashes with the
;; same implementation the product hashes with agrees with it whatever
;; either of them computes -- that pair checks plumbing, not arithmetic.
;; base64 came in as part of the codec, and the codec's round trips only
;; ever exercise the lengths its own values happen to have.
;;
;; THE THREE REMAINDERS ARE THE WHOLE OF THE ENCODING. Every input is
;; 0, 1 or 2 bytes past a multiple of three, and each takes a different
;; branch with a different amount of padding. A suite that never sends
;; one of the three has not tested base64; it has tested two thirds of it.
(define (b64-round-trip bv)
  (let ((s (sexpr->string-extended bv)))
    (equal? bv (string->sexpr-extended s))))

(want "remainder 0: three bytes, no padding"
      (b64-round-trip (u8-list->bytevector '(1 2 3)))
      #t)
(want "remainder 1: one byte over, two pad characters"
      (b64-round-trip (u8-list->bytevector '(1 2 3 4)))
      #t)
(want "remainder 2: two bytes over, one pad character"
      (b64-round-trip (u8-list->bytevector '(1 2 3 4 5)))
      #t)
(want "the empty bytevector survives too"
      (b64-round-trip (u8-list->bytevector '()))
      #t)
;; THE WHOLE BYTE RANGE, not the printable part of it. The alphabet maps
;; six bits at a time and the failure that matters is a byte that lands
;; on the wrong character, which a sample of small values would miss.
(want "every byte value 0..255 makes the round trip"
      (b64-round-trip (u8-list->bytevector
                        (let loop ((i 0) (out '()))
                          (if (= i 256) (reverse out) (loop (+ i 1) (cons i out))))))
      #t)

;; AND WHAT IT REFUSES. A decoder that accepts anything is not a decoder:
;; these are the shapes a corrupted record actually arrives in.
(define (decode-refuses? text)
  (guard (e (#t #t))
    (begin (string->sexpr-extended text) #f)))

(want "a character outside the alphabet is refused"
      (decode-refuses? "#vu8\"AB*D\"")
      #t)
(want "a length that is not a multiple of four is refused"
      (decode-refuses? "#vu8\"ABCDE\"")
      #t)
(want "padding in the middle is refused"
      (decode-refuses? "#vu8\"AB=D\"")
      #t)
;; TWIN: A WELL-FORMED PAYLOAD IS STILL ACCEPTED. Without this the three
;; rows above are also passed by a decoder that refuses everything.
(want "TWIN: a well-formed payload is accepted"
      (not (decode-refuses? "#vu8\"AAEC\""))
      #t)

(printf "\n~a failures\n" bad)
(printf "rows: ~a\n" rows-run)
(printf "vendored complete\n")
