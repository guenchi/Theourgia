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

;; THE REFERENCE ARM OF test/load-budget.sc: the work of decoding a log's
;; lines, written here and importing nothing but (chezscheme). For each
;; line: the eight-digit checksum field read and compared with a CRC-32 of
;; the text computed from this file's own table, the text decoded to a
;; string, and the datum read by this file's own reader -- lists, dotted
;; pairs, strings with their escapes, symbols, numbers, booleans, and the
;; `#vu8"<base64>"` form the log writes a bytevector as, decoded. That is
;; the kind of work a load repeats per line, and none of it is the
;; product's, so a change in the product's decoder does not move this arm.
;;
;; A `.ss` file: the fixture runner takes *.sc and *.py, so this is never
;; run as a fixture. load-budget.sc loads it and reads its procedure from
;; the library's own environment; its import form, its size and its md5
;; are pinned there.
;;
;;   (reference-decode-all <list of segment bytevectors>) -> how many lines
;;   it read whole: the checksum agreed and the datum is a five-element list

(library (load-budget-reference)
  (export reference-decode-all)
  (import (chezscheme))

  (define crc-table
    (let ((t (make-vector 256)))
      (do ((n 0 (+ n 1))) ((= n 256) t)
        (vector-set! t n (let loop ((c n) (k 0))
                           (if (= k 8)
                               c
                               (loop (if (fxodd? c) (fxxor #xEDB88320 (fxsrl c 1)) (fxsrl c 1))
                                     (+ k 1))))))))

  (define (crc bv start end)
    (let loop ((i start) (c #xFFFFFFFF))
      (if (= i end)
          (fxxor c #xFFFFFFFF)
          (loop (+ i 1)
                (fxxor (vector-ref crc-table (fxand (fxxor c (bytevector-u8-ref bv i)) 255))
                       (fxsrl c 8))))))

  (define (hex-field bv)
    (let loop ((i 0) (v 0))
      (if (= i 8)
          v
          (let* ((b (bytevector-u8-ref bv i))
                 (d (cond ((<= 48 b 57) (- b 48)) ((<= 97 b 102) (- b 87)) (else 0))))
            (loop (+ i 1) (+ (* v 16) d))))))

  (define (base64-value c)
    (cond ((char<=? #\A c #\Z) (- (char->integer c) 65))
          ((char<=? #\a c #\z) (+ 26 (- (char->integer c) 97)))
          ((char<=? #\0 c #\9) (+ 52 (- (char->integer c) 48)))
          ((char=? c #\+) 62)
          ((char=? c #\/) 63)
          (else #f)))

  (define (base64-decode s)
    (let-values (((out done) (open-bytevector-output-port)))
      (let loop ((i 0) (acc 0) (bits 0))
        (if (= i (string-length s))
            (done)
            (let ((v (base64-value (string-ref s i))))
              (if (not v)
                  (loop (+ i 1) acc bits)
                  (let ((acc (fxior (fxsll (fxand acc #xFFFF) 6) v)) (bits (+ bits 6)))
                    (if (>= bits 8)
                        (begin (put-u8 out (fxand (fxsrl acc (- bits 8)) 255))
                               (loop (+ i 1) acc (- bits 8)))
                        (loop (+ i 1) acc bits)))))))))

  (define (delimiter? c)
    (or (char-whitespace? c) (memv c '(#\( #\) #\"))))

  (define (skip s i)
    (if (and (< i (string-length s)) (char-whitespace? (string-ref s i))) (skip s (+ i 1)) i))

  (define (read-string-body s i)
    (let loop ((i i) (acc '()))
      (let ((c (string-ref s i)))
        (cond ((char=? c #\") (values (list->string (reverse acc)) (+ i 1)))
              ((char=? c #\\)
               (let ((e (string-ref s (+ i 1))))
                 (case e
                   ((#\n) (loop (+ i 2) (cons #\newline acc)))
                   ((#\t) (loop (+ i 2) (cons #\tab acc)))
                   ((#\x)
                    (let find ((j (+ i 2)))
                      (if (char=? (string-ref s j) #\;)
                          (loop (+ j 1) (cons (integer->char (string->number (substring s (+ i 2) j) 16)) acc))
                          (find (+ j 1)))))
                   (else (loop (+ i 2) (cons e acc))))))
              (else (loop (+ i 1) (cons c acc)))))))

  (define (read-atom s i)
    (let loop ((j i))
      (if (or (= j (string-length s)) (delimiter? (string-ref s j)))
          (let ((text (substring s i j)))
            (values (or (string->number text) (string->symbol text)) j))
          (loop (+ j 1)))))

  ;; -> (values datum next-index)
  (define (read-datum s i)
    (let* ((i (skip s i)) (c (string-ref s i)))
      (cond
        ((char=? c #\() (read-list s (+ i 1)))
        ((char=? c #\") (read-string-body s (+ i 1)))
        ((and (char=? c #\#) (< (+ i 4) (string-length s)) (string=? (substring s i (+ i 4)) "#vu8"))
         (let-values (((text j) (read-string-body s (+ i 5))))
           (values (base64-decode text) j)))
        ((and (char=? c #\#) (< (+ i 1) (string-length s)) (memv (string-ref s (+ i 1)) '(#\t #\f)))
         (values (char=? (string-ref s (+ i 1)) #\t) (+ i 2)))
        (else (read-atom s i)))))

  (define (read-list s i)
    (let loop ((i i) (acc '()))
      (let* ((i (skip s i)) (c (string-ref s i)))
        (cond
          ((char=? c #\)) (values (reverse acc) (+ i 1)))
          ((and (char=? c #\.) (delimiter? (string-ref s (+ i 1))))
           (let-values (((tail j) (read-datum s (+ i 1))))
             (let ((j (skip s j)))
               (values (append (reverse acc) tail) (+ j 1)))))
          (else
           (let-values (((d j) (read-datum s i)))
             (loop j (cons d acc))))))))

  ;; -> the line's datum, or #f when its checksum does not agree
  (define (decode-one line)
    (let* ((n (bytevector-length line)) (end (- n 1)))
      (and (= (crc line 9 end) (hex-field line))
        (let ((text (utf8->string (let ((b (make-bytevector (- end 9))))
                                    (bytevector-copy! line 9 b 0 (- end 9))
                                    b))))
          (let-values (((d j) (read-datum text 0))) d)))))

  (define (reference-decode-all segments)
    (let each ((bvs segments) (count 0))
      (if (null? bvs)
          count
          (let* ((bv (car bvs)) (n (bytevector-length bv)))
            (let line ((start 0) (count count))
              (if (>= start n)
                  (each (cdr bvs) count)
                  (let find ((i start))
                    (cond ((= i n) (each (cdr bvs) count))
                          ((= 10 (bytevector-u8-ref bv i))
                           (let ((one (make-bytevector (+ 1 (- i start)))))
                             (bytevector-copy! bv start one 0 (+ 1 (- i start)))
                             (let ((d (decode-one one)))
                               (line (+ i 1) (if (and (list? d) (= 5 (length d))) (+ count 1) count)))))
                          (else (find (+ i 1))))))))))))
