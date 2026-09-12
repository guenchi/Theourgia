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

;; The comment in crc32.sc claims the halved, generic-arithmetic loop is
;; slower than a fixnum-specific one on Chez. That is a measurement, so
;; measure it: same algorithm, same table content, one loop written the
;; way the constraint forbids.
(import (chezscheme) (theourgia crc32))

(define fx-table
  (let ((t (make-vector 256 0)))
    (do ((n 0 (+ n 1))) ((= n 256) t)
      (let loop ((k 0) (c n))
        (if (= k 8)
            (vector-set! t n c)
            (loop (+ k 1)
                  (if (odd? c) (fxxor #xEDB88320 (fxsrl c 1)) (fxsrl c 1))))))))

(define (crc32-fx bv)
  (let ((n (bytevector-length bv)))
    (let loop ((i 0) (c #xFFFFFFFF))
      (if (fx= i n)
          (fxxor c #xFFFFFFFF)
          (loop (fx+ i 1)
                (fxxor (fxsrl c 8)
                       (vector-ref fx-table
                                   (fxand (fxxor c (bytevector-u8-ref bv i)) #xFF))))))))

(define big (make-bytevector 4000000))
(let loop ((i 0) (x 1)) 
  (unless (= i 4000000)
    (let ((x (modulo (+ (* x 1103515245) 12345) 2147483648)))
      (bytevector-u8-set! big i (modulo (quotient x 65536) 256))
      (loop (+ i 1) x))))

(define (ms thunk)
  (let ((t0 (current-time 'time-monotonic)))
    (let ((v (thunk)))
      (let ((t1 (current-time 'time-monotonic)))
        (values (+ (* 1000.0 (- (time-second t1) (time-second t0)))
                   (/ (- (time-nanosecond t1) (time-nanosecond t0)) 1000000.0))
                v)))))

;; Same answer first, or the timing compares two different computations.
(printf "same answer: ~a\n" (= (crc32-fx big) (crc32-bytes big)))
(let-values (((a _) (ms (lambda () (crc32-fx big)))))
  (let-values (((b _) (ms (lambda () (crc32-bytes big)))))
    (let-values (((a2 _) (ms (lambda () (crc32-fx big)))))
      (let-values (((b2 _) (ms (lambda () (crc32-bytes big)))))
        (printf "4 MB, fixnum-specific: ~a ms then ~a ms\n" a a2)
        (printf "4 MB, halved generic:  ~a ms then ~a ms\n" b b2)
        (printf "ratio (second runs):   ~a x\n" (/ b2 a2))))))

;; A run that did not reach here is not a pass. The runner requires
;; this line AND a zero failure count: they are two propositions.
(printf "crc-cost complete\n")
