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

(import (chezscheme) (theourgia wire) (theourgia crc32))
(define (bv-append . bs)
  (let* ((n (apply + (map bytevector-length bs))) (out (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) out
          (begin (bytevector-copy! (car bs) 0 out i (bytevector-length (car bs)))
                 (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define (line text-bytes)
  (let* ((hex (crc32-hex text-bytes))
         (out (make-bytevector (+ 9 (bytevector-length text-bytes) 1))))
    (bytevector-copy! (string->utf8 hex) 0 out 0 8)
    (bytevector-u8-set! out 8 32)
    (bytevector-copy! text-bytes 0 out 9 (bytevector-length text-bytes))
    (bytevector-u8-set! out (+ 9 (bytevector-length text-bytes)) 10)
    out))
(define (bytes . l) (u8-list->bytevector l))
(printf "1. invalid UTF-8 in a string, CRC correct:\n   ~s\n"
        (decode-line (line (bv-append (string->utf8 "(0 0 \"") (bytes 255) (string->utf8 "\" () ())")))))
(printf "2. does utf8->string round trip exactly?\n")
(for-each (lambda (bv)
            (let ((s (utf8->string bv)))
              (printf "   ~s -> ~s -> ~s   exact=~a\n" bv s (string->utf8 s)
                      (equal? bv (string->utf8 s)))))
          (list (bytes 255) (bytes 239 187 191) (string->utf8 "ok") (bytes 192 175)))
(printf "3. leading BOM before the datum, CRC over the BOM too:\n   ~s\n"
        (decode-line (line (bv-append (bytes 239 187 191) (string->utf8 "(0 0 \"a\" () ())")))))
(printf "4. an LF in the middle of the buffer:\n   ~s\n"
        (decode-line (line (bytes 40 48 10 48 32 34 97 34 32 40 41 32 40 41 41))))
;; The payload is a pair because the writer refuses any other; the gensym
;; is its second element, and that element is what is read back.
(printf "5. gensym payload without storable-encode:\n   ~s\n"
        (let ((g (gensym "a")))
          (list 'wrote g 'read
                (cadr (list-ref (decode-line (encode-record 0 0 "a" '() (list 'x g))) 5)))))

;; A run that did not reach here is not a pass. The runner requires
;; this line AND a zero failure count: they are two propositions.
(printf "verify-l4 complete\n")
