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
(import (chezscheme) (theourgia crc32))

;; Cases whose expected values come from python3 zlib.crc32, printed by
;; the companion shell script -- not from this implementation.
(define (show label bv)
  (printf "~a ~a ~a\n" label (crc32-hex bv) (crc32-bytes bv)))

(show "empty" (string->utf8 ""))
(show "a" (string->utf8 "a"))
(show "fox" (string->utf8 "The quick brown fox jumps over the lazy dog"))
(show "utf8" (string->utf8 "a2\x00b7;\x65e5;\x672c;\x8a9e;\x1f600;"))

;; 5000 bytes, deterministic LCG so the shell side can rebuild the exact
;; same bytes without shipping them through the terminal.
(define big
  (let ((bv (make-bytevector 5000)))
    (let loop ((i 0) (x 12345))
      (if (= i 5000)
          bv
          (let ((x (modulo (+ (* x 1103515245) 12345) 2147483648)))
            (bytevector-u8-set! bv i (modulo (quotient x 65536) 256))
            (loop (+ i 1) x))))))
(show "big5000" big)
(printf "big5000-md5-check-first16 ~a\n"
        (let loop ((i 0) (acc '()))
          (if (= i 16) (reverse acc)
              (loop (+ i 1) (cons (bytevector-u8-ref big i) acc)))))

;; The string entry point must agree with encoding then hashing.
(printf "string-agrees ~a\n"
        (equal? (crc32-string "a2\x00b7;\x65e5;\x672c;\x8a9e;\x1f600;")
                (crc32-bytes (string->utf8 "a2\x00b7;\x65e5;\x672c;\x8a9e;\x1f600;"))))
(printf "hex-agrees ~a\n"
        (equal? (crc32-string-hex "hello") (crc32-hex (string->utf8 "hello"))))

;; Completion sentinel: run-all.sh treats a suite that ends without this line as a crash, not a pass.
(printf "smoke-crc32 complete\n")
