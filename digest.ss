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

;;; (theourgia digest) -- SHA-256 (FIPS 180-4), and bytes as lower-case hex.
;;;
;;; One algorithm, one file, which is how (theourgia crc32) is arranged and
;;; the reason this is a library rather than a section of another one. What
;;; a manifest entry and a block hash are made of belongs beside the other
;;; digest, not beside the wire format: putting it in `wire.ss` would have
;;; cost nothing today and left a name that disagrees with its contents for
;;; as long as the file exists.
;;;
;;; BASE64 IS NOT HERE. It lives in `wire.ss`, where it arrived as part of
;;; the s-expression codec that cannot be read without it. `b64-chars` and
;;; `b64-value` therefore have exactly one definition each in this tree,
;;; and a cell says so -- copying them again here would have been a second
;;; copy made by the very work that exists to remove copies.

(library (theourgia digest)
  (export sha256 bytevector->hex)
  (import (chezscheme))

  ;; BEGIN COPIED FROM IGROPYR -- SHA-256 and hex
  ;;
  ;;   repository  https://github.com/guenchi/Igropyr
  ;;   commit      7196bfe
  ;;   files       igropyr/crypto.sc
  ;;   licence     Apache-2.0, same author as this file
  ;;   extracted   mask32, add32, rotr32, shr32, sha256-k, sha256,
  ;;               hex-digits, bytevector->hex
  ;;
  ;; No digest is claimed here; the digests are in the table under test/,
  ;; outside this file, so that editing the copy does not edit the claim.
  ;;
  ;; THE CLOSURE WAS TAKEN BY WHAT IS FREE, NOT BY WHAT IS DEFINED. A walk
  ;; that matched identifiers against the names defined anywhere in the
  ;; source also wanted `out` and `b64-chars`: `out` occurs six times in
  ;; sha256 as a local binding and is separately the name of a helper
  ;; inside base64-encode. Copying on that basis would have produced a
  ;; second base64 alphabet in the same work that exists to remove copies.

  (define mask32 #xFFFFFFFF)

  (define (add32 . xs)
    (fxand (apply fx+ xs) mask32))

  (define (rotr32 x n)
    (fxior (fxsrl x n)
           (fxsll (fxand x (fx- (fxsll 1 n) 1)) (fx- 32 n))))

  (define (shr32 x n) (fxsrl x n))

  (define sha256-k
    '#(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1
       #x923f82a4 #xab1c5ed5 #xd807aa98 #x12835b01 #x243185be #x550c7dc3
       #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174 #xe49b69c1 #xefbe4786
       #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
       #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147
       #x06ca6351 #x14292967 #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
       #x650a7354 #x766a0abb #x81c2c92e #x92722c85 #xa2bfe8a1 #xa81a664b
       #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
       #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a
       #x5b9cca4f #x682e6ff3 #x748f82ee #x78a5636f #x84c87814 #x8cc70208
       #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

  (define (sha256 msg)
    (let* ((len (bytevector-length msg))
           (padlen (let ((r (mod (+ len 1) 64)))
                     (if (<= r 56) (- 56 r) (- 120 r))))
           (total (+ len 1 padlen 8))
           (buf (make-bytevector total 0))
           (w (make-vector 64 0)))
      (bytevector-copy! msg 0 buf 0 len)
      (bytevector-u8-set! buf len #x80)
      (do ((i 0 (+ i 1))) ((= i 8))
        (bytevector-u8-set! buf (- total 1 i)
          (fxand (bitwise-arithmetic-shift-right (* len 8) (* 8 i)) #xFF)))
      (let blocks ((blk 0)
                   (h0 #x6a09e667) (h1 #xbb67ae85) (h2 #x3c6ef372)
                   (h3 #xa54ff53a) (h4 #x510e527f) (h5 #x9b05688c)
                   (h6 #x1f83d9ab) (h7 #x5be0cd19))
        (if (= blk (div total 64))
            (let ((out (make-bytevector 32)))
              (let put ((i 0) (hs (list h0 h1 h2 h3 h4 h5 h6 h7)))
                (unless (null? hs)
                  (let ((h (car hs)))
                    (bytevector-u8-set! out i (fxsrl h 24))
                    (bytevector-u8-set! out (+ i 1) (fxand (fxsrl h 16) #xFF))
                    (bytevector-u8-set! out (+ i 2) (fxand (fxsrl h 8) #xFF))
                    (bytevector-u8-set! out (+ i 3) (fxand h #xFF)))
                  (put (+ i 4) (cdr hs))))
              out)
            (begin
              (do ((t 0 (+ t 1))) ((= t 16))
                (let ((b (+ (* blk 64) (* t 4))))
                  (vector-set! w t
                    (fxior (fxsll (bytevector-u8-ref buf b) 24)
                           (fxsll (bytevector-u8-ref buf (+ b 1)) 16)
                           (fxsll (bytevector-u8-ref buf (+ b 2)) 8)
                           (bytevector-u8-ref buf (+ b 3))))))
              (do ((t 16 (+ t 1))) ((= t 64))
                (let ((s0 (let ((x (vector-ref w (- t 15))))
                            (fxxor (rotr32 x 7) (rotr32 x 18) (shr32 x 3))))
                      (s1 (let ((x (vector-ref w (- t 2))))
                            (fxxor (rotr32 x 17) (rotr32 x 19) (shr32 x 10)))))
                  (vector-set! w t
                    (add32 (vector-ref w (- t 16)) s0 (vector-ref w (- t 7)) s1))))
              (let rounds ((t 0) (a h0) (b h1) (c h2) (d h3)
                           (e h4) (f h5) (g h6) (h h7))
                (if (= t 64)
                    (blocks (+ blk 1)
                            (add32 h0 a) (add32 h1 b) (add32 h2 c) (add32 h3 d)
                            (add32 h4 e) (add32 h5 f) (add32 h6 g) (add32 h7 h))
                    (let* ((S1 (fxxor (rotr32 e 6) (rotr32 e 11) (rotr32 e 25)))
                           (ch (fxxor (fxand e f) (fxand (fxxor e mask32) g)))
                           (t1 (add32 h S1 ch (vector-ref sha256-k t) (vector-ref w t)))
                           (S0 (fxxor (rotr32 a 2) (rotr32 a 13) (rotr32 a 22)))
                           (mj (fxxor (fxand a b) (fxand a c) (fxand b c)))
                           (t2 (add32 S0 mj)))
                      (rounds (+ t 1)
                              (add32 t1 t2) a b c
                              (add32 d t1) e f g)))))))))

  (define hex-digits "0123456789abcdef")

  (define (bytevector->hex bv)
    (let* ((n (bytevector-length bv)) (s (make-string (* n 2))))
      (do ((i 0 (fx+ i 1))) ((fx= i n) s)
        (let ((b (bytevector-u8-ref bv i)))
          (string-set! s (fx* i 2) (string-ref hex-digits (fxsrl b 4)))
          (string-set! s (fx+ (fx* i 2) 1) (string-ref hex-digits (fxand b 15)))))))
  ;; END COPIED FROM IGROPYR
)
