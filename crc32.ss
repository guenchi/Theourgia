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

;;; (theourgia crc32) -- CRC-32 (IEEE 802.3, the one zlib computes).
;;;
;;; Reflected algorithm, polynomial 0xEDB88320, initial value
;;; 0xFFFFFFFF, final xor 0xFFFFFFFF, table driven over 256 entries.
;;; Byte for byte the same answer as zlib's crc32 and Python's
;;; zlib.crc32.
;;;
;;; THE STATE IS CARRIED AS TWO 16-BIT HALVES, AND THAT IS A PORTING
;;; CONSTRAINT RATHER THAN A STYLE. This source is meant to compile,
;;; unchanged, through goeteia to wasm, where a fixnum is an i31: a
;;; value at or above 2^30 is a bignum there, and a bitwise operation
;;; on a bignum is an illegal cast, not a slow path. A 32-bit CRC held
;;; in one integer reaches that range -- not on every byte, but on
;;; enough of them that no run of any length is safe -- and every one of
;;; those intermediates is xored. So the state here is a pair (hi, lo),
;;; each in [0, 65536), and the lookup table is two vectors of 16-bit
;;; halves rather than one vector of 32-bit words.
;;;
;;; THE RULE THE CODE KEEPS: no operand of a bitwise operation is ever
;;; 2^30 or larger. Everything xored below is a half word (< 65536) or a
;;; byte (< 256); shifts and masks are written as quotient and remainder
;;; on non-negative integers, which are exactly equivalent for these
;;; values and need no bitwise primitive at all. bitwise-xor is the only
;;; one used, so a port needs exactly one binding rather than a set --
;;; and it is spelled with the generic name because that is the one
;;; R6RS puts in (rnrs arithmetic bitwise). Having it is NOT implied by
;;; having fixnum bitwise operations: fxxor lives in a different R6RS
;;; library, and a host that offers only that one needs this name bound
;;; to it. One binding, named here so the port knows where to look.
;;;
;;; THE RESULT IS A 32-BIT INTEGER and may therefore itself exceed 2^30
;;; on such a host -- but it is produced by multiply and add, not by a
;;; bitwise operation, so it is a bignum that nothing here then operates
;;; on bitwise. crc32-hex never forms it at all: it renders the two
;;; halves directly, and that is the path the log format uses.
;;;
;;; SAY WHAT IS GUARANTEED AND NOT MORE. The guarantee is about bitwise
;;; operands, not about every value in the file: a bytevector of length
;;; 2^30 or more would make its own length and the loop index bignums on
;;; such a host. Those are compared and incremented, never masked or
;;; xored, so the guarantee holds -- but "no bignum anywhere on the hex
;;; path" would be a wider claim than the code makes good on.
;;;
;;; COST, MEASURED RATHER THAN ASSERTED. Against the same algorithm
;;; written the way the constraint forbids -- one 32-bit state, fxxor,
;;; fxsrl, fxand, one table of full words -- and checked to produce the
;;; same checksum first: 4 MB in 33.5 ms here against 8.8 ms, so 3.8x,
;;; on Chez 10.1.0 / macOS 15.3 arm64. That is the price of the
;;; constraint above and it is paid deliberately, because a record is a
;;; line and not a megabyte. A caller who ever needs a whole segment
;;; hashed on a hot path should re-measure on that path rather than
;;; carry this ratio over to it.
;;;
;;;   (crc32-bytes bv)         -> exact integer in [0, 2^32)
;;;   (crc32-hex bv)           -> 8 lowercase hex characters
;;;   (crc32-string s)         -> crc32-bytes of the UTF-8 encoding of s
;;;   (crc32-string-hex s)     -> crc32-hex of the same

(library (theourgia crc32)
  (export crc32-bytes crc32-hex crc32-string crc32-string-hex)
  (import (chezscheme))

  ;; 0xEDB88320 split at the halfway point.
  (define poly-hi #xEDB8)
  (define poly-lo #x8320)

  (define half 65536)

  ;; One right shift of the (hi, lo) pair by one bit. The bit leaving hi
  ;; enters lo at position 15, which is what the multiply by 32768 does.
  (define (shift-right-1 hi lo)
    (values (quotient hi 2)
            (+ (quotient lo 2) (* (remainder hi 2) 32768))))

  ;; One right shift of the pair by eight bits, used once per input byte.
  (define (shift-right-8 hi lo)
    (values (quotient hi 256)
            (+ (quotient lo 256) (* (remainder hi 256) 256))))

  ;; The table is built once, at library initialisation, from the
  ;; polynomial -- there is no transcribed constant table in this file to
  ;; go wrong, and no second place the polynomial appears.
  (define-values (table-hi table-lo)
    (let ((th (make-vector 256 0))
          (tl (make-vector 256 0)))
      (do ((n 0 (+ n 1))) ((= n 256) (values th tl))
        (let loop ((k 0) (hi 0) (lo n))
          (if (= k 8)
              (begin (vector-set! th n hi)
                     (vector-set! tl n lo))
              (let ((low-bit (remainder lo 2)))
                (let-values (((shi slo) (shift-right-1 hi lo)))
                  (if (= low-bit 1)
                      (loop (+ k 1)
                            (bitwise-xor shi poly-hi)
                            (bitwise-xor slo poly-lo))
                      (loop (+ k 1) shi slo)))))))))

  ;; The whole computation, returning the two halves of the finished
  ;; CRC. Both public entry points go through here, so there is one
  ;; implementation of the algorithm and two renderings of its answer.
  (define (crc32-halves bv start end)
    (let loop ((i start) (hi 65535) (lo 65535))
      (if (>= i end)
          (values (bitwise-xor hi 65535) (bitwise-xor lo 65535))
          (let ((idx (bitwise-xor (remainder lo 256) (bytevector-u8-ref bv i))))
            (let-values (((shi slo) (shift-right-8 hi lo)))
              (loop (+ i 1)
                    (bitwise-xor shi (vector-ref table-hi idx))
                    (bitwise-xor slo (vector-ref table-lo idx))))))))

  ;; The range arguments are optional and default to the whole
  ;; bytevector: a caller checksumming a slice should not have to copy
  ;; it out first, and a caller checksumming all of it should not have
  ;; to say so.
  (define (range-of bv opts)
    (let ((n (bytevector-length bv)))
      ;; A THIRD ARGUMENT IS A MISTAKE, NOT A COURTESY. Silently ignoring
      ;; it would accept a caller who thinks they asked for something and
      ;; give them the answer to a different question.
      (when (and (pair? opts) (pair? (cdr opts)) (pair? (cddr opts)))
        (assertion-violation 'crc32 "expected at most a start and an end" opts))
      (let ((start (if (pair? opts) (car opts) 0)))
        (let ((end (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) n)))
          (unless (and (exact? start) (integer? start) (<= 0 start n))
            (assertion-violation 'crc32 "start out of range" start n))
          (unless (and (exact? end) (integer? end) (<= start end n))
            (assertion-violation 'crc32 "end out of range" end start n))
          (values start end)))))

  (define (crc32-bytes bv . opts)
    (unless (bytevector? bv)
      (assertion-violation 'crc32-bytes "not a bytevector" bv))
    (let-values (((start end) (range-of bv opts)))
      (let-values (((hi lo) (crc32-halves bv start end)))
        (+ (* hi half) lo))))

  (define hex-digits "0123456789abcdef")

  ;; Four hex digits of one half word, most significant first. Written
  ;; with quotient and remainder for the same reason as everything else
  ;; here.
  (define (put-half p v)
    (do ((shift 4096 (quotient shift 16))) ((= shift 0))
      (put-char p (string-ref hex-digits (remainder (quotient v shift) 16)))))

  (define (crc32-hex bv . opts)
    (unless (bytevector? bv)
      (assertion-violation 'crc32-hex "not a bytevector" bv))
    (let-values (((start end) (range-of bv opts)))
      (let-values (((hi lo) (crc32-halves bv start end)))
        (call-with-string-output-port
          (lambda (p) (put-half p hi) (put-half p lo))))))

  ;; A string has no checksum of its own; its UTF-8 encoding does. The
  ;; encoding is named here rather than left to the caller because the
  ;; log format writes UTF-8 and nothing else, so any other answer would
  ;; be a checksum of bytes that were never on disk.
  (define (crc32-string s)
    (unless (string? s)
      (assertion-violation 'crc32-string "not a string" s))
    (crc32-bytes (string->utf8 s)))

  (define (crc32-string-hex s)
    (unless (string? s)
      (assertion-violation 'crc32-string-hex "not a string" s))
    (crc32-hex (string->utf8 s))))
