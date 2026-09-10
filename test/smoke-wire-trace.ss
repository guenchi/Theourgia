#!chezscheme
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
;; The claim under test is an ORDER, not an outcome: text whose CRC
;; fails must never reach the parser. A good line is the control -- if
;; it also emitted no parse event the silence below would mean nothing.
(import (chezscheme) (theourgia wire) (theourgia crc32) (theourgia trace))
(define good (encode-record 412 1757300000123 "who" '() '(put "x" ((a . 1)))))
(define bad (let ((c (bytevector-copy good)))
              (bytevector-u8-set! c 20 (if (= 65 (bytevector-u8-ref c 20)) 66 65))
              c))
(define torn (let* ((l (bytevector->u8-list good)))
               (u8-list->bytevector (reverse (cdr (reverse l))))))
(define padded
  (let* ((t (utf8->string (u8-list->bytevector
                            (list-tail (reverse (cdr (reverse (bytevector->u8-list good)))) 9))))
         (t2 (string-append t " ")))
    (string->utf8 (string-append
                    (crc32-string-hex t2) " " t2 "\n"))))

(define (run label bv)
  (printf "~a: " label)
  (let ((r (parameterize ((theourgia-trace? #t)) (decode-line bv))))
    (printf "-> ~s\n" r)))
(run "good line (control: a parse event MUST appear)" good)
(run "bad crc  (no parse event may appear)" bad)
(run "torn     (no parse event may appear)" torn)
(run "padded   (no parse event may appear)" padded)

;; Completion sentinel: run-all.sh treats a suite that ends without this line as a crash, not a pass.
(printf "smoke-wire-trace complete\n")
