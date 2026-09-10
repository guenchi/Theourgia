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
(import (chezscheme) (theourgia wire))
(printf "string dep accepted: ~a\n"
        (bytevector? (encode-record 1 2 "w" '(("zzzz" . 1)) '(a))))
(printf "symbol dep refused:  ~a\n"
        (guard (e (#t #t)) (encode-record 1 2 "w" '((zzzz . 1)) '(a)) #f))
(import (theourgia crc32))
(let* ((t "(1 2 \"w\" ((zzzz . 1)) (a))")
       (line (string->utf8 (string-append (crc32-string-hex t) " " t "\n"))))
  (printf "hand-made symbol dep still READS: ~s\n" (decode-line line)))

;; Completion sentinel: run-all.sh treats a suite that ends without this line as a crash, not a pass.
(printf "dep-check complete\n")
