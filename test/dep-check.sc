#!chezscheme
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
