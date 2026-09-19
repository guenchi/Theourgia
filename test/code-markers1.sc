#!r6rs
(import (chezscheme) (theourgia code-markers) (theourgia text-code) (theourgia languages))
(define bad 0)
(define (want name got expected)
  (if (equal? got expected) (printf "ok ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" name got expected))))
(define h '("store-a" "writer.a" (("writer" . 9))))
(for-each
  (lambda (e)
    (let* ((language (language-property e 'lang "unknown"))
           (b (apply bytes-append
                (map (lambda (s) (marker-line e s))
                     '("@block writer.x" "@@block writer.y" "@@@block writer.z" "@file literal" "@@file literal")))))
      (want (string-append "CT-15 " language " all marker counts are injective")
            (cadr (projection-decode e (projection-encode e h (list (list "writer.1" b)))))
            (list (list "writer.1" b))))) (language-table))
(define py (language-for-name 'python))
(for-each (lambda (b)
            (define entries (list (list "writer.1" b) (list "writer.2" (string->utf8 "two")) (list "writer.3" #vu8())))
            (want "CT-19 exact framing including empty and unterminated blocks"
                  (cadr (projection-decode py (projection-encode py h entries))) entries))
          (map string->utf8 '("one" "one\n" "one\r\n" "one\r" "")))
(for-each (lambda (b)
            (let* ((entries (list (list "writer.1" b))) (out (projection-encode py h entries)) (n (source-prefix-size b)))
              (want "CT-17 sensitive prefix stays at original byte offset" (byte-slice out 0 n) (byte-slice b 0 n))
              (want "CT-17 prefix including unterminated directive roundtrips"
                    (cadr (projection-decode py out)) entries)))
          (map string->utf8 '("#!/bin/python\n# coding: utf-8\nhello" "#!r6rs" "\xFEFF;#!/bin/python\r\nhello" "# coding: utf-8\nhello" "# note\n# coding=utf-8\nhello")))
(define md (language-for-name 'markdown))
(define body (string->utf8 "x <!-- @block writer.x -->\n<!-- @block writer.x --> tail\n"))
(want "CT-18 partial HTML wrapper stays literal" (projection-decode md body) (list #f (list (list "new" body))))
(want "CT-18 exact wrapper creates a boundary"
      (projection-decode md (string->utf8 "<!-- @block new -->\none\n<!-- @block new -->\ntwo"))
      (list #f (list (list "new" (string->utf8 "one\n")) (list "new" (string->utf8 "two")))))
(want "CT-16 manually placed boundary inside body is respected"
      (cadr (projection-decode py (string->utf8 "# @block new\ndef f():\n# @block new\n  pass\n")))
      (list (list "new" (string->utf8 "def f():\n")) (list "new" (string->utf8 "  pass\n"))))
(want "CT-15 raw escaped-family text without controls is not decoded"
      (projection-decode py (string->utf8 "# @@block whatever\n"))
      (list #f (list (list "new" (string->utf8 "# @@block whatever\n")))))
(printf "~a failures\ncode-markers1 complete\n" bad)
(exit (if (zero? bad) 0 1))
