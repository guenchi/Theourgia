#!r6rs
(import (chezscheme) (theourgia code-markers) (theourgia text-code) (theourgia languages)
        (only (theourgia derived) projection-range))
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
;; ---- CT-20: a projected byte range back to one block's own src -----------------
;; Three blocks in python: b1 opens with a #! line and a coding cookie (the
;; prefix, 30 bytes, which stays above the @file line and is b1's source),
;; b2 does not end in LF (so a pad LF follows it), b3 holds a marker-like
;; line (one "@" inserted after "# "). Every expected offset is found in
;; the projected bytes by searching for text, never read from the map.
(define (offset-of b needle)
  (let ((n (string->utf8 needle)))
    (let loop ((i 0))
      (cond ((> (+ i (bytevector-length n)) (bytevector-length b)) #f)
            ((let check ((k 0)) (or (= k (bytevector-length n))
                                    (and (= (bytevector-u8-ref b (+ i k)) (bytevector-u8-ref n k)) (check (+ k 1)))))
             i)
            (else (loop (+ i 1)))))))
(define m-b1 (string->utf8 "#!/bin/python\n# coding: utf-8\ndef f():\n  return 1\n"))
(define m-b2 (string->utf8 "y = 3"))
(define m-b3 (string->utf8 "# @block writer.q\nz = 4\n"))
(define m-entries (list (list "writer.1" m-b1) (list "writer.2" m-b2) (list "writer.3" m-b3)))
(define-values (m-out m-pieces) (projection-encode-map py h m-entries))
(define p1 (offset-of m-out "def f()"))
(define p2 (offset-of m-out "y = 3"))
(define q3 (offset-of m-out "# @@block writer.q"))
(define f0 (offset-of m-out "# @file"))
(define m2 (offset-of m-out "# @block writer.2"))
(define m3 (offset-of m-out "# @block writer.3"))
(define (rng s e) (projection-range m-pieces s e))
(want "CT-20 setup: the map's bytes are projection-encode's, the prefix is 30 bytes above the @file line, b2 is followed by a pad LF, b3's line is escaped"
      (list (equal? m-out (projection-encode py h m-entries)) (source-prefix-size m-b1) f0
            (bytevector-u8-ref m-out (+ p2 5)) (= m3 (+ p2 6)) (and q3 #t))
      (list #t 30 30 10 #t #t))
(want "CT-20 (1) a range inside the prefix is b1's src at the same offsets"
      (rng 2 5) '(mapped "writer.1" 2 5))
(want "CT-20 (2) a range from the prefix into b1's body, across the @file and @block lines, is contiguous in b1"
      (rng 2 (+ p1 3)) '(mapped "writer.1" 2 33))
(want "CT-20 (3) around the inserted byte: the byte before, the inserted byte alone (empty), the byte after, and a range over it"
      (list (rng (+ q3 1) (+ q3 2)) (rng (+ q3 2) (+ q3 3)) (rng (+ q3 3) (+ q3 4)) (rng q3 (+ q3 6)))
      '((mapped "writer.3" 1 2) (mapped "writer.3" 2 2) (mapped "writer.3" 2 3) (mapped "writer.3" 0 5)))
(want "CT-20 (4) a body to its end, and to the pad LF's start, is the whole body; one byte more takes the pad LF and is unmappable; b1's body is at the prefix's size"
      (list (rng p2 (+ p2 5)) (rng p2 (+ p2 6)) (rng p1 (+ p1 20)))
      '((mapped "writer.2" 0 5) (unmappable "writer.2") (mapped "writer.1" 30 50)))
(want "CT-20 (5) a range from b1's body into b2's is unmappable on b1, the block s touches"
      (rng (+ p1 1) (+ p2 1)) '(unmappable "writer.1"))
(want "CT-20 (6) a range inside b2's @block line is unmappable on b2, the block it precedes"
      (rng (+ m2 1) (+ m2 3)) '(unmappable "writer.2"))
(want "CT-20 (7) empty ranges: at b1's body end (also b2's marker start) b1; at b2's end (the pad LF's start) b2; at b3's marker start after the pad LF b3 at 0; at EOF b3's end"
      (list (rng (+ p1 20) (+ p1 20)) (rng (+ p2 5) (+ p2 5)) (rng m3 m3)
            (rng (bytevector-length m-out) (bytevector-length m-out)))
      (list '(mapped "writer.1" 50 50) '(mapped "writer.2" 5 5) '(mapped "writer.3" 0 0)
            (list 'mapped "writer.3" (bytevector-length m-b3) (bytevector-length m-b3))))
(want "CT-20 empty ranges strictly inside a source piece: in the prefix, in b1's body, in b3's escaped line before the escape"
      (list (rng 3 3) (rng (+ p1 3) (+ p1 3)) (rng (+ q3 1) (+ q3 1)))
      '((mapped "writer.1" 3 3) (mapped "writer.1" 33 33) (mapped "writer.3" 1 1)))
(want "CT-20 (10) empty ranges inside the @file line and inside b2's @block line: the block each precedes, at its piece's start"
      (list (rng (+ f0 3) (+ f0 3)) (rng (+ m2 3) (+ m2 3)))
      '((mapped "writer.1" 30 30) (mapped "writer.2" 0 0)))
;; A block with no source still owns its place: the empty range where its
;; empty piece sits is its own, not the next block's.
(define-values (e-out e-pieces)
  (projection-encode-map py h (list (list "writer.1" (string->utf8 "a\n")) (list "writer.2" #vu8())
                                    (list "writer.3" (string->utf8 "c\n")))))
(want "CT-20 an empty block: the empty range at its offset (where the next marker starts) is its own"
      (let ((m (offset-of e-out "# @block writer.3"))) (projection-range e-pieces m m))
      '(mapped "writer.2" 0 0))
(want "CT-20 a file with no blocks maps nothing"
      (let-values (((b ps) (projection-encode-map py h '()))) (projection-range ps 0 0))
      '(none))

(printf "~a failures\ncode-markers1 complete\n" bad)
(exit (if (zero? bad) 0 1))
