#!r6rs
(import (chezscheme) (theourgia datum-match))
(define bad 0)
(define (want label got expected)
  (if (equal? got expected) (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define old '(("a.1" (A) "" ()) ("a.2" (B) "" ()) ("a.3" (C) "" ())))
(want "CD-10 middle insertion preserves all uniquely aligned identities"
      (datum-match old '((#f (A) "" ()) (#f (X) "" ()) (#f (B) "" ()) (#f (C) "" ()))) '("a.1" #f "a.2" "a.3"))
(want "CD-10 middle deletion preserves remaining identities"
      (datum-match old '((#f (A) "" ()) (#f (C) "" ()))) '("a.1" "a.3"))
(want "CD-10 duplicate anonymous source refuses ambiguous survivor"
      (guard (e (#t (cadr e))) (datum-match '(("a.1" (A) "" ()) ("a.2" (A) "" ())) '((#f (A) "" ()))) 'accepted)
      'ambiguous-identity)
(want "CD-10 explicit ID removes anonymous ambiguity"
      (datum-match '(("a.1" (A) "" ()) ("a.2" (A) "" ())) '(("a.2" (A) "" ()))) '("a.2"))
(want "CD-11 explicit ID, primary name and anonymous sequence share one claim set"
      (datum-match '(("a.1" (define x 1) "" (x)) ("a.2" (define y 2) "" (y)) ("a.3" (use 1) "" ()))
                   '(("a.1" (define y 3) "" (y)) (#f (define y 4) "" (y)) (#f (use 1) "" ()))) '("a.1" "a.2" "a.3"))
(want "CD-09 duplicate name candidates require a marker"
      (guard (e (#t (cadr e)))
        (datum-match '(("a.1" (define x 1) "" (x)) ("a.2" (define x 2) "" (x))) '((#f (define x 3) "" (x)))) 'accepted)
      'ambiguous-identity)
(want "CD-11 inferred and explicit identity cannot reuse one target"
      (guard (e (#t (cadr e)))
        (datum-match '(("a.1" (define x 1) "" (x))) '(("a.1" (define y 1) "" (y)) (#f (define x 2) "" (x)))) 'accepted)
      'ambiguous-identity)
(want "CD-10 no equal datum means a unique empty mapping"
      (datum-match old '((#f (X) "" ()) (#f (Y) "" ()))) '(#f #f))
(want "CD-10 named anchor partitions duplicate anonymous forms"
      (guard (e (#t e))
        (datum-match '(("a.1" (A) "" ()) ("a.2" (define x 1) "" (x)) ("a.3" (A) "" ()))
                     '((#f (define x 2) "" (x)) (#f (A) "" ())))) '("a.2" "a.3"))
(want "CD-10 changed doc does not inherit anonymous identity"
      (datum-match '(("a.1" (A) ";; old\n" ())) '((#f (A) ";; new\n" ()))) '(#f))
(printf "~a failures\ndatum-match complete\n" bad)
(exit (if (zero? bad) 0 1))
