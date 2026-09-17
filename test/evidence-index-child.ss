#!r6rs
(import (chezscheme) (theourgia store) (theourgia request))
(define args (command-line-arguments))
(putenv "THEOURGIA_HOME" (string-append (car args) "-home"))
(define es (store-evidence (car args) (cons (cadr args) (if (> (length args) 3) (list-ref args 3) "indexed"))))
(call-with-output-file (caddr args)
  (lambda (p) (write (map ev-placement es) p)) 'replace)
(printf "evidence-index-child complete\n")
