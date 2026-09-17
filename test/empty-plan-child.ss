#!r6rs
(import (chezscheme) (theourgia rpc) (theourgia log))
(define args (command-line-arguments))
(define store (cadr args))
(putenv "THEOURGIA_HOME" (string-append store "-home"))
(define writer
  (if (string=? (car args) "setup")
      (cadr (assq 'writer (cdr (rpc-dispatch store '(init) "test"))))
      (caddr args)))
(define answer
  (rpc-dispatch store (list 'commit "--req" "barrier-empty" "--cursor" (string-append writer ":0")) "test"))
(write answer) (newline)
(when (string=? (car args) "setup")
  (printf "WRITER ~a\n" writer)
  (for-each (lambda (e) (printf "ARTIFACT ~a ~a\n" (car e) (cdr e)))
            (barrier-artefacts store writer 1)))
(printf "empty-plan-child complete\n")
