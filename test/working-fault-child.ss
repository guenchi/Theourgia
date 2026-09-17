#!r6rs
(import (chezscheme) (theourgia rpc))
(define args (cdr (command-line)))
(define answer (rpc-dispatch (car args) (list 'write (cadr args) "not-durable") "test"))
(write answer)
(newline)
(exit (if (eq? (car answer) 'error) 0 1))
