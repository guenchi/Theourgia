#!chezscheme
(import (chezscheme) (theourgia ffi))
(with-shared-lock (cadr (command-line)) (lambda (fd) (void)))
