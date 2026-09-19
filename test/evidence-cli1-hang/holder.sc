#!r6rs
(import (chezscheme) (theourgia store))
(with-store-write (cadr (command-line))
  (lambda (st v)
    '((insert root #f ((kind . section) (title . "held"))))))
