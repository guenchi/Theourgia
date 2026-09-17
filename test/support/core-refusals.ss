#!r6rs
(import (chezscheme))
;; Read trusted implementation forms; comments and string contents are not code.
(define (literal-symbol form)
  (and (pair? form) (eq? (car form) 'quote) (pair? (cdr form))
       (null? (cddr form)) (symbol? (cadr form)) (cadr form)))
(for-each
 (lambda (file)
   (let ((found '()))
     (define (remember kind)
       (unless (memq kind found)
         (set! found (cons kind found))
         (printf "~a\t~a\n" file kind)))
     (define (walk form)
       (when (pair? form)
         (cond
           ((and (eq? (car form) 'list) (pair? (cdr form)) (pair? (cddr form))
                 (eq? (literal-symbol (cadr form)) 'error) (literal-symbol (caddr form)))
            (remember (literal-symbol (caddr form))))
           ((and (eq? (car form) 'quote) (pair? (cdr form))
                 (pair? (cadr form)) (eq? (caadr form) 'error)
                 (pair? (cdadr form)) (symbol? (cadadr form)))
            (remember (cadadr form))))
         (walk (car form)) (walk (cdr form))))
     (call-with-input-file file
       (lambda (port)
         (let loop ((form (read port)))
           (unless (eof-object? form) (walk form) (loop (read port))))))))
 (cdr (command-line)))
