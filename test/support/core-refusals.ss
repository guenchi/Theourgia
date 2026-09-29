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
           ;; NOTE: A TABLE OF KINDS THE CORE MAKES THROUGH A VARIABLE (f5ebd58,
           ;; answers.sc: `(define table-kinds '(absent unreadable unwritable))`,
           ;; answered as `(cons* 'error (failure-kind c) ...)`). No literal
           ;; `(error <kind> ...)` names them, so the census read none of them
           ;; until the re-pin found two reaching a save; the table itself is
           ;; read here, so a kind added to it is counted.
           ((and (eq? (car form) 'define) (pair? (cdr form)) (eq? (cadr form) 'table-kinds)
                 (pair? (cddr form)) (pair? (caddr form)) (eq? (car (caddr form)) 'quote)
                 (pair? (cdr (caddr form))) (list? (cadr (caddr form))))
            (for-each (lambda (kind) (when (symbol? kind) (remember kind))) (cadr (caddr form))))
           ;; NOTE: THE SYMBOLS FILE'S REFUSALS ARE MADE THROUGH A NAMED
           ;; CONSTRUCTOR (code-suggest.sc: `(symbols-refusal 'symbols-stale ...)`,
           ;; which raises `(error <kind> ...)` from a variable). No literal
           ;; `(error <kind> ...)` names them, so the constructor's calls are
           ;; read; the name is this one constructor's, not a pattern.
           ((and (eq? (car form) 'symbols-refusal) (pair? (cdr form)) (literal-symbol (cadr form)))
            (remember (literal-symbol (cadr form))))
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
