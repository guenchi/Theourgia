#!r6rs
(import (chezscheme))
;; The Scheme reader excludes comments and treats strings as leaves.
;; Audit the entire binding body, rejecting any new use or shadowing of line.
(define form (call-with-input-file "project.ss" read))
(define definition
  (find (lambda (x) (and (pair? x) (eq? 'define (car x))
                         (equal? '(strip-recovery text) (cadr x)))) (cddddr form)))
(define binding-body #f)
(define (find-binding x)
  (when (pair? x)
    (when (and (eq? 'let* (car x)) (assq 'line (cadr x))) (set! binding-body x))
    (for-each find-binding (filter pair? x))))
(find-binding definition)
(unless binding-body (error 'crlf-consumers "Missing lexical binding"))
(define uses '())
(define (walk x parent)
  (cond ((eq? x 'line) (set! uses (cons parent uses)))
        ((pair? x) (for-each (lambda (a) (walk a x)) x))))
;; Later let* initializers and the body are the scope of this binding.
(for-each (lambda (b) (walk (cadr b) b)) (cdr (memq (assq 'line (cadr binding-body)) (cadr binding-body))))
(for-each (lambda (body) (walk body #f)) (cddr binding-body))
(printf "QR-04 lexical consumers: ~s\n" (reverse uses))
(unless (equal? (reverse uses) '((recovery-id line) (heading-line? line)))
  (error 'crlf-consumers "The dead-copy rationale must be reviewed" uses))
(printf "crlf-consumers complete\n")
