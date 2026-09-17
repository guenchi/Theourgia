#!r6rs
(library (theourgia baseline)
  (export baseline-refusal)
  (import (rnrs) (theourgia reduce) (theourgia wire))

  (define (encoded-size value)
    (bytevector-length
      (string->utf8 (sexpr->string-extended (storable-encode value)))))

  (define (touches? rec id)
    (let ((p (list-ref rec 3)))
      (and (pair? p)
        (case (car p)
          ((put) (equal? id (block-id (car rec) (cadr rec))))
          ((set move del link unlink) (and (pair? (cdr p)) (equal? id (cadr p))))
          (else #f)))))

  ;; The history cut is captured with the original draft. Both the number
  ;; of records and their encoded size are bounded, including actor text.
  (define (baseline-refusal state id wanted . rest)
    (let* ((now (block-hash state id))
           (cut (if (pair? rest) (car rest) '()))
           (rows (state->rows state))
           (history (cadr (assq 'request-history rows))))
      (and (not (equal? wanted now))
        (let loop ((xs (reverse history)) (out '()) (count 0) (size 0) (truncated? #f))
          (cond
            ((or (null? xs) (= count 8))
             (append (list 'error 'stale-baseline (list 'block id)
                           (list 'based-on wanted) (list 'now now)
                           (cons 'since (reverse out)))
                     (if (or truncated? (pair? xs))
                         (list '(truncated #t) (list 'retrieve (list 'log id) (list 'read id)))
                         '())))
            (else
             (let* ((r (car xs)) (seen (assoc (car r) cut))
                    (item (list (cons (car r) (cadr r)) (list-ref r 4) (list-ref r 3))))
               (if (and (touches? r id) (or (not seen) (> (cadr r) (cdr seen))))
                   (let ((n (encoded-size item)))
                     (if (> (+ size n) 8192)
                         (loop (cdr xs) out count size #t)
                         (loop (cdr xs) (cons item out) (+ count 1) (+ size n) truncated?)))
                   (loop (cdr xs) out count size truncated?)))))))))
)
