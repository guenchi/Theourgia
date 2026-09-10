#!chezscheme
(import (chezscheme) (theourgia wire))
(define (raises? t) (guard (e (#t #t)) (t) #f))
(printf "gensym actor refused:            ~a\n"
        (raises? (lambda () (encode-record 0 0 (list (gensym "a") 0 0 0 0) '() '()))))
(printf "gensym nested in plan-event-id:  ~a\n"
        (raises? (lambda () (encode-record 0 0 (list "w" "r" 0 "f" (cons (gensym "a") 3)) '() '()))))
(printf "character in the actor refused:  ~a\n"
        (raises? (lambda () (encode-record 0 0 (list "w" "r" #\a "f" #f) '() '()))))
(printf "CONTROL string actor accepted:   ~a\n"
        (not (raises? (lambda () (encode-record 0 0 "agent:claude" '() '())))))
(printf "CONTROL five-element accepted:   ~s\n"
        (decode-line (encode-record 0 0 (list "agent:claude" "req-1" 0 "fp" (cons "w" 3)) '() '(a))))
(printf "CONTROL #%-prefixed actor accepted: ~s\n"
        (decode-line (encode-record 0 0 (list "#%alice" "b" "c" "d" "e") '() '(a))))
(printf "cyclic vector in actor refused:  ~a\n"
        (raises? (lambda ()
          (let ((v (make-vector 1)))
            (vector-set! v 0 v)
            (encode-record 0 0 (list v 0 0 0 0) '() '())))))
(printf "shared (acyclic) subtree accepted: ~a\n"
        (not (raises? (lambda ()
          (let ((shared (list "s")))
            (encode-record 0 0 (list shared "b" 0 "d" shared) '() '(a)))))))
(printf "CONTROL 'single accepted:        ~s\n"
        (decode-line (encode-record 0 0 (list "agent:claude" "req-1" 'single "fp" #f) '() '(a))))

;; Completion sentinel: run-all.sh treats a suite that ends without this line as a crash, not a pass.
(printf "actor-check complete\n")
