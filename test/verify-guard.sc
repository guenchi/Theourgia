#!chezscheme
;; Does R6RS guard re-enter the dynamic extent when a clause DECLINES?
;; If it does, the re-entry check I added to call-with-lock fires during
;; ordinary exception propagation and replaces the real error.
(import (chezscheme) (theourgia ffi))
(define dir "/private/tmp/claude-501/-Users-guenchi-Workshop/ff8debcd-6740-4e42-80ca-8d637b6249df/scratchpad/tg/gwork")
(system (string-append "rm -rf " dir "; mkdir -p " dir))
(define lock (string-append dir "/lock"))
(printf "outer guard sees: ~s\n"
  (guard (e (#t (list 'outer (if (and (vector? e) (> (vector-length e) 0)) (vector-ref e 0) 'not-a-vector))))
    (guard (e ((and (vector? e) (eq? (vector-ref e 0) 'never-matches)) 'inner))
      (with-exclusive-lock lock
        (lambda (fd) (raise (vector 'durable-error 'write (cons "/x" 5))))))))
(printf "plain dynamic-wind control: ~s\n"
  (let ((log '()))
    (guard (e (#t (reverse (cons 'caught log))))
      (guard (e ((eq? e 'never) 'inner))
        (dynamic-wind
          (lambda () (set! log (cons 'in log)))
          (lambda () (raise 'boom))
          (lambda () (set! log (cons 'out log))))))))
