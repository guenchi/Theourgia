#!r6rs
(import (chezscheme) (theourgia store) (theourgia rpc) (theourgia reduce) (theourgia log))
;; A USAGE LINE, BECAUSE THIS TAKES ARGUMENTS. The suite runner decides
;; what a script is from what it prints: a probe says `usage: <name>`
;; and is not counted, a fixture prints its own completion sentinel.
;; Without this line a probe run with no arguments dies in `cadr` and is
;; counted as a red fixture -- which reads exactly like a case that
;; failed, for a script that was never given anything to do.
(when (< (length (command-line)) 3)
  (display "usage: q8-report <store-dir> <mode>\n")
  (exit 0))
(define d (cadr (command-line)))
(define mode (caddr (command-line)))
(define bad 0)
(define (check label value)
  (write (list (if value 'PASS 'FAIL) label value)) (newline)
  (unless value (set! bad (+ 1 bad))))
(define init (store-init! d))
(check 'init (eq? (car init) 'ok))
(define w (car (store-writers d)))
(define payload "((insert root #f ((title . \"A\"))) (insert (from 0) #f ((title . \"B\"))) (insert (from 1) #f ((title . \"C\"))))")
(define tracked? (string=? mode "tracked"))
(define reply (rpc-dispatch d (append '(batch)
                                     (if tracked? (list "--req" "verify-1" "--cursor" (string-append w ":0")) '())
                                     (list payload)) "review"))
(check 'all-three-operations-succeed (and (rpc-ok? reply) (= 3 (length (cadr reply)))))
(define first-seq (if tracked? 2 1))
(define a (block-id w first-seq))
(define b (block-id w (+ first-seq 1)))
(define c (block-id w (+ first-seq 2)))
(define state (open-and-reduce d))
(check 'all-three-blocks-persist (= 3 (length (state-datum state))))
(check 'backreferences-are-real-block-ids
       (and (state-read state b) (state-read state c)
            (equal? (cdr (assq 'position (state-read state b))) (cons a 0))
            (equal? (cdr (assq 'position (state-read state c))) (cons b 0))))
(when (string=? mode "outline")
  (check 'delete-parent (rpc-ok? (rpc-dispatch d (list 'del a) "review")))
  (let ((answer (rpc-dispatch d '(outline) "review")))
    (check 'orphan-subtree-visible
      (equal? answer (list 'ok (list 'text (string-append "orphans:\n- " b "  B\n  - " c "  C\n"))))))
  (check 'depth-limit-on-orphan-subtree
    (equal? (rpc-dispatch d '(outline "--depth" "1") "review")
            (list 'ok (list 'text (string-append "orphans:\n- " b "  B\n"))))))
(write (list 'failures bad)) (newline)
(exit (if (= bad 0) 0 1))
