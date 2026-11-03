;; A consumer's file of this shape: one library body holding five
;; define-checked forms with multi-line bodies, internal defines and
;; let-bound lambdas, and one define that calls the first. Its strings carry
;; accented Latin letters, so its bytes are not its characters.
(library (consumer ledger)
  (export money-op! refund-op! hold-op! release-op! settle-op! run-day)
  (import (chezscheme) (contracts))

  ;; the first definition: a contract on each argument
  (define-checked (money-op! [account string?] [amount integer?])
    (define (label) "opération sur le compte")
    (let ((check (lambda (n) (and (integer? n) (>= n 0)))))
      (if (check amount)
          (list 'ok account amount (label))
          (list 'refused account "montant négatif"))))

  (define-checked (refund-op! [account string?] [amount integer?])
    (let* ((sign (lambda (n) (- n)))
           (note "remboursement à vérifier"))
      (money-op! account (abs (sign amount)))))

  (define-checked (hold-op! [account string?])
    (define reason "retenue préventive")
    (list 'held account reason))

  (define-checked (release-op! [account string?])
    (let ((undo (lambda (x) (list 'released x "libéré"))))
      (undo account)))

  (define-checked (settle-op! [account string?] [days integer?])
    (define (step k) (if (= k 0) '() (cons k (step (- k 1)))))
    (let ((total (lambda (xs) (apply + xs))))
      (list 'settled account (total (step days)) "réglé")))

  (define (run-day accounts)
    (map (lambda (a) (money-op! a 1)) accounts)))
