#!r6rs
;; Copyright 2026 guenchi
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
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
  ;;
  ;; `truncated` MEANS AN ENTRY WAS LEFT OUT, not that the log holds more
  ;; records. The two are not the same question: a block always has the
  ;; record that created it, so "is any history left" is true for every
  ;; refusal that reaches the count bound, and an answer carrying all
  ;; eight of the eight commits that existed still told its reader to go
  ;; and fetch the rest. Measured before the change, on one store per
  ;; row: one earlier commit gave one entry and no mark, seven gave seven
  ;; and no mark, eight gave eight WITH the mark, nine gave eight with
  ;; the mark. The loser was being sent back to `log` for a history it
  ;; already held in full.
  ;;
  ;; So reaching the count bound is not the end of the walk: the rest of
  ;; the history is scanned for one more record that WOULD have been an
  ;; entry -- about this block, and after the draft's cut -- and only
  ;; finding one makes the answer truncated. Records about other blocks,
  ;; and records the draft already saw, are not omissions.
  (define (eligible? r id cut)
    (let ((seen (assoc (car r) cut)))
      (and (touches? r id) (or (not seen) (> (cadr r) (cdr seen))))))

  (define (baseline-refusal state id wanted . rest)
    (let* ((now (block-hash state id))
           (cut (if (pair? rest) (car rest) '()))
           (rows (state->rows state))
           (history (cadr (assq 'request-history rows))))
      (and (not (equal? wanted now))
        (let loop ((xs (reverse history)) (out '()) (count 0) (size 0) (truncated? #f))
          (cond
            ((or (null? xs) (= count 8))
             (let ((omitted?
                     (or truncated?
                         (exists (lambda (r) (eligible? r id cut)) xs))))
               (append (list 'error 'stale-baseline (list 'block id)
                             (list 'based-on wanted) (list 'now now)
                             (cons 'since (reverse out)))
                       (if omitted?
                           (list '(truncated #t) (list 'retrieve (list 'log id) (list 'read id)))
                           '()))))
            (else
             (let* ((r (car xs))
                    (item (list (cons (car r) (cadr r)) (list-ref r 4) (list-ref r 3))))
               (if (eligible? r id cut)
                   (let ((n (encoded-size item)))
                     (if (> (+ size n) 8192)
                         (loop (cdr xs) out count size #t)
                         (loop (cdr xs) (cons item out) (+ count 1) (+ size n) truncated?)))
                   (loop (cdr xs) out count size truncated?)))))))))
)
