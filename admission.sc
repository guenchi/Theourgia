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
(library (theourgia admission)
  (export make-admission admission-copy admission-add! admission-gates)
  (import (rnrs) (theourgia request)
          (only (theourgia wire) sexpr->string-extended))

  ;; This index tracks causal availability independently of application.
  ;; Revoking an applied gate still makes the reducer replay its retained set.
  (define-record-type (admission make-admission/raw admission?)
    (fields buckets events dependents waits pasts clock
            (mutable gates)))
  (define (table) (make-hashtable string-hash string=?))
  (define (key x) (sexpr->string-extended x))
  (define (ref t x default) (hashtable-ref t (key x) default))
  (define (put! t x value) (hashtable-set! t (key x) value))
  (define (add! t x value) (put! t x (cons value (ref t x '()))))
  (define (make-admission)
    (make-admission/raw (table) (table) (table) (table) (table) (table) '()))
  ;; A COPY THAT SHARES NOTHING IT COULD CHANGE: each table copied (its values
  ;; are replaced on update, never changed in place), the gate list shared
  ;; (it too is replaced).
  (define (admission-copy a)
    (make-admission/raw (hashtable-copy (admission-buckets a) #t)
                        (hashtable-copy (admission-events a) #t)
                        (hashtable-copy (admission-dependents a) #t)
                        (hashtable-copy (admission-waits a) #t)
                        (hashtable-copy (admission-pasts a) #t)
                        (hashtable-copy (admission-clock a) #t)
                        (admission-gates a)))

  (define (event rec) (cons (car rec) (cadr rec)))
  (define (actor rec) (list-ref rec 4))
  (define (identity rec) (actor-identity (actor rec)))
  (define (premises rec)
    (append (if (= (cadr rec) 1) '() (list (cons (car rec) (- (cadr rec) 1))))
            (caddr rec)))
  (define (join a b)
    (fold-left
      (lambda (out p)
        (let ((old (assoc (car p) out)))
          (cond ((and old (>= (cdr old) (cdr p))) out)
                (old (cons p (filter (lambda (x) (not (string=? (car x) (car p)))) out)))
                (else (cons p out))))) a b))
  (define (missing a rec)
    (find (lambda (p) (< (ref (admission-clock a) (car p) -1) (cdr p))) (premises rec)))
  (define (evidence a rec)
    (let ((past (ref (admission-pasts a) (event rec) #f)))
      (make-evidence (event rec) (actor rec) (if past past (caddr rec))
                     (cadddr rec) 'valid-history (and past #t) '())))

;; REHEARSAL?, when given and true, says the admission belongs to a
  ;; rehearsal's copy of a reduction: the probes' trace lines are marked.
  (define (admission-add! a rec . rehearsal?)
    (let ((changed (table)) (id (identity rec)))
      (define (touch! id) (when id (put! changed id id)))
      (define (touch-event! e)
        (let ((r (ref (admission-events a) e #f))) (when r (touch! (identity r))))
        (for-each touch! (ref (admission-dependents a) e '())))
      (define (schedule! rec)
        (unless (ref (admission-pasts a) (event rec) #f)
          (let ((need (missing a rec)))
            (if need
                (add! (admission-waits a) need rec)
                (let* ((e (event rec))
                       (past (fold-left
                               (lambda (out p) (join out (ref (admission-pasts a) p '())))
                               (list e) (premises rec))))
                  (put! (admission-pasts a) e past)
                  (put! (admission-clock a) (car e) (cdr e))
                  (touch-event! e)
                  (let ((waiting (append (ref (admission-waits a) e '())
                                         (if (= (cdr e) 1)
                                             (ref (admission-waits a) (cons (car e) 0) '()) '()))))
                    (hashtable-delete! (admission-waits a) (key e))
                    (when (= (cdr e) 1)
                      (hashtable-delete! (admission-waits a) (key (cons (car e) 0))))
                    (for-each schedule! waiting)))))))
      (put! (admission-events a) (event rec) rec)
      (when id
        (add! (admission-buckets a) id rec)
        (touch! id)
        (let ((plan (actor-plan-event (actor rec))))
          (when plan (add! (admission-dependents a) plan id))))
      (touch-event! (event rec))
      (schedule! rec)
      (let-values (((keys ids) (hashtable-entries changed)))
        (vector-for-each
          (lambda (id)
            (let* ((records (ref (admission-buckets a) id '()))
                   (events (map event records))
                   (fresh (filter (lambda (p) (not (eq? (cdr p) 'valid)))
                                  (request-gates (map (lambda (r) (evidence a r)) records)
                                                 (and (pair? rehearsal?) (car rehearsal?))))))
              (admission-gates-set! a
                (append fresh (filter (lambda (p) (not (member (car p) events)))
                                      (admission-gates a)))))) ids))
      (admission-gates a)))
)
