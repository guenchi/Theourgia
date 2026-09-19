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
(library (theourgia datum-match)
  (export datum-match)
  (import (rnrs))
  ;; old/new rows: (id-or-#f body doc names). The result is one old id or
  ;; #f per new row. Explicit ids are claimed before any inferred identity.
  (define (ambiguous candidates)
    (raise (list 'error 'ambiguous-identity (list 'candidates candidates) '(need marker))))
  (define (primary row) (and (pair? (list-ref row 3)) (car (list-ref row 3))))
  (define (datum-match old new)
    (let* ((rows (list->vector new)) (answer (make-vector (length new) #f)) (used '()))
      (define (claim! index id)
        (when (member id used) (ambiguous (list id)))
        (vector-set! answer index id) (set! used (cons id used)))
      (do ((i 0 (+ i 1))) ((= i (vector-length rows)))
        (let ((id (car (vector-ref rows i))))
          (when (and id (not (eq? id 'new)))
            (unless (assoc id old) (raise (list 'error 'projection-invalid '(reason foreign-library) (list 'ids (list id)))))
            (claim! i id))))
      (do ((i 0 (+ i 1))) ((= i (vector-length rows)))
        (let* ((row (vector-ref rows i)) (name (primary row)))
          (when (and name (not (car row)))
            (let ((candidates (filter (lambda (r) (equal? (primary r) name)) old)))
              (cond ((null? candidates) (if #f #f))
                    ((pair? (cdr candidates)) (ambiguous (map car candidates)))
                    (else (claim! i (caar candidates))))))))
      (let* ((previous (list->vector (filter (lambda (r) (and (not (primary r)) (not (member (car r) used)))) old)))
             (positions (list->vector
                          (filter (lambda (i) (and (not (car (vector-ref rows i))) (not (primary (vector-ref rows i)))))
                                  (let loop ((i 0) (out '())) (if (= i (vector-length rows)) (reverse out) (loop (+ i 1) (cons i out)))))))
             (n (vector-length previous)) (m (vector-length positions))
             (table (begin
                      (when (> (* n m) 65536) (raise '(error projection-invalid (reason alignment-limit))))
                      (make-vector (* (+ n 1) (+ m 1)) #f))))
        (define (index i j) (+ (* i (+ m 1)) j))
        (define (cell i j) (vector-ref table (index i j)))
        (define (old-position id)
          (let loop ((rs old) (i 0))
            (if (equal? (caar rs) id) i (loop (cdr rs) (+ i 1)))))
        (define (within-anchors? a j)
          (let ((position (old-position (car a))) (target (vector-ref positions j)))
            (let loop ((i 0))
              (or (= i (vector-length answer))
                  (and (or (not (vector-ref answer i))
                           (if (< i target) (< (old-position (vector-ref answer i)) position)
                               (> (old-position (vector-ref answer i)) position)))
                       (loop (+ i 1)))))))
        (define (same? a b) (and (equal? (cadr a) (cadr b)) (equal? (caddr a) (caddr b))))
        (define (distinct-two maps)
          (let loop ((xs maps) (out '()))
            (cond ((or (null? xs) (= (length out) 2)) (reverse out))
                  ((member (car xs) out) (loop (cdr xs) out))
                  (else (loop (cdr xs) (cons (car xs) out))))))
        ;; Crossing explicit anchors cannot establish an anonymous interval.
        (when (and (> m 0) (> n 0))
          (let loop ((i 0) (last -1))
            (unless (= i (vector-length answer))
              (let ((id (vector-ref answer i)))
                (if id
                    (let ((p (old-position id)))
                      (when (< p last) (ambiguous (map car (vector->list previous))))
                      (loop (+ i 1) p))
                    (loop (+ i 1) last))))))
        ;; Each cell keeps at most two distinct optimal mappings, not two
        ;; edit paths: many skip paths can represent the same empty mapping.
        (do ((i n (- i 1))) ((< i 0))
          (do ((j m (- j 1))) ((< j 0))
            (vector-set! table (index i j)
              (if (or (= i n) (= j m)) '(0 ())
                  (let* ((a (cell (+ i 1) j)) (b (cell i (+ j 1))) (diagonal (cell (+ i 1) (+ j 1)))
                         (equal? (and (within-anchors? (vector-ref previous i) j) (same? (vector-ref previous i) (vector-ref rows (vector-ref positions j)))))
                         (best (max (car a) (car b) (if equal? (+ 1 (car diagonal)) 0)))
                         (maps (append (if (= best (car a)) (cdr a) '()) (if (= best (car b)) (cdr b) '())
                                       (if (and equal? (= best (+ 1 (car diagonal))))
                                           (map (lambda (mapping) (cons (cons (vector-ref positions j) (car (vector-ref previous i))) mapping))
                                                (cdr diagonal)) '()))))
                    (cons best (distinct-two maps)))))))
        (let ((maps (cdr (cell 0 0))))
          (when (pair? (cdr maps)) (ambiguous (map car (vector->list previous))))
          (for-each (lambda (p) (claim! (car p) (cdr p))) (car maps))))
      (vector->list answer)))
)
