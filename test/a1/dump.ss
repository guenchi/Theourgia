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

;; ONE LOAD OF A STORE IN A FRESH PROCESS, printed as one datum.
;;
;; usage: scheme --script dump.ss <store>
;;
;; NEVER: A FRESH PROCESS. A restart is what the fixture's second and third
;; paths are about, so each is its own process: nothing the writing process
;; holds (its state, or the resident cache of store.sc when a process turns
;; it on with store-resident-cache!) can answer for the load. The load is traced; the lines this prints are the ones the
;; fixture asserts on, and the rest of the trace is dropped.
;;
;; NOTE: IT PRINTS WHAT THE STORE HOLDS AND DECIDES NOTHING. Ids are mapped to
;; places, and compared with the expected list, by the fixture.

(import (chezscheme) (theourgia store) (theourgia reduce) (theourgia trace)
        (only (theourgia md) md-refs))

(define store (cadr (command-line)))

(define (lines-of text)
  (let ((n (string-length text)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((>= i n) (reverse (if (> i start) (cons (substring text start i) acc) acc)))
            ((char=? (string-ref text i) #\newline)
             (loop (+ i 1) (+ i 1) (cons (substring text start i) acc)))
            (else (loop (+ i 1) start acc))))))

(define (contains? s p)
  (let ((n (string-length s)) (m (string-length p)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) p) #t)
            (else (loop (+ i 1)))))))

(define trace-lines '())
(define state
  (let ((p (open-output-string)))
    (let ((s (parameterize ((current-error-port p))
               (trace-enable! #t)
               (let ((s (open-and-reduce store))) (trace-enable! #f) s))))
      (set! trace-lines (lines-of (get-output-string p)))
      s)))

(define ids (map cadr (state-datum state)))

;; THE PRODUCT IS ASKED ONLY ABOUT THE BLOCKS SOME TEXT NAMES. Asking
;; `store-refs` of every block made the dump quadratic in blocks -- each call
;; lexes every block's text. Each block's
;; src is lexed once here with the product's own lexer (md.sc md-refs); a key
;; that is a block id is a candidate target; the product's `refs` then
;; answers for each candidate, so the edges are still the product's.
(define id-set
  (let ((t (make-hashtable string-hash string=?)))
    (for-each (lambda (id) (hashtable-set! t id #t)) ids)
    t))
(define referenced
  (let ((t (make-hashtable string-hash string=?)))
    (for-each
      (lambda (id)
        (let* ((b (state-read state id))
               (src (let ((p (and b (assq 'src (cdr (assq 'fields b)))))) (and p (cdr p))))
               (text (cond ((string? src) src) ((bytevector? src) (utf8->string src)) (else #f))))
          (when text
            (for-each (lambda (r)
                        (let ((k (caddr r)))
                          (when (and (string? k) (hashtable-ref id-set k #f))
                            (hashtable-set! t k #t))))
                      (md-refs text)))))
      ids)
    (list-sort string<? (vector->list (hashtable-keys t)))))

(define (derived-into id)
  (let ((a (store-refs store id)))
    (if (and (pair? a) (eq? (car a) 'ok))
        (map (lambda (r) (list (car r) id))
             (filter (lambda (r) (eq? (caddr r) 'md)) (cadr a)))
        (list (list 'REFS-FAILED id a)))))

(write
  (list 'dump
        (cons 'blocks
              (map (lambda (id)
                     (let ((b (state-read state id)))
                       (list id (cdr (assq 'fields b)) (cdr (assq 'deleted b)))))
                   ids))
        (cons 'outline (map (lambda (r) (list (car r) (caddr r))) (state-outline state)))
        (cons 'links (map cadr (filter (lambda (row) (eq? (car row) 'link)) (state->rows state))))
        (cons 'derived (apply append (map derived-into referenced)))
        (cons 'snapshot-read (filter (lambda (l) (contains? l "(trace snapshot-read")) trace-lines))
        (list 'parsed (length (filter (lambda (l) (contains? l "(trace parse")) trace-lines)))
        (cons 'replayed (reduce-trace state))
        (list 'applied (reduce-applied-cut state))))
(newline)
