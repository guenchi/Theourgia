#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
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

;; THE RECEIPT CHANGES NOTHING ELSE: a reading, not a row. One tree cannot
;; hold two products, so identity with the product before receipts existed
;; is measured by running this script against a checkout of that product:
;; one seeded store, read by both products' handlers; the newer answers with
;; the receipt's clauses stripped must equal the older ones verb by verb,
;; and so must the human output, except for the two verbs printed whole
;; (plain read and reach), whose human output shows their clauses.
;;
;; usage, from this directory, CHEZSCHEMELIBDIRS naming the product to use:
;;   scheme --script read-receipt-base.ss seed <store> <scratch-dir> (this tree)
;; <scratch-dir> is a new directory the seed writes a two-definition library
;; into and imports; it is not the product's library directory.
;;   scheme --script read-receipt-base.ss read <store> <out> keep    (the older product)
;;   scheme --script read-receipt-base.ss read <store> <out> strip   (this tree)
;;   scheme --script read-receipt-base.ss compare <older-out> <this-out>
;; `compare` prints one line per verb and exits 0 only when every answer is
;; identical and every human output is, the two printed whole excepted.
;; Ids are the store's, so both reads see the same blocks.
(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) state-read state-block-ids)
        (only (theourgia render) render-human))
(register-verbs! extension-verbs)
(define args (cdr (command-line)))
(define mode (car args))
(define (store-call store a) (rpc-dispatch store a "read-receipt-base"))
(define (state-of store) (open-and-reduce store))

(define (field-of s id f)
  (let ((row (state-read s id)))
    (and row (let ((e (assq f (cdr (assq 'fields row))))) (and e (cdr e))))))
(define (id-by-title store title)
  (let ((s (state-of store)))
    (find (lambda (id) (equal? (field-of s id 'title) title)) (state-block-ids s))))
(define (definition-of store name)
  (let ((s (state-of store)))
    (find (lambda (id)
            (let ((b (field-of s id 'body)))
              (and (pair? b) (pair? (cdr b)) (pair? (cadr b)) (eq? (car (cadr b)) (string->symbol name)))))
          (state-block-ids s))))

(define (seed! store lib)
  ;; A SEED THAT DID NOT SEED MUST NOT SAY IT DID: every call has to succeed,
  ;; and the two definitions have to be found after the import.
  (let ((run (lambda a
               (let ((answer (store-call store a)))
                 (unless (rpc-ok? answer)
                   (printf "NOT SEEDED: ~s answered ~s\n" a answer)
                   (exit 1))
                 answer))))
    (run 'init)
    (run 'insert "--title" "Xray block" "--text" "xray body alpha")
    (let ((x (id-by-title store "Xray block")))
      (run 'insert "--title" "Yankee block" "--text" "yankee body beta" "--under" x)
      (run 'insert "--title" "Zulu block" "--text" "zulu body gamma\nzulu body again")
      (run 'link (id-by-title store "Yankee block") "cites" x))
    (run 'insert "--title" "Decision one" "--text" "the ruling")
    (run 'set (id-by-title store "Decision one") "kind" "decision")
    (run 'insert "--title" "Task one" "--text" "task body")
    (let ((t (id-by-title store "Task one")))
      (run 'set t "kind" "task") (run 'set t "status" "todo")
      (run 'link t "implements" (id-by-title store "Decision one")))
    (system (string-append "mkdir -p '" lib "'"))
    (call-with-output-file (string-append lib "/rr.sc")
      (lambda (p) (put-string p "(library (rr) (export h g) (import (rnrs))\n(define (h) 1)\n(define (g) (h)))\n"))
      'replace)
    (run 'import-code lib "--datum")
    (unless (and (definition-of store "h") (definition-of store "g"))
      (printf "NOT SEEDED: the import of ~a did not define h and g\n" lib)
      (exit 1))))

;; The calls, and the clauses the receipt appends to each answer.
(define (calls store)
  (let ((x (id-by-title store "Xray block")) (h (definition-of store "h")) (g (definition-of store "g")))
    (list (list 'read (list 'read x) '(cut versions))
          (list 'read-md (list 'read x "--md") '(cut versions))
          (list 'read-md-recursive (list 'read x "--md" "--recursive") '(cut versions))
          (list 'read-recursive (list 'read x "--recursive") '(cut))
          (list 'outline (list 'outline) '(cut))
          (list 'refs (list 'refs x) '(cut versions))
          (list 'reach (list 'reach h) '(cut))
          (list 'search (list 'search "xray") '(versions))
          (list 'grep (list 'grep "body") '(versions))
          (list 'whereis (list 'whereis "h") '(versions))
          (list 'commitments (list 'commitments "--all") '(cut versions))
          (list 'tasks (list 'tasks) '(cut versions))
          (list 'names (list 'names g) '(cut versions))
          (list 'uses (list 'uses "h") '(cut versions)))))

;; The named trailing clauses removed, only from the end and only when they
;; are there in that order; otherwise the answer is marked as not strippable.
(define (strip answer heads)
  (if (or (null? heads) (not (list? answer)))
      answer
      (let* ((n (length answer)) (k (length heads)))
        (if (and (>= n k)
                 (equal? (map (lambda (c) (and (pair? c) (car c))) (list-tail answer (- n k))) heads))
            (list-head answer (- n k))
            (list 'NOT-STRIPPABLE answer)))))

(define (read-all path)
  (call-with-input-file path
    (lambda (p) (let loop ((acc '())) (let ((x (read p))) (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))

(cond
  ((string=? mode "seed") (seed! (cadr args) (caddr args)) (printf "seeded\n"))
  ((string=? mode "read")
   (let ((store (cadr args)) (out (caddr args)) (strip? (string=? (cadddr args) "strip")))
     (call-with-output-file out
       (lambda (p)
         (for-each
           (lambda (c)
             (let ((a (store-call store (cadr c))))
               ;; THE HUMAN OUTPUT OF THE WHOLE ANSWER: what a person sees.
               (write (list (car c) (if strip? (strip a (caddr c)) a) (render-human a)) p)
               (newline p)))
           (calls store)))
       'replace)
     (printf "read ~a calls\n" (length (calls store)))))
  ((string=? mode "compare")
   (let ((older (read-all (cadr args))) (this (read-all (caddr args)))
         (names '(read read-md read-md-recursive read-recursive outline refs reach search grep whereis
                  commitments tasks names uses)))
     ;; BOTH READINGS ARE WHOLE: each names exactly the calls above, in order,
     ;; each once. Two empty files, or two equal truncated ones, are not a
     ;; comparison.
     (unless (and (equal? (map car older) names) (equal? (map car this) names))
       (printf "NOT A COMPARISON: the readings name ~s and ~s, not ~s\n" (map car older) (map car this) names)
       (exit 1))
     (let loop ((o older) (t this) (bad 0))
       (cond
         ((or (null? o) (null? t))
          (printf "compared ~a verbs (~a and ~a lines); ~a differ\n"
                  (min (length older) (length this)) (length older) (length this) bad)
          (exit (if (and (= bad 0) (= (length older) (length this))) 0 1)))
         (else
          (let* ((name (car (car t)))
                 (same-answer (equal? (cadr (car o)) (cadr (car t))))
                 (same-human (or (memq name '(read reach)) (equal? (caddr (car o)) (caddr (car t))))))
            (printf "~a ~a answer ~a, human ~a\n" (if (and same-answer same-human) "same   " "DIFFERS")
                    name (if same-answer "identical" "differs")
                    (cond ((memq name '(read reach)) "printed whole") (same-human "identical") (else "differs")))
            (loop (cdr o) (cdr t) (if (and same-answer same-human) bad (+ bad 1)))))))))
  (else (printf "usage: seed <store> <scratch-dir> | read <store> <out> keep|strip | compare <older> <this>\n")
        (exit 2)))
