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

;; THE SEARCH, TEXT REFERENCES AND THE DEFINITIONS ANSWER AS BEFORE: a reading,
;; not a row, since one tree cannot hold two products. The query language
;; took the search walk into a procedure of the state and reads text
;; references and the definitions index itself; no answer of search, grep,
;; whereis or refs may change. One seeded store -- blocks, a subtree, text
;; references, a datum library -- read by the older product's handlers and by
;; this tree's; every answer and every human output must be identical.
;;
;; usage, from this directory, CHEZSCHEMELIBDIRS naming the product to use:
;;   scheme --script query-base.ss seed <store> <scratch-dir>   (this tree)
;;   scheme --script query-base.ss read <store> <out>           (each product)
;;   scheme --script query-base.ss compare <older-out> <this-out>
;; <scratch-dir> is a new directory the seed writes a one-definition library
;; into and imports. `seed` exits 1 when a call fails; `compare` exits 0 only
;; when both readings name exactly the calls below, in order, each once, and
;; every answer and human output is identical.
(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) state-read state-block-ids)
        (only (theourgia render) render-human))
(register-verbs! extension-verbs)
(define args (cdr (command-line)))
(define mode (car args))
(define (store-call store a) (rpc-dispatch store a "query-base"))
(define (field-of s id f)
  (let ((row (state-read s id)))
    (and row (let ((e (assq f (cdr (assq 'fields row))))) (and e (cdr e))))))
(define (id-by-title store title)
  (let ((s (open-and-reduce store)))
    (find (lambda (id) (equal? (field-of s id 'title) title)) (state-block-ids s))))

(define (seed! store lib)
  (let ((run (lambda a
               (let ((answer (store-call store a)))
                 (unless (rpc-ok? answer)
                   (printf "NOT SEEDED: ~s answered ~s\n" a answer)
                   (exit 1))
                 answer))))
    (run 'init)
    (run 'insert "--title" "alpha osprey" "--text" "alpha text")
    (let ((a (id-by-title store "alpha osprey")))
      (run 'insert "--title" "beta osprey" "--text" "beta text" "--under" a)
      (let ((b (id-by-title store "beta osprey")))
        (run 'link a "documents" b)
        (run 'insert "--title" "gamma" "--text" (string-append "gamma cites [[" a "]] and [[" b "]]"))))
    (system (string-append "mkdir -p '" lib "'"))
    (call-with-output-file (string-append lib "/qq.sc")
      (lambda (p) (put-string p "(library (qq) (export qx) (import (rnrs))\n(define (qx) 1))\n"))
      'replace)
    (run 'import-code lib "--datum")))

(define names '(search search-all grep whereis refs-a refs-b))
(define (calls store)
  (let ((a (id-by-title store "alpha osprey")) (b (id-by-title store "beta osprey")))
    (list (list 'search 'search "osprey") (list 'search-all 'search "text" "--all")
          (list 'grep 'grep "text") (list 'whereis 'whereis "qx")
          (list 'refs-a 'refs a) (list 'refs-b 'refs b))))

(define (read-all path)
  (call-with-input-file path
    (lambda (p) (let loop ((acc '())) (let ((x (read p))) (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))

(cond
  ((string=? mode "seed") (seed! (cadr args) (caddr args)) (printf "seeded\n"))
  ((string=? mode "read")
   (let ((store (cadr args)) (out (caddr args)))
     (call-with-output-file out
       (lambda (p)
         (for-each (lambda (c) (let ((a (store-call store (cdr c))))
                                 (write (list (car c) a (render-human a)) p) (newline p)))
                   (calls store)))
       'replace)
     (printf "read ~a calls\n" (length (calls store)))))
  ((string=? mode "compare")
   (let ((older (read-all (cadr args))) (this (read-all (caddr args))))
     (unless (and (equal? (map car older) names) (equal? (map car this) names))
       (printf "NOT A COMPARISON: the readings name ~s and ~s, not ~s\n" (map car older) (map car this) names)
       (exit 1))
     (let ((bad (filter (lambda (pair) (not (equal? (cdr (car pair)) (cdr (cadr pair))))) (map list older this))))
       (for-each (lambda (pair) (printf "~a ~a\n" (if (member pair bad) "DIFFERS" "same   ") (car (car pair))))
                 (map list older this))
       (printf "compared ~a verbs; ~a differ\n" (length this) (length bad))
       (exit (if (null? bad) 0 1)))))
  (else (printf "usage: seed <store> <scratch-dir> | read <store> <out> | compare <older> <this>\n")
        (exit 2)))
