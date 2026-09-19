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

;; THE DERIVED FIELDS STILL ARRIVE, BY THE ROUTE A CALLER USES.
;;
;; `view-fields.ss` asks `view-read` directly. That is the layer the
;; split created, and a tree in which every caller had been left on
;; `state-read` would pass every row of it: the derivation would work
;; perfectly and nobody would see it.
;;
;; SO THESE ROWS GO THROUGH `rpc-dispatch`, and through the three verbs
;; that show a name to a person -- `outline`, `read`, and `read
;; --recursive`. NEVER: Not through `code-field`, which is a fourth caller
;; and would only say that ONE of them was rewired.
;;
;; AND EACH NAME IS READ TWICE, BEFORE AND AFTER THE SOURCE CHANGES. A
;; row that only checked that `alpha` appears is also satisfied by a
;; stored label that happens to say `alpha`; the rows that matter are
;; the ones where the source is edited and the label follows, because
;; only a derivation can do that.

(import (chezscheme) (theourgia store) (theourgia reduce) (theourgia rpc) (theourgia ffi))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/view-rpc-" (number->string (get-process-id))))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))
(define input (string-append root "/input"))
(mkdir-p! input)
(rpc-dispatch store '(init) "t")

(define (write! path text)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (o) (put-bytevector o (string->utf8 text)))))

(write! (string-append input "/m.py") "# module doc\ndef alpha(x):\n    return x\n")
(want "VR-00 the Python file imports" (car (rpc-dispatch store (list 'import-code input) "t")) 'ok)

(define (ids) (map cadr (state-datum (open-and-reduce store))))
(define (kind-of id)
  (let ((b (state-read (open-and-reduce store) id)))
    (and b (cdr (assq 'kind (cdr (assq 'fields b)))))))
(define file-id (car (filter (lambda (id) (eq? 'file (kind-of id))) (ids))))
(define code-id (car (filter (lambda (id) (eq? 'code (kind-of id))) (ids))))

;; THE ANSWER IS ASKED FOR ONE FIELD, so a row names the field it is
;; about rather than comparing a whole record that also carries the
;; block's bytes, its position and its edges.
(define (rpc-field id key . args)
  (let ((answer (apply rpc-dispatch store (append (list 'read id) args) (list "t"))))
    (and (pair? answer) (eq? 'ok (car answer))
         (let* ((record (cadr answer))
                (fs (assq 'fields record))
                (p (and fs (assq key (cdr fs)))))
           (and p (cdr p))))))

(define (outline-text)
  (let ((answer (rpc-dispatch store '(outline) "t")))
    (and (pair? answer) (eq? 'ok (car answer)) (cadr (cadr answer)))))

(define (contains? text needle)
  (let ((n (string-length text)) (m (string-length needle)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? needle (substring text i (+ i m))) #t)
            (else (loop (+ i 1)))))))

(want "VR-01 read gives the name derived from the source"
      (rpc-field code-id 'name) "alpha")
(want "VR-02 outline shows that name too"
      (contains? (outline-text) "alpha") #t)

;; `read --recursive` ON THE FILE BLOCK reaches the child, which is a
;; different assembly path from `read` on the child itself.
(define (recursive-text)
  (let ((answer (rpc-dispatch store (list 'read file-id "--recursive") "t")))
    (call-with-string-output-port (lambda (p) (write answer p)))))

(want "VR-03 read --recursive carries the child's derived name"
      (contains? (recursive-text) "alpha") #t)

;; NOW THE SOURCE CHANGES AND EVERY LABEL HAS TO FOLLOW.
(want "VR-04 the source edit is accepted"
      (car (rpc-dispatch store (list 'set code-id "src" "# module doc\ndef beta(x):\n    return x\n") "t"))
      'ok)

(want "VR-05 read follows the source"
      (rpc-field code-id 'name) "beta")
(want "VR-05 outline follows the source"
      (list (contains? (outline-text) "beta") (contains? (outline-text) "alpha"))
      '(#t #f))
(want "VR-05 read --recursive follows the source"
      (list (contains? (recursive-text) "beta") (contains? (recursive-text) "alpha"))
      '(#t #f))

;; THE DATUM SIDE, WHICH DERIVES ITS NAME FROM A BODY RATHER THAN FROM
;; SOURCE TEXT -- a different branch of `view-read`.
(with-store-write store
  (lambda (s v)
    (list (list 'insert 'root #f
                (list '(kind . code) '(mode . datum) '(body define gamma 1)))))
  "t")
(define datum-id
  (car (filter (lambda (id) (and (not (equal? id file-id)) (not (equal? id code-id))))
               (ids))))

(want "VR-06 read gives a datum block's derived name"
      (rpc-field datum-id 'name) 'gamma)
(want "VR-06 and the whole list of names it binds"
      (rpc-field datum-id 'names) '(gamma))
(want "VR-07 outline shows the datum name"
      (contains? (outline-text) "gamma") #t)

(want "VR-08 the body edit is accepted"
      (caar (with-store-write store
              (lambda (s v) (list (list 'set datum-id 'body (list 'define 'delta 1))))
              "t"))
      'ok)
(want "VR-09 read follows the body"
      (rpc-field datum-id 'name) 'delta)
(want "VR-09 outline follows the body"
      (list (contains? (outline-text) "delta") (contains? (outline-text) "gamma"))
      '(#t #f))

(printf "rows: ~a\n~a failures\nview-rpc complete\n" rows bad)
