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

;;; (theourgia view) -- what a block LOOKS LIKE, as against what it says.
;;;
;;; TWO READERS, AND THE DIFFERENCE IS WHO IS ASKING.
;;;
;;;   `state-read`  gives the fields that were written. It is what the
;;;                 reduction, every hash and every `--based-on` are
;;;                 taken over, so its answer must depend on the log and
;;;                 on nothing else.
;;;   `view-read`   gives those fields with the derived ones filled in:
;;;                 a text block's `name` and `doc`, read out of its
;;;                 source by the language table, and a datum block's
;;;                 `names`/`name`, read out of its body.
;;;
;;; WHY THEY ARE NOT ONE PROCEDURE. Until v129 they were, and the derived
;;; fields therefore went into the reduction -- which made the language
;;; table, a thing edited at runtime by `register-language!`, part of
;;; what a stored block meant. Register a language whose name rule
;;; differs and every draft's `--based-on` in the tree could go stale at
;;; once, with nothing written to the log to explain it.
;;;
;;; THE SPLIT IS NOT A CACHE. `view-read` recomputes; there is no stored
;;; derived field to go out of date, and no second place for the truth to
;;; live. The cost is a re-derivation per read, which is what the RPC
;;; layer was doing anyway.
;;;
;;; WHO CALLS WHICH. Anything answering a person or an editor wants the
;;; view: `rpc.ss`'s `read`, `outline` and `title-of`, and
;;; `code-project.ss`'s `code-field`. Anything deciding what is stored,
;;; what conflicts or what a hash is over wants `state-read`.

(library (theourgia view)
  (export view-read)
  (import (rnrs)
          (only (theourgia reduce) state-read)
          (only (theourgia languages) language-for-name)
          (only (theourgia text-code) text-properties)
          (only (theourgia datum-code) datum-names))

  (define (sorted-fields fields)
    (list-sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b))))
               fields))
  (define (with-fields b fields)
    (map (lambda (p) (if (eq? (car p) 'fields) (cons 'fields (sorted-fields fields)) p)) b))

  ;; A DATUM BLOCK'S NAMES COME OUT OF ITS BODY. `names` is every name the
  ;; form binds and `name` is the first of them, so a `define-values`
  ;; answers with the whole list and a heading with the one it has.
  (define (datum-view b)
    (let* ((fields (cdr (assq 'fields b)))
           (body (assq 'body fields))
           (names (datum-names (and body (cdr body)))))
      (with-fields b
        (append (filter (lambda (f) (not (memq (car f) '(name names)))) fields)
                (list (cons 'names names))
                (if (pair? names) (list (cons 'name (car names))) '())))))

  ;; A TEXT BLOCK'S NAME AND DOC COME OUT OF ITS SOURCE, through the
  ;; language named in its own `lang` field. A language the table does
  ;; not know leaves both absent rather than guessed at.
  (define (text-view b fields src lang)
    (let* ((entry (language-for-name lang))
           (properties (and entry (or (bytevector? src) (string? src))
                            (text-properties entry src))))
      (with-fields b
        (append (filter (lambda (f) (not (memq (car f) '(name doc)))) fields)
                (if (and properties (car properties)) (list (cons 'name (car properties))) '())
                (if properties (list (cons 'doc (cadr properties))) '())))))

  (define (view-read r id)
    (let* ((b (state-read r id))
           (fs (and b (cdr (assq 'fields b))))
           (get (lambda (k) (let ((p (and fs (assq k fs)))) (and p (cdr p))))))
      (cond
        ((not (and b (eq? (get 'kind) 'code))) b)
        ((eq? (get 'mode) 'datum) (datum-view b))
        ((eq? (get 'mode) 'text) (text-view b fs (get 'src) (get 'lang)))
        (else b))))
)
