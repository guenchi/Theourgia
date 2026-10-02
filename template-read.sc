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
;; limitations under the License.

;;; (theourgia template-read) -- the store's template, as the store holds it.
;;;
;;; A STORE'S TEMPLATE IS DATA IN ONE BLOCK: top level, kind `template`, its
;;; src a string holding exactly one datum,
;;;
;;;   (template 1 (roots (<slug> doc|path "<path>" "<sentence>") ...)
;;;               (relations (<name> "<direction>" "<meaning>") ...)
;;;               (queries (<verb> <root slug>) ...))
;;;
;;; ONE READER, AND EVERY CONSUMER ASKS IT: `store-template` answers the
;;; datum or #f, and a consumer that gets #f behaves as it does in a store
;;; with no template. The reasons a template block cannot be read are
;;; answered apart (`template-problem`), for the conflicts listing.
;;;
;;; NEVER: THIS LIBRARY READS THE REDUCTION AND NOTHING ELSE. The store
;;; imports it for its conflicts clause, so it may not import the store.
(library (theourgia template-read)
  (export store-template template-problem template-block-ids
          template-roots template-relations template-query
          root-slug root-kind root-path root-sentence parse-template-text
          slug-block-ids query-scope-root root-insert-payload)
  (import (rnrs)
          (only (theourgia reduce) state-read state-block-ids caller-payload-reason)
          (only (theourgia field-reading) field-of))

  ;; THE TEMPLATE BLOCKS: live, of kind template, and at the TOP LEVEL. A block
  ;; of that kind under another block is nothing to the reader, to apply's
  ;; template-present or to export: it is not where a store keeps its template.
  (define (template-block-ids state)
    (filter (lambda (id)
              (let ((row (state-read state id)))
                (and row (eq? (field-of row 'kind) 'template)
                     (let ((p (cdr (assq 'position row)))) (and (pair? p) (eq? (car p) 'root))))))
            (state-block-ids state)))

  ;; -> (ok <datum>) | (problem <reason>) | #f when there is no template block.
  (define (read-template state)
    (let ((ids (template-block-ids state)))
      (cond
        ((null? ids) #f)
        ((pair? (cdr ids)) (list 'problem 'several-template-blocks))
        (else
         (let ((src (field-of (state-read state (car ids)) 'src)))
           (if (not (string? src))
               (list 'problem 'src-not-a-string)
               (parse-template-text src)))))))

  ;; The text of a template, read as exactly one well-formed template datum.
  ;; -> (ok <datum>) | (problem <reason>).
  (define (parse-template-text text)
    (let ((data (guard (e (#t 'unreadable))
                  (let ((in (open-string-input-port text)))
                    (let loop ((out '()))
                      (let ((d (get-datum in)))
                        (if (eof-object? d) (reverse out) (loop (cons d out)))))))))
      (cond
        ((eq? data 'unreadable) (list 'problem 'src-unreadable))
        ((not (= 1 (length data))) (list 'problem 'src-not-one-datum))
        (else
         (let ((d (car data)))
           (cond
             ((not (and (list? d) (>= (length d) 2) (eq? (car d) 'template) (eqv? (cadr d) 1)))
              (list 'problem 'not-a-template-datum))
             ((not (clause d 'roots)) (list 'problem 'roots-absent))
             ((not (and (clause-items (clause d 'roots)) (for-all root? (cdr (clause d 'roots)))))
              (list 'problem 'roots-unreadable))
             ;; TWO ROOTS WITH ONE SLUG OR ONE PATH, or a root slugged like
             ;; the template block itself, would be created as two blocks
             ;; answering to one name.
             ((roots-repeated? (cdr (clause d 'roots))) (list 'problem 'roots-repeated))
             ((not (let ((c (clause d 'relations))) (or (not c) (and (clause-items c) (for-all relation? (cdr c))))))
              (list 'problem 'relations-unreadable))
             ((not (let ((c (clause d 'queries))) (or (not c) (and (clause-items c) (for-all query? (cdr c))))))
              (list 'problem 'queries-unreadable))
             (else (list 'ok d))))))))

;; The clause with this head, or #f. A clause with the head and not a proper
  ;; list is found, so that the reader can call it unreadable rather than absent.
  (define (clause d name)
    (find (lambda (c) (and (pair? c) (eq? (car c) name))) (cddr d)))
  (define (clause-items c) (if (list? c) (cdr c) #f))

  (define (roots-repeated? roots)
    (let loop ((rs roots) (slugs '(template)) (paths '()))
      (cond ((null? rs) #f)
            ((memq (car (car rs)) slugs) #t)
            ((member (caddr (car rs)) paths) #t)
            (else (loop (cdr rs) (cons (car (car rs)) slugs) (cons (caddr (car rs)) paths))))))

  ;; A DOCUMENT ROOT IS CREATED AS IT IS WRITTEN HERE, so it reads only if the
  ;; store would take the insert apply makes for it: the store's own rule for
  ;; a caller's put is asked (a slug with a line terminator becomes a title the
  ;; store refuses), not restated. A template apply would get halfway through
  ;; is not a template. A path is never empty.
  (define (root? r)
    (and (list? r) (= 4 (length r)) (symbol? (car r)) (memq (cadr r) '(doc path))
         (string? (caddr r)) (string? (cadddr r)) (> (string-length (caddr r)) 0)
         (or (eq? (cadr r) 'path)
             (not (caller-payload-reason (root-insert-payload r))))))

  ;; The put apply makes for a document root.
  (define (root-insert-payload r)
    (let ((slug (symbol->string (car r))))
      (list 'put (list (cons 'kind 'doc) (cons 'title slug) (cons 'path (caddr r)) (cons 'slug slug)))))
  (define (relation? r)
    (and (list? r) (= 3 (length r)) (symbol? (car r)) (string? (cadr r)) (string? (caddr r))))
  (define (query? q)
    (and (list? q) (= 2 (length q)) (symbol? (car q)) (symbol? (cadr q))))

  (define (store-template state)
    (let ((r (read-template state))) (and r (eq? (car r) 'ok) (cadr r))))

  ;; The reason a store's template block cannot be read, or #f when there is
  ;; none or it reads.
  (define (template-problem state)
    (let ((r (read-template state))) (and r (eq? (car r) 'problem) (cadr r))))

  (define (template-roots t) (cdr (or (clause t 'roots) '(roots))))
  (define (template-relations t) (cdr (or (clause t 'relations) '(relations))))
  ;; The root slug a verb's default scope is, or #f.
  (define (template-query t verb)
    (let ((q (find (lambda (q) (eq? (car q) verb)) (cdr (or (clause t 'queries) '(queries))))))
      (and q (cadr q))))

;; EVERY LIVE BLOCK WHOSE `slug` FIELD IS THIS STRING, of any kind, by a
  ;; direct scan. Not `read`'s resolver, which tries ids first and would take
  ;; a block whose id happens to be spelled like the slug.
  (define (slug-block-ids state slug)
    (filter (lambda (id)
              (let ((row (state-read state id)))
                (and row (equal? (field-of row 'slug) slug))))
            (state-block-ids state)))

  ;; THE ROOT A VERB'S DEFAULT SCOPE IS, as an id: the one live block carrying
  ;; the slug the template's queries table names for that verb. #f when there
  ;; is no readable template, no entry for the verb, or not exactly one block,
  ;; and the verb then answers for the whole store.
  (define (query-scope-root state verb)
    (let* ((t (store-template state))
           (slug (and t (template-query t verb)))
           (ids (if slug (slug-block-ids state (symbol->string slug)) '())))
      (and (pair? ids) (null? (cdr ids)) (car ids))))

  (define (root-slug r) (car r))
  (define (root-kind r) (cadr r))
  (define (root-path r) (caddr r))
  (define (root-sentence r) (cadddr r)))
