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

;;; (theourgia template) -- applying a template to a store, and printing it.
;;;
;;; APPLY CREATES AND NEVER CHANGES. It adds the template block and each
;;; document root the store does not have yet, and answers the ids it made.
;;; A root is found by a direct scan of the live blocks: the blocks whose
;;; `slug` field is the root's slug, and the top-level documents at the
;;; root's path. Exactly one live document with that slug at that path: the
;;; root is there and is left alone. Neither: it is created. Anything else
;;; refuses, by name, and nothing is written:
;;;
;;;   template-present   the store already has a template block
;;;   slug-conflict      two live blocks carry the slug
;;;   template-mismatch  the block with the slug is not a top-level document
;;;                      at the root's path
;;;   path-occupied      a top-level document without the slug holds the path
;;;
;;; A path root (docs/, code/) names where things go and is not a block, so
;;; there is nothing to create for it. Deleted blocks do not count.
;;;
;;; Entered on dispatch of `template` and by `init --template`, never at a
;;; start that does neither.
(library (theourgia template)
  (export template-verb template-apply! template-datum-for)
  (import (rnrs)
          (only (theourgia rpc) dispatch-helper)
          (only (theourgia arguments) argument-option)
          (only (theourgia store) open-and-reduce with-store-write premises-preflight)
          (only (theourgia reduce) state-read state-block-ids block-id)
          (only (theourgia field-reading) field-of written-text)
          (only (theourgia ffi) entry-bytes)
          (only (theourgia templates) built-in-template built-in-template-names)
          (only (theourgia template-read) parse-template-text template-block-ids template-roots
                slug-block-ids root-slug root-kind root-path root-insert-payload template-problem)
          (only (theourgia extensions) template-usage))

  ;; `template apply <name>`, `template apply --file <template-file>`,
  ;; `template export`, with the eight arguments every handler takes.
  (define (template-verb store actor args req options state writer cwd)
    (let ((file (argument-option options "--file")))
      (cond
        ((and (equal? args '("export")) (not file))
         ((dispatch-helper 'guarded) (lambda () (template-export ((dispatch-helper 'reduction-for) store state)))))
        ((or (and (= 2 (length args)) (equal? (car args) "apply") (not file))
             (and (equal? args '("apply")) file))
         ((dispatch-helper 'guarded)
          (lambda ()
            (let ((d (template-datum-for (and (pair? (cdr args)) (cadr args)) file)))
              (if (eq? (car d) 'ok)
                  (let-values (((check finish run) (premises-preflight store (argument-option options "--premises") #f)))
                    (template-apply! store actor req (cadr d) check finish run))
                  d)))))
        (else ((dispatch-helper 'usage) template-usage)))))

  ;; A NAME IS A BUILT-IN'S NAME AND NOTHING ELSE: one that is not a built-in
  ;; is refused with the names there are, never looked for as a file. A file
  ;; is named by --file, whose path the caller's directory has already been
  ;; applied to. -> (ok <datum>) | (error ...)
  (define (template-datum-for name file)
    (cond
      (file
       ;; A FILE THAT CANNOT BE READ IS THE FILESYSTEM'S ANSWER, raised to the
       ;; dispatcher's table like every other verb's, not caught here.
       (let ((p (parse-template-text (utf8->string (entry-bytes file)))))
         (if (eq? (car p) 'ok) p (list 'error 'template-unreadable (list 'file file) (list 'reason (cadr p))))))
      ((built-in-template name) => (lambda (d) (list 'ok d)))
      (else (list 'error 'unknown-template (list 'name name) (cons 'known (built-in-template-names))))))

  ;; A TEMPLATE THE READER CANNOT READ IS NOT EXPORTED AS IF IT WERE ONE: the
  ;; answer names the reason, as conflicts does, and `read` still shows the
  ;; block's src to whoever means to fix it.
  (define (template-export state)
    (let ((ids (template-block-ids state)))
      (cond ((null? ids) (list 'error 'no-template))
            ((template-problem state)
             => (lambda (r) (list 'error 'template-unreadable (list 'reason r) (cons 'ids ids))))
            (else
             (let ((src (field-of (state-read state (car ids)) 'src)))
               (if (string? src)
                   (list 'ok (list 'text (string-append src (if (and (> (string-length src) 0)
                                                                    (char=? #\newline (string-ref src (- (string-length src) 1))))
                                                               "" "\n"))))
                   (list 'error 'template-src-not-a-string (list 'id (car ids)))))))))

  ;; ---- what apply would do ---------------------------------------------------

  (define (top-level-doc-at? state id path)
    (let ((row (state-read state id)))
      (and row
           (eq? (field-of row 'kind) 'doc)
           (equal? (field-of row 'path) path)
           (eq? 'root (let ((p (cdr (assq 'position row)))) (and (pair? p) (car p)))))))

  (define (top-level-docs-at state path)
    (filter (lambda (id) (top-level-doc-at? state id path)) (state-block-ids state)))

  ;; -> (refuse <answer>) | (create (<slug> <fields>) ...)
  (define (apply-plan state datum)
    (let ((present (template-block-ids state)))
      (if (pair? present)
          (list 'refuse (list 'error 'template-present (cons 'ids present)))
          (let loop ((roots (filter (lambda (r) (eq? (root-kind r) 'doc)) (template-roots datum)))
                     (creates '()))
            (if (null? roots)
                (cons 'create
                      (append (reverse creates)
                              (list (list 'template
                                          (list (cons 'kind 'template) (cons 'title "template")
                                                (cons 'slug "template") (cons 'src (written-text datum)))))))
                (let* ((r (car roots))
                       (slug (symbol->string (root-slug r)))
                       (path (root-path r))
                       (with-slug (slug-block-ids state slug))
                       (at-path (top-level-docs-at state path)))
                  (cond
                    ((and (pair? with-slug) (pair? (cdr with-slug)))
                     (list 'refuse (list 'error 'slug-conflict (list 'slug slug) (cons 'ids with-slug))))
                    ((and (pair? with-slug) (not (top-level-doc-at? state (car with-slug) path)))
                     (list 'refuse (list 'error 'template-mismatch (list 'slug slug) (list 'id (car with-slug)))))
                    ((pair? (filter (lambda (id) (not (member id with-slug))) at-path))
                     (list 'refuse (list 'error 'path-occupied (list 'path path)
                                         (list 'id (car (filter (lambda (id) (not (member id with-slug))) at-path))))))
                    ((pair? with-slug) (loop (cdr roots) creates))
                    (else
                     (loop (cdr roots)
                           (cons (list (root-slug r) (cadr (root-insert-payload r)))
                                 creates))))))))))

  ;; NEVER: A REFUSAL WRITES NOTHING. The plan is made against the store as
  ;; it is before any write session opens, so a refusal never begins one; and
  ;; made again inside the session, against the state the write lands on, so
  ;; that a root created in between refuses instead of being created twice.
  ;; CHECK, FINISH and RUN are the request's premises (store.sc,
  ;; premises-preflight): the check is the write's preflight, finish is given
  ;; the answer of the write -- not a plan refused before it -- and run holds
  ;; the write, so a raise inside it says so.
  (define (template-apply! store actor req datum . premises)
    (let ((before (apply-plan (open-and-reduce store) datum))
          (check (and (pair? premises) (car premises)))
          (finish (if (and (pair? premises) (pair? (cdr premises))) (cadr premises) (lambda (a) a)))
          (run (if (and (pair? premises) (pair? (cdr premises)) (pair? (cddr premises)))
                   (caddr premises)
                   (lambda (thunk) (thunk)))))
      (if (eq? (car before) 'refuse)
          (cadr before)
          (run (lambda ()
          (finish
          (let* ((late #f)
                 (inner #f)
                 (answers
                   (with-store-write
                     store
                     (lambda (state view)
                       (let ((p (apply-plan state datum)))
                         (set! inner p)
                         (if (eq? (car p) 'refuse)
                             (begin (set! late (cadr p)) '())
                             (map (lambda (c) (list 'insert 'root #f (cadr c))) (cdr p)))))
                     actor req check))
                 (made (if (and inner (eq? (car inner) 'create)) (map car (cdr inner)) '())))
            (cond
              (late late)
              ((not (for-all (lambda (a) (and (pair? a) (eq? (car a) 'ok))) answers))
               (list 'error 'template-apply-failed (cons 'answers answers)))
              (else
               (list 'ok (cons 'created
                               (map (lambda (slug a) (list slug (event-block-id a))) made answers))))))))))))

  (define (event-block-id answer)
    (let ((ev (assq 'events (cdr answer))))
      (and ev (pair? (cadr ev)) (block-id (car (car (cadr ev))) (cdr (car (cadr ev))))))))
