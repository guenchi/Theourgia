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

;;; (theourgia templates) -- the templates this build ships, as data.
;;;
;;; NEVER: DATA, AND ENTERED ONLY BY WHAT APPLIES A TEMPLATE. A template is a
;;; quoted datum in the shape (theourgia template-read) reads; nothing here
;;; runs. A store's own template is a block, and changing it is an ordinary
;;; write: these are only the starting points `init --template` and
;;; `template apply` copy from. They live in a library rather than in files
;;; because nothing resolves a data file from the library path, and a
;;; compiled install ships none.
(library (theourgia templates)
  (export built-in-template built-in-template-names)
  (import (rnrs base))

  (define templates
    (list
      (cons "project"
            '(template 1
               (roots
                 (design doc "design.md"
                         "Record each decision as a block of kind decision under design, with the ruling and its reason.")
                 (tasks doc "tasks.md"
                        "Record each task as a block of kind task under tasks, with a status of todo, doing, done or dropped.")
                 (docs path "docs/"
                       "Write documents for readers as top-level docs whose path starts with docs/.")
                 (code path "code/"
                       "Import code so that the paths of its files start with code/."))
               (relations
                 (implements "<task or code> -> <decision>" "it carries out the decision")
                 (depends-on "<task or code> -> <task or code>" "it needs the other first")
                 (supersedes "<decision> -> <decision>" "it replaces the earlier decision")
                 (refutes "<decision or doc> -> <decision or doc>" "it shows the other is wrong")
                 (verifies "<test> -> <code or decision>" "it checks it")
                 (conflicts-with "<decision> -> <decision>" "the two cannot both hold")
                 (documents "<doc> -> <code or decision>" "it explains it"))
               (queries (tasks tasks) (commitments design))))
      (cons "memory"
            '(template 1
               (roots
                 (lessons doc "lessons.md"
                          "Record each lesson under lessons: what happened, why, and what to do next time.")
                 (decisions doc "decisions.md"
                            "Record each decision as a block of kind decision under decisions.")
                 (references doc "references.md"
                             "Record each pointer to an outside resource under references."))
               (relations
                 (supersedes "<block> -> <block>" "it replaces the earlier one"))
               (queries (commitments decisions))))))

  ;; The datum of a built-in template, or #f.
  (define (built-in-template name)
    (let loop ((ts templates))
      (cond ((null? ts) #f)
            ((equal? (car (car ts)) name) (cdr (car ts)))
            (else (loop (cdr ts))))))

  (define (built-in-template-names) (map car templates)))
