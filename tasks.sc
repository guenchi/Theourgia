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

;;; (theourgia tasks) -- the tasks of a store, and what each implements.
;;;
;;; A TASK IS A BLOCK OF KIND `task`. Its `status` says todo, doing, done or
;;; dropped, read leniently by the same procedure that reads a decision's;
;;; its `batch` is an ordinary text field. An `implements` edge from a task
;;; names what it carries out, and a task with no such edge to a live
;;; decision is `(unlinked)`: written down, attached to nothing.
;;;
;;; THE DEFAULT SCOPE IS THE TEMPLATE'S: the root its queries table names for
;;; `tasks`, when the store has a readable template and exactly one block
;;; carries that root's slug. Otherwise, and with `--under root`, the whole
;;; store. Nothing is written.
;;;
;;; Entered on dispatch of `tasks` only (the registry in rpc.sc).
(library (theourgia tasks)
  (export tasks-verb tasks-answer)
  (import (rnrs)
          (only (theourgia rpc) dispatch-helper)
          (only (theourgia arguments) argument-option)
          (only (theourgia reduce) state-read state-block-ids state-field-contested? relation-kind)
          (only (theourgia project) subtree-ids)
          (only (theourgia field-reading) field-of field-missing? lenient-status task-statuses
                written-text rows-left-out)
          (only (theourgia template-read) query-scope-root)
          (only (theourgia extensions) tasks-usage))

  (define (tasks-verb store actor args req options state writer cwd)
    (let ((status (argument-option options "--status"))
          (batch (argument-option options "--batch"))
          (under (argument-option options "--under")))
      (cond
        ((or (not (null? args)) (and under (= 0 (string-length under))))
         ((dispatch-helper 'usage) tasks-usage))
        ((and status (not (memq (string->symbol status) task-statuses)))
         (list 'error 'bad-request 'status-not-known (list 'status status) (cons 'known task-statuses)))
        (else
         ((dispatch-helper 'guarded)
          (lambda ()
            (tasks-answer ((dispatch-helper 'reduction-for) store state)
                          (and status (string->symbol status)) batch under)))))))

  (define (live? state id)
    (let ((row (state-read state id))) (and row (not (cdr (assq 'deleted row))))))

  (define (status-of state id row)
    (let ((s (lenient-status (field-of row 'status) task-statuses (state-field-contested? state id 'status (field-of row 'status)))))
      (if (pair? s) s (list s))))

  (define (batch-of row)
    (let ((b (field-of row 'batch)))
      (cond ((field-missing? b) '(absent))
            ((string? b) (list b))
            (else (list 'unreadable (written-text b))))))

  ;; One task's row, or #f for a block that is not a live task.
  (define (task-row state id)
    (let ((row (state-read state id)))
      (and row (not (cdr (assq 'deleted row))) (eq? (field-of row 'kind) 'task)
           (let* ((targets (map cdr (filter (lambda (e) (eq? (relation-kind state (car e)) 'implements))
                                            (cdr (assq 'edges row)))))
                  (implements (map (lambda (to) (list to (if (live? state to) 'live 'tombstoned))) targets))
                  ;; AN EDGE TO A DELETED BLOCK, OR TO ONE THAT IS NOT A
                  ;; DECISION, LINKS THE TASK TO NOTHING THAT IS OWED.
                  (linked (exists (lambda (to)
                                    (and (live? state to)
                                         (eq? (field-of (state-read state to) 'kind) 'decision)))
                                  targets)))
             (append (list 'task id
                           (let ((t (field-of row 'title))) (list 'title (if (string? t) t (written-text t))))
                           (cons 'status (status-of state id row))
                           (cons 'batch (batch-of row))
                           (cons 'implements implements))
                     (if linked '() (list '(unlinked))))))))

;; NEVER: A SCOPE THE CALLER DID NOT ASK FOR IS SAID. When the template chose
  ;; the scope, the last item is (scope <root-id> (outside <n>)): n is the
  ;; number of rows this same request would list with --under root that this
  ;; answer does not, so a narrowed listing never reads as a whole one and the
  ;; count obeys the same filters as the rows. An item and not a clause beside
  ;; the items: the human rendering prints items and drops other clauses.
  (define (tasks-answer state status batch under)
    (let* ((root (and (not under) (query-scope-root state 'tasks)))
           (scope (cond ((and under (equal? under "root")) #f)
                        (under (subtree-ids state under))
                        (root (subtree-ids state root))
                        (else #f))))
      (if (and under (not (equal? under "root")) (not scope))
          ((dispatch-helper 'unknown-id) state under)
          (let ((rows (task-rows state scope status batch)))
            (append
              ((dispatch-helper 'items)
               (if (and root scope)
                   (append rows (list (list 'scope root (list 'outside (rows-left-out rows (task-rows state #f status batch))))))
                   rows))
              ;; Each task listed; the scope item names a root, not a result.
              ((dispatch-helper 'receipt) state (map cadr rows)))))))

  ;; The rows for one scope (#f: the whole store), with the request's filters.
  (define (task-rows state scope status batch)
    (filter (lambda (t)
              (and t
                   (or (not status) (equal? (cdr (assq 'status (cddr t))) (list status)))
                   (or (not batch) (equal? (cdr (assq 'batch (cddr t))) (list batch)))))
            (map (lambda (id) (task-row state id))
                 (or scope (state-block-ids state))))))
