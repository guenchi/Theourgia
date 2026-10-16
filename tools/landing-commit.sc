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

;; Commit a frozen writer's drafts, only if the store is still at the cut they
;; were frozen at.
;;
;;   scheme --script tools/landing-commit.sc <store> <freeze-dir>
;;
;; <freeze-dir> holds the freeze's draft list, DRAFTS.sexp as
;; tools/drafts-dump.sc writes it: the store, the writer, the pinned cut (the
;; join of the drafts' cuts) and each draft's block and version.
;;
;; THE CUT CHECK COMES FIRST. A commit on another block since the freeze moves
;; the store's applied cut while leaving every frozen draft's baseline and
;; version as they were, so the store itself would accept the commit -- and
;; what was gated, the export at the pinned cut, would not be what lands. So
;; when the applied cut is not the pinned cut, nothing is committed and the
;; answer is (error cut-moved (pinned <cut>) (now <cut>)): the coder rebases
;; the drafts onto the new cut and freezes again.
;;
;; THEN THE DRAFTS ARE COMMITTED BY NAME, each with its frozen version
;; (--working-version <block>=<version>), so a draft rewritten since the
;; freeze is refused by the store (working-version-changed).
;;
;; ONE COMMITTER: the cut is read and the commit made in one run, and nothing
;; holds the store between them. The landing protocol makes this the only
;; path that commits to the store, run by one session at a time.
;;
;; A refusal prints (error <kind> ...) and exits 1; the commit's own answer is
;; printed as it is, and the run exits 0 when it is ok.
(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia reduce) reduce-applied-cut)
        (only (theourgia store) open-and-reduce)
        (only (theourgia log) store-id-of)
        (only (theourgia ffi) entry-bytes))

(define actor "landing-commit")
(define (refuse kind . details) (raise (cons 'error (cons kind details))))
;; THE MANIFEST IS READ AS tools/drafts-restore.sc READS IT (clause,
;; read-manifest and cut-set are the same there): each tool is a script on
;; its own, and the fixture that writes a manifest with drafts-dump reads it
;; back with both.
(define (clause name datum)
  (let ((c (and (list? datum) (assq name (cdr datum))))) (and c (cadr c))))
(define (cut-set c)
  (list-sort (lambda (a b) (string<? (car a) (car b)))
             (filter (lambda (e) (and (pair? e) (integer? (cdr e)) (> (cdr e) 0))) c)))
(define (read-manifest dir)
  (let ((m (guard (e (#t #f))
             (read (open-string-input-port (utf8->string (entry-bytes (string-append dir "/DRAFTS.sexp"))))))))
    (unless (and (pair? m) (eq? (car m) 'drafts) (list? m)) (refuse 'manifest-unreadable (list 'dir dir)))
    m))

(define (land store dir)
  (let* ((m (read-manifest dir))
         (writer (clause 'writer m))
         (pinned (clause 'pinned-cut m))
         (drafts (filter (lambda (d) (and (pair? d) (eq? (car d) 'draft))) (cdr m))))
    (unless (equal? (clause 'store m) (store-id-of store))
      (refuse 'other-store (list 'frozen (clause 'store m)) (list 'here (store-id-of store))))
    (when (null? drafts) (refuse 'nothing-frozen (list 'dir dir)))
    (let ((now (reduce-applied-cut (open-and-reduce store))))
      (unless (equal? (cut-set now) (cut-set pinned))
        (refuse 'cut-moved (list 'pinned pinned) (list 'now now))))
    (rpc-dispatch store
                  (append (list 'commit)
                          (map (lambda (d) (clause 'block d)) drafts)
                          (list "--writer" writer)
                          (apply append
                                 (map (lambda (d)
                                        (list "--working-version"
                                              (string-append (clause 'block d) "=" (clause 'version d))))
                                      drafts)))
                  actor)))

(define (main args)
  (if (not (= 2 (length args)))
      (begin (write '(usage (landing-commit <store> <freeze-dir>))) (newline) (exit 2))
      (let ((a (guard (e ((and (pair? e) (eq? (car e) 'error)) e))
                 (land (car args) (cadr args)))))
        (write a)
        (newline)
        (exit (if (and (pair? a) (eq? (car a) 'ok)) 0 1)))))

(main (cdr (command-line)))
