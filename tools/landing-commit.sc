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
;;   scheme --script tools/landing-commit.sc <store> <freeze-dir> [--start <tag>]
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
;; freeze is refused by the store (working-version-changed). The structural
;; drafts go in the same commit, one batch (working.sc): --working-new
;; <provisional id>=<version> and --working-delete <holder>=<version>; the
;; answer's (created (<provisional id> . <holder id>) ...) is the mapping a
;; review collected after the landing reads.
;;
;; A REFUSED COMMIT LEAVES THE START TAG it was preceded by: the tag binds the
;; pinned cut and is harmless, but it is a record, so the store's cut has
;; moved past the drafts' (a retry is cut-moved): the drafts are rebased onto
;; it (a structural draft discarded and written again) and frozen again, and
;; the next attempt takes a new start name (the old one is refused
;; start-tag-exists).
;;
;; THE START TAG, with --start <tag>: publish-landing publishes from a start
;; tag whose export is the git base, so the landing names its start before it
;; commits. A name already bound is refused before anything is written
;; (error start-tag-exists (tag <name>)): binding it again would move an
;; earlier landing's start. After the cut check the store is tagged. A tag
;; binds the cut the store has applied when its record is written, and its
;; answer carries that cut, so a commit made between the check and the tag
;; shows there: nothing is committed, and the answer is (error start-tag-cut
;; (tag <name>) (pinned <cut>) (bound <cut>)). The tag stays (it is a tag,
;; not a commit), and a retry takes another name. A landing that commits
;; adds (start-tag <name> (cut <cut>)) to the commit's answer.
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
        (only (theourgia store) open-and-reduce store-tags)
        (only (theourgia log) store-id-of)
        (only (theourgia ffi) entry-bytes hold-point!))

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

;; -> whether NAME is bound as a tag now, settled or not
(define (tag-bound? store name)
  (exists (lambda (t) (equal? (cadr t) name)) (store-tags store)))

;; -> the cut the new start tag binds, when it is the pinned cut; else the
;; refusal, with the tag written
(define (start-tag! store name pinned)
  (hold-point! 'landing-before-tag)
  (let ((a (rpc-dispatch store (list 'tag name) actor)))
    (unless (and (pair? a) (eq? (car a) 'ok))
      (refuse 'start-tag-refused (list 'tag name) (list 'answer a)))
    (let ((bound (clause 'cut a)))
      (unless (equal? (cut-set bound) (cut-set pinned))
        (refuse 'start-tag-cut (list 'tag name) (list 'pinned pinned) (list 'bound bound)))
      bound)))

(define (land store dir start)
  (let* ((m (read-manifest dir))
         (writer (clause 'writer m))
         (pinned (clause 'pinned-cut m))
         (drafts (filter (lambda (d) (and (pair? d) (eq? (car d) 'draft))) (cdr m)))
         (news (filter (lambda (d) (and (pair? d) (eq? (car d) 'new))) (cdr m)))
         (deletes (filter (lambda (d) (and (pair? d) (eq? (car d) 'delete))) (cdr m))))
    (let ((here (store-id-of store)))
      (unless (equal? (clause 'store m) here)
        (refuse 'other-store (list 'frozen (clause 'store m)) (list 'here here))))
    (when (and (null? drafts) (null? news) (null? deletes)) (refuse 'nothing-frozen (list 'dir dir)))
    (when (and start (tag-bound? store start)) (refuse 'start-tag-exists (list 'tag start)))
    (let ((now (reduce-applied-cut (open-and-reduce store))))
      (unless (equal? (cut-set now) (cut-set pinned))
        (refuse 'cut-moved (list 'pinned pinned) (list 'now now))))
    (let* ((bound (and start (start-tag! store start pinned)))
           (a (rpc-dispatch store
                            (append (list 'commit)
                                    (map (lambda (d) (clause 'block d)) drafts)
                                    (list "--writer" writer)
                                    (apply append
                                           (map (lambda (d)
                                                  (list "--working-version"
                                                        (string-append (clause 'block d) "=" (clause 'version d))))
                                                drafts))
                                    (apply append
                                           (map (lambda (d)
                                                  (list "--working-new"
                                                        (string-append (clause 'provisional d) "=" (clause 'version d))))
                                                news))
                                    (apply append
                                           (map (lambda (d)
                                                  (list "--working-delete"
                                                        (string-append (clause 'holder d) "=" (clause 'version d))))
                                                deletes)))
                            actor)))
      (if (and bound (pair? a) (eq? (car a) 'ok))
          (append a (list (list 'start-tag start (list 'cut bound))))
          a))))

(define (main args)
  (if (not (or (= 2 (length args))
               (and (= 4 (length args)) (string=? (caddr args) "--start"))))
      (begin (write '(usage (landing-commit <store> <freeze-dir> ["--start" <tag>]))) (newline) (exit 2))
      (let ((a (guard (e ((and (pair? e) (eq? (car e) 'error)) e))
                 (land (car args) (cadr args) (and (= 4 (length args)) (cadddr args))))))
        (write a)
        (newline)
        (exit (if (and (pair? a) (eq? (car a) 'ok)) 0 1)))))

(main (cdr (command-line)))
