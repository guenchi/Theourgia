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

;; Dump a writer's drafts: each draft's block, exact bytes, based-on, cut and
;; version, into a new directory, so they can be kept (committed to another
;; repository) and written back by tools/drafts-restore.sc. Drafts do not
;; travel with a store's git: the working areas are ignored there.
;;
;;   scheme --script tools/drafts-dump.sc <store> --writer <W> <dir>
;;
;; <dir> must not exist; it is made, and holds DRAFTS.sexp and one
;; <n>.bytes file per draft. DRAFTS.sexp is one datum:
;;
;;   (drafts (store "<store id>") (writer "<W>") (pinned-cut <cut>)
;;           (draft (block "<id>") (version "<hex>") (based-on "<hex>") (cut <cut>)
;;                  (bytes "<n>.bytes") (sha256 "<hex>"))
;;           ...)
;;
;; The pinned cut is the join of the drafts' cuts: the cut a landing of these
;; drafts must find the store at (tools/landing-commit.sc).
;;
;; EVERY DRAFT IS CHECKED BEFORE ANYTHING IS WRITTEN: its working-info is the
;; draft the listing named (the same version), and its bytes, based-on and
;; cut hash to that version, so what is dumped is exactly what restore will
;; write back. A draft that is not text has no working-info and is refused by
;; name; so is a draft that changed between the listing and its read.
;;
;; A refusal prints (error <kind> ...) and exits 1; success prints
;; (ok (dumped <n>) (dir "<dir>")) and exits 0.
(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia reduce) draft-version)
        (only (theourgia log) store-id-of)
        (only (theourgia working) working-baseline)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (theourgia ffi) mkdir-exclusive! overwrite-entry!))

(define actor "drafts-dump")
(define (refuse kind . details) (raise (cons 'error (cons kind details))))
(define (clause name datum)
  (let ((c (and (list? datum) (assq name (cdr datum))))) (and c (cadr c))))

;; -> ((<id> <version>) ...), the writer's live drafts as the listing names them.
(define (listed-drafts store writer)
  (let ((a (rpc-dispatch store (list 'drafts "--writer" writer) actor)))
    (unless (and (pair? a) (eq? (car a) 'ok)) (refuse 'drafts-refused a))
    (let ((items (let ((i (assq 'items (cdr a)))) (if i (cdr i) '()))))
      (map (lambda (d) (list (clause 'block d) (clause 'version d)))
           (filter (lambda (d) (and (pair? d) (eq? (car d) 'draft))) items)))))

;; -> (<id> <version> <based-on> <cut> <bytes>), checked; or a refusal.
(define (draft-facts store writer id listed-version)
  (let ((a (rpc-dispatch store (list 'read id "--working-info" "--writer" writer) actor)))
    (cond
      ((not (and (pair? a) (eq? (car a) 'ok)))
       (refuse 'draft-unreadable (list 'block id) (list 'answer a)))
      (else
       (let ((p (cadr a)))
         ;; (projection working <writer> <id> <version> <based-on> <cut> <text> <heading>)
         (unless (and (pair? p) (eq? (car p) 'projection) (eq? (cadr p) 'working))
           (refuse 'draft-gone (list 'block id)))
         (let ((version (list-ref p 4)) (based-on (list-ref p 5)) (cut (list-ref p 6))
               (bytes (string->utf8 (list-ref p 7))))
           (unless (equal? version listed-version)
             (refuse 'draft-changed (list 'block id) (list 'listed listed-version) (list 'read version)))
           (unless (equal? (draft-version bytes based-on cut) version)
             (refuse 'draft-unreproducible (list 'block id) (list 'version version)))
           (list id version based-on cut bytes)))))))

(define (dump store writer dir)
  (let* ((drafts (map (lambda (d) (draft-facts store writer (car d) (cadr d)))
                      (listed-drafts store writer)))
         (baseline (working-baseline store #f writer))
         (pinned (if (and (pair? baseline) (eq? (car baseline) 'ok)) (caddr baseline)
                     (refuse 'baseline-refused baseline))))
    (when (eq? (mkdir-exclusive! dir) 'exists) (refuse 'dir-exists (list 'dir dir)))
    (let loop ((ds drafts) (n 1) (entries '()))
      (if (pair? ds)
          (let* ((d (car ds)) (name (string-append (number->string n) ".bytes")))
            (overwrite-entry! (string-append dir "/" name) (list-ref d 4))
            (loop (cdr ds) (+ n 1)
                  (cons (list 'draft (list 'block (car d)) (list 'version (cadr d))
                              (list 'based-on (caddr d)) (list 'cut (cadddr d))
                              (list 'bytes name) (list 'sha256 (bytevector->hex (sha256 (list-ref d 4)))))
                        entries)))
          (let ((manifest (append (list 'drafts (list 'store (store-id-of store)) (list 'writer writer)
                                        (list 'pinned-cut pinned))
                                  (reverse entries))))
            (overwrite-entry! (string-append dir "/DRAFTS.sexp")
                              (string->utf8 (string-append (format "~s" manifest) "\n")))
            (list 'ok (list 'dumped (length drafts)) (list 'dir dir)))))))

(define (main args)
  (if (not (and (= 4 (length args)) (string=? (cadr args) "--writer")))
      (begin (write '(usage (drafts-dump <store> "--writer" <W> <dir>))) (newline) (exit 2))
      (let ((a (guard (e ((and (pair? e) (eq? (car e) 'error)) e))
                 (dump (car args) (caddr args) (cadddr args)))))
        (write a)
        (newline)
        (exit (if (eq? (car a) 'ok) 0 1)))))

(main (cdr (command-line)))
