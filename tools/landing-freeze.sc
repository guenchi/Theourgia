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

;; Freeze a writer's drafts for a landing: what the gate runs on, and what
;; tools/landing-commit.sc commits.
;;
;;   scheme --script tools/landing-freeze.sc <store> --writer <W> <dir>
;;
;; <dir> must not exist. In order:
;; - tools/drafts-dump.sc writes the draft list into <dir> (DRAFTS.sexp, with
;;   the pinned cut, the join of the drafts' cuts, and <n>.bytes);
;; - the working view is read ONCE, its own drafts are checked to be the
;;   dumped ones, and <dir>/tree is written from that view by export-code's
;;   own procedure, raw: the tree is that view's by construction;
;;   MANIFEST.sha256 lists every file's sha256 and path;
;; - CHANGED.sexp is changed-files between the store at the pinned cut and the
;;   working view: the files the drafts change;
;; - THE VIEW THE TREE IS WRITTEN FROM HOLDS THE DUMPED DRAFTS, each block
;;   and version, so the tree is the dumped drafts' tree, whatever was
;;   written in between: a draft rewritten and written back to its old
;;   version has the same bytes, and a draft still changed when the view was
;;   read differs from the dump;
;; - the draft list is read again at the end, and must still be the one
;;   dumped, so a freeze does not report a draft list a writer has since left.
;; A draft written during the freeze is refused (error drafts-changed ...),
;; with nothing written after it; the directory is left for the coder to
;; remove before freezing again.
;;
;; A refusal prints (error <kind> ...) and exits 1; success prints
;; (ok (frozen <n>) (changed <k>) (dir "<dir>")) and exits 0.
(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia store) store-state-at-cut)
        (only (theourgia working) working-state)
        (only (theourgia landing-files) raw-projection changed-files-between ambiguous-groups)
        (only (theourgia code-project) export-code-view)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (theourgia ffi) entry-bytes overwrite-entry! hold-point!)
        (only (theourgia log) load-declaration-set!)
        (only (theourgia incomplete) incomplete-refused))

(define actor "landing-freeze")
(define (refuse kind . details) (raise (cons 'error (cons kind details))))
(define (clause name datum)
  (let ((c (and (list? datum) (assq name (cdr datum))))) (and c (cadr c))))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(define (path-parent p)
  (let loop ((i (- (string-length p) 1))) (cond ((< i 0) ".") ((char=? (string-ref p i) #\/) (substring p 0 i)) (else (loop (- i 1))))))

;; THE DUMP IS tools/drafts-dump.sc ITSELF, run as a child beside this script,
;; so the draft list a freeze holds is the one a dump writes, by one program.
;; -> its printed datum: its standard output alone, since the library notices
;; a run may print go to standard error
(define (run-dump store writer dir)
  (let-values (((to from err pid)
                (open-process-ports
                  (string-append "scheme --script " (quoted (string-append (path-parent (car (command-line))) "/drafts-dump.sc"))
                                 " " (quoted store) " --writer " (quoted writer) " " (quoted dir) " < /dev/null")
                  (buffer-mode block) (native-transcoder))))
    (close-port to)
    (let ((text (get-string-all from)))
      (get-string-all err)
      (close-port from) (close-port err)
      (read (open-string-input-port (if (eof-object? text) "" text))))))

;; -> ((<block> . <version>) ...), sorted: the writer's live drafts now
(define (listed store writer)
  (let ((a (rpc-dispatch store (list 'drafts "--writer" writer) actor)))
    (unless (and (pair? a) (eq? (car a) 'ok)) (refuse 'drafts-refused a))
    (list-sort (lambda (x y) (string<? (car x) (car y)))
               (map (lambda (d) (cons (clause 'block d) (clause 'version d)))
                    (filter (lambda (d) (and (pair? d) (eq? (car d) 'draft)))
                            (let ((i (assq 'items (cdr a)))) (if i (cdr i) '())))))))

(define (freeze store writer dir)
  (let ((dumped (run-dump store writer dir)))
    (unless (and (pair? dumped) (eq? (car dumped) 'ok)) (refuse 'dump-refused dumped))
    (let* ((manifest (read (open-string-input-port (utf8->string (entry-bytes (string-append dir "/DRAFTS.sexp"))))))
           (drafts (filter (lambda (d) (and (pair? d) (eq? (car d) 'draft))) (cdr manifest)))
           (frozen (list-sort (lambda (x y) (string<? (car x) (car y)))
                              (map (lambda (d) (cons (clause 'block d) (clause 'version d))) drafts)))
           (pinned (clause 'pinned-cut manifest)))
      (when (null? drafts) (refuse 'nothing-to-freeze (list 'dir dir)))
      (hold-point! 'freeze-after-dump)
      ;; THE FREEZE'S LOADS ARE STRICT, as export-code's are: a store whose
      ;; history is cut by damage, or missing a writer, is refused rather
      ;; than frozen from its readable part. The declaration rides on a copy
      ;; of the store string for the extent of the freeze, as the dispatcher
      ;; sets one for a verb.
      (let ((key (string-copy store)))
        (dynamic-wind
          (lambda () (load-declaration-set! key incomplete-refused))
          (lambda () (freeze-view key writer dir frozen pinned (length drafts)))
          (lambda () (load-declaration-set! key #f)))))))

;; -> the freeze's answer, from one read of the working view
(define (freeze-view store writer dir frozen pinned count)
  (let* ((w (working-state store #f writer))
         (view (if (and (pair? w) (eq? (car w) 'ok)) (caddr w) (refuse 'working-view-refused w)))
         ;; the view's own drafts, each (<id> <version> <based-on> <bytes>)
         (in-view (list-sort (lambda (x y) (string<? (car x) (car y)))
                             (map (lambda (d) (cons (car d) (cadr d))) (cadddr w))))
         (base (store-state-at-cut store #f (format "~s" pinned))))
    (when (and (pair? base) (eq? (car base) 'error)) (refuse 'pinned-cut-unusable (list 'answer base)))
    (unless (equal? in-view frozen)
      (refuse 'drafts-changed (list 'dumped frozen) (list 'in-view in-view)))
    ;; THE TREE IS WRITTEN FROM THIS VIEW, by the procedure export-code
    ;; uses (export-code-view, raw), not by a second read of the working
    ;; view: a second read could see other drafts -- one written and taken
    ;; back between the two reads moves the view's cut, and with it the
    ;; files there are -- and the tree would hold what was never checked.
    ;; NO TWO PATHS THAT A VOLUME COULD FOLD INTO ONE, files or the directories
    ;; above them: a tree written to such a volume would not be the view's,
    ;; and identical bytes under both names would pass the read-back. Refused
    ;; on any volume, as the export's result must not depend on where it lands.
    (let ((paths (map car (raw-projection view))))
      (for-each (lambda (kind)
                  (let ((groups (ambiguous-groups kind paths)))
                    (unless (null? groups)
                      (refuse 'paths-ambiguous (list 'kind kind) (list 'groups groups)))))
                ;; each folding a volume may apply, and the two together: a
                ;; pair can meet under both and under neither alone
                '(case normalisation both)))
    (let ((e (export-code-view store (string-append dir "/tree") #t (lambda () view))))
      (unless (and (pair? e) (eq? (car e) 'ok)) (refuse 'export-refused e)))
    ;; ((<path> <bytes>) ...), the files export-code-view wrote, each read
    ;; back: on a volume that folds case or normalisation two paths can
    ;; land on one file, and the tree would not be the view's
    (let ((files (raw-projection view)))
      (for-each (lambda (f)
                  (unless (equal? (entry-bytes (string-append dir "/tree/" (car f))) (cadr f))
                    (refuse 'export-mismatch (list 'path (car f)))))
                files)
      (overwrite-entry! (string-append dir "/MANIFEST.sha256")
                        (string->utf8 (apply string-append
                                             (map (lambda (f) (string-append (bytevector->hex (sha256 (cadr f))) "  " (car f) "\n"))
                                                  (list-sort (lambda (x y) (string<? (car x) (car y))) files))))))
    (let ((changed (changed-files-between store base view)))
      (unless (and (pair? changed) (eq? (car changed) 'ok)) (refuse 'changed-files-refused changed))
      (let ((now (listed store writer)))
        (unless (equal? now frozen)
          (refuse 'drafts-changed (list 'dumped frozen) (list 'now now))))
      (overwrite-entry! (string-append dir "/CHANGED.sexp")
                        (string->utf8 (string-append (format "~s" changed) "\n")))
      (list 'ok (list 'frozen count)
            (list 'changed (length (let ((i (assq 'items (cdr changed)))) (if i (cdr i) '()))))
            (list 'dir dir)))))

(define (main args)
  (if (not (and (= 4 (length args)) (string=? (cadr args) "--writer")))
      (begin (write '(usage (landing-freeze <store> "--writer" <W> <dir>))) (newline) (exit 2))
      (let ((a (guard (e ((and (pair? e) (eq? (car e) 'error)) e))
                 (freeze (car args) (caddr args) (cadddr args)))))
        (write a)
        (newline)
        (exit (if (eq? (car a) 'ok) 0 1)))))

(main (cdr (command-line)))
