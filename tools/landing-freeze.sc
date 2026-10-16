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
;; - export-code --working --raw --writer W writes the working view into
;;   <dir>/tree, and each file is read back and compared with the projection
;;   it came from; MANIFEST.sha256 lists every file's sha256 and path;
;; - CHANGED.sexp is changed-files between the store at the pinned cut and the
;;   working view: the files the drafts change;
;; - the draft list is read again, and must be the one dumped.
;; A draft written during the freeze makes the last step differ, and the
;; freeze is refused (error drafts-changed ...), with nothing written after
;; it: the tree in <dir> is then not what the drafts are, and the directory
;; is left for the coder to remove before freezing again.
;;
;; A refusal prints (error <kind> ...) and exits 1; success prints
;; (ok (frozen <n>) (changed <k>) (dir "<dir>")) and exits 0.
(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia store) store-state-at-cut)
        (only (theourgia working) working-state)
        (only (theourgia landing-files) raw-projection changed-files-between)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (theourgia ffi) entry-bytes overwrite-entry! hold-point!))

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
      (let ((e (rpc-dispatch store (list 'export-code (string-append dir "/tree") "--working" "--raw" "--writer" writer) actor)))
        (unless (and (pair? e) (eq? (car e) 'ok)) (refuse 'export-refused e)))
      (let* ((w (working-state store #f writer))
             (view (if (and (pair? w) (eq? (car w) 'ok)) (caddr w) (refuse 'working-view-refused w)))
             (base (store-state-at-cut store #f (format "~s" pinned)))
             ;; ((<path> <bytes>) ...), what export-code --raw wrote
             (files (raw-projection view)))
        (when (and (pair? base) (eq? (car base) 'error)) (refuse 'pinned-cut-unusable (list 'answer base)))
        (for-each (lambda (f)
                    (unless (equal? (entry-bytes (string-append dir "/tree/" (car f))) (cadr f))
                      (refuse 'export-mismatch (list 'path (car f)))))
                  files)
        (overwrite-entry! (string-append dir "/MANIFEST.sha256")
                          (string->utf8 (apply string-append
                                               (map (lambda (f) (string-append (bytevector->hex (sha256 (cadr f))) "  " (car f) "\n"))
                                                    (list-sort (lambda (x y) (string<? (car x) (car y))) files)))))
        (let ((changed (changed-files-between store base view)))
          (unless (and (pair? changed) (eq? (car changed) 'ok)) (refuse 'changed-files-refused changed))
          (unless (equal? (listed store writer) frozen)
            (refuse 'drafts-changed (list 'dumped frozen) (list 'now (listed store writer))))
          (overwrite-entry! (string-append dir "/CHANGED.sexp")
                            (string->utf8 (string-append (format "~s" changed) "\n")))
          (list 'ok (list 'frozen (length drafts))
                (list 'changed (length (let ((i (assq 'items (cdr changed)))) (if i (cdr i) '()))))
                (list 'dir dir)))))))

(define (main args)
  (if (not (and (= 4 (length args)) (string=? (cadr args) "--writer")))
      (begin (write '(usage (landing-freeze <store> "--writer" <W> <dir>))) (newline) (exit 2))
      (let ((a (guard (e ((and (pair? e) (eq? (car e) 'error)) e))
                 (freeze (car args) (caddr args) (cadddr args)))))
        (write a)
        (newline)
        (exit (if (eq? (car a) 'ok) 0 1)))))

(main (cdr (command-line)))
