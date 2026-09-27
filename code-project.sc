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
(library (theourgia code-project)
  (export import-code export-code export-code-view code-field code-children code-files read-code-bytes
          code-input-files code-safe-path? code-parent-directory)
  (import (only (theourgia view) view-read)
          (rnrs) (theourgia languages) (theourgia text-code) (theourgia code-markers)
          (theourgia store) (theourgia reduce) (theourgia baseline) (theourgia operation-packet)
          (only (theourgia log) store-id-of atomic-write!)
          (only (theourgia ffi) directory-entries file-is-directory? file-is-regular? mkdir-p!
                entry-bytes))
  (define (code-field state id name)
    ;; THE VIEW, because a projection writes what a reader would see --
    ;; a code block's `name` is derived from its source and is not in the
    ;; stored fields at all. The two calls below stay on `state-read`:
    ;; they ask whether a block EXISTS, which no derivation changes.
    (let* ((b (view-read state id)) (p (and b (assq name (cdr (assq 'fields b)))))) (and p (cdr p))))
  (define (alive? state id)
    (let ((b (state-read state id))) (and b (not (cdr (assq 'deleted b))))))
  (define (code-children state id)
    (map caddr (filter (lambda (r) (equal? (car r) id)) (state-outline state))))
  (define (code-files state)
    (filter (lambda (id) (and (alive? state id) (eq? 'file (code-field state id 'kind))))
            (map cadr (state-datum state))))
  ;; THROUGH THE DOOR (F100a): the file must be there, so absence raises
  ;; like any other failure, as the native read did.
  (define (read-code-bytes path) (entry-bytes path))
  (define (relative-safe? s)
    (and (string? s) (> (string-length s) 0) (not (char=? (string-ref s 0) #\/))
         (not (exists (lambda (c) (memv c '(#\nul #\\))) (string->list s)))
         (let loop ((i 0) (start 0))
           (if (or (= i (string-length s)) (char=? (string-ref s i) #\/))
               (and (not (member (substring s start i) '("" "." "..")))
                    (or (= i (string-length s)) (loop (+ i 1) (+ i 1))))
               (loop (+ i 1) start)))))
  (define (directory-files dir)
    (define (walk rel)
      (let ((path (if (string=? rel "") dir (string-append dir "/" rel))))
        (cond
          ((file-is-directory? path)
           (apply append (map (lambda (name)
                                (if (or (member name '("." "..")) (char=? (string-ref name 0) #\.)) '()
                                    (walk (if (string=? rel "") name (string-append rel "/" name)))))
                              (list-sort string<? (directory-entries path)))))
          ((file-is-regular? path) (list rel))
          (else (projection-failure 'unsupported-file (list 'path rel))))))
    (unless (file-is-directory? dir) (projection-failure 'not-a-directory))
    (walk ""))
  (define (parent-directory path)
    (let loop ((i (- (string-length path) 1)))
      (cond ((< i 0) ".") ((char=? (string-ref path i) #\/) (substring path 0 i)) (else (loop (- i 1))))))
  (define code-input-files directory-files)
  (define code-safe-path? relative-safe?)
  (define code-parent-directory parent-directory)
  (define (answer thunk)
    (guard (e ((and (list? e) (pair? e) (eq? (car e) 'error)) e)) (thunk)))
  (define (lang-fields entry)
    (if entry (list (cons 'lang (string->symbol (language-property entry 'lang #f)))) '()))
  (define (projection-input rel b)
    (let* ((entry (language-for-path rel))
           ;; A renamed extension does not change the wrapper already on disk.
           ;; The destination language is still chosen only from the data table.
           (wrapper (or (find (lambda (candidate) (projection-header-wrapper? candidate b)) (language-table)) entry))
           (parsed (projection-decode wrapper b)))
      (list rel entry parsed b)))

  ;; The capture contains complete intents and original cuts, never a directory
  ;; name to reread after a request has acquired an identity.
  (define (capture-import store dir allow-delete?)
    (let* ((inputs (map (lambda (rel) (unless (relative-safe? rel) (projection-failure 'unsafe-path))
                                       (projection-input rel (read-code-bytes (string-append dir "/" rel))))
                        (directory-files dir)))
           (seen-files '()) (seen-ids '()) (baselines '()) (memberships '()) (intents '()))
      (define (emit! x) (set! intents (append intents (list x))) (- (length intents) 1))
      (define (check-id! state id file old)
        (when (member id seen-ids) (projection-failure 'duplicate-id (list 'ids (list id))))
        (set! seen-ids (cons id seen-ids))
        (cond ((not (state-read state id)) (projection-failure 'foreign-file (list 'ids (list id))))
              ((not (alive? state id)) (projection-failure 'tombstone (list 'ids (list id))))
              ((not (member id old)) (projection-failure 'foreign-file (list 'ids (list id))))
              ((not (and (eq? (code-field state id 'kind) 'code) (eq? (code-field state id 'mode) 'text)))
               (projection-failure 'mode-mismatch (list 'ids (list id))))))
      (for-each
        (lambda (input)
          (let* ((rel (car input)) (entry (cadr input)) (parsed (caddr input))
                 (header (car parsed)) (entries (cadr parsed))
                 (file (and header (list-ref header 3)))
                 (cut (and header (list-ref header 4)))
                 (baseline (and header
                   (begin
                     (unless (equal? (list-ref header 2) (store-id-of store)) (projection-failure 'foreign-store))
                     (guard (e (#t (projection-failure 'unavailable-cut)))
                       (let ((s (open-and-reduce store cut))) (reduce-applied-cut s) s)))))
                 (old (if baseline (code-children baseline file) '())))
            (when header
              (unless (eq? (list-ref header 5) 'text) (projection-failure 'mode-mismatch))
              (when (member file seen-files) (projection-failure 'duplicate-file))
              (set! seen-files (cons file seen-files))
              (unless (alive? baseline file) (projection-failure 'tombstone (list 'ids (list file))))
              (unless (and (eq? 'file (code-field baseline file 'kind)) (eq? 'text (code-field baseline file 'mode)))
                (projection-failure 'mode-mismatch))
              (set! memberships (cons (list file old) memberships))
              (for-each (lambda (id) (set! baselines (cons (list id (block-hash baseline id) cut) baselines))) (cons file old)))
            (for-each (lambda (e)
                        (unless (string=? (car e) "new")
                          (unless header (projection-failure 'unavailable-cut))
                          (check-id! baseline (car e) file old))) entries)
            (let ((gone (filter (lambda (id) (not (assoc id entries))) old)))
              (when (and (pair? gone) (not allow-delete?))
                (projection-failure 'would-delete (list 'ids gone)))
              (for-each (lambda (id) (emit! (list 'del id))) gone))
            (let* ((parent (if file file
                             (list 'from (emit! (list 'insert 'root #f
                               (append (list '(kind . file) '(mode . text) (cons 'path rel)) (lang-fields entry)))))))
                   (previous #f) (unchanged-order? (equal? (map car entries) old)))
              (when (and file (not (equal? rel (code-field baseline file 'path)))) (emit! (list 'set file 'path rel)))
              (when (and file (not (equal? (and entry (string->symbol (language-property entry 'lang #f)))
                                         (code-field baseline file 'lang))))
                (emit! (if entry (list 'set file 'lang (string->symbol (language-property entry 'lang #f))) (list 'set file 'lang))))
              (for-each
                (lambda (e)
                  (let ((id (car e)) (src (cadr e)))
                    (if (string=? id "new")
                        (set! previous (list 'from (emit! (list 'insert parent previous
                          (append (list '(kind . code) '(mode . text) (cons 'src src)) (lang-fields entry))))))
                        (begin
                          (unless (equal? src (code-field baseline id 'src)) (emit! (list 'set id 'src src)))
                          (unless (equal? (and entry (string->symbol (language-property entry 'lang #f))) (code-field baseline id 'lang))
                            (emit! (if entry (list 'set id 'lang (string->symbol (language-property entry 'lang #f))) (list 'set id 'lang))))
                          (unless unchanged-order? (emit! (list 'move id parent previous)))
                          (set! previous id))))) entries)))) inputs)
      (list intents baselines memberships (map (lambda (i) (list (car i) (list-ref i 3))) inputs))))
  (define (import-code store dir actor req allow-delete?)
    (answer
      (lambda ()
        (let* ((packet (frozen-operation store req (lambda () (capture-import store dir allow-delete?))))
               (captured (cadr packet))
               (results (with-store-write store (lambda (state view) (car captured)) actor (car packet)
                          (lambda (state)
                            (or (exists (lambda (b) (baseline-refusal state (car b) (cadr b) (caddr b))) (cadr captured))
                                (exists (lambda (m) (and (not (equal? (cadr m) (code-children state (car m))))
                                                         (list 'error 'stale-baseline (list 'block (car m)) '(reason changed-children))))
                                        (caddr captured)))) #t)))
          (if (for-all (lambda (a) (and (pair? a) (eq? (car a) 'ok))) results)
              (list 'ok (cons 'items results))
              (if (= (length results) 1) (car results) (batch-answer results)))))))
;; NOTE: THE VIEW IS A PARAMETER (F17), as for export-md-view: `view` hands
  ;; back the reduction to project, the committed state or a writer's
  ;; working view.
  (define (export-code store dir raw?)
    (export-code-view store dir raw? (lambda () (open-and-reduce store))))

  (define (export-code-view store dir raw? view)
    (answer
      (lambda ()
        (let* ((state (view)) (cut (reduce-applied-cut state))
               (files (filter (lambda (id) (eq? 'text (code-field state id 'mode))) (code-files state)))
               (paths '())
               (outputs
                 (map (lambda (id)
                        (let* ((rel (code-field state id 'path)) (lang (code-field state id 'lang))
                               (entry (language-for-name lang))
                               (entries (map (lambda (child)
                                               (let ((src (code-field state child 'src)))
                                                 (unless (and (eq? 'code (code-field state child 'kind))
                                                              (eq? 'text (code-field state child 'mode)) (bytevector? src))
                                                   (projection-failure 'unexportable-block (list 'ids (list child))))
                                                 (list child src))) (code-children state id))))
                          (unless (relative-safe? rel) (projection-failure 'unsafe-path))
                          (when (member rel paths)
                            (projection-failure 'duplicate-path (list 'path rel)
                                                (list 'ids (state-path-claimants state 'file 'text rel))))
                          (set! paths (cons rel paths))
                          (list rel (if raw? (apply bytes-append (map cadr entries))
                                        (projection-encode entry (list (store-id-of store) id cut) entries))))) files)))
          (for-each (lambda (out) (let ((path (string-append dir "/" (car out))))
                                   (mkdir-p! (parent-directory path)) (atomic-write! path (cadr out) 'working))) outputs)
          (list 'ok (list 'files (length files)))))))
)
