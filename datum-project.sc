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
(library (theourgia datum-project)
  (export import-datum export-datum export-datum-view def-datum)
  (import (rnrs) (theourgia datum-code) (theourgia datum-match) (theourgia datum-metadata)
          (theourgia code-project) (theourgia code-markers) (theourgia text-code) (theourgia languages)
          (theourgia store) (theourgia reduce) (theourgia baseline) (theourgia operation-packet)
          (only (theourgia log) store-id-of atomic-write!) (only (theourgia ffi) mkdir-p!))
  (define scheme-entry (language-for-name 'scheme))
  (define (answer thunk)
    (guard (e ((and (pair? e) (eq? (car e) 'error)) e)) (thunk)))
  (define (alive? state id)
    (let ((b (state-read state id))) (and b (not (cdr (assq 'deleted b))))))
  (define (libraries state)
    (filter (lambda (id) (and (alive? state id) (eq? 'library (code-field state id 'kind)))) (map cadr (state-datum state))))
  (define (control line)
    (guard (e ((and (pair? e) (equal? (assq 'reason (filter pair? e)) '(reason invalid-marker))) #f))
      (projection-control scheme-entry line)))
  (define (clean-doc doc)
    (let ((b (string->utf8 doc)))
      (utf8->string (apply bytes-append
        (map (lambda (r) (if (control (byte-slice b (car r) (cadr r))) #vu8() (byte-slice b (car r) (caddr r)))) (byte-lines b))))))
  ;; NEVER: A REFUSAL NAMES ITS FILE. The lexer's bad-source carries a
  ;; reason and an offset and no path, and an import reads many files: the
  ;; refusal is raised again with `(path <rel>)` appended, so the answer says
  ;; which file it is about. def reads its source from an argument and keeps
  ;; the lexer's shape.
  (define (parse-file rel bytes)
    (let* ((context (guard (e ((and (pair? e) (eq? (car e) 'error) (pair? (cdr e)) (eq? (cadr e) 'bad-source))
                               (raise (append e (list (list 'path rel))))))
                      (datum-source-read bytes 'context)))
           (top (car context))
           (comments (cadr context)) (text (list-ref context 3))
           (wrapper? (and (= (length top) 1) (list? (caar top)) (pair? (caar top)) (eq? (caaar top) 'library)))
           (body (and wrapper? (caar top)))
           (plain-import? (and (not wrapper?) (pair? top) (list? (caar top)) (pair? (caar top)) (eq? (caaar top) 'import)))
           (forms (if wrapper? (if (>= (length body) 4) (list-tail (list-ref (car top) 6) 4) '()) (if plain-import? (cdr top) top)))
           (header #f) (markers '()) (char-offset 0))
      (when wrapper?
        (unless (and (>= (length body) 4) (list? (cadr body)) (pair? (cadr body))
                     (list? (caddr body)) (pair? (caddr body)) (eq? (caaddr body) 'export)
                     (list? (cadddr body)) (pair? (cadddr body)) (eq? (car (cadddr body)) 'import))
          (projection-failure 'invalid-library)))
      (for-each
        (lambda (row)
          (let* ((raw (byte-slice bytes (car row) (caddr row)))
                 (line (byte-slice bytes (car row) (cadr row)))
                 (actual? (exists (lambda (c) (= (car c) char-offset)) comments))
                 (c (and actual? (control line))))
            (when c
              (case (car c)
                ((file)
                 (when header (projection-failure 'duplicate-header))
                 (when (and (pair? top) (>= char-offset (list-ref (car top) 3))) (projection-failure 'misplaced-header))
                 (set! header (cadr c)))
                ((block)
                 (unless (= (caddr c) 0) (projection-failure 'invalid-padding))
                 (set! markers (cons (list char-offset (cadr c)) markers)))))
            (set! char-offset (+ char-offset (string-length (utf8->string raw)))))) (byte-lines bytes))
      (when (and header (not (eq? (list-ref header 5) 'datum))) (projection-failure 'mode-mismatch))
      ;; NEVER: A PROGRAM MAY CARRY A HEADER, NOTHING ELSE UNWRAPPED MAY.
      ;; An exported program comes back as its import form and its forms,
      ;; under the same header a library carries; a file that is neither a
      ;; library nor a program (no `(import ...)` first) is refused as before.
      (when (and header (not wrapper?) (not plain-import?)) (projection-failure 'missing-library-wrapper))
      (let* ((assigned (make-vector (length forms) #f)) (form-vector (list->vector forms)))
        (for-each
          (lambda (m)
            (let find ((i 0))
              (cond
                ((= i (vector-length form-vector)) (projection-failure 'orphan-marker))
                ((< (list-ref (vector-ref form-vector i) 3) (car m) (list-ref (vector-ref form-vector i) 4))
                 (projection-failure 'marker-inside-form))
                ((< (car m) (list-ref (vector-ref form-vector i) 3))
                 (when (vector-ref assigned i) (projection-failure 'duplicate-marker))
                 (vector-set! assigned i (if (string=? (cadr m) "new") 'new (cadr m))))
                (else (find (+ i 1)))))) (reverse markers))
        (let ((fields (if wrapper?
                          (list (cons 'name (cadr body)) (cons 'exports (cdr (caddr body))) (cons 'imports (cdr (cadddr body))))
                          (append (list (cons 'name (list (string->symbol rel))) '(exports) (cons 'imports (if plain-import? (cdaar top) '((rnrs)))))
                                  ;; THE SHAPE IS RECORDED, NOT DERIVED FROM THE FILE LATER:
                                  ;; a program -- a file whose first form is
                                  ;; `(import ...)`, not `library` -- is exported as one.
                                  ;; It is a field of the library block, so it is in the
                                  ;; log and survives replay.
                                  (if plain-import? (list '(shape . program)) '())))))
          (list rel header fields
                (map (lambda (form id) (list id (car form) (clean-doc (cadr form)) (caddr form))) forms (vector->list assigned))
                (apply append (map (lambda (f) (list-ref f 5)) forms)) bytes)))))

  (define (code-fields row)
    (list '(kind . code) '(mode . datum) '(lang . chez) (cons 'body (cadr row)) (cons 'doc (caddr row))))
  ;; NEVER: A DATUM IMPORT READS SCHEME FILES ONLY, and says which it did not
  ;; read. A file is Scheme when the language table, AS IT IS NOW, gives its
  ;; path the entry whose lang is "scheme" (its extensions, matched exactly);
  ;; every other regular file the walk returns is captured, in the walk's
  ;; order, as the skipped list beside the warnings. The table is the one
  ;; supplier of the selection: there is no second list of extensions here,
  ;; and no entry held from load time, which a later register-language! of
  ;; Scheme would leave behind.
  (define (scheme-file? rel)
    (let ((e (language-for-path rel)))
      (and e (equal? (language-property e 'lang #f) "scheme"))))
  ;; NEVER: A FILE WITHOUT A HEADER WHOSE PATH THE STORE HOLDS UPDATES THAT
  ;; LIBRARY; it is not a second copy of it. The path is resolved against
  ;; the alive datum libraries of the committed reduction captured here:
  ;; none is a new library, as before; one is that library, updated through
  ;; the same matching as a headed file, with its baselines taken at this
  ;; reduction's cut; more than one is refused by name, path and ids.
  ;; RAW NEVER DELETES: a child the file does not hold is kept and named in
  ;; the answer's `kept` clause, where a headed file (an export, which is
  ;; the whole library) deletes it.
  ;; THE HOLDERS ARE CAPTURED FOR EVERY RAW FILE, none included, and the
  ;; write checks them again: another library that took the path after the
  ;; capture, or a second import of the same new path that wrote first,
  ;; refuses the write stale-baseline, changed-claimants.
  (define (capture-import store dir)
    (let* ((files (code-input-files dir))
           (skipped (filter (lambda (rel) (not (scheme-file? rel))) files))
           (inputs (map (lambda (rel) (parse-file rel (read-code-bytes (string-append dir "/" rel)))) (filter scheme-file? files)))
           (current-cache #f)
           (intents '()) (baselines '()) (memberships '()) (seen '()) (claims '()) (kept '()))
      (define (emit! intent) (set! intents (append intents (list intent))) (- (length intents) 1))
      ;; THE COMMITTED REDUCTION A RAW FILE IS RESOLVED AGAINST, loaded the
      ;; first time a file without a header asks for it and never before:
      ;; an import of headed files alone loads nothing it did not load
      ;; before, so its refusals keep their order (foreign-store before any
      ;; read of the writers).
      (define (current)
        (or current-cache (begin (set! current-cache (open-and-reduce store)) current-cache)))
      (define (field-changes! state id fields)
        (for-each (lambda (p) (unless (equal? (cdr p) (code-field state id (car p))) (emit! (list 'set id (car p) (cdr p))))) fields))
      (for-each
        (lambda (input)
          (let* ((rel (car input)) (header (cadr input)) (fields (caddr input)) (entries (list-ref input 3))
                 (claimants (and (not header) (state-path-claimants (current) 'library 'datum rel)))
                 (raw-target (and claimants (= (length claimants) 1) (car claimants)))
                 (id (if header (list-ref header 3) raw-target))
                 (cut (if header (list-ref header 4) (and raw-target (reduce-applied-cut (current)))))
                 (state (cond
                          (header
                           (unless (equal? (list-ref header 2) (store-id-of store)) (projection-failure 'foreign-store))
                           (let ((s (open-and-reduce store cut)))
                             (unless (reduction? s) (projection-failure 'unavailable-cut)) s))
                          (raw-target (current))
                          (else #f)))
                 (children (if state (code-children state id) '())))
            (when claimants
              (when (> (length claimants) 1)
                (projection-failure 'duplicate-path (list 'path rel) (list 'ids claimants)))
              (set! claims (append claims (list (list rel claimants)))))
            (when id
              (when (member id seen) (projection-failure 'duplicate-file))
              (set! seen (cons id seen))
              (unless (alive? state id) (projection-failure 'tombstone))
              (unless (and (eq? (code-field state id 'kind) 'library) (eq? (code-field state id 'mode) 'datum)) (projection-failure 'mode-mismatch))
              (set! memberships (cons (list id children) memberships))
              (for-each (lambda (b) (set! baselines (cons (list b (block-hash state b) cut) baselines))) (cons id children)))
            (let ((claimed '()))
              (for-each (lambda (e)
                          (when (and (car e) (not (eq? (car e) 'new)))
                            (unless header (projection-failure 'unavailable-cut))
                            (when (member (car e) claimed) (projection-failure 'duplicate-id))
                            (set! claimed (cons (car e) claimed))
                            (unless (alive? state (car e)) (projection-failure 'tombstone))
                            (unless (member (car e) children) (projection-failure 'foreign-library)))) entries))
            (for-each (lambda (child)
                        (unless (and (eq? (code-field state child 'kind) 'code) (eq? (code-field state child 'mode) 'datum))
                          (projection-failure 'mode-mismatch))) children)
            (let* ((old (map (lambda (b) (list b (code-field state b 'body) (or (code-field state b 'doc) "") (datum-names (code-field state b 'body)))) children))
                   (matched (datum-match old entries))
                   (parent (or id (list 'from (emit! (list 'insert 'root #f (append '((kind . library) (mode . datum) (lang . chez)) (list (cons 'path rel)) fields))))))
                   (omitted (filter (lambda (old-id) (not (member old-id matched))) children))
                   (previous #f)
                   ;; A raw update keeps what it omits AFTER what it holds, so its
                   ;; order is unchanged only when the file adds nothing and the
                   ;; children already read so. A new form is placed by the
                   ;; chain like every other, so any new form means the chain.
                   (same-order? (if header
                                    (equal? matched children)
                                    (and (not (memv #f matched))
                                         (equal? children (append matched omitted))))))
              ;; A file that is a library now, over a block recorded as a program,
              ;; says so: the shape is set back, as any other field that changed.
              ;; ONLY THE VALUE THIS IMPORT WRITES IS RESET: a `shape` field that
              ;; is not `program` was not written by this import and is left as
              ;; it is.
              (when id (field-changes! state id (cons (cons 'path rel)
                                                      (if (and (not (assq 'shape fields)) (eq? (code-field state id 'shape) 'program))
                                                          (cons '(shape . library) fields)
                                                          fields))))
              (if header
                  (for-each (lambda (old-id) (emit! (list 'del old-id))) omitted)
                  (set! kept (append kept omitted)))
              (for-each
                (lambda (row target)
                  (if target
                      (begin
                        (field-changes! state target (filter (lambda (p) (memq (car p) '(body doc))) (code-fields row)))
                        (unless same-order? (emit! (list 'move target parent previous)))
                        (set! previous target))
                      (set! previous (list 'from (emit! (list 'insert parent previous (code-fields row))))))) entries matched)
              ;; WHAT A RAW FILE KEPT GOES AFTER WHAT IT HOLDS: once the file's
              ;; forms are placed, each kept child follows the last one placed,
              ;; in its own order, so a kept child that stood first does not
              ;; stay in front of them.
              (unless (or header same-order?)
                (for-each (lambda (k) (emit! (list 'move k parent previous)) (set! previous k)) omitted))))) inputs)
      (list intents baselines memberships (apply append (map (lambda (i) (list-ref i 4)) inputs))
            (map (lambda (i) (list (car i) (list-ref i 5))) inputs)
            (list 'skipped skipped)
            (list 'claimants claims)
            (list 'kept kept))))
  ;; A packet's tagged slot after the fifth, or #f: a packet captured
  ;; before these slots existed, and def's, have five.
  (define (packet-extra captured tag)
    (let ((x (and (> (length captured) 5) (assq tag (list-tail captured 5)))))
      (and x (cadr x))))
  (define (execute store actor packet)
    (let* ((captured (cadr packet))
           (results (with-store-write store (lambda (state view) (car captured)) actor (car packet)
             (lambda (state)
               (or (exists (lambda (b) (baseline-refusal state (car b) (cadr b) (caddr b))) (cadr captured))
                   (exists (lambda (m) (and (not (equal? (cadr m) (code-children state (car m))))
                                            (list 'error 'stale-baseline (list 'block (car m)) '(reason changed-children)))) (caddr captured))
                   (exists (lambda (c) (and (not (equal? (cadr c) (state-path-claimants state 'library 'datum (car c))))
                                            (list 'error 'stale-baseline (list 'path (car c)) '(reason changed-claimants))))
                           (or (packet-extra captured 'claimants) '())))) #t)))
      ;; THE SKIPPED AND KEPT CLAUSES ARE THERE ONLY WHEN THEY NAME SOMETHING,
      ;; and only on an answer that succeeded. A packet captured before these
      ;; slots existed has five, and def's has five: neither gains one.
      (if (for-all (lambda (a) (and (pair? a) (eq? (car a) 'ok))) results)
          (append (list 'ok (cons 'items results) (list 'warnings (list-ref captured 3)))
                  (let ((skipped (packet-extra captured 'skipped)))
                    (if (pair? skipped) (list (list 'skipped skipped)) '()))
                  (let ((kept (packet-extra captured 'kept)))
                    (if (pair? kept) (list (list 'kept (list 'ids kept))) '())))
          (if (= (length results) 1) (car results) (batch-answer results)))))
  (define (import-datum store dir actor req)
    (answer (lambda () (execute store actor (frozen-operation store req (lambda () (capture-import store dir)))))))
;; NOTE: THE VIEW IS A PARAMETER (F17), as for export-code-view.
  (define (export-datum store dir)
    (export-datum-view store dir (lambda () (open-and-reduce store))))

  (define (export-datum-view store dir view)
    (answer
      (lambda ()
        (let* ((state (view)) (paths '())
               (outputs (map
                 (lambda (id)
                   (let ((path (code-field state id 'path)))
                     (unless (code-safe-path? path) (projection-failure 'unsafe-path))
                     (when (member path paths)
                       (projection-failure 'duplicate-path (list 'path path)
                                           (list 'ids (state-path-claimants state 'library 'datum path))))
                     (set! paths (cons path paths))
                     (let ((parts
                       (map (lambda (child)
                              (unless (and (eq? (code-field state child 'kind) 'code) (eq? (code-field state child 'mode) 'datum))
                                (projection-failure 'mode-mismatch))
                              (let ((doc (or (code-field state child 'doc) "")))
                                (when (datum-doc-marker? doc) (projection-failure 'marker-in-doc))
                                (unless (datum-doc-format? doc) (projection-failure 'invalid-doc))
                                (string-append (utf8->string (marker-line scheme-entry (string-append "@block " child)))
                                               doc (datum-print (code-field state child 'body))))) (code-children state id))))
                       ;; A PROGRAM IS WRITTEN BACK AS A PROGRAM: its import
                       ;; form and its forms, under the same header, with no
                       ;; `(library ...)` around them.
                       (list path (string->utf8
                         (if (eq? (code-field state id 'shape) 'program)
                             (string-append "#!chezscheme\n"
                               (utf8->string (projection-header-line scheme-entry (list (store-id-of store) id (reduce-applied-cut state)) 'datum 0))
                               (datum-print (cons 'import (code-field state id 'imports)))
                               (apply string-append parts))
                             (string-append "#!chezscheme\n"
                               (utf8->string (projection-header-line scheme-entry (list (store-id-of store) id (reduce-applied-cut state)) 'datum 0))
                               "(library " (let ((s (datum-print (code-field state id 'name)))) (substring s 0 (- (string-length s) 1))) "\n"
                               (datum-print (cons 'export (code-field state id 'exports)))
                               (datum-print (cons 'import (code-field state id 'imports)))
                               (apply string-append parts) ")\n")))))))
                 (filter (lambda (id) (eq? (code-field state id 'mode) 'datum)) (libraries state)))))
          (for-each (lambda (out) (let ((path (string-append dir "/" (car out))))
                                   (mkdir-p! (code-parent-directory path)) (atomic-write! path (cadr out) 'working))) outputs)
          (list 'ok (list 'files (length outputs)))))))
  (define (def-datum store name under source actor req)
    (answer
      (lambda ()
        (execute store actor (frozen-operation store req
          (lambda ()
            (let* ((forms (datum-source-read (string->utf8 source))) (state (open-and-reduce store))
                   (libs (libraries state)) (id (or under (and (= (length libs) 1) (car libs))))
                   (cut (reduce-applied-cut state)))
              (unless (= (length forms) 1) (projection-failure 'expected-one-form))
              (unless (and (pair? (caddr (car forms))) (equal? (symbol->string (car (caddr (car forms)))) name))
                (projection-failure 'name-mismatch))
              (unless id (projection-failure 'library-required))
              (unless (and (alive? state id) (eq? (code-field state id 'mode) 'datum) (eq? (code-field state id 'kind) 'library))
                (projection-failure 'mode-mismatch))
              (let* ((children (code-children state id))
                     (taken (filter (lambda (b) (exists (lambda (n) (member n (datum-names (code-field state b 'body)))) (caddr (car forms)))) children))
                     (form (car forms)))
                (unless (null? taken) (raise (list 'error 'name-exists (list 'name name) (list 'ids taken))))
                (list (list (list 'insert id (and (pair? children) (car (reverse children)))
                                  (code-fields (list #f (car form) (cadr form) (caddr form)))))
                      (map (lambda (b) (list b (block-hash state b) cut)) (cons id children))
                      (list (list id children)) (list-ref form 5) source)))))))))
)
