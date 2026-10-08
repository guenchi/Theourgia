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
          code-input-files code-safe-path? code-parent-directory duplicate-path-failure
          file-projection-key projected-files)
  (import (only (theourgia view) view-read)
          (rnrs) (theourgia languages) (theourgia text-code) (theourgia code-markers)
          (theourgia store) (theourgia reduce) (theourgia baseline) (theourgia operation-packet)
          (only (theourgia log) store-id-of atomic-write!)
          (only (theourgia ffi) directory-entries file-is-directory? file-is-regular? mkdir-p!
                entry-bytes)
          (only (theourgia markers) import-text?)
          (only (theourgia wire) storable-encode sexpr->string-extended)
          (only (theourgia digest) sha256 bytevector->hex))
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
  ;;
  ;; NEVER: A TEXT IMPORT KEEPS WHAT IS TEXT, and lists the rest. A file
  ;; whose bytes are not UTF-8, or that holds a NUL, is not imported: it is
  ;; captured, in the walk's order, as the skipped list, and it touches
  ;; nothing -- no new identity, no update, no deletion. The test runs on
  ;; the raw bytes BEFORE projection-decode, so a binary file that happens
  ;; to open with a marker line is skipped, not refused, and a marked
  ;; projection whose body became binary never reaches the header path that
  ;; would overwrite or delete its children.
  ;;
  ;; NEVER: ONE FILE AT A TIME, IN THE WALK'S ORDER: its path checked, its
  ;; bytes read, the text test, and then its decoding, before the next file
  ;; is read. Reading every file first answered an unreadable later file
  ;; where the base had refused an earlier one's header.
  ;; Each entry is (rel bytes input), input #f for a skipped file.
  ;;
  ;; AN EDITOR'S SYMBOLS SPLIT A FIRST IMPORT, AND ONLY THAT. `symbols` is #f,
  ;; or (read-sections . cuts-of): the reader of `--symbols`' sections by
  ;; path, and the procedure that turns a section and a file's bytes and
  ;; language entry into the cuts, or raises the file's refusal
  ;; (code-suggest.sc, read-import-symbols and import-symbol-cuts). The
  ;; sections are read here, in the capture and before the directory, so a
  ;; symbols file that does not read refuses the request before anything
  ;; else is read, and a request already captured is replayed from its
  ;; packet whatever the symbols file holds now. A file a section names is split at those cuts
  ;; when it carries no marker line -- no file block holds it yet -- into
  ;; the blocks a marked first import of the same cuts makes. A file that
  ;; carries markers follows them and nothing else, so identity is proved
  ;; by markers alone, and is listed as symbols-ignored. A file whose cuts
  ;; are refused is not imported at all, and is listed with its refusal as
  ;; symbols-refused, as is a section that names no text file the walk
  ;; found; the rest of the import goes on.
  (define (capture-import store dir allow-delete? symbols)
    (let* ((sections (and symbols ((car symbols))))
           (ignored '()) (refused '()) (used '())
           (by-symbols
             (lambda (rel b input)
               (let ((section (and symbols input (assoc rel sections))))
                 (if (not section) input
                     (let ((parsed (caddr input)))
                       (set! used (cons rel used))
                       (if (or (car parsed) (not (= 1 (length (cadr parsed)))) (not (equal? (cadar (cadr parsed)) b)))
                           (begin (set! ignored (cons rel ignored)) input)
                           (guard (e ((and (pair? e) (eq? (car e) 'error))
                                      (set! refused (cons (list rel e) refused)) #f))
                             (let ((cuts ((cdr symbols) section b (cadr input))))
                               (list rel (cadr input)
                                     (list #f (map (lambda (from to) (list "new" (byte-slice b from to)))
                                                   (reverse (cdr (reverse cuts))) (cdr cuts)))
                                     b)))))))))
           (files (map (lambda (rel)
                         (unless (relative-safe? rel) (projection-failure 'unsafe-path))
                         (let ((b (read-code-bytes (string-append dir "/" rel))))
                           (list rel b (and (import-text? b) (by-symbols rel b (projection-input rel b))))))
                       (directory-files dir)))
           (skipped (map car (filter (lambda (f) (not (or (caddr f) (assoc (car f) refused)))) files)))
           (inputs (map caddr (filter caddr files)))
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
      (list intents baselines memberships (map (lambda (i) (list (car i) (list-ref i 3))) inputs)
            (list 'skipped skipped)
            (list 'symbols-ignored (reverse ignored))
            (list 'symbols-refused
                  (append (reverse refused)
                          (if symbols
                              (map (lambda (section) (list (car section) '(error symbols-no-file)))
                                   (filter (lambda (section) (not (member (car section) used))) sections))
                              '()))))))
  ;; A text packet's tagged slot after the fourth, or #f: a packet captured
  ;; before the slot existed has four.
  (define (text-packet-extra captured tag)
    (let ((x (and (> (length captured) 4) (assq tag (list-tail captured 4)))))
      (and x (cadr x))))
  ;; PREMISES, when given, is the list (check finish run) the handler made of
  ;; the request's `--premises` (store.sc, premises-preflight): the check is
  ;; asked before this verb's own, finish is given the answer of the write --
  ;; only of the write, so a refusal made before it carries no clause -- and
  ;; run holds the part from the write on, so a raise inside it says so.
  ;; SYMBOLS, the optional argument after premises (#f for none), is what
  ;; capture-import takes: the reader of `--symbols`' sections and their cutter.
  (define (import-code store dir actor req allow-delete? . premises)
    (answer
      (lambda ()
        (let* ((given (and (pair? premises) (car premises)))
               (symbols (and (pair? premises) (pair? (cdr premises)) (cadr premises)))
               (pc (and given (car given)))
               (finish (if given (cadr given) (lambda (a) a)))
               (run (if given (caddr given) (lambda (thunk) (thunk))))
               (packet (frozen-operation store req (lambda () (capture-import store dir allow-delete? symbols))))
               (captured (cadr packet)))
          (run
            (lambda ()
              (let ((results (with-store-write store (lambda (state view) (car captured)) actor (car packet)
                               (premises-also pc
                                 (lambda (state)
                                   (or (exists (lambda (b) (baseline-refusal state (car b) (cadr b) (caddr b))) (cadr captured))
                                       (exists (lambda (m) (and (not (equal? (cadr m) (code-children state (car m))))
                                                                (list 'error 'stale-baseline (list 'block (car m)) '(reason changed-children))))
                                               (caddr captured)))))
                               #t)))
                ;; THE SKIPPED CLAUSE IS THERE ONLY WHEN SOMETHING WAS SKIPPED,
                ;; and only on an answer that succeeded; so are the two
                ;; clauses of `--symbols`, each only when it lists something.
                (finish
                  (if (for-all (lambda (a) (and (pair? a) (eq? (car a) 'ok))) results)
                      (append (list 'ok (cons 'items results))
                              (apply append
                                     (map (lambda (tag)
                                            (let ((listed (text-packet-extra captured tag)))
                                              (if (pair? listed) (list (list tag listed)) '())))
                                          '(skipped symbols-ignored symbols-refused))))
                      (if (= (length results) 1) (car results) (batch-answer results)))))))))))
;; NOTE: THE VIEW IS A PARAMETER (F17), as for export-md-view: `view` hands
  ;; back the reduction to project, the committed state or a writer's
  ;; working view.
  (define (export-code store dir raw?)
    (export-code-view store dir raw? (lambda () (open-and-reduce store))))

  ;; A PATH SEVERAL BLOCKS HOLD IS REFUSED WITH EVERY SUCH PATH IN THE STORE,
  ;; and the way out. `path` and `ids` are the first one met, as before;
  ;; `paths` is the whole list `check` reports, so a user who fixes one path
  ;; does not run again only to meet the next. The remedy is one `del` per
  ;; surplus holder, and it does not say which: there is no oldest holder
  ;; across writers, and the newer copy may be the corrected one.
  (define (duplicate-path-failure state rel ids)
    (projection-failure 'duplicate-path (list 'path rel) (list 'ids ids)
                        (list 'paths (map cdr (state-duplicated-paths state)))
                        (list 'remedy 'del-all-but-one-per-path)))

  ;; WHETHER A FILE PROJECTS, AND WHAT ITS PROJECTION IS A FUNCTION OF.
  ;; -> (key "<sha256>") | (failure <reason> <detail> ...)
  ;;
  ;; KEY: ONE FUNCTION FOR THE EXPORTER AND FOR EVERY FRESHNESS CHECK. A
  ;; supplied fact is stamped with this key and judged by it again later;
  ;; export-code-view runs its own checks through it. So "this file would
  ;; project" and "this file has a key" cannot come apart: a file the
  ;; exporter refuses has no key, for the exporter's own reason, in the
  ;; exporter's own order -- every child first (any child that is not
  ;; code text with bytes is unexportable-block: the exporter refuses it,
  ;; it does not skip it), then the path (unsafe-path), then its holders
  ;; (duplicate-path, when more than one live text file holds the path).
  ;; `no-holder` is this layer's own answer, for a block that is no longer
  ;; a live text-mode file: the exporter never meets one, it lists only
  ;; such files.
  ;;
  ;; THE KEY hashes what the projected bytes depend on beyond each block's
  ;; own src: the comment wrapping the language entry gives the marker
  ;; lines and the escape family (read through the entry actually
  ;; registered, so a replacement under the same name with another
  ;; wrapping changes it, and metadata that does not reach the bytes does
  ;; not), and the ordered ids of ALL the file's children. A child's src is
  ;; stamped on its own (projected-files); the header's store id, file id
  ;; and cut are not inputs of any fact.
  (define (file-projection-key state id)
    (let ((b (state-read state id)))
      (if (not (and b (not (cdr (assq 'deleted b)))
                    (eq? 'file (code-field state id 'kind)) (eq? 'text (code-field state id 'mode))))
          '(failure no-holder)
          (let* ((children (code-children state id))
                 (bad (find (lambda (child)
                              (not (and (eq? 'code (code-field state child 'kind))
                                        (eq? 'text (code-field state child 'mode))
                                        (bytevector? (code-field state child 'src)))))
                            children))
                 (rel (code-field state id 'path)))
            (cond
              (bad (list 'failure 'unexportable-block (list 'ids (list bad))))
              ((not (relative-safe? rel)) '(failure unsafe-path))
              ((let ((holders (state-path-claimants state 'file 'text rel)))
                 (and (> (length holders) 1) holders))
               => (lambda (holders) (list 'failure 'duplicate-path (list 'path rel) (list 'ids holders))))
              (else
               (let ((w (projection-wrapping (language-for-name (code-field state id 'lang)))))
                 (list 'key (bytevector->hex (sha256 (string->utf8 (sexpr->string-extended
                                                (storable-encode (list 'file-projection (car w) (cdr w) children))))))))))))))

  ;; A key's failure, answered the way the exporter has always answered it.
  (define (key-failure state k)
    (case (cadr k)
      ((unexportable-block) (projection-failure 'unexportable-block (caddr k)))
      ((unsafe-path) (projection-failure 'unsafe-path))
      ((duplicate-path) (duplicate-path-failure state (cadr (caddr k)) (cadr (cadddr k))))
      (else (projection-failure (cadr k)))))

  ;; EVERY TEXT FILE OF A VIEW AS THE EXPORTER PROJECTS IT, in the exporter's
  ;; order: -> ((<path> <file-id> <entry> ((<child> <src>) ...) <key>) ...),
  ;; raising the exporter's refusal for the first file that does not
  ;; project. The exporter writes these; `supply` projects them in memory
  ;; and stamps its facts from them.
  (define (projected-files state)
    ;; A loop, not `map`: the refusal answered is the first file's in the
    ;; exporter's order, and R6RS leaves map's order of application open.
    (let loop ((ids (filter (lambda (id) (eq? 'text (code-field state id 'mode))) (code-files state))) (out '()))
      (if (null? ids)
          (reverse out)
          (loop (cdr ids)
                (cons (let* ((id (car ids)) (k (file-projection-key state id)))
                        (unless (eq? (car k) 'key) (key-failure state k))
                        (list (code-field state id 'path) id (language-for-name (code-field state id 'lang))
                              (map (lambda (child) (list child (code-field state child 'src))) (code-children state id))
                              (cadr k)))
                      out)))))

  (define (export-code-view store dir raw? view)
    (answer
      (lambda ()
        (let* ((state (view)) (cut (reduce-applied-cut state))
               (files (projected-files state))
               (outputs
                 (map (lambda (f)
                        (let ((rel (car f)) (id (cadr f)) (entry (caddr f)) (entries (cadddr f)))
                          (list rel (if raw? (apply bytes-append (map cadr entries))
                                        (projection-encode entry (list (store-id-of store) id cut) entries)))))
                      files)))
          (for-each (lambda (out) (let ((path (string-append dir "/" (car out))))
                                   (mkdir-p! (parent-directory path)) (atomic-write! path (cadr out) 'working))) outputs)
          (list 'ok (list 'files (length files)))))))
)
