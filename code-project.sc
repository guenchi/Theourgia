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
          file-projection-key projected-files
          projection-outputs raw-projection path-collision spelling-key ambiguous-groups
          volume-insensitivity changed-files-between)
  (import (only (theourgia view) view-read)
          (rnrs) (theourgia languages) (theourgia text-code) (theourgia code-markers)
          (theourgia store) (theourgia reduce) (theourgia baseline) (theourgia operation-packet)
          (only (theourgia log) store-id-of atomic-write!)
          (only (theourgia timing-wrappers) timed)
          (only (theourgia ffi) directory-entries file-is-directory? file-is-regular? mkdir-p!
                entry-bytes entry-name-type mkdir-exclusive! rmdir! unlink! process-id machine-home)
          (theourgia degeneracy)
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
               ;; THE CAPTURE IS TIMED HERE (the timing seam, timing-wrappers.sc): it runs
               ;; before the write request's clock starts, and builds every
               ;; intent of the import. `capture` is the capture alone, inside
               ;; the thunk (a request whose packet already exists runs none);
               ;; `packet` is the whole frozen operation around it -- its
               ;; directory, lock and packet file included.
               (packet (timed 'packet
                         (frozen-operation store req
                           (lambda () (timed 'capture (capture-import store dir allow-delete? symbols))))))
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

  ;; THE PROJECTION, ONE COMPUTATION FOR EVERY CONSUMER: -> ((<path> <bytes>)
  ;; ...) in the exporter's order, raw (the children's srcs run together) or
  ;; marked. export-code writes it; changed-files compares it (raw) without
  ;; writing anything.
  (define (projection-outputs store state raw?)
    (let ((cut (reduce-applied-cut state)))
      (map (lambda (f)
             (let ((rel (car f)) (id (cadr f)) (entry (caddr f)) (entries (cadddr f)))
               (list rel (if raw? (apply bytes-append (map cadr entries))
                             (projection-encode entry (list (store-id-of store) id cut) entries)))))
           (projected-files state))))
  (define (raw-projection state) (projection-outputs #f state #t))

  (define (export-code-view store dir raw? view)
    (answer
      (lambda ()
        (let* ((state (view))
               (outputs (projection-outputs store state raw?)))
          (for-each (lambda (out) (let ((path (string-append dir "/" (car out))))
                                   (mkdir-p! (parent-directory path)) (atomic-write! path (cadr out) 'working))) outputs)
          (list 'ok (list 'files (length outputs)))))))

  ;; ---- the files that differ between two cuts --------------------------------
  ;;
  ;; WHAT IS COMPARED IS WHAT IS PUBLISHED: each text file's raw bytes (its
  ;; children's srcs run together) and its presence. A title, a level, a link
  ;; or a mode between two non-text modes changes no published byte and is not
  ;; reported; two src changes that cancel across a block boundary report
  ;; nothing; missing and empty are different (an empty file is present).

  ;; the directories above a path, nearest the root first: "a/b/c" -> ("a" "a/b")
  (define (path-ancestors p)
    (let loop ((i 0) (out '()))
      (cond ((= i (string-length p)) (reverse out))
            ((char=? (string-ref p i) #\/) (loop (+ i 1) (cons (substring p 0 i) out)))
            (else (loop (+ i 1) out)))))

  ;; A SET OF PATHS THAT CANNOT FORM A TREE: a path that is another's
  ;; DIRECTORY prefix (p followed by "/" begins q): x and x/y, not ab and abc.
  ;; -> (p q) for the first such pair, or #f.
  (define (path-collision paths)
    (let ((dirs (make-hashtable string-hash string=?)))
      (for-each (lambda (q) (for-each (lambda (d) (unless (hashtable-ref dirs d #f) (hashtable-set! dirs d q)))
                                      (path-ancestors q)))
                paths)
      (let loop ((ps (list-sort string<? paths)))
        (cond ((null? ps) #f)
              ((hashtable-ref dirs (car ps) #f) => (lambda (q) (list (car ps) q)))
              (else (loop (cdr ps)))))))

  ;; THE KEY A VOLUME MERGES SPELLINGS BY. A volume insensitive to case alone
  ;; (NTFS) merges by a SIMPLE per-character fold of the raw string, with no
  ;; normalisation; one insensitive to normalisation alone (APFS case-
  ;; sensitive) by NFC; one insensitive to both (APFS, Linux casefold) by
  ;; NFC of the fold of NFC -- folding can leave canonically equivalent but
  ;; unequal strings (U+0390 against U+03AA U+0301), hence the final NFC.
  ;; Full string-foldcase is not used: it merges Strasse and Strasse written
  ;; with U+00DF, which a per-character fold keeps apart. The known limit:
  ;; Chez's Unicode tables may differ from a volume's.
  (define (simple-fold s) (list->string (map char-foldcase (string->list s))))
  (define (spelling-key kind s)
    (case kind
      ((case) (simple-fold s))
      ((normalisation) (string-normalize-nfc s))
      ((both) (string-normalize-nfc (simple-fold (string-normalize-nfc s))))
      (else (assertion-violation 'spelling-key "kind must be case, normalisation or both" kind))))

  ;; THE GROUPS OF SPELLINGS A VOLUME OF THAT KIND WOULD MERGE, over every
  ;; path and every directory above one: -> a list of groups, each the sorted
  ;; distinct raw spellings sharing a key, only groups of two or more, sorted.
  (define (ambiguous-groups kind paths)
    (let ((by-key (make-hashtable string-hash string=?)) (seen (make-hashtable string-hash string=?)))
      (for-each
        (lambda (e)
          (unless (hashtable-ref seen e #f)
            (hashtable-set! seen e #t)
            (hashtable-update! by-key (spelling-key kind e) (lambda (l) (cons e l)) '())))
        (apply append (map (lambda (p) (cons p (path-ancestors p))) paths)))
      (list-sort (lambda (x y) (string<? (car x) (car y)))
                 (filter (lambda (g) (> (length g) 1))
                         (map (lambda (g) (list-sort string<? g))
                              (let-values (((ks vs) (hashtable-entries by-key))) (vector->list vs)))))))

  ;; WHETHER THE VOLUME HOLDING DIR merges spellings, by creating one probe
  ;; there: a directory named with an uppercase ASCII letter and U+00E9 (NFC),
  ;; asked for under the lowercase spelling (case) and the NFD spelling
  ;; (normalisation), then removed. -> (values case? normalisation?); a probe
  ;; that cannot be made, asked or removed raises (error probe-failed (dir
  ;; <dir>)). An existing path cannot serve: an ASCII-only tree has none to
  ;; ask under two spellings.
  ;;
  ;; NEVER: A NAME IS TAKEN ONLY WHEN ALL THREE SPELLINGS ARE ABSENT. Only the
  ;; first is created exclusively; on a volume that keeps them apart, a
  ;; leftover directory under the second or third spelling would answer
  ;; present and read as a merge. A name with any spelling present moves on
  ;; to the next k.
  ;;
  ;; NEVER: THE PROBE IS REMOVED WHETHER OR NOT THE QUESTIONS ANSWERED. A
  ;; lookup that raises is a failed probe, after the removal, not a probe
  ;; left behind.
  (define (volume-insensitivity dir)
    (define (failed) (raise (list 'error 'probe-failed (list 'dir dir))))
    (let try ((k 0))
      (when (> k 16) (failed))
      (let* ((stem (string-append dir "/.theourgia-probe-" (number->string (process-id)) "-" (number->string k) "-"))
             (made-name (string-append stem "A" (string #\xE9)))
             (folded (string-append stem "a" (string #\xE9)))
             (decomposed (string-append stem "A" (string #\e #\x301)))
             (made (guard (e (#t 'failed))
                     (if (for-all (lambda (p) (eq? 'absent (entry-name-type p))) (list made-name folded decomposed))
                         (mkdir-exclusive! made-name)
                         'exists))))
        (cond
          ((eq? made 'failed) (failed))
          ((eq? made 'exists) (try (+ k 1)))
          (else
           (let* ((seen (guard (e (#t #f))
                          (list (not (eq? 'absent (entry-name-type folded)))
                                (not (eq? 'absent (entry-name-type decomposed))))))
                  (removed (guard (e (#t #f)) (rmdir! made-name) #t)))
             (unless (and seen removed) (failed))
             (values (car seen) (cadr seen))))))))

  ;; -> ((path <p> <reason>) ...) sorted by p: content (present at both, bytes
  ;; differ), added (absent at a), removed (absent at b).
  (define (changed-items ra rb)
    (let ((a (make-hashtable string-hash string=?)) (b (make-hashtable string-hash string=?)))
      (for-each (lambda (o) (hashtable-set! a (car o) (cadr o))) ra)
      (for-each (lambda (o) (hashtable-set! b (car o) (cadr o))) rb)
      (let ((paths (list-sort string<? (vector->list (hashtable-keys
                     (let ((u (hashtable-copy a #t))) (vector-for-each (lambda (k) (hashtable-set! u k #t)) (hashtable-keys b)) u))))))
        (filter values
                (map (lambda (p)
                       (let ((x (hashtable-ref a p #f)) (y (hashtable-ref b p #f)))
                         (cond ((not x) (list 'path p 'added))
                               ((not y) (list 'path p 'removed))
                               ((bytevector=? x y) #f)
                               (else (list 'path p 'content)))))
                     paths)))))

  ;; changed-files over two states already loaded at their cuts: the
  ;; projection's refusals (unsafe-path, duplicate-path, unexportable-block)
  ;; answered as the exporter answers them, then a set that cannot form a
  ;; tree refused path-collision naming its cut, then the items, the two
  ;; cuts, and a mark for each kind of volume that would merge spellings.
  (define (changed-files-between store a b)
    (answer
      (lambda ()
        (let* ((ra (raw-projection a)) (rb (raw-projection b))
               (pa (map car ra)) (pb (map car rb))
               (collision (lambda (paths side)
                            (let ((c (path-collision paths)))
                              (and c (list 'error 'projection-invalid '(reason path-collision)
                                           (list 'paths (car c) (cadr c)) (list 'cut side)))))))
          (or (collision pa 'from)
              (collision pb 'to)
              (let* ((marks (lambda (kind name)
                              (let ((groups (unique-groups (append (ambiguous-groups kind pa) (ambiguous-groups kind pb)))))
                                (if (null? groups) '() (list (list name groups))))))
                     (result (append (list 'ok (cons 'items (changed-items ra rb))
                                           (list 'from (reduce-applied-cut a)) (list 'to (reduce-applied-cut b)))
                                     (marks 'case 'case-ambiguous)
                                     (marks 'normalisation 'normalisation-ambiguous))))
                (degeneracy-check (set! result (changed-files-reference store a b pa pb result)))
                result))))))
  (define (unique-groups gs)
    (let loop ((gs (list-sort (lambda (x y) (string<? (car x) (car y))) gs)) (out '()))
      (cond ((null? gs) (reverse out))
            ((member (car gs) out) (loop (cdr gs) out))
            (else (loop (cdr gs) (cons (car gs) out))))))

  ;; ---- the reference, under THEOURGIA_DEGENERACY: the same answer by a
  ;; second route. Both cuts are written out with the exporter into two fresh
  ;; directories and compared file by file. It checks the comparison and the
  ;; presence logic; a projection or reduction defect reaches both routes
  ;; alike. A temporary volume that would merge spellings the cuts hold
  ;; cannot materialise them, so then the reference is not taken and the
  ;; answer says (reference-unavailable <kind>).
  (degeneracy-definitions
    (define (changed-files-reference store a b pa pb answer)
      (let* ((root (string-append (machine-home) "/changed-files-reference-" (number->string (process-id))))
             (_ (begin (mkdir-p! (machine-home)) (remove-tree! root) (mkdir-p! root))))
        (let-values (((case? normalisation?) (volume-insensitivity root)))
          (let* ((paths (append pa pb))
                 (blocked
                   (cond ((and case? normalisation?
                               (or (pair? (ambiguous-groups 'both pa)) (pair? (ambiguous-groups 'both pb))))
                          'both)
                         ((and case? (or (pair? (ambiguous-groups 'case pa)) (pair? (ambiguous-groups 'case pb)))) 'case)
                         ((and normalisation? (or (pair? (ambiguous-groups 'normalisation pa))
                                                  (pair? (ambiguous-groups 'normalisation pb))))
                          'normalisation)
                         (else #f))))
            (if blocked
                (begin (remove-tree! root) (append answer (list (list 'reference-unavailable blocked))))
                (let ((da (string-append root "/a")) (db (string-append root "/b")))
                  (export-code-view store da #t (lambda () a))
                  (export-code-view store db #t (lambda () b))
                  (let ((reference (compare-trees (read-tree da) (read-tree db)))
                        (items (cdr (cadr answer))))
                    (remove-tree! root)
                    (unless (equal? reference items)
                      (assertion-violation 'degeneracy "changed-files differs from the materialised reference"
                                           items reference))
                    answer)))))))
    ;; NEVER: NOT changed-items. The second route compares by its own loop, so
    ;; a defect in the product's comparison is not repeated here. Both inputs
    ;; come sorted by path from read-tree; a path at one side only is added or
    ;; removed, and one at both is content when its bytes differ.
    (define (compare-trees ta tb)
      (let loop ((ta ta) (tb tb) (out '()))
        (cond ((and (null? ta) (null? tb)) (reverse out))
              ((or (null? tb) (and (pair? ta) (string<? (car (car ta)) (car (car tb)))))
               (loop (cdr ta) tb (cons (list 'path (car (car ta)) 'removed) out)))
              ((or (null? ta) (string<? (car (car tb)) (car (car ta))))
               (loop ta (cdr tb) (cons (list 'path (car (car tb)) 'added) out)))
              ((bytevector=? (cadr (car ta)) (cadr (car tb))) (loop (cdr ta) (cdr tb) out))
              (else (loop (cdr ta) (cdr tb) (cons (list 'path (car (car ta)) 'content) out))))))
    ;; every regular file under DIR as ((<relative path> <bytes>) ...), sorted
    ;; by path.
    ;;
    ;; NEVER: NOT directory-files, which skips every name beginning with a dot
    ;; (an import input's hidden files). The export writes a file at .hidden,
    ;; and a reference that cannot see it would disagree with a right answer.
    ;; Only "." and ".." are skipped; anything but a file or a directory in a
    ;; directory the exporter just wrote is a defect, raised.
    (define (read-tree dir)
      (list-sort (lambda (x y) (string<? (car x) (car y)))
        (let walk ((rel ""))
          (let ((path (if (string=? rel "") dir (string-append dir "/" rel))))
            (case (entry-name-type path)
              ((absent) '())
              ((directory)
               (apply append (map (lambda (name)
                                    (if (member name '("." "..")) '()
                                        (walk (if (string=? rel "") name (string-append rel "/" name)))))
                                  (directory-entries path))))
              ((regular) (list (list rel (entry-bytes path))))
              (else (assertion-violation 'degeneracy "the reference export holds an entry that is neither a file nor a directory" path)))))))
    (define (remove-tree! path)
      (case (entry-name-type path)
        ((absent) #f)
        ((directory)
         (for-each (lambda (name) (unless (member name '("." "..")) (remove-tree! (string-append path "/" name))))
                   (directory-entries path))
         (rmdir! path))
        (else (unlink! path)))))
)
