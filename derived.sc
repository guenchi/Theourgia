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

;;; (theourgia derived) -- facts an editor computed from a projection, kept
;;; beside the store and never in it.
;;;
;;; A SUPPLIED FACT IS NEVER A RECORD. `supply` writes a table under
;;; <store>/derived/, one per kind, writer and language; the reducer never
;;; reads it, the evidence index does not list it, `check` does not look at
;;; it, and the store's .gitignore keeps it out of a checkout. Losing a table
;;; loses nothing a later supply cannot give back.
;;;
;;; EVERY FACT CAN BE TOLD STALE. What an editor saw is a projection, and a
;;; projection is a function of a few inputs: each block's effective src
;;; bytes, and per file its language's comment wrapping and the ordered ids
;;; of its children (code-project.sc's file-projection-key). A fact carries
;;; those inputs as they were when it was supplied -- the sha256 of every
;;; block it lists as a dependency, and the key of every file those blocks
;;; belong to -- and a reader recomputes them in its own view and drops the
;;; fact when any differs, counting it. A block's title, keywords, edges or
;;; position reach no projection, so no stamp holds them.
;;;
;;; THE SUPPLY IS CHECKED AGAINST THE CORE'S OWN RE-PROJECTION, not trusted:
;;; the core projects the view again as export-code would, and every file the
;;; supply lists must have the digest the plugin computed. A supply made from
;;; a buffer that was not the disk, or before a draft or a commit, is refused
;;; `supply-stale` naming the file.
(library (theourgia derived)
  (export supply-derived clear-derived derived-facts derived-facts* derived-kinds derived-clauses
          derived-signature derived-signature-table derived-keyword-table
          derived-calls-into derived-reach supplied-relations derived-table-named?
          table-file-name percent-encode read-supply-header)
  (import (rnrs)
          (only (theourgia reduce) state-read reduce-applied-cut)
          (only (theourgia code-project) code-field projected-files path-projection-key)
          (only (theourgia code-markers) projection-encode-map projection-range)
          (only (theourgia markers) byte-lines byte-slice safe-utf8)
          (only (theourgia log) store-id-of atomic-write!)
          (only (theourgia ffi) entry-bytes entry-type mkdir-p! file-ensure! with-exclusive-lock
                unlink! directory-entries file-is-directory?)
          (only (theourgia wire) storable-encode storable-decode string->sexpr-extended
                sexpr->string-extended)
          (only (theourgia digest) sha256 bytevector->hex))

  ;; ---- names ----------------------------------------------------------------

  (define derived-kinds '(signatures calls diagnostics))
  ;; THE RELATIONS A SUPPLY PRODUCES EDGES OF: `reach` walks these and
  ;; refuses any other name. uses and guards are reserved but no supply
  ;; produces them yet.
  (define supplied-relations '(calls))
  (define severities '(error warning information hint))

  ;; EVERY BYTE OUTSIDE [A-Za-z0-9_.] IS %XX, so a writer name or a language
  ;; id holding "-" (objective-c; a writer named iter-code) cannot make two
  ;; tables share a file name: `<kind>-<writer>-<language>` parses one way.
  ;; The committed state's writer "-" is therefore "%2D".
  (define (percent-encode s)
    (apply string-append
      (map (lambda (b)
             (let ((c (integer->char b)))
               (if (or (and (char<=? #\a c) (char<=? c #\z)) (and (char<=? #\A c) (char<=? c #\Z))
                       (and (char<=? #\0 c) (char<=? c #\9)) (char=? c #\_) (char=? c #\.))
                   (string c)
                   (let ((h (string-upcase (number->string b 16))))
                     (string-append "%" (if (< b 16) "0" "") h)))))
           (bytevector->u8-list (string->utf8 s)))))

  (define (derived-dir store) (string-append store "/derived"))
  (define (table-file-name kind writer language)
    (string-append (symbol->string kind) "-" (percent-encode writer) "-" (percent-encode language) ".sexp"))
  (define (table-path store kind writer language)
    (string-append (derived-dir store) "/" (table-file-name kind writer language)))

  (define (encode x) (string->utf8 (sexpr->string-extended (storable-encode x))))
  (define (digest x) (bytevector->hex (sha256 (encode x))))
  (define (bytes-sha b) (bytevector->hex (sha256 b)))

  ;; ---- refusals -------------------------------------------------------------

  (define (malformed line reason)
    (raise (list 'error 'supply-malformed (list 'line line) (list 'reason reason))))
  (define (stale-file path)
    (raise (list 'error 'supply-stale (list 'file path))))

  ;; ---- the supply file ------------------------------------------------------

  ;; One datum per line, read with the wire's datum reader (data only, one
  ;; datum, trailing text refused); a line that does not read is `unread`.
  ;; -> ((<line number> . <datum>) ...), numbered as the file is, so a
  ;; refusal names the line an editor shows. A line with no bytes is not
  ;; a datum and is left out; the header is line 1 all the same.
  (define unread (list 'unread))
  (define (supply-lines bytes)
    (let loop ((rows (byte-lines bytes)) (n 1) (out '()))
      (cond ((null? rows) (reverse out))
            ((and (> n 1) (= (caar rows) (cadar rows))) (loop (cdr rows) (+ n 1) out))
            (else
             (let ((text (safe-utf8 (byte-slice bytes (caar rows) (cadar rows)))))
               (loop (cdr rows) (+ n 1)
                     (cons (cons n (if text (guard (e (#t unread)) (string->sexpr-extended text)) unread))
                           out)))))))

  ;; The header line's datum, or the refusal of line 1.
  (define (header-datum data)
    (if (or (null? data) (not (= 1 (caar data))) (eq? (cdar data) unread))
        (malformed 1 'header)
        (cdar data)))

  (define (hex64? s)
    (and (string? s) (= 64 (string-length s))
         (for-all (lambda (c) (or (and (char<=? #\0 c) (char<=? c #\9)) (and (char<=? #\a c) (char<=? c #\f))))
                  (string->list s))))
  (define (distinct? xs)
    (or (null? xs) (and (not (member (car xs) (cdr xs))) (distinct? (cdr xs)))))
  ;; IN ORDER, because the first refusal met is the one answered and R6RS
  ;; leaves `map`'s order of application open.
  (define (map-in-order f xs)
    (let loop ((xs xs) (out '()))
      (if (null? xs) (reverse out) (loop (cdr xs) (cons (f (car xs)) out)))))
  (define (strings? xs) (and (list? xs) (for-all string? xs)))

  ;; THE HEADER, its clauses by name, each once:
  ;;   (supply <kind> (writer "<w>") (language "<l>") (source (vscode "<v>"))
  ;;           (files ((<path> "<sha256>") ...)) (replaces (<path> ...)))
  ;; The kind and the writer must be the ones the command names; the table
  ;; a supply writes is identified by the command and the header together,
  ;; never by one of them alone. -> (kind writer language version files replaces)
  (define header-keys '(writer language source files replaces))
  (define (read-supply-header d kind writer)
    (unless (and (list? d) (>= (length d) 2) (eq? (car d) 'supply) (symbol? (cadr d))
                 (= (length (cddr d)) (length header-keys))
                 (for-all (lambda (c) (and (list? c) (= 2 (length c)) (memq (car c) header-keys))) (cddr d))
                 (for-all (lambda (k) (assq k (cddr d))) header-keys))
      (malformed 1 'header))
    (let* ((get (lambda (k) (cadr (assq k (cddr d)))))
           (w (get 'writer)) (language (get 'language)) (source (get 'source))
           (files (get 'files)) (replaces (get 'replaces)))
      (unless (and (string? w) (string? language) (> (string-length language) 0)
                   (list? source) (= 2 (length source)) (eq? (car source) 'vscode) (string? (cadr source))
                   (list? files)
                   (for-all (lambda (f) (and (list? f) (= 2 (length f)) (string? (car f))
                                             (> (string-length (car f)) 0) (hex64? (cadr f))))
                            files)
                   (distinct? (map car files))
                   (strings? replaces) (distinct? replaces))
        (malformed 1 'header))
      (unless (eq? (cadr d) kind) (malformed 1 'kind-mismatch))
      (unless (equal? w writer) (malformed 1 'writer-mismatch))
      (list kind w language (cadr source) files replaces)))

  (define (depends? d) (and (list? d) (= 2 (length d)) (eq? (car d) 'depends) (pair? (cadr d)) (strings? (cadr d))))
  (define (exact-offset? x) (and (integer? x) (exact? x) (>= x 0)))

  ;; A FACT LINE, by kind. -> (subject payload depends range) with range
  ;; (s e) for a diagnostic and #f otherwise. The first dependency is the
  ;; fact's own block (a supply replaces a file's facts by it), so it must
  ;; be the fact's subject.
  (define (read-fact kind d line)
    (let ((parsed
            (case kind
              ((signatures)
               (cond
                 ((and (list? d) (= 5 (length d)) (eq? (car d) 'signature) (string? (cadr d))
                       (string? (caddr d))
                       (let ((k (cadddr d))) (and (list? k) (= 2 (length k)) (eq? (car k) 'kind) (symbol? (cadr k))))
                       (depends? (list-ref d 4)))
                  (list (cadr d) (list 'signature (cadr d) (caddr d) (cadddr d)) (cadr (list-ref d 4)) #f))
                 ((and (list? d) (= 4 (length d)) (eq? (car d) 'keywords) (string? (cadr d))
                       (pair? (caddr d)) (strings? (caddr d)) (depends? (cadddr d)))
                  (list (cadr d) (list 'keywords (cadr d) (caddr d)) (cadr (cadddr d)) #f))
                 (else #f)))
              ((calls)
               (and (list? d) (= 4 (length d)) (eq? (car d) 'calls) (string? (cadr d)) (string? (caddr d))
                    (depends? (cadddr d))
                    (list (cadr d) (list 'calls (cadr d) (caddr d)) (cadr (cadddr d)) #f)))
              ((diagnostics)
               (and (list? d) (= 6 (length d)) (eq? (car d) 'diagnostic) (string? (cadr d))
                    (memq (caddr d) severities) (string? (cadddr d))
                    (let ((r (list-ref d 4)))
                      (and (list? r) (= 3 (length r)) (eq? (car r) 'range)
                           (exact-offset? (cadr r)) (exact-offset? (caddr r)) (<= (cadr r) (caddr r))))
                    (depends? (list-ref d 5))
                    (list (cadr d) (list 'diagnostic (cadr d) (caddr d) (cadddr d))
                          (cadr (list-ref d 5)) (cdr (list-ref d 4)))))
              (else #f))))
      (cond ((eq? d unread) (malformed line 'unreadable))
            ((not parsed) (malformed line 'fact-shape))
            ((not (equal? (car (caddr parsed)) (car parsed))) (malformed line 'depends-not-led-by-subject))
            (else parsed))))

  ;; ---- the re-projection ----------------------------------------------------

  ;; The view projected as export-code projects it, in memory:
  ;; path -> (file-id bytes pieces key ((child src) ...)). The exporter's own
  ;; refusal leaves here unchanged (projected-files raises it).
  (define (reproject store view)
    (let ((cut (reduce-applied-cut view)) (sid (store-id-of store)))
      (map (lambda (f)
             (let ((rel (car f)) (id (cadr f)) (entry (caddr f)) (entries (cadddr f)) (key (list-ref f 4)))
               (let-values (((bytes pieces) (projection-encode-map entry (list sid id cut) entries)))
                 (list rel id bytes pieces key entries))))
           (projected-files view))))

  ;; ---- the table ------------------------------------------------------------

  ;; (derived-table 1 <store-id> <digest of body> <body>), body
  ;; (<kind> <writer> <language> (<fact> ...)), a fact
  ;; (fact <own-file> <payload> (<id> "<src sha256>") ... ) -- see make-fact.
  ;; Checksummed like the evidence checkpoint: a table that does not read,
  ;; whose digest or identity is wrong, or whose facts are not facts, is
  ;; ABSENT as a whole, never used in part.
  ;; A FACT IS USED ONLY WITH ITS STAMPS: a fact without one would be fresh
  ;; for ever, so a table holding one is not a table this library wrote.
  (define (stamp? s) (and (list? s) (= 2 (length s)) (string? (car s)) (hex64? (cadr s))))
  (define (fact? f)
    (and (list? f) (= 6 (length f)) (eq? (car f) 'fact) (string? (cadr f))
         (let ((p (caddr f)))
           (and (list? p) (>= (length p) 3) (memq (car p) '(signature keywords calls diagnostic))
                (string? (cadr p))))
         (let ((deps (list-ref f 3)))
           (and (list? deps) (pair? deps) (for-all stamp? deps)
                (equal? (car (car deps)) (cadr (caddr f)))))
         (let ((files (list-ref f 4))) (and (list? files) (pair? files) (for-all stamp? files)))
         (let ((v (list-ref f 5)))
           (and (list? v) (= 3 (length v)) (eq? (car v) 'vscode) (for-all string? (cdr v))))))
  (define (make-fact own-file payload dep-stamps file-stamps via)
    (list 'fact own-file payload dep-stamps file-stamps via))
  (define (fact-own-file f) (cadr f))
  (define (fact-payload f) (caddr f))
  (define (fact-dep-stamps f) (list-ref f 3))
  (define (fact-file-stamps f) (list-ref f 4))
  (define (fact-via f) (list-ref f 5))

  (define (read-table store path kind writer language)
    (guard (e (#t #f))
      (and (eq? (entry-type path) 'regular)
           (let* ((x (storable-decode (string->sexpr-extended (utf8->string (entry-bytes path)))))
                  (body (and (list? x) (= 5 (length x)) (list-ref x 4))))
             (and body (eq? (car x) 'derived-table) (eqv? (cadr x) 1)
                  (equal? (caddr x) (store-id-of store)) (equal? (cadddr x) (digest body))
                  (list? body) (= 4 (length body))
                  (eq? (car body) kind) (equal? (cadr body) writer) (string? (caddr body))
                  (or (not language) (equal? (caddr body) language))
                  (list? (cadddr body)) (for-all fact? (cadddr body))
                  body)))))

  (define (write-table! store kind writer language facts)
    (let ((body (list kind writer language facts)))
      (atomic-write! (table-path store kind writer language)
                     (encode (list 'derived-table 1 (store-id-of store) (digest body) body))
                     'derived)))

  ;; THE STORE'S .gitignore KEEPS THE TABLES OUT, including in a store made
  ;; before the pattern was in init's list: the line is appended to a file
  ;; that lacks it, everything already there kept, and the file is made when
  ;; there is none.
  (define (ensure-ignored! store)
    (let* ((path (string-append store "/.gitignore"))
           (old (if (eq? (entry-type path) 'absent) #f (entry-bytes path)))
           (text (if old (utf8->string old) "")))
      (unless (member "/derived/" (map (lambda (r) (utf8->string (byte-slice (string->utf8 text) (car r) (cadr r))))
                                       (byte-lines (string->utf8 text))))
        (atomic-write! path
                       (string->utf8 (string-append text
                                                    (if (and (> (string-length text) 0)
                                                             (not (char=? #\newline (string-ref text (- (string-length text) 1)))))
                                                        "\n" "")
                                                    "/derived/\n"))
                       'derived))))

  ;; ONE TABLE CHANGES AT A TIME. A supply reads the table, keeps the facts
  ;; about files it does not replace and writes the rest back; two of those
  ;; interleaved would lose one. The stamps come from a view read before
  ;; this lock, so a commit landing in between makes the facts stale at
  ;; their next reading -- never falsely fresh.
  (define (with-table-lock store thunk)
    (mkdir-p! (derived-dir store))
    (let ((lock (string-append (derived-dir store) "/lock")))
      (file-ensure! lock)
      (with-exclusive-lock lock (lambda (fd) (thunk)))))

  ;; ---- supply ---------------------------------------------------------------

  (define (supply-bytes path)
    (entry-bytes path))

  ;; (supply-derived store kind path writer view)
  ;;   writer: the table's writer, --for or "-"
  ;;   view:   a thunk answering the reduction the plugin projected -- the
  ;;           committed state for "-", the writer's working view otherwise
  ;; -> (ok (supplied (facts <n>) (files <n>))), or the refusal.
  ;;
  ;; THE ORDER OF THE CHECKS IS THE ORDER A REASON IS NAMED IN: the header
  ;; (line 1), every fact line's shape (line n), the exporter's own refusal
  ;; of the view, the digest of every listed file (supply-stale), the
  ;; replaced files listed, then each fact against the view in line order
  ;; (unknown-id, dependency-file-not-listed, own-file-not-replaced,
  ;; range-outside-file, id-range-mismatch). Nothing is written before all
  ;; of them pass.
  (define (supply-derived store kind path writer view)
    (guard (e ((and (pair? e) (eq? (car e) 'error)) e))
      (let* ((data (supply-lines (supply-bytes path)))
             (header (read-supply-header (header-datum data) kind writer))
             (language (caddr header)) (version (cadddr header))
             (files (list-ref header 4)) (replaces (list-ref header 5))
             (facts (map-in-order (lambda (row) (cons (car row) (read-fact kind (cdr row) (car row)))) (cdr data)))
             (state (view))
             (projection (reproject store state))
             (by-path (lambda (p) (assoc p projection))))
        (for-each (lambda (f)
                    (let ((p (by-path (car f))))
                      (unless (and p (equal? (bytes-sha (caddr p)) (cadr f))) (stale-file (car f)))))
                  files)
        (unless (for-all (lambda (p) (assoc p files)) replaces) (malformed 1 'replaces-not-listed))
        (let* ((file-of (let ((t (make-hashtable string-hash string=?)))
                          (for-each (lambda (p)
                                      (for-each (lambda (e) (hashtable-set! t (car e) (car p))) (list-ref p 5)))
                                    projection)
                          (lambda (id) (hashtable-ref t id #f))))
               (src-of (lambda (id)
                         (let ((p (by-path (file-of id))))
                           (cadr (assoc id (list-ref p 5))))))
               (via (list 'vscode version language))
               (made
                 (map-in-order (lambda (nf)
                        (let* ((n (car nf)) (subject (cadr nf)) (payload (caddr nf))
                               (deps (cadddr nf)) (range (list-ref nf 4)))
                          ;; EVERY ID THE FACT NAMES IS KNOWN before any is asked
                          ;; about its file, so one line answers one reason
                          ;; whatever the order of its depends.
                          (for-each (lambda (id) (unless (file-of id) (malformed n 'unknown-id)))
                                    (if (eq? (car payload) 'calls) (append deps (list (caddr payload))) deps))
                          (for-each (lambda (id)
                                      (unless (assoc (file-of id) files) (malformed n 'dependency-file-not-listed)))
                                    deps)
                          (let ((own (file-of subject)))
                            (unless (member own replaces) (malformed n 'own-file-not-replaced))
                            (let ((payload
                                    (if range
                                        (let* ((p (by-path own)) (size (bytevector-length (caddr p))))
                                          (unless (<= (cadr range) size) (malformed n 'range-outside-file))
                                          (let ((m (projection-range (cadddr p) (car range) (cadr range))))
                                            (cond
                                              ((and (eq? (car m) 'mapped) (equal? (cadr m) subject))
                                               (append payload (list (list 'at (caddr m) (cadddr m)))))
                                              ((and (eq? (car m) 'unmappable) (equal? (cadr m) subject))
                                               (append payload (list '(at unmappable))))
                                              ;; A RANGE THAT MAPS TO NO BLOCK AT ALL -- in a file with
                                              ;; no blocks -- lands here too. R1 v11 holds it
                                              ;; unreachable (no fact can name a block of such a file);
                                              ;; code-markers1's row for a file with no blocks is a
                                              ;; tripwire, not a measurement.
                                              (else (malformed n 'id-range-mismatch)))))
                                        payload)))
                              (make-fact own payload
                                         (map (lambda (id) (list id (bytes-sha (src-of id)))) deps)
                                         (let loop ((ids deps) (seen '()))
                                           (cond ((null? ids)
                                                  (map (lambda (path) (list path (list-ref (by-path path) 4)))
                                                       (reverse seen)))
                                                 ((member (file-of (car ids)) seen) (loop (cdr ids) seen))
                                                 (else (loop (cdr ids) (cons (file-of (car ids)) seen)))))
                                         via)))))
                      facts)))
          (with-table-lock store
            (lambda ()
              (let* ((old (read-table store (table-path store kind writer language) kind writer language))
                     (kept (if old
                               (filter (lambda (f) (not (member (fact-own-file f) replaces))) (cadddr old))
                               '())))
                (ensure-ignored! store)
                (write-table! store kind writer language (append kept made)))))
          (list 'ok (list 'supplied (list 'facts (length made)) (list 'files (length replaces))))))))

  ;; (clear-derived store kind path writer) -> (ok (cleared (kind <k>)
  ;; (writer <w>) (language <l>))). The table is named by the file's header
  ;; (checked against the command as a supply's is); the body is not read.
  (define (clear-derived store kind path writer)
    (guard (e ((and (pair? e) (eq? (car e) 'error)) e))
      (let* ((data (supply-lines (supply-bytes path)))
             (header (read-supply-header (header-datum data) kind writer))
             (language (caddr header)))
        (with-table-lock store
          (lambda ()
            (let ((p (table-path store kind writer language)))
              (unless (eq? (entry-type p) 'absent) (unlink! p)))))
        (list 'ok (list 'cleared (list 'kind kind) (list 'writer writer) (list 'language language))))))

  ;; ---- reading --------------------------------------------------------------

  ;; THE FRESH FACTS OF ONE KIND FOR ONE WRITER, judged in the reader's view.
  ;; -> (values ((<payload> <via>) ...) (<stale payload> ...) <tables read>)
  ;; Every language's table for the kind and writer is read; a table that is
  ;; absent (missing, unreadable, tampered with) contributes nothing and is
  ;; not counted as read. Only the facts `relevant?` accepts (by payload)
  ;; are judged and counted. A fact is fresh when every block it lists
  ;; still has the src bytes it was stamped with and every file it was
  ;; stamped on still has its key, in `view`; a deleted block, a changed
  ;; src, a changed child list or language wrapping, or a file that no
  ;; longer projects make it stale.
  ;; THE FILE NAMES OF ONE KIND'S TABLES FOR ONE WRITER, one per language,
  ;; sorted; '() when there is no derived directory. Names only: nothing is
  ;; read, so a caller can ask whether there is anything to consult before
  ;; it builds a view to consult it against.
  (define (table-names store kind writer)
    (let ((dir (derived-dir store))
          (prefix (string-append (symbol->string kind) "-" (percent-encode writer) "-")))
      (if (file-is-directory? dir)
          (list-sort string<?
            (filter (lambda (n) (and (> (string-length n) (+ (string-length prefix) 5))
                                     (string=? prefix (substring n 0 (string-length prefix)))
                                     (string=? ".sexp" (substring n (- (string-length n) 5) (string-length n)))))
                    (directory-entries dir)))
          '())))
  (define (derived-table-named? store kind writer) (pair? (table-names store kind writer)))

  (define (derived-facts* store kind writer view relevant?)
    (let* ((dir (derived-dir store))
           (names (table-names store kind writer))
           (key-memo (make-hashtable string-hash string=?))
           (key-of (lambda (path)
                     (or (hashtable-ref key-memo path #f)
                         (let ((k (path-projection-key view path)))
                           (hashtable-set! key-memo path k) k))))
           (src-sha (lambda (id)
                      (let ((b (state-read view id)))
                        (and b (not (cdr (assq 'deleted b)))
                             (let ((src (code-field view id 'src)))
                               (and (bytevector? src) (bytes-sha src)))))))
           (fresh? (lambda (f)
                     (and (for-all (lambda (s) (equal? (src-sha (car s)) (cadr s))) (fact-dep-stamps f))
                          (for-all (lambda (s) (equal? (key-of (car s)) (list 'key (cadr s))))
                                   (fact-file-stamps f))))))
      (let loop ((ns names) (out '()) (stale '()) (tables 0))
        (if (null? ns)
            (values (reverse out) (reverse stale) tables)
            (let* ((body (read-table store (string-append dir "/" (car ns)) kind writer #f))
                   ;; the name must be the one this table's own identity gives
                   (body (and body (string=? (car ns) (table-file-name kind writer (caddr body))) body))
                   (facts (if body (filter (lambda (f) (relevant? (fact-payload f))) (cadddr body)) '())))
              (let inner ((fs facts) (out out) (stale stale))
                (cond ((null? fs) (loop (cdr ns) out stale (if body (+ tables 1) tables)))
                      ((fresh? (car fs))
                       (inner (cdr fs) (cons (list (fact-payload (car fs)) (fact-via (car fs))) out) stale))
                      (else (inner (cdr fs) out (cons (fact-payload (car fs)) stale))))))))))

  ;; -> (values ((<payload> <via>) ...) <stale count> <tables read>)
  (define (derived-facts store kind writer view relevant?)
    (let-values (((fresh stale tables) (derived-facts* store kind writer view relevant?)))
      (values fresh (length stale) tables)))

  ;; WHAT AN ANSWER THAT CONSULTED A TABLE CARRIES, after its body and in
  ;; this order for every verb: `(stale <n>)`, the stale facts among those
  ;; the answer consulted (possibly 0), then `(via <provenance> ...)`, the
  ;; distinct provenances of the facts it USED (possibly none). A stale
  ;; fact's provenance is never named: it was not used. An answer that read
  ;; no table carries neither, and is the answer it was before tables
  ;; existed.
  (define (derived-clauses tables vias stale)
    (if (= tables 0)
        '()
        (list (list 'stale stale)
              (cons 'via (let loop ((vs vias) (out '()))
                           (cond ((null? vs) (reverse out))
                                 ((member (car vs) out) (loop (cdr vs) out))
                                 (else (loop (cdr vs) (cons (car vs) out)))))))))

  (define (payload-about? tag id)
    (lambda (payload) (and (eq? (car payload) tag) (equal? (cadr payload) id))))
  (define (payload-tagged? tag)
    (lambda (payload) (eq? (car payload) tag)))

  ;; ONE BLOCK'S SIGNATURE. -> (values "<text>" | #f, (<via> ...), <stale>, <tables read>)
  ;; When two fresh facts give one block a signature (two languages' tables,
  ;; say), the first in table order is the answer.
  (define (derived-signature store writer view id)
    (let-values (((facts stale tables) (derived-facts store 'signatures writer view (payload-about? 'signature id))))
      (if (null? facts)
          (values #f '() stale tables)
          (values (caddr (car (car facts))) (list (cadr (car facts))) stale tables))))

  ;; EVERY BLOCK'S SIGNATURE, for a listing.
  ;; -> (values <lookup: id -> (<text> <via>) | #f> <stale> <tables read>)
  (define (derived-signature-table store writer view)
    (let-values (((facts stale tables) (derived-facts store 'signatures writer view (payload-tagged? 'signature))))
      (let ((t (make-hashtable string-hash string=?)))
        (for-each (lambda (f)
                    (let ((id (cadr (car f))))
                      (unless (hashtable-contains? t id)
                        (hashtable-set! t id (list (caddr (car f)) (cadr f))))))
                  facts)
        (values (lambda (id) (hashtable-ref t id #f)) stale tables))))

  ;; EVERY BLOCK'S DERIVED KEYWORDS, for search.
  ;; -> (list <lookup: id -> ((("<word>" ...) <via>) ...) | #f> <tables read> <stale>)
  ;; A block's words are kept in groups, one per fact, each with the
  ;; provenance that supplied it, so a hit names the provenance of the
  ;; words it was found by; whether a block with author keywords uses them
  ;; is the search's rule, not this one's.
  (define (derived-keyword-table store writer view)
    (let-values (((facts stale tables) (derived-facts store 'signatures writer view (payload-tagged? 'keywords))))
      (let ((t (make-hashtable string-hash string=?)))
        (for-each (lambda (f)
                    (let ((id (cadr (car f))))
                      (hashtable-set! t id (append (hashtable-ref t id '())
                                                   (list (list (caddr (car f)) (cadr f)))))))
                  facts)
        (list (lambda (id) (hashtable-ref t id #f)) tables stale))))

  ;; THE SUPPLIED EDGES INTO ONE BLOCK, for `refs`.
  ;; -> (values ((<from> <to> <via>) ...) <stale> <tables read>)
  (define (derived-calls-into store writer view id)
    (let-values (((facts stale tables)
                  (derived-facts store 'calls writer view
                                 (lambda (p) (and (eq? (car p) 'calls) (equal? (caddr p) id))))))
      (values (map (lambda (f) (list (cadr (car f)) (caddr (car f)) (cadr f))) facts) stale tables)))

  ;; WHAT A BLOCK REACHES OVER SUPPLIED EDGES OF ONE RELATION, outward, up to
  ;; `depth` hops; the block itself is at depth 0, and every block is listed
  ;; once, at the fewest hops that reach it.
  ;; -> (values ((<id> <depth>) ...) (<via> ...) <stale> <tables read>)
  ;; The vias are those of the edges that reached a block; the stale count
  ;; is of the edges out of the blocks the walk went on from, dropped
  ;; because what they were computed from has changed.
  (define (derived-reach store writer view id relation depth)
    (let-values (((facts stale tables)
                  (derived-facts* store 'calls writer view (lambda (p) (eq? (car p) relation)))))
      (let ((out-of (lambda (from facts)
                      (list-sort (lambda (a b) (string<? (caddr (car a)) (caddr (car b))))
                                 (filter (lambda (f) (equal? (cadr (car f)) from)) facts)))))
        (let walk ((frontier (list id)) (d 0) (seen (list (list id 0))) (vias '()) (expanded '()))
          (if (or (null? frontier) (>= d depth))
              (values (reverse seen) (reverse vias)
                      (length (filter (lambda (p) (member (cadr p) expanded)) stale))
                      tables)
              (let step ((ns frontier) (next '()) (seen seen) (vias vias))
                (if (null? ns)
                    (walk (reverse next) (+ d 1) seen vias (append expanded frontier))
                    (let edge ((es (out-of (car ns) facts)) (next next) (seen seen) (vias vias))
                      (cond
                        ((null? es) (step (cdr ns) next seen vias))
                        ((assoc (caddr (car (car es))) seen) (edge (cdr es) next seen vias))
                        (else
                         (let ((to (caddr (car (car es)))))
                           (edge (cdr es) (cons to next) (cons (list to (+ d 1)) seen)
                                 (cons (cadr (car es)) vias)))))))))))))
)
