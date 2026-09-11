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

;; The markdown projection: a directory of files and a store, each
;; derivable from the other.
;;
;; EXPORT IS A REPLAY OF STORED BYTES, not a rendering. Every range a
;; file was cut into is kept as it arrived, so a file nobody edited
;; comes back byte for byte and a file someone edited comes back with
;; only the edited range regenerated. That is the whole reason section
;; 2.4 exists, and it is why a parser that understands very little is
;; enough for a projection that loses nothing.
(library (theourgia project)
  (export export-md import-md md-tree)
  (import (rnrs base) (rnrs control) (rnrs lists) (rnrs sorting)
          (rnrs io ports) (rnrs io simple) (rnrs files) (rnrs bytevectors)
          (only (theourgia ffi) mkdir-p! directory-entries file-is-directory?)
          (theourgia md)
          (theourgia reduce)
          (theourgia store))

  (define (field b name)
    (let* ((fs (and b (cdr (assq 'fields b))))
           (e (and fs (assq name fs))))
      (and e (cdr e))))

  (define (text-field b name)
    (let ((v (field b name))) (if (string? v) v "")))

  (define (kind-of b) (field b 'kind))

  ;; ---- the tree a projection walks ----------------------------------------

  ;; THE OUTLINE ALREADY ANSWERS "what is under what, in what order", so
  ;; this does not re-derive it. A second opinion about the shape of the
  ;; tree is a second place for the shape to be wrong.
  (define (children-of rows parent)
    (map caddr (filter (lambda (r) (equal? (car r) parent)) rows)))

  (define (md-tree state)
    (let ((rows (state-outline state)))
      (map (lambda (id)
             (cons id (subtree rows state id)))
           (filter (lambda (id)
                     (eq? 'doc (kind-of (state-read state id))))
                   (children-of rows 'root)))))

  (define (subtree rows state id)
    (map (lambda (child) (cons child (subtree rows state child)))
         (children-of rows id)))

  ;; ---- export --------------------------------------------------------------

  ;; SECTIONS ARE FLATTENED BACK INTO DOCUMENT ORDER. The tree says which
  ;; section is under which; the file is a sequence, and a section's
  ;; children follow its own body -- never interleaved with a sibling's.
  (define (flatten node)
    (cons (car node) (apply append (map flatten (cdr node)))))

  (define (recovery-comment id) (string-append "<!-- theourgia: " id " -->\n"))

;; SECTION 2.4: A STORED HEADING LINE IS ONLY GOOD WHILE IT STILL SAYS
  ;; WHAT THE BLOCK SAYS. Its level and title are both derivable from
  ;; it, so if either has since been changed the line is stale and the
  ;; heading is written afresh from the fields.
  ;; WITHOUT THIS, `set title` LANDED AND EXPORT IGNORED IT: the stored
  ;; bytes were replayed, the file came back with the OLD heading, and
  ;; re-importing that file wrote a second record putting the title back
  ;; to what the file said. The change could not be made to stick.
  (define (effective-heading state id)
    (let* ((b (state-read state id))
           (stored (text-field b 'heading-src))
           (level (or (field b 'level) 1))
           (title (text-field b 'title))
           (parsed (parse-heading stored)))
      (if (and parsed (= (car parsed) level) (string=? (cdr parsed) title))
          stored
          (string-append (make-string level #\#) " " title "
"))))

  ;; Asked of one stored line: the splitter is the only thing that
  ;; decides what a heading line means, so it decides here too.
  (define (parse-heading line)
    (let ((ss (doc-sections (md-split line))))
      (and (= 1 (length ss))
           (cons (section-level (car ss)) (section-title (car ss))))))

  (define (export-md store dir . opts)
    (let* ((recover? (and (pair? opts) (car opts)))
           (state (open-and-reduce store))
           (docs (md-tree state)))
      (for-each
        (lambda (doc)
          (let* ((id (car doc))
                 (b (state-read state id))
                 (path (text-field b 'path))
                 (ids (apply append (map flatten (cdr doc))))
                 (full (string-append dir "/" path)))
            (mkdir-p! (parent-directory full))
            (write-file full
              (md-join (text-field b 'front)
                       (text-field b 'src)
                       (map (lambda (sid)
                              (let ((sb (state-read state sid)))
                                (make-section
                                  (or (field sb 'level) 1)
                                  (text-field sb 'title)
                                  (let ((h (effective-heading state sid)))
                                    (if recover?
                                        (string-append (recovery-comment sid) h)
                                        h))
                                  (text-field sb 'src))))
                            ids)))))
        docs)
      (list 'ok (list 'files (length docs)))))

  (define (parent-directory path)
    (let loop ((i (string-length path)))
      (cond ((<= i 0) ".")
            ((char=? (string-ref path (- i 1)) #\/) (substring path 0 (- i 1)))
            (else (loop (- i 1))))))

  (define (write-file path text)
    (call-with-port (open-file-output-port path (file-options no-fail))
      (lambda (p) (put-bytevector p (string->utf8 text)))))

  (define (read-file path)
    (call-with-port (open-file-input-port path)
      (lambda (p)
        (let ((b (get-bytevector-all p)))
          (if (eof-object? b) "" (utf8->string b))))))

  ;; ---- import --------------------------------------------------------------

  ;; A RECOVERY COMMENT IS TAKEN OUT OF THE TEXT BEFORE IT IS SPLIT.
  ;;
  ;; IT SITS ON ITS OWN LINE BEFORE THE HEADING, which means the splitter
  ;; puts it at the END of the previous range, not at the start of the
  ;; heading's. Looking for it in the heading range found it never: the
  ;; comments survived into the stored bytes and every file exported
  ;; afterwards carried a second copy.
  ;; Returns (cleaned-text . (id-or-#f ...)) with one entry per heading
  ;; in order, so the ids can be zipped onto the sections the splitter
  ;; then finds.
  (define marker-head "<!-- theourgia: ")

  (define (recovery-id line)
    (and (>= (string-length line) (string-length marker-head))
         (string=? (substring line 0 (string-length marker-head)) marker-head)
         (let loop ((i (string-length marker-head)))
           (cond ((> (+ i 4) (string-length line)) #f)
                 ((string=? (substring line i (+ i 4)) " -->")
                  (substring line (string-length marker-head) i))
                 (else (loop (+ i 1)))))))

  (define (strip-recovery text)
    (let ((n (string-length text)))
      (let loop ((i 0) (kept '()) (ids '()) (pending #f))
        (if (>= i n)
            (cons (apply string-append (reverse kept)) (reverse ids))
            (let* ((stop (let scan ((j i))
                           (cond ((>= j n) j)
                                 ((char=? (string-ref text j) #\newline) (+ j 1))
                                 (else (scan (+ j 1))))))
                   (raw (substring text i stop))
                   (line (let back ((k (string-length raw)))
                           (if (and (> k 0)
                                    (memv (string-ref raw (- k 1)) '(#\newline #\return)))
                               (back (- k 1))
                               (substring raw 0 k))))
                   (id (recovery-id line)))
              (cond
                (id (loop stop kept ids id))
                ((heading-line? line)
                 (loop stop (cons raw kept) (cons pending ids) #f))
                (else (loop stop (cons raw kept) ids pending))))))))

  ;; The same rule md-split uses, asked of one line on its own.
  (define (heading-line? line)
    (let ((d (md-split (string-append line "\n"))))
      (= 1 (length (doc-sections d)))))

  (define (md-files dir)
    (let walk ((d dir) (prefix ""))
      (apply append
             (map (lambda (name)
                    (let ((full (string-append d "/" name))
                          (rel (if (string=? prefix "") name
                                   (string-append prefix "/" name))))
                      (cond
                        ((file-is-directory? full) (walk full rel))
                        ((md-name? name) (list rel))
                        (else '()))))
                  (list-sort string<? (directory-entries d))))))

  (define (md-name? name)
    (let ((n (string-length name)))
      (and (> n 3) (string=? (substring name (- n 3) n) ".md"))))

;; DELETION IS NOT INFERRED FROM ABSENCE WITHOUT BEING ASKED. A file
  ;; that is missing from the directory may have been deleted, or the
  ;; directory may be a partial copy, or a sync may be half finished --
  ;; and a tombstone is permanent. So absence is reported by default and
  ;; acted on only when the caller says to.
  (define (import-md store dir . opts)
    (let* ((actor (if (pair? opts) (car opts) "unknown"))
           (allow-delete? (and (pair? opts) (pair? (cdr opts)) (cadr opts)))
           (files (md-files dir)))
;; THE OFFSET IS THREADED BECAUSE `from` COUNTS THE WHOLE BATCH. Each
      ;; file's intents are built on their own, so a file's doc is its
      ;; own intent 0 -- but the batch concatenates them, and the second
      ;; file's `(from 0)` then names the FIRST file's doc. Every
      ;; section of every file after the first was hung under the wrong
      ;; document, and the export put them all in one file.
      (with-store-write store
        (lambda (state view)
          (let ((gone (missing-blocks state dir files)))
            (cond
              ((and (pair? gone) (not allow-delete?))
               (list (list 'error 'would-delete
                           (list 'blocks gone)
                           (list 'remedy 'allow-delete))))
              (else
               (let loop ((fs files) (base 0) (out '()))
                 (if (null? fs)
                     (append (apply append (reverse out))
                             (map (lambda (id) (list 'del id)) gone))
                     (let ((is (file-intents state dir (car fs) base)))
                       (loop (cdr fs) (+ base (length is)) (cons is out)))))))))
        actor)))

  ;; Every document whose file is gone, and every section of a surviving
  ;; document that nothing in the file matches. Sections come first so a
  ;; document is never tombstoned before its children.
  (define (missing-blocks state dir files)
    (let loop ((bs (state-datum state)) (docs '()) (sections '()))
      (cond
        ((null? bs) (append (reverse sections) (reverse docs)))
        (else
         (let* ((id (cadr (car bs)))
                (b (state-read state id)))
           (cond
             ((not (eq? 'doc (kind-of b))) (loop (cdr bs) docs sections))
             ((not (member (text-field b 'path) files))
              (loop (cdr bs) (cons id docs)
                    (append (reverse (doc-sections-of state id)) sections)))
             (else
              (loop (cdr bs) docs
                    (append (reverse (unmatched-sections state dir id b)) sections)))))))))

  (define (unmatched-sections state dir doc-id b)
    (let* ((rel (text-field b 'path))
           (raw (read-file (string-append dir "/" rel)))
           (sections (file-sections raw))
           (old (doc-sections-of state doc-id))
           (matched (match-sections state old sections)))
      (if (and (pair? matched) (eq? (car matched) 'error))
          '()
          (filter (lambda (id)
                    (not (exists (lambda (p) (equal? (car p) id)) matched)))
                  old))))

  ;; The section list of a file, each entry
  ;;   (declared-id level title heading-src src)
  ;; with the recovery comment already taken off the heading.
  (define (file-sections text)
    (let* ((stripped (strip-recovery text))
           (ss (doc-sections (md-split (car stripped)))))
      (let loop ((ss ss) (ids (cdr stripped)) (out '()))
        (if (null? ss)
            (reverse out)
            (loop (cdr ss) (if (null? ids) '() (cdr ids))
                  (cons (list (if (null? ids) #f (car ids))
                              (section-level (car ss)) (section-title (car ss))
                              (section-heading-src (car ss)) (section-src (car ss)))
                        out))))))

  (define (sec-id s) (car s))
  (define (sec-level s) (cadr s))
  (define (sec-title s) (caddr s))
  (define (sec-heading s) (cadddr s))
  (define (sec-src s) (car (cddddr s)))

  ;; NESTING COMES FROM THE LEVELS, and the parent graph must be
  ;; re-derivable from them -- that is the invariant section 2.1 states,
  ;; so import derives it rather than storing a second opinion.
  ;; Each entry is 'doc, or the INDEX of the section that is its parent.
  (define (parents-of sections)
    (let loop ((ss sections) (i 0) (stack '()) (out '()))
      (if (null? ss)
          (reverse out)
          (let* ((lv (sec-level (car ss)))
                 (kept (filter (lambda (e) (< (car e) lv)) stack))
                 (parent (if (null? kept) 'doc (cdr (car kept)))))
            (loop (cdr ss) (+ i 1)
                  (cons (cons lv i) kept)
                  (cons parent out))))))

  (define (file-intents state dir rel base)
;; THE RAW TEXT GOES TO file-sections, NOT THE STRIPPED ONE. Stripping
    ;; here and again in there left the second pass with no comments to
    ;; find, so every declared id was thrown away and a file carrying
    ;; markers was matched as if it carried none.
    (let* ((raw (read-file (string-append dir "/" rel)))
           (text (car (strip-recovery raw)))
           (split (md-split text))
           (sections (file-sections raw))
           (existing (doc-with-path state rel)))
      (if (not existing)
          (new-file-intents rel split sections base)
          (changed-intents state existing rel split sections base))))

  (define (doc-with-path state rel)
    (let loop ((bs (state-datum state)))
      (cond
        ((null? bs) #f)
        ((let ((b (state-read state (cadr (car bs)))))
           (and (eq? 'doc (kind-of b))
                (string=? rel (text-field b 'path))
                (cadr (car bs))))
         => (lambda (id) id))
        (else (loop (cdr bs))))))

  ;; A FILE NOBODY HAS SEEN BEFORE: one doc and one section per heading,
  ;; each placed under the parent its level implies.
  (define (new-file-intents rel split sections base)
    (cons (list 'insert 'root #f
                (list (cons 'kind 'doc)
                      (cons 'path rel)
                      (cons 'front (doc-front split))
                      (cons 'src (doc-src split))))
          (let ((parents (parents-of sections)))
            (let loop ((ss sections) (ps parents) (n 1) (out '()))
              (if (null? ss)
                  (reverse out)
                  (loop (cdr ss) (cdr ps) (+ n 1)
                        ;; INDEX 0 IS THE doc INTENT above; a section
                        ;; whose level puts it at the top of the file
                        ;; hangs from it, and a nested one hangs from
                        ;; the intent that made its parent.
                        (cons (list 'insert
                                    (if (eq? (car ps) 'doc)
                                        (list 'from base)
                                        (list 'from (+ base 1 (car ps))))
                                    #f
                                    (section-fields (car ss)))
                              out)))))))

  ;; A FILE THAT IS ALREADY IN THE STORE. Identity is decided in the
  ;; order section 8.1 gives: a declared id wins outright; otherwise the
  ;; structural position (parent, index among siblings); and where the
  ;; position does not line up, the heading and body together are the
  ;; signature -- but only when that signature picks out exactly one
  ;; candidate. Two refusals come out of this and they are different
  ;; things: the file moved something (position), or the file cannot say
  ;; which of several identical sections it means (ambiguous).
  (define (changed-intents state doc-id rel split sections base)
    (let* ((old (doc-sections-of state doc-id))
           (matched (match-sections state old sections)))
      (cond
        ((and (pair? matched) (eq? (car matched) 'error)) (list matched))
        (else
         (let ((doc-intents (doc-field-intents state doc-id split)))
           (append
             doc-intents
             (section-intents state doc-id sections matched
                              (+ base (length doc-intents)))))))))

  ;; A NEW SECTION GOES WHERE ITS HEADING SAYS, not under the document.
  ;; Hanging every new section off the doc made `# A / ## B` come back
  ;; with B as A's SIBLING -- so re-parsing the exported file gave a
  ;; different parent graph from the one that was imported, which is the
  ;; one thing section 2.1 says must always re-derive.
  ;;
  ;; THE PARENT MAY NOT EXIST YET. If B's parent A is also new, A's id
  ;; is only known once its own insert commits, so B names it with the
  ;; batch back-reference -- which is what that mechanism is for.
  (define (section-intents state doc-id sections matched base)
    (let ((parents (parents-of sections))
          (refs (make-vector (length sections) #f)))
      (let loop ((i 0) (ms matched) (ps parents) (n base) (out '()))
        (if (null? ms)
            (reverse out)
            (let* ((stored (car (car ms)))
                   (incoming (cdr (car ms)))
                   (parent-ix (car ps)))
              (if stored
                  (let ((is (section-field-intents state stored incoming)))
                    (vector-set! refs i stored)
                    (loop (+ i 1) (cdr ms) (cdr ps) (+ n (length is))
                          (append (reverse is) out)))
                  (let* ((parent (if (eq? parent-ix 'doc)
                                     doc-id
                                     (vector-ref refs parent-ix)))
                         (after (previous-sibling refs parents i parent-ix))
                         (intent (list 'insert parent after
                                       (section-fields incoming))))
                    (vector-set! refs i (list 'from n))
                    (loop (+ i 1) (cdr ms) (cdr ps) (+ n 1)
                          (cons intent out)))))))))

  ;; The nearest earlier section with the same parent, so a new one lands
  ;; after the sibling it follows in the file rather than at the end.
  (define (previous-sibling refs parents i parent-ix)
    (let loop ((j (- i 1)))
      (cond
        ((< j 0) #f)
        ((equal? (list-ref parents j) parent-ix) (vector-ref refs j))
        (else (loop (- j 1))))))

  (define (section-fields s)
    (list (cons 'kind 'section)
          (cons 'level (sec-level s))
          (cons 'title (sec-title s))
          (cons 'heading-src (sec-heading s))
          (cons 'src (sec-src s))))
  (define (doc-sections-of state doc-id)
    (let ((rows (state-outline state)))
      (let walk ((id doc-id))
        (apply append
               (map (lambda (child) (cons child (walk child)))
                    (children-of rows id))))))

  ;; IDENTITY, IN THE ORDER SECTION 8.1 GIVES IT.
  ;;
  ;;   1. a declared id wins outright -- that is what the recovery form
  ;;      is for, and a file that carries one is not guessing;
  ;;   2. otherwise the structural position, when the counts line up;
  ;;   3. otherwise the heading and body together are the signature, and
  ;;      it has to pick out exactly one stored section.
  ;;
  ;; THE TWO REFUSALS ARE DIFFERENT SITUATIONS AND SAY SO.
  ;;   `position-mismatch`   the file declares an id this document does
  ;;                         not contain -- it is talking about some
  ;;                         other document, or about a deleted block.
  ;;   `ambiguous-identity`  the file changed, and a section in it looks
  ;;                         exactly like more than one stored section,
  ;;                         so nothing in the bytes says which one it
  ;;                         is. This is the case a marker fixes, which
  ;;                         is why the refusal asks for one.
  (define (signature-of-stored state id)
    (let ((b (state-read state id)))
      (cons (text-field b 'heading-src) (text-field b 'src))))

  (define (signature-of-incoming s) (cons (sec-heading s) (sec-src s)))

  (define (match-sections state old incoming)
    (let ((declared (filter (lambda (s) (sec-id s)) incoming)))
      (cond
        ;; a declared id that is not one of this document's sections
        ((exists (lambda (s) (not (member (sec-id s) old))) declared)
         => (lambda (ignored)
              (list 'error 'position-mismatch
                    (list 'declared
                          (map sec-id
                               (filter (lambda (s) (not (member (sec-id s) old)))
                                       declared))))))
        ((= (length declared) (length incoming))
         (map (lambda (s) (cons (sec-id s) s)) incoming))
        ((= (length old) (length incoming))
         (let loop ((o old) (i incoming) (out '()))
           (if (null? o)
               (reverse out)
               (loop (cdr o) (cdr i) (cons (cons (car o) (car i)) out)))))
        (else (match-by-signature state old incoming)))))

  ;; THE HEADING IS THE KEY; THE BODY ONLY BREAKS TIES.
  ;;
  ;; Matching on the whole (heading, body) signature cannot tell an
  ;; EDITED section from a DELETED one. Deleting the last section of a
  ;; file changes the previous section's byte range -- its body now runs
  ;; to the end of the file instead of to the next heading, so a blank
  ;; line that used to belong to it is gone. Its signature therefore
  ;; changed too, it matched nothing, and it was reported as about to be
  ;; tombstoned alongside the section that really had been deleted --
  ;; and with --allow-delete it was tombstoned and rebuilt under a new
  ;; id. One deletion cost two blocks and moved an id that nobody
  ;; touched.
  ;;
  ;; A heading is what a person means by "this section"; its body is
  ;; what they edit. So sections are aligned by level and title, and the
  ;; body is consulted only where several stored sections carry the same
  ;; heading -- which is exactly the case A6 is about.
  (define (heading-key s) (cons (sec-level s) (sec-title s)))

  (define (stored-heading-key state id)
    (let ((b (state-read state id)))
      (cons (or (field b 'level) 1) (text-field b 'title))))

  (define (match-by-signature state old incoming)
    (let loop ((is incoming) (free old) (out '()))
      (if (null? is)
          (reverse out)
          (let* ((want (heading-key (car is)))
                 (hits (filter (lambda (id) (equal? (stored-heading-key state id) want))
                               free)))
            (cond
              ((null? hits) (loop (cdr is) free (cons (cons #f (car is)) out)))
              ((null? (cdr hits))
               (loop (cdr is) (remp (lambda (id) (equal? id (car hits))) free)
                     (cons (cons (car hits) (car is)) out)))
              (else
               ;; SEVERAL SECTIONS SHARE THIS HEADING. The body decides,
               ;; and only an exact body match decides: if one of them is
               ;; this section's body, that is the one. If none is, the
               ;; body was edited and the earliest unused candidate is
               ;; taken -- document order is the only thing left to go
               ;; on. If several have the SAME body too, nothing in the
               ;; file says which, and that is the refusal A6(iii) wants.
               (let ((exact (filter (lambda (id)
                                      (string=? (text-field (state-read state id) 'src)
                                                (sec-src (car is))))
                                    hits)))
                 (cond
                   ((and (pair? exact) (null? (cdr exact)))
                    (loop (cdr is) (remp (lambda (id) (equal? id (car exact))) free)
                          (cons (cons (car exact) (car is)) out)))
                   ((> (length exact) 1)
                    (list 'error 'ambiguous-identity
                          (list 'candidates exact)
                          (list 'remedy 'marker)))
                   (else
                    (loop (cdr is) (remp (lambda (id) (equal? id (car hits))) free)
                          (cons (cons (car hits) (car is)) out)))))))))))

  (define (doc-field-intents state doc-id split)
    (let ((b (state-read state doc-id)))
      (append
        (if (string=? (text-field b 'front) (doc-front split))
            '()
            (list (list 'set doc-id 'front (doc-front split))))
        (if (string=? (text-field b 'src) (doc-src split))
            '()
            (list (list 'set doc-id 'src (doc-src split)))))))

  ;; ONLY THE FIELDS THAT DIFFER. This is what makes a local edit produce
  ;; one record: every other section compares equal and contributes
  ;; nothing at all.
  ;; THE COMPARISON IS AGAINST WHAT EXPORT WOULD WRITE, not against the
  ;; stored bytes. After a `set title` the stored heading line is stale;
  ;; the file on disk carries the regenerated one, and comparing it to
  ;; the stale bytes said "the file changed the heading" and wrote a
  ;; record for a file nobody had touched -- which put the old title
  ;; back. Comparing against the effective heading makes re-importing an
  ;; untouched export cost nothing, which is the property that makes a
  ;; projection a projection.
  (define (section-field-intents state id incoming)
    (let ((b (state-read state id)))
      (if (string=? (effective-heading state id) (sec-heading incoming))
          ;; the heading is already what the file says; only the body
          ;; can differ
          (if (string=? (text-field b 'src) (sec-src incoming))
              '()
              (list (list 'set id 'src (sec-src incoming))))
          (append
            (if (equal? (field b 'level) (sec-level incoming))
                '()
                (list (list 'set id 'level (sec-level incoming))))
            (if (string=? (text-field b 'title) (sec-title incoming))
                '()
                (list (list 'set id 'title (sec-title incoming))))
            (list (list 'set id 'heading-src (sec-heading incoming)))
            (if (string=? (text-field b 'src) (sec-src incoming))
                '()
                (list (list 'set id 'src (sec-src incoming)))))))))
