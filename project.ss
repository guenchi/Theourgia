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
                                  (if recover?
                                      (string-append (recovery-comment sid)
                                                     (text-field sb 'heading-src))
                                      (text-field sb 'heading-src))
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

  (define (import-md store dir . opts)
    (let* ((actor (if (pair? opts) (car opts) "unknown"))
           (files (md-files dir)))
;; THE OFFSET IS THREADED BECAUSE `from` COUNTS THE WHOLE BATCH. Each
      ;; file's intents are built on their own, so a file's doc is its
      ;; own intent 0 -- but the batch concatenates them, and the second
      ;; file's `(from 0)` then names the FIRST file's doc. Every
      ;; section of every file after the first was hung under the wrong
      ;; document, and the export put them all in one file.
      (with-store-write store
        (lambda (state view)
          (let loop ((fs files) (base 0) (out '()))
            (if (null? fs)
                (apply append (reverse out))
                (let ((is (file-intents state dir (car fs) base)))
                  (loop (cdr fs) (+ base (length is)) (cons is out))))))
        actor)))

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
          (changed-intents state existing rel split sections))))

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
                                    (list (cons 'kind 'section)
                                          (cons 'level (sec-level (car ss)))
                                          (cons 'title (sec-title (car ss)))
                                          (cons 'heading-src (sec-heading (car ss)))
                                          (cons 'src (sec-src (car ss)))))
                              out)))))))

  ;; A FILE THAT IS ALREADY IN THE STORE. Identity is decided in the
  ;; order section 8.1 gives: a declared id wins outright; otherwise the
  ;; structural position (parent, index among siblings); and where the
  ;; position does not line up, the heading and body together are the
  ;; signature -- but only when that signature picks out exactly one
  ;; candidate. Two refusals come out of this and they are different
  ;; things: the file moved something (position), or the file cannot say
  ;; which of several identical sections it means (ambiguous).
  (define (changed-intents state doc-id rel split sections)
    (let* ((old (doc-sections-of state doc-id))
           (matched (match-sections state old sections)))
      (cond
        ((and (pair? matched) (eq? (car matched) 'error)) (list matched))
        (else
         (append
           (doc-field-intents state doc-id split)
           ;; A PAIR WITH NO STORED SIDE IS A SECTION THE FILE GREW, and
           ;; it becomes one. Skipping it read as "nothing to do": the
           ;; import answered with no intents at all and the new section
           ;; was silently dropped, so the next export wrote the file
           ;; back WITHOUT the text somebody had just added.
           ;; IT GOES UNDER THE DOCUMENT. Putting it at the level its
           ;; heading implies is what a later batch owes; losing it is
           ;; not a lesser version of that.
           (apply append
                  (map (lambda (pair)
                         (if (car pair)
                             (section-field-intents state (car pair) (cdr pair))
                             (list (new-section-intent doc-id (cdr pair)))))
                       matched)))))))

  (define (new-section-intent parent s)
    (list 'insert parent #f
          (list (cons 'kind 'section)
                (cons 'level (sec-level s))
                (cons 'title (sec-title s))
                (cons 'heading-src (sec-heading s))
                (cons 'src (sec-src s)))))

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

  (define (match-by-signature state old incoming)
    (let ((sigs (map (lambda (id) (cons id (signature-of-stored state id))) old)))
      (let loop ((is incoming) (out '()))
        (if (null? is)
            (reverse out)
            (let* ((want (signature-of-incoming (car is)))
                   (hits (filter (lambda (e) (equal? (cdr e) want)) sigs)))
              (cond
                ((> (length hits) 1)
                 (list 'error 'ambiguous-identity
                       (list 'candidates (map car hits))
                       (list 'remedy 'marker)))
                ;; no stored section looks like this one: it is new
                ((null? hits) (loop (cdr is) (cons (cons #f (car is)) out)))
                (else (loop (cdr is) (cons (cons (car (car hits)) (car is)) out)))))))))

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
  (define (section-field-intents state id incoming)
    (let ((b (state-read state id)))
      (append
        (if (equal? (field b 'level) (sec-level incoming))
            '()
            (list (list 'set id 'level (sec-level incoming))))
        (if (string=? (text-field b 'title) (sec-title incoming))
            '()
            (list (list 'set id 'title (sec-title incoming))))
        (if (string=? (text-field b 'heading-src) (sec-heading incoming))
            '()
            (list (list 'set id 'heading-src (sec-heading incoming))))
        (if (string=? (text-field b 'src) (sec-src incoming))
            '()
            (list (list 'set id 'src (sec-src incoming))))))))
