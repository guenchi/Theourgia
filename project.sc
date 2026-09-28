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
  (export export-md export-md-view import-md import-md-report md-tree subtree-ids block-text md-kinds)
  (import (rnrs base) (rnrs control) (rnrs lists) (rnrs sorting) (rnrs hashtables)
          (rnrs unicode)
          (rnrs io ports) (rnrs io simple) (rnrs files) (rnrs bytevectors)
          (only (rnrs exceptions) raise guard)
          (only (theourgia ffi) mkdir-p! file-is-directory?
                real-path path-case-sensitive? entry-type entry-bytes overwrite-entry!
                list-entries unreadable-entry? unreadable-entry-path unreadable-entry-reason
                fs-error? fs-error-target fs-error-errno)
          ;; NEVER: THE RULE IS SHARED, NOT COPIED. `code-safe-path?` is
          ;; `relative-safe?` in the code projection, and it already says what
          ;; a path a projection may write has to be. A second rule here --
          ;; and the first version of this file had one, weaker: it allowed a
          ;; NUL and a backslash and DROPPED `.` components rather than
          ;; refusing them -- is how two projections come to disagree about
          ;; what is safe. This is a sibling import rather than a shared
          ;; library because moving the rule somewhere lower touches both
          ;; projections; that move is recorded, not done here.
          (only (theourgia code-project) code-safe-path?)
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

  ;; A DOCUMENT IS A TOP-LEVEL BLOCK OF KIND `doc`, AND EACH IS ONE FILE
  ;; HOLDING ITS WHOLE SUBTREE (F85 R2). The write path refuses to put a
  ;; document under another block. One that arrives there anyway, from
  ;; history written before that rule or from another store, is to this
  ;; projection a block like any other: it is written, under its title, into
  ;; the file of the top-level document above it when that one is written,
  ;; and goes with it into `skipped` otherwise. It is reported once, as
  ;; `nested-documents` by the structure rules and as `nested-document` by
  ;; `conflicts`, and no reader branches on it (F85 R3).
  ;;
  ;; Each entry is the document's id followed by every block under it, in
  ;; document order: the outline's own walk, the one definition of "under"
  ;; (F85 R1).
  (define (md-tree state)
    (let ((rows (state-outline state)))
      (map (lambda (id) (outline-subtree rows id))
           (map caddr
                (filter (lambda (r)
                          (and (eq? (car r) 'root)
                               (eq? 'doc (kind-of (state-read state (caddr r))))))
                        rows)))))

  ;; ---- export --------------------------------------------------------------

  (define (recovery-comment id) (string-append "<!-- theourgia: " id " -->\n"))

;; SECTION 2.4: A STORED HEADING LINE IS ONLY GOOD WHILE IT STILL SAYS
  ;; WHAT THE BLOCK SAYS. Its level and title are both derivable from
  ;; it, so if either has since been changed the line is stale and the
  ;; heading is written afresh from the fields.
  ;; WITHOUT THIS, `set title` LANDED AND EXPORT IGNORED IT: the stored
  ;; bytes were replayed, the file came back with the OLD heading, and
  ;; re-importing that file wrote a second record putting the title back
  ;; to what the file said. The change could not be made to stick.
  (define (effective-level b)
    (let ((v (field b 'level)))
      (if (and (integer? v) (exact? v) (> v 0) (< v 7)) v 1)))

  (define (effective-heading state id)
    (let* ((b (state-read state id))
           (stored (text-field b 'heading-src))
           ;; A LEVEL THAT IS NOT A LEVEL IS TREATED AS ABSENT. This is
           ;; used as a string length two lines down, so a value of "2"
           ;; -- which is what the command line produces, because it
           ;; passes every field value as text -- raised instead of
           ;; rendering. A reader may refuse to understand a value; it
           ;; may not fall over on one.
           (level (effective-level b))
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

  ;; ONE RENDERER, AND EXPORT IS ONE OF ITS CALLERS. Reading a document
  ;; back and writing it to a file are the same question asked in two
  ;; places, so a second renderer for `read` would drift from this one --
  ;; and the drift would surface as a file that round-trips and a `read`
  ;; that does not agree with it, which is the hardest kind of
  ;; disagreement to notice.
  ;;
  ;; A DOCUMENT'S OWN TEXT COMES BEFORE ITS SECTIONS; a section's heading
  ;; comes before its body, and its children follow its body rather than
  ;; being interleaved with a sibling's. That is the same flattening the
  ;; file has, because it IS the file.
  (define (block-text state id recover?)
    (let ((b (state-read state id)))
      (and b
           (if (eq? 'doc (kind-of b))
               (md-join (text-field b 'front) (text-field b 'src)
                        (map (lambda (sid) (rendered-section state sid recover?))
                             (descendant-ids state id)))
               ;; A SECTION IS ITS OWN HEADING AND BODY FIRST. The
               ;; document's front matter belongs to the document, so a
               ;; section asked for on its own does not carry it.
               (md-join "" ""
                        (map (lambda (sid) (rendered-section state sid recover?))
                             (cons id (descendant-ids state id))))))))

  (define (rendered-section state sid recover?)
    (let ((sb (state-read state sid)))
      (make-section
        (or (field sb 'level) 1)
        (text-field sb 'title)
        (let ((h (effective-heading state sid)))
          (if recover? (string-append (recovery-comment sid) h) h))
        (text-field sb 'src))))

  ;; THE BLOCK AND EVERYTHING UNDER IT, in document order: the outline's own
  ;; walk (`outline-subtree`) and nothing else, the one definition of
  ;; "under" (F85 R1). `read --recursive` answers with this and `grep
  ;; --under` searches `outline-subtree` itself, so the two name the same set
  ;; of blocks by construction. The root is kept whatever its kind.
  ;;
  ;; NEVER: A SECOND RULE ABOUT WHAT IS UNDER A BLOCK. This once dropped every
  ;; document found below the root, with everything under it, while `grep
  ;; --under` did not: two answers to one question, differing exactly on the
  ;; stores that were already malformed.
  (define (subtree-with-root state id)
    (outline-subtree (state-outline state) id))

  ;; Every block under this one, in document order and not including it.
  (define (descendant-ids state id)
    (cdr (subtree-with-root state id)))

  ;; The block and everything under it, which is what `--recursive`
  ;; answers with, or #f when there is no such block. `#f` here means that
  ;; and nothing else; the walk underneath never returns it.
  (define (subtree-ids state id)
    (and (state-read state id) (subtree-with-root state id)))

  ;; THE KINDS THIS PROJECTION WRITES. Not a second opinion about which kinds
  ;; are legal -- `known-kinds` in the reducer is the only thing that decides
  ;; that, and every write route asks it. This says which of those kinds this
  ;; verb is responsible for, so that a `code` block no markdown file contains
  ;; is understood as addressed elsewhere rather than reported as lost.
  (define md-kinds '(doc section))

  ;; THE PATH IS CHECKED BEFORE ANYTHING IS WRITTEN, AND TWO SPELLINGS OF ONE
  ;; FILE ARE ONE FILE. Readings that got here, each of them something this
  ;; verb had always done:
  ;;
  ;;   "../x.md" wrote OUTSIDE the directory the caller named and answered ok
  ;;   a document with no path opened that directory as a file
  ;;   "same.md" and "./same.md" replaced each other with no conflict reported
  ;;   "a.md" and "A.md" are one file on a case-insensitive volume, and the
  ;;     conflict key compared them as text
  ;;   a NUL in a path is a truncation point for the system and was not for us
  ;;   a symlink inside the target directory leads out of it
  ;;
  ;; THE TEXTUAL RULE IS `code-safe-path?`, shared with the code projection.
  ;; THE IDENTITY OF A FILE IS ASKED OF THE FILESYSTEM, which is what
  ;; `client.sc`'s `key-name` does for store keys, under a comment worth
  ;; reading: a key derived from the spelling gives one store several
  ;; identities. The primitives are the same ones (`real-path`,
  ;; `path-case-sensitive?`); this is a second CALLER of that technique, not a
  ;; second statement of it, because `key-name` is not exported and closing
  ;; over it would mean moving it.
  ;;
  ;; TWO NUANCES TAKEN FROM THERE, BOTH MEASURED THERE FIRST: the question
  ;; goes to the nearest EXISTING directory, because a file that is not there
  ;; yet has no filesystem to answer for it; and `unknown` folds nothing,
  ;; since folding on a case-sensitive volume would give one key to two files
  ;; that really are different -- the worse direction of the two.
  ;;
  ;; NOT DONE HERE, AND RECORDED RATHER THAN LEFT SILENT: nothing is `stat`ed
  ;; before writing, so a path naming an existing directory, or `x.md` beside
  ;; `x.md/y.md`, still reaches `mkdir-p!` and `write-file`. The code
  ;; projection refuses those with `unsupported-file` after looking; doing the
  ;; same here is a separate piece of work.
  (define (nearest-existing path)
    (if (not (eq? (entry-type path) 'absent))
        path
        (let ((parent (parent-directory path)))
          (if (string=? parent path) path (nearest-existing parent)))))

  (define (under? dir path)
    (let ((n (string-length dir)))
      (and (>= (string-length path) n)
           (string=? (substring path 0 n) dir)
           (or (= (string-length path) n)
               (char=? (string-ref path n) #\/)))))

  ;; The absolute name of the file this path would write, with symlinks
  ;; resolved as far as anything exists, or #f when the path may not be
  ;; written at all.
  (define (file-key dir path)
    (and (code-safe-path? path)
         (let* ((home (or (real-path dir) dir))
                (full (string-append home "/" path))
                (anchor (real-path (nearest-existing full))))
           (and anchor
                (under? home anchor)
                (if (eq? #f (path-case-sensitive? (nearest-existing full)))
                    (string-downcase full)
                    full)))))

  ;; NEVER: A DIRECTORY THAT IS NOT THERE IS REFUSED BY NAME, before anything
  ;; is read or written (F82, F83), in the tree's own words for a directory
  ;; input that is not one (code-project.sc's not-a-directory), with the
  ;; directory named. Export used to answer (ok (files 0)) with every
  ;; document's path marked unusable, because each file's key could not be
  ;; resolved under a directory that did not exist; import raised a bare
  ;; condition from the listing, answered (error internal ...). Neither
  ;; creates the directory: a mistyped path is the likelier cause, and a
  ;; directory made for it would be a second mistake.
  (define (require-md-directory dir)
    (unless (file-is-directory? dir)
      (raise (list 'error 'projection-invalid
                   (list 'reason 'not-a-directory)
                   (list 'dir dir)))))

;; NOTE: THE VIEW IS A PARAMETER (F17). `export-md` exports the committed
  ;; state; `export-md-view` exports whatever reduction `view` hands it --
  ;; the writer's working view for `--working`. `view` is called after the
  ;; directory is checked, so a bad directory is answered before the store
  ;; is opened on both routes, as it was on the one route before.
  (define (export-md store dir . opts)
    (export-md-view dir (lambda () (open-and-reduce store)) (and (pair? opts) (car opts))))

  (define (export-md-view dir view recover?)
    (require-md-directory dir)
    (let* ((state (view))
           (docs (md-tree state))
           (path-of (lambda (id) (text-field (state-read state id) 'path)))
           (winner (make-hashtable string-hash string=?))
           (doc-key (make-hashtable string-hash string=?)))
      ;; NEVER: TWO DOCUMENTS WITH ONE PATH ARE NOT TWO FILES. Nothing stops
      ;; two doc blocks carrying the same `path`. Both were written to it, the
      ;; second replacing the first, and the answer counted two files while one
      ;; document's text no longer existed anywhere -- the exact shape this
      ;; round is about, in the one place that was still doing it.
      ;;
      ;; The first by ID wins. Not the first in `md-tree`'s order, which
      ;; follows the generated writer name and would make which document
      ;; survives a matter of chance.
      ;; The key of each document is computed ONCE, here, and every later
      ;; question -- was it written, did it lose, is its path usable -- is
      ;; answered from this table. Asking the filesystem again further down
      ;; would be a second reading of something that can change underneath.
      (for-each
        (lambda (doc)
          (hashtable-set! doc-key (car doc) (file-key dir (path-of (car doc)))))
        docs)
      ;; NEVER: A FILE THAT CANNOT BE WRITTEN IS NAMED, NOT CALLED INTERNAL
      ;; (F98). A target directory that stats but cannot be written into --
      ;; mode 000 passes the directory test above, which needs search only on
      ;; its parent -- failed at the first write with a bare port error. It is
      ;; answered in R1's words, the file and the system's reason, AND with
      ;; the files already written: an export stopped part way has changed
      ;; the directory, and the answer says how far.
      (let ((written '()))
        (for-each
          (lambda (doc)
            (let* ((id (car doc))
                   (key (hashtable-ref doc-key id #f)))
              (when (and key (not (hashtable-ref winner key #f)))
                (hashtable-set! winner key id)
                (let ((full (string-append dir "/" (path-of id))))
                  (guard (e ((unreadable-entry? e)
                             (raise (unwritten (unreadable-entry-path e)
                                               (unreadable-entry-reason e)
                                               written)))
                            ((fs-error? e)
                             (raise (unwritten (fs-error-target e)
                                               (durable-failure-reason e)
                                               written))))
                    (mkdir-p! (parent-directory full))
                    (write-file full (block-text state id recover?)))
                  (set! written (cons (path-of id) written))))))
          (list-sort (lambda (a b) (string<? (car a) (car b))) docs)))
      (let* ((rows (state-outline state))
             (written (make-hashtable string-hash string=?))
             (parent (make-hashtable string-hash string=?))
             (children (make-hashtable string-hash string=?))
             (enumerated (make-hashtable string-hash string=?))
             (a-document?
               (let ((ids (make-hashtable string-hash string=?)))
                 (for-each (lambda (doc) (hashtable-set! ids (car doc) #t)) docs)
                 (lambda (id) (hashtable-ref ids id #f))))
             ;; NEVER: AND ONLY A DOCUMENT CAN LOSE A PATH. `text-field`
             ;; answers "" for a block that has no path at all, which is most
             ;; of them, so asking this question of every block would make one
             ;; document with a missing path the winner of "" and every
             ;; unwritten SECTION its loser -- a reason that is wrong about
             ;; both blocks it names.
             (lost-path
               (lambda (id)
                 (and (a-document? id)
                      (let ((key (hashtable-ref doc-key id #f)))
                        (and key
                             (let ((w (hashtable-ref winner key #f)))
                               (and w (not (equal? w id)) w)))))))
             (unusable-path
               (lambda (id)
                 (and (a-document? id) (not (hashtable-ref doc-key id #f))))))
        ;; NEVER: THE ROWS ARE WALKED ONCE, NOT ONCE PER DOC AND NOT ONCE PER
        ;; ENTRY. `subtree-ids` and `descendant-ids` each rebuild the whole
        ;; outline, so asking either inside a loop made export quadratic in
        ;; whatever the loop ran over. Both were measured after the fact, on
        ;; shapes the fixtures do not have:
        ;;
        ;;   once per doc   -- 200 one-heading documents: 280 ms before this
        ;;                     round, 540 ms with the per-doc call
        ;;   once per entry -- 600 unwritten blocks at root: 806 ms, and
        ;;                     climbing as the cube; at a few thousand it is
        ;;                     seconds
        ;;
        ;; A fixture with one document and two skipped blocks cannot see
        ;; either. One pass builds the parent map, the child map and the set
        ;; of rows; the reach is walked from the child map.
        (for-each (lambda (r)
                    (hashtable-set! enumerated (caddr r) #t)
                    (hashtable-set! parent (caddr r) (car r))
                    (when (string? (car r))
                      (hashtable-set! children (car r)
                                      (cons (caddr r)
                                            (hashtable-ref children (car r) (quote ()))))))
                  rows)
        ;; THE WALK TAKES EACH WRITTEN DOCUMENT'S WHOLE SUBTREE, because the
        ;; file holds its whole subtree (F85 R2): a block of kind `doc` below
        ;; it was written into it like any other, and is marked written here
        ;; for the same reason.
        ;; NEVER: THE WALK STARTS FROM THE DOCUMENTS THAT GOT A FILE. "Not a
        ;; path loser" is not the same set: a document whose path cannot be
        ;; used is not a loser either, and starting from it marked it and its
        ;; whole subtree written when nothing had been written at all -- the
        ;; answer then had no skip clause to put it in.
        (let walk ((ids (map car
                             (filter (lambda (doc)
                                       (let ((k (hashtable-ref doc-key (car doc) #f)))
                                         (and k (equal? (hashtable-ref winner k #f) (car doc)))))
                                     docs))))
          (cond ((null? ids) 'done)
                ((hashtable-ref written (car ids) #f) (walk (cdr ids)))
                (else
                  (hashtable-set! written (car ids) #t)
                  (walk (append (hashtable-ref children (car ids) (quote ()))
                                (cdr ids))))))
        (let* ((unwritten? (lambda (id) (not (hashtable-ref written id #f))))
               ;; NEVER: AND THE KIND SAYS WHOSE BLOCK IT IS, NOT WHETHER IT
               ;; IS LEGAL. `md-kinds` is the set this projection writes; a
               ;; `code`, `library` or `file` block that no markdown file
               ;; contains was addressed to another projection, not skipped by
               ;; this one. THE ONE AUTHORITY ON WHICH KINDS EXIST AT ALL IS
               ;; `known-kinds` IN THE REDUCER -- this list says which of them
               ;; are this verb's business, a different question, and
               ;; `facade-gate` names both places and checks this one is a
               ;; subset of that one.
               ;; NEVER: AND THE REACH COUNTS THE SAME BLOCKS THE REASONS DO.
               ;; It used to count every unwritten descendant, including ones
               ;; addressed to another projection: a section with a `code`
               ;; child reported `(subtree 1)`, making `1 + n` say two blocks
               ;; were lost when only one was this verb's to lose. The sum has
               ;; to be over one population or it is not an invariant.
               (ours?
                 (lambda (id)
                   (let* ((b (state-read state id))
                          (fs (and b (assq 'fields b)))
                          (e (and fs (assq 'kind (cdr fs)))))
                     (and b
                          (or (not e)
                              (not (symbol? (cdr e)))
                              (memq (cdr e) md-kinds))
                          #t))))
               (reason-for
                 (lambda (id)
                   (let* ((b (state-read state id))
                          (fs (and b (assq 'fields b)))
                          ;; `field` answers #f both for "there is no kind"
                          ;; and for "the kind is #f", so a block carrying
                          ;; `(kind . #f)` -- which only an older store can
                          ;; hold, since the write path now refuses it -- was
                          ;; reported as having no kind at all.
                          (e (and fs (assq 'kind (cdr fs)))))
                     (cond ((not b) #f)
                           ((not (unwritten? id)) #f)
                           ((not e) 'kind-absent)
                           ((not (symbol? (cdr e))) 'kind-not-a-symbol)
                           ((not (memq (cdr e) md-kinds)) #f)
                           ((unusable-path id) 'path-not-usable)
                           ((lost-path id) 'path-conflict)
                           (else 'not-in-any-document)))))
               ;; NEVER: SUPPRESSION REQUIRES AN ANCESTOR THAT IS ACTUALLY
               ;; LISTED, NOT MERELY ONE THAT HAS A REASON. A DELETED doc
               ;; stays readable through `state-read` while dropping out of
               ;; the outline rows, so an ancestor could have a reason and yet
               ;; never appear in the answer. Measured: delete a doc with a
               ;; live section under it and the section, which now belongs to
               ;; no document, was suppressed in favour of an entry that was
               ;; never written -- `(ok (files 0))`, nothing on disk, two live
               ;; blocks reported by nobody.
               (listed?
                 (lambda (id)
                   (and (hashtable-ref enumerated id #f) (reason-for id) #t)))
               (named?
                 (lambda (id)
                   (let loop ((up (hashtable-ref parent id #f)))
                     (cond ((not up) #f)
                           ((eq? up (quote root)) #f)
                           ((listed? up) #t)
                           (else (loop (hashtable-ref parent up #f)))))))
               ;; NEVER: AND THE CAUSE IS NAMED ONCE, WITH ITS REACH. When a
               ;; doc goes unwritten its whole subtree goes with it, and a doc
               ;; of two hundred blocks would otherwise report two hundred
               ;; lines. The outermost unwritten block is named and carries
               ;; the number of this projection's blocks below it. Every one
               ;; of them went unwritten too: a file holds its document's
               ;; whole subtree (F85 R2), so a block below an unwritten one
               ;; has no other way into a file. Measured before the filter
               ;; that asked "unwritten?" here was removed: every export of a
               ;; full suite run, 626 entries, counted the same with it and
               ;; without it.
               ;;
               ;; Entries never nest, so `1 + n` summed over them counts each
               ;; lost block exactly once, and a caller can add them up.
               (reach
                 (lambda (id)
                   (let loop ((ids (hashtable-ref children id (quote ()))) (n 0))
                     (cond ((null? ids) n)
                           (else
                             (loop (append (hashtable-ref children (car ids) (quote ()))
                                           (cdr ids))
                                   (if (ours? (car ids)) (+ n 1) n)))))))
               (entry
                 (lambda (id r)
                   (cons id
                         (cons r
                               (append (cond
                                         ((eq? r 'path-conflict)
                                          (list (list 'with (lost-path id))))
                                         ;; The RAW path, not a normalised
                                         ;; one: the caller has to be able to
                                         ;; recognise what it wrote.
                                         ((eq? r 'path-not-usable)
                                          (list (list 'path (path-of id))))
                                         (else (quote ())))
                                       (list (list (quote subtree) (reach id))))))))
               (skipped
                 (list-sort
                   (lambda (a b) (string<? (car a) (car b)))
                   (fold-right
                     (lambda (id acc)
                       (let ((r (reason-for id)))
                         (if (and r (not (named? id)))
                             (cons (entry id r) acc)
                             acc)))
                     (quote ())
                     (map caddr rows)))))
          ;; NEVER: A WRITTEN BLOCK WHOSE STORED FRONT MATTER IS IN NO FILE IS
          ;; SAID, NOT LEFT OUT. Only a document that got a file has its
          ;; front written, at the top of that file. A block of kind `doc`
          ;; below the root is written into its ancestor's file like any other
          ;; block (F85 R2) -- heading, title and body -- and its front has
          ;; nowhere to go. Measured: on the tree before F85 such a front was
          ;; written into a file of its own; after it, the answer was `ok`
          ;; and the front was in no file.
          ;;
          ;; NOT IN `skipped`: the block's body WAS written, and `1 + n`
          ;; summed over `skipped` counts exactly the blocks lost. This clause
          ;; names a written block and the field of it that was not written,
          ;; `(<id> front)`, and is absent when there is none, so no answer
          ;; that had nothing to say changes shape.
          (let* ((file-root (make-hashtable string-hash string=?))
                 (not-written
                   (begin
                     (let-values (((keys ids) (hashtable-entries winner)))
                       (vector-for-each (lambda (id) (hashtable-set! file-root id #t)) ids))
                     (fold-right
                       (lambda (id acc)
                         (let ((b (state-read state id)))
                           (if (and b
                                    (hashtable-ref written id #f)
                                    (not (hashtable-ref file-root id #f))
                                    (not (string=? "" (text-field b 'front))))
                               (cons (list id 'front) acc)
                               acc)))
                       (quote ())
                       (list-sort string<? (map caddr rows))))))
            (append (list 'ok (list 'files (hashtable-size winner)))
                    (if (null? skipped) (quote ()) (list (cons 'skipped skipped)))
                    (if (null? not-written)
                        (quote ())
                        (list (cons 'fields-not-written not-written)))))))))

  (define (parent-directory path)
    (let loop ((i (string-length path)))
      (cond ((<= i 0) ".")
            ((char=? (string-ref path (- i 1)) #\/) (substring path 0 (- i 1)))
            (else (loop (- i 1))))))

  ;; The refusal for an export that could not write a file: R1's shape, with
  ;; the files written before it, in the order they were written.
  (define (unwritten path reason written)
    (list 'error 'unreadable
          (list 'path path)
          (list 'reason reason)
          (list 'written (reverse written))))

  ;; A WRITE THAT THE DOOR REFUSED (F100a): the file is written through
  ;; overwrite-entry!, so a directory that cannot be written into raises the
  ;; door's durable-error, a value and not a condition, carrying the errno.
  ;; Before the door the native port raised an i/o-error whose irritants
  ;; held the system's words; that clause caught nothing once the port was
  ;; gone, and is replaced by this one.
  (define (durable-failure-reason e)
    (string-append "the file could not be written, errno "
                   (number->string (fs-error-errno e))))

  ;; THROUGH THE DOOR (F100a). The write replaces the file in place, as
  ;; the truncating native port did, so its inode stays; the read raises on
  ;; absence, as the native one did.
  (define (write-file path text)
    (overwrite-entry! path (string->utf8 text)))

  (define (read-file path)
    (utf8->string (entry-bytes path)))

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
                                    (char=? (string-ref raw (- k 1)) #\newline))
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

  ;; LISTED WITH list-entries (F98), R1's listing: a directory that cannot
  ;; be read raises unreadable-entry naming it, which the RPC layer answers
  ;; as (error unreadable (path ...) (reason ...)). The earlier listing was
  ;; the host's, which raised a bare port error for a directory at mode 000,
  ;; answered internal. A directory gone between being seen and being
  ;; listed is not an empty one: see below.
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
                  ;; NEVER: ABSENT IS NOT EMPTY. The directory was a directory
                  ;; when it was checked; gone by the time it is listed, it
                  ;; is not a directory with no files in it. Read as empty,
                  ;; import-md --allow-delete would delete every block that
                  ;; came from it.
                  ;;
                  ;; NEVER: AND IT IS NOT LISTED A SECOND TIME. The host's listing
                  ;; used before list-entries raised at this point, and the RPC
                  ;; layer answered (error internal (condition "failed for ~a:
                  ;; ~(~a~)")) -- the condition's message, which is that format
                  ;; string, unformatted. It is raised here with that message.
                  ;; Listing again instead let a directory made anew in between
                  ;; read as empty after all (review r2).
                  (let ((names (list-entries d)))
                    (when (eq? names 'absent)
                      (error 'list-entries "failed for ~a: ~(~a~)" d "no such file or directory"))
                    (list-sort string<? names))))))

  (define (md-name? name)
    (let ((n (string-length name)))
      (and (> n 3) (string=? (substring name (- n 3) n) ".md"))))

;; DELETION IS NOT INFERRED FROM ABSENCE WITHOUT BEING ASKED. A file
  ;; that is missing from the directory may have been deleted, or the
  ;; directory may be a partial copy, or a sync may be half finished --
  ;; and a tombstone is permanent. So absence is reported by default and
  ;; acted on only when the caller says to.
  ;;
  ;; NEVER: A DOCUMENT THE DIRECTORY DOES NOT HOLD IS REPORTED, NOT REFUSED.
  ;; The directory ADDS and UPDATES: its files are imported, a document
  ;; whose file it lacks is left alone, and the report names it,
  ;; `(absent (files (<path> ...)))`. Refusing the whole import there made
  ;; the only remedy on offer the deletion of that document.
  ;; A SECTION a PRESENT file no longer holds is different: that file is
  ;; the whole document, its author removed the section, and the import
  ;; still refuses would-delete for it until --allow-delete is given.
  ;; With --allow-delete the documents the directory lacks are deleted
  ;; after the imports, as before, and the report is
  ;; `(deleted (files (<path> ...)))`, read from the deletions that RAN: a
  ;; batch stops at its first error, and a deletion planned after it did
  ;; not happen.
  ;; -> (results report), report #f or the one clause above.
  (define (import-md-report store dir . opts)
    (require-md-directory dir)
    (let* ((actor (if (pair? opts) (car opts) "unknown"))
           (allow-delete? (and (pair? opts) (pair? (cdr opts)) (cadr opts)))
           (files (md-files dir))
           (absent '())
           (doc-deletions '()))
;; THE OFFSET IS THREADED BECAUSE `from` COUNTS THE WHOLE BATCH. Each
      ;; file's intents are built on their own, so a file's doc is its
      ;; own intent 0 -- but the batch concatenates them, and the second
      ;; file's `(from 0)` then names the FIRST file's doc. Every
      ;; section of every file after the first was hung under the wrong
      ;; document, and the export put them all in one file.
      (let ((results
              (with-store-write store
                (lambda (state view)
                  (let* ((docs (absent-documents state files))
                         (gone (missing-sections state dir files))
                         (gone-sections (map car gone))
                         (roots (apply append (map cdr gone))))
                    (set! absent (map cadr docs))
                    (set! doc-deletions '())
                    (cond
                      ;; The refusal names what deleting the sections would
                      ;; strand, under `holds`, and only when there is
                      ;; something to name: its first four elements are the
                      ;; shape they always were.
                      ((and (pair? gone-sections) (not allow-delete?))
                       (list (append (list 'error 'would-delete
                                           (list 'blocks gone-sections)
                                           (list 'remedy 'allow-delete))
                                     (if (null? roots)
                                         '()
                                         (list (list 'holds (map car roots)))))))
                      (else
                       (let loop ((fs files) (base 0) (out '()))
                         (if (null? fs)
                             (let* ((imports (apply append (reverse out)))
                                    ;; What a deleted section holds that the
                                    ;; file does not describe is moved out
                                    ;; before the section goes.
                                    (rehomes (map (lambda (r) (list 'move (car r) (cdr r) #f)) roots))
                                    ;; Sections first, so a document is never
                                    ;; tombstoned before its children.
                                    (section-dels
                                      (map (lambda (id) (list 'del id))
                                           (append gone-sections
                                                   (if allow-delete? (apply append (map caddr docs)) '()))))
                                    (doc-dels (if allow-delete? (map (lambda (d) (list 'del (car d))) docs) '()))
                                    (first-doc (+ (length imports) (length rehomes) (length section-dels))))
                               (set! doc-deletions
                                 (let number ((ds docs) (k first-doc) (acc '()))
                                   (if (or (null? ds) (not allow-delete?)) (reverse acc)
                                       (number (cdr ds) (+ k 1) (cons (cons (cadr (car ds)) k) acc)))))
                               (append imports rehomes section-dels doc-dels))
                             (let ((is (file-intents state dir (car fs) base)))
                               (loop (cdr fs) (+ base (length is)) (cons is out)))))))))
                actor)))
        (list results
              (if allow-delete?
                  (let ((ran (filter (lambda (d)
                                       (let ((k (cdr d)))
                                         (and (< k (length results))
                                              (let ((r (list-ref results k))) (and (pair? r) (eq? (car r) 'ok))))))
                                     doc-deletions)))
                    (and (pair? ran) (list 'deleted (list 'files (map car ran)))))
                  (and (pair? absent) (list 'absent (list 'files absent))))))))

  ;; The results alone, for a caller that reads only them.
  (define (import-md store dir . opts)
    (car (apply import-md-report store dir opts)))

  ;; Every LIVE document whose file the directory does not hold, as
  ;; `(<doc-id> <path> (<section-id> ...))`, in the store's order. A deleted
  ;; document is not one the store holds: it is neither reported nor
  ;; deleted again.
  (define (live-doc? b) (and (eq? 'doc (kind-of b)) (not (cdr (assq 'deleted b)))))
  (define (absent-documents state files)
    (let loop ((bs (state-datum state)) (out '()))
      (if (null? bs)
          (reverse out)
          (let* ((id (cadr (car bs))) (b (state-read state id)))
            (loop (cdr bs)
                  (if (and (live-doc? b) (not (member (text-field b 'path) files)))
                      (cons (list id (text-field b 'path) (doc-sections-of state id)) out)
                      out))))))

  ;; Every section of a document whose file is PRESENT that nothing in the
  ;; file matches, as `(<section-id> (<root-id> . <target>) ...)`: each with
  ;; the roots it holds that go to `target` before it is deleted. A
  ;; document's kind decides here, as before, deleted or not: a deleted
  ;; document whose file is still there keeps its sections under the
  ;; section rule.
  (define (missing-sections state dir files)
    (let loop ((bs (state-datum state)) (sections '()))
      (if (null? bs)
          (reverse sections)
          (let* ((id (cadr (car bs))) (b (state-read state id)))
            (loop (cdr bs)
                  (if (and (eq? 'doc (kind-of b)) (member (text-field b 'path) files))
                      (append (reverse (unmatched-sections state dir id b files)) sections)
                      sections))))))

  ;; ONLY A SECTION CAN BE MISSING: a code block, a file or a nested
  ;; document under the document is never missing, so the file saying
  ;; nothing about it is not the file removing it.
  (define (unmatched-sections state dir doc-id b files)
    (let* ((rel (text-field b 'path))
           (raw (read-file (string-append dir "/" rel)))
           (sections (file-sections raw))
           (candidates (heading-candidates state doc-id))
           (matched (match-sections state (doc-sections-of state doc-id) candidates sections)))
      (if (and (pair? matched) (eq? (car matched) 'error))
          '()
          (let* ((found (filter string? (map car matched)))
                 (named (map car (filter (lambda (m) (and (car m) (not (recognised-only? state m))))
                                         matched)))
                 (missing (filter (lambda (id)
                                    (and (eq? 'section (kind-of (state-read state id)))
                                         (not (member id found))))
                                  candidates))
                 (rows (state-outline state)))
            (map (lambda (id) (cons id (retained-roots state rows id missing named files)))
                 missing)))))

  ;; A DELETED SECTION DOES NOT ORPHAN WHAT THE FILE DOES NOT DESCRIBE.
  ;; Deletion does not cascade, so a block left under a tombstone is
  ;; live and in no file. The roots of what the section holds -- its direct
  ;; children that are not sections and that the file does not name -- go,
  ;; in their order and each with everything under it, to the nearest
  ;; ancestor that survives the import. A section child is the file's to
  ;; place or to delete, and a named block is placed where the file puts
  ;; it: the file's word wins.
  ;;
  ;; NO GUESS ABOUT WHICH NEW SECTION REPLACES THE DELETED ONE. The file
  ;; does not say, so the roots go where they certainly belong.
  ;;
  ;; A NESTED DOCUMENT GOES TO ROOT, its one legal place, when its own file
  ;; is present. When that file is absent the absent-document rule governs
  ;; it, and it is not moved here: a deletion and a move of one block never
  ;; share a plan.
  (define (retained-roots state rows id missing named files)
    (let ((target (surviving-ancestor rows id missing)))
      (fold-right
        (lambda (child acc)
          (let ((b (state-read state child)))
            (cond ((eq? 'section (kind-of b)) acc)
                  ((member child named) acc)
                  ((eq? 'doc (kind-of b))
                   (if (member (text-field b 'path) files) (cons (cons child 'root) acc) acc))
                  (else (cons (cons child target) acc)))))
        '()
        (children-of rows id))))

  ;; Up from the block past every section this import deletes: the first
  ;; ancestor left is a matched section, a block of another kind, or the
  ;; document, and all of those survive it.
  (define (surviving-ancestor rows id missing)
    (let up ((p (parent-in rows id)))
      (if (member p missing) (up (parent-in rows p)) p)))

  (define (parent-in rows id)
    (let ((r (find (lambda (r) (equal? (caddr r) id)) rows)))
      (and r (car r))))

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
  ;; order `match-sections` gives: a declared id wins outright; otherwise
  ;; the position, when every pair it makes agrees on the heading; and
  ;; otherwise the heading key, with the body breaking ties -- but only
  ;; when that picks out exactly one candidate. Two refusals come out of
  ;; this and they are different things: the file names a block it cannot
  ;; mean (position), or the file cannot say which of several identical
  ;; sections it means (ambiguous).
  (define (changed-intents state doc-id rel split sections base)
    (let* ((old (doc-sections-of state doc-id))
           (matched (match-sections state old (heading-candidates state doc-id) sections)))
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
  ;;
  ;; A MATCHED SECTION GOES THERE TOO. Updating its fields only
  ;; left it where it was stored: a parent renamed in the file became a new
  ;; section, and the child it had kept its place under the old one, which
  ;; --allow-delete then deleted -- the child live, under a tombstone, in no
  ;; file. Its parent is resolved as a new section's is, and under each
  ;; parent the file's order is compared with the stored one; see
  ;; `placed-parents`.
  ;;
  ;; A BLOCK MATCHED BY ITS OWN RENDERING IS LEFT WHERE IT IS. It is not a
  ;; section, the file carries it only because the export wrote it there,
  ;; and the level the export wrote it at is a default, not a nesting.
  (define (section-intents state doc-id sections matched base)
    (let* ((rows (state-outline state))
           (parents (parents-of sections))
           (placing (placed-parents state rows doc-id parents matched))
           (refs (make-vector (length sections) #f))
           (homes (make-vector (length sections) #f))
           (fixed (make-vector (length sections) #f)))
      (let loop ((i 0) (ms matched) (ps parents) (n base) (out '()))
        (if (null? ms)
            (reverse out)
            (let* ((stored (car (car ms)))
                   (incoming (cdr (car ms)))
                   (parent-ix (car ps))
                   (parent (if (eq? parent-ix 'doc)
                               doc-id
                               (vector-ref refs parent-ix)))
                   (placed? (member parent-ix placing))
                   (j (predecessor fixed parents i parent-ix))
                   (after (and j (vector-ref refs j))))
              (cond
                ((and stored (recognised-only? state (car ms)))
                 (vector-set! refs i stored)
                 (vector-set! homes i (parent-in rows stored))
                 (vector-set! fixed i #t)
                 (loop (+ i 1) (cdr ms) (cdr ps) n out))
                (stored
                 (let ((is (append (section-field-intents state stored incoming)
                                   (if placed? (list (list 'move stored parent after)) '()))))
                   (vector-set! refs i stored)
                   (vector-set! homes i (if placed? parent (parent-in rows stored)))
                   (loop (+ i 1) (cdr ms) (cdr ps) (+ n (length is))
                         (append (reverse is) out))))
                (else
                 ;; WHERE NOTHING IS PLACED, A NEW SECTION GOES WHERE ITS
                 ;; PREDECESSOR IS. The file shows it right after the one
                 ;; before it, and the export writes it there only when it is
                 ;; that one's next sibling: a predecessor stored under a
                 ;; section of its own level is written after that section,
                 ;; at that level, and the new section goes under the same
                 ;; section. Inserted under the parent the levels give, it
                 ;; came after that section's whole subtree, the export no
                 ;; longer said what the file said, and the next untouched
                 ;; import wrote.
                 (let ((home (if (and j (not placed?)) (vector-ref homes j) parent)))
                   (vector-set! refs i (list 'from n))
                   (vector-set! homes i home)
                   (loop (+ i 1) (cdr ms) (cdr ps) (+ n 1)
                         (cons (list 'insert home after (section-fields incoming))
                               out))))))))))

  ;; A stored block the file matched without a marker and that is not a
  ;; section: the file carries it only as the export's rendering of it.
  (define (recognised-only? state pair)
    (and (car pair)
         (not (sec-id (cdr pair)))
         (not (eq? 'section (kind-of (state-read state (car pair)))))))

  ;; THE PARENTS WHOSE CHILDREN ARE PLACED IN FILE ORDER, as the
  ;; values `parents-of` gives ('doc or an index). Under each parent the
  ;; ids the file places there -- matched sections, and blocks of any kind
  ;; a marker names -- are compared, in file order, with the order of those
  ;; same ids under that parent in the arrangement the store's own export
  ;; gives (`rendered-arrangement`); blocks the file does not name take no
  ;; part. When the two agree and every new section can be
  ;; inserted after its predecessor, nothing moves. Otherwise each of that
  ;; parent's sections is placed: an existing one by a move, a new one by
  ;; its insert.
  ;;
  ;; AND A SECTION STORED UNDER ONE THE FILE NO LONGER HOLDS IS PLACED, whatever
  ;; the order says. The export can write a section under another of its own
  ;; level as that one's sibling, so the file may agree with the export
  ;; while dropping the section the other one is stored under; left in
  ;; place, it stayed under that section's tombstone after --allow-delete,
  ;; live and in no file. A retained block is re-homed for that reason; a
  ;; section is the file's to place, and this is where it is placed.
  ;;
  ;; NEVER: A MOVE FOR AN ORDER THAT DID NOT CHANGE. An unchanged file
  ;; writes nothing; that is what makes a projection a projection.
  (define (placed-parents state rows doc-id parents matched)
    (let ((rendered (rendered-arrangement state doc-id))
          (missing (let ((found (filter string? (map car matched))))
                     (filter (lambda (id) (and (eq? 'section (kind-of (state-read state id)))
                                               (not (member id found))))
                             (heading-candidates state doc-id)))))
     (let loop ((keys (unique parents)) (out '()))
      (if (null? keys)
          (reverse out)
          (let* ((key (car keys))
                 (group (let pick ((ps parents) (ms matched) (acc '()))
                          (cond ((null? ps) (reverse acc))
                                ((and (equal? (car ps) key)
                                      (not (recognised-only? state (car ms))))
                                 (pick (cdr ps) (cdr ms) (cons (car ms) acc)))
                                (else (pick (cdr ps) (cdr ms) acc)))))
                 (named (filter string? (map car group)))
                 (parent (if (eq? key 'doc) doc-id (car (list-ref matched key))))
                 (stored-order
                   (if (string? parent)
                       (filter (lambda (c) (member c named))
                               (rendered-children rendered (if (eq? key 'doc) 'doc parent)))
                       '()))
                 (insertable? (new-sections-insertable? group named))
                 (stranded? (exists (lambda (id) (member (parent-in rows id) missing)) named)))
            (loop (cdr keys)
                  (if (and (equal? stored-order named) insertable? (not stranded?))
                      out
                      (cons key out))))))))

  ;; THE ARRANGEMENT THE STORE'S OWN EXPORT WOULD GIVE: the heading
  ;; candidates in outline order, each at the level its heading is written
  ;; with, nested by those levels as the file's sections are. The file is
  ;; compared with this and not with the stored children, because the
  ;; export cannot say every nesting the store holds: a section moved under
  ;; another of the same level is written after it at that level. Compared
  ;; with the stored children, an untouched export read as a move and put
  ;; it back.
  ;; -> ((<id> . <parent>) ...), in order, the parent 'doc or an id.
  (define (rendered-arrangement state doc-id)
    (let* ((ids (heading-candidates state doc-id))
           (levels (map (lambda (id) (list id (car (rendered-heading-key state id)))) ids))
           (ps (parents-of levels)))
      (map (lambda (id p) (cons id (if (eq? p 'doc) 'doc (list-ref ids p)))) ids ps)))

  (define (rendered-children rendered parent)
    (map car (filter (lambda (e) (equal? (cdr e) parent)) rendered)))

  ;; A NEW SECTION IS INSERTED AFTER ITS PREDECESSOR IN THE FILE, and an
  ;; insert can say "after this one" or "at the end", never "first". So a
  ;; new section with no predecessor under its parent, followed there by
  ;; one the store already holds, cannot be put where the file has it
  ;; without placing the whole group; any other new section can.
  (define (new-sections-insertable? group named)
    (or (null? group)
        (car (car group))
        (null? named)))

  (define (unique xs)
    (let loop ((xs xs) (out '()))
      (cond ((null? xs) (reverse out))
            ((member (car xs) out) (loop (cdr xs) out))
            (else (loop (cdr xs) (cons (car xs) out))))))

;; The index of the nearest earlier entry the file puts under the same
  ;; parent, or #f: a block lands after the one it follows in the file
  ;; rather than at the end. A block matched by its own rendering is none: it
  ;; is not moved, and while its parent's children are placed they end after
  ;; every block the placement leaves alone.
  (define (predecessor fixed parents i parent-ix)
    (let loop ((j (- i 1)))
      (cond
        ((< j 0) #f)
        ((and (equal? (list-ref parents j) parent-ix) (not (vector-ref fixed j))) j)
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

  ;; WHAT AN UNMARKED HEADING MAY BE: the document's
  ;; descendants that are not documents and are not inside one, in outline
  ;; order. A nested document's sections belong to its own file, and are
  ;; matched or missing there, never here. Of these, a section is matched by
  ;; its heading, and any other block only by its own rendering
  ;; (`agrees?`); only a section can be missing. This is the one answer to
  ;; "which blocks can a heading be", asked at the three places that match:
  ;; the positional pairing, the signature pool, and the missing set.
  ;; `doc-sections-of` stays the whole subtree: a marker is checked against
  ;; it, and an absent file's document is deleted with all of it.
  (define (heading-candidates state doc-id)
    (let ((rows (state-outline state)))
      (let walk ((id doc-id))
        (apply append
               (map (lambda (child)
                      (if (eq? 'doc (kind-of (state-read state child)))
                          '()
                          (cons child (walk child))))
                    (children-of rows id))))))

  ;; IDENTITY, IN THE ORDER SECTION 8.1 GIVES IT.
  ;;
  ;;   1. a declared id wins outright, section by section -- that is what
  ;;      the recovery form is for, and a section that carries one is not
  ;;      guessing;
  ;;   2. otherwise the position, when the counts line up and every pair
  ;;      agrees (`agrees?`);
  ;;   3. otherwise the heading key, with the body breaking ties, and it
  ;;      has to pick out exactly one stored block.
  ;;
  ;; THE TWO REFUSALS ARE DIFFERENT SITUATIONS AND SAY SO.
  ;;   `position-mismatch`   the file declares an id this document does
  ;;                         not contain -- it is talking about some
  ;;                         other document, or about a deleted block --
  ;;                         or declares one id twice (`claimed-twice`).
  ;;   `ambiguous-identity`  the file changed, and a section in it looks
  ;;                         exactly like more than one stored section,
  ;;                         so nothing in the bytes says which one it
  ;;                         is. This is the case a marker fixes, which
  ;;                         is why the refusal asks for one.
  (define (signature-of-stored state id)
    (let ((b (state-read state id)))
      (cons (text-field b 'heading-src) (text-field b 'src))))

  (define (signature-of-incoming s) (cons (sec-heading s) (sec-src s)))

  ;; A MARKER IS PER SECTION. A section that carries one is that
  ;; block, whatever its kind; the rest are matched among the candidates
  ;; no marker claimed. Markers were used only when EVERY section carried
  ;; one, so a partly marked file had its markers ignored.
  ;;
  ;; POSITION NEEDS AGREEING HEADINGS. Pairing by position when
  ;; the counts were equal, with no look at the headings, gave a block the
  ;; identity of whatever section stood in its place: a section a batch had
  ;; inserted, the only one left in its document, was rewritten into the
  ;; file's one heading. A count of one is not an identity. Position is
  ;; used only when every pair it would make agrees; one pair that does not
  ;; sends the whole file to the heading key, as a count difference does.
  (define (match-sections state old candidates incoming)
    (let* ((declared (filter (lambda (s) (sec-id s)) incoming))
           (ids (map sec-id declared)))
      (cond
        ;; a declared id that is not one of this document's sections
        ((exists (lambda (s) (not (member (sec-id s) old))) declared)
         => (lambda (ignored)
              (list 'error 'position-mismatch
                    (list 'declared
                          (map sec-id
                               (filter (lambda (s) (not (member (sec-id s) old)))
                                       declared))))))
        ;; two markers naming one block: the file claims it twice
        ((pair? (claimed-twice ids))
         (list 'error 'position-mismatch (list 'claimed-twice (claimed-twice ids))))
        (else
         (let* ((rest (filter (lambda (s) (not (sec-id s))) incoming))
                (free (filter (lambda (id) (not (member id ids))) candidates))
                (paired (or (paired-by-position state free rest)
                            (match-by-signature state free rest))))
           (if (and (pair? paired) (eq? (car paired) 'error))
               paired
               (let merge ((is incoming) (ps paired) (out '()))
                 (cond ((null? is) (reverse out))
                       ((sec-id (car is))
                        (merge (cdr is) ps (cons (cons (sec-id (car is)) (car is)) out)))
                       (else (merge (cdr is) (cdr ps) (cons (car ps) out)))))))))))

  (define (claimed-twice ids)
    (let loop ((xs ids) (seen '()) (out '()))
      (cond ((null? xs) (reverse out))
            ((and (member (car xs) seen) (not (member (car xs) out)))
             (loop (cdr xs) seen (cons (car xs) out)))
            (else (loop (cdr xs) (cons (car xs) seen) out)))))

  ;; -> the pairs, or #f when the counts differ or one pair disagrees.
  (define (paired-by-position state free incoming)
    (and (= (length free) (length incoming))
         (for-all (lambda (id s) (agrees? state id s)) free incoming)
         (map cons free incoming)))

  ;; A SECTION AGREES BY ITS HEADING KEY; ANY OTHER BLOCK ONLY BY ITS OWN
  ;; RENDERING: that key and the body as the split reads them back. Both
  ;; keys are the key of the heading the export writes. The export writes every
  ;; block of a document into its file, so a code block comes back as a
  ;; heading; matched to nothing, it was a new section on every import of
  ;; an unmarked export. Matched by its heading alone, a heading the author
  ;; wrote took the block over.
  (define (agrees? state id s)
    (if (eq? 'section (kind-of (state-read state id)))
        (equal? (rendered-heading-key state id) (heading-key s))
        (and (equal? (rendered-heading-key state id) (heading-key s))
             (same-body? (text-field (state-read state id) 'src) (sec-src s)))))

  ;; THE JOIN ADDS ONE NEWLINE AFTER A BODY THAT LACKS ONE when another
  ;; block follows it (md-join), and the split keeps that newline in the
  ;; body it reads, so a stored "x" comes back as "x\n".
  (define (same-body? stored incoming)
    (or (string=? stored incoming)
        (and (> (string-length stored) 0)
             (not (char=? #\newline (string-ref stored (- (string-length stored) 1))))
             (string=? (string-append stored "\n") incoming))))

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

  ;; THE KEY OF THE HEADING THE EXPORT WRITES, as the split reads it back.
  ;; A stored title the split normalises -- one with a trailing space, say
  ;; -- is written as it is and read back without the space, so the raw
  ;; stored key did not match the block's own untouched export and the
  ;; import refused it as missing. When the written line does not parse as
  ;; exactly one heading -- a title carrying a line break, which only a
  ;; record from elsewhere can hold, is written as two -- the stored title
  ;; is paired with the level the line was written at: a stored level can
  ;; be a value that is no level at all, a conflict of two writers, and the
  ;; arrangement compares levels as numbers.
  (define (rendered-heading-key state id)
    (or (parse-heading (effective-heading state id))
        (cons (effective-level (state-read state id))
              (text-field (state-read state id) 'title))))

  (define (match-by-signature state old incoming)
    (let loop ((is incoming) (free old) (out '()))
      (if (null? is)
          (reverse out)
          (let* ((want (heading-key (car is)))
                 (hits (filter (lambda (id) (agrees? state id (car is))) free)))
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
