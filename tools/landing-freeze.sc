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
;;   scheme --script tools/landing-freeze.sc <store> --writer <W> --check <freeze-dir>
;;
;; <dir> must not exist. In order:
;; - EVERY DRAFT IS BROUGHT ONTO THE STORE'S APPLIED CUT first, since
;;   tools/landing-commit.sc commits only at the pinned cut, the join of the
;;   drafts' cuts. A committed record moves the applied cut whatever block it
;;   is on (a review letter as much as a code change); a draft never does. A
;;   draft whose block's committed version is still its based-on is MOVED:
;;   the same bytes, written again at the applied cut. A draft whose block was
;;   committed since (`drafts` lists it `behind`) is REBASED: its change and
;;   the committed one are merged line by line against the block's text at
;;   the draft's cut (merge3 below), and the merged text is written onto the
;;   committed version. Both go through `write --rebase --based-on
;;   --working-cut` under the writer's name. A STRUCTURAL DRAFT (new, delete)
;;   at another cut is MOVED when it still applies there unchanged: a new
;;   file whose path no live file holds at the applied cut, a delete whose
;;   file's baseline there is still its based-on. Its entry is rewritten in
;;   place at the applied cut (working-move-structural!, one draft lock: the
;;   same bytes, path and language, the version recomputed), and only while
;;   it is still the version judged. A refusal, or a failure before the
;;   write's rename, leaves the entry as it was; a failure of the directory
;;   flush after the rename leaves the whole new entry at the applied cut.
;;   The freeze is refused either way, (error structural-move-refused (key
;;   <k>) (answer ..)), and a second freeze reads the entry as it stands.
;;   When any draft's change and the committed one touch the
;;   same or adjacent lines (and are not the same edit), or lines repeat so
;;   that the alignment is in doubt, or a new file's path is held at the
;;   applied cut, or a deleted file is gone or changed there, NOTHING is
;;   written and the freeze is refused (error drafts-behind-conflict (blocks
;;   (<block> ...)) [(structural (<key> ...))]), naming every one: the coder
;;   merges those by hand. Each draft written is recorded in
;;   <dir>/REBASED.sexp as (rebased <block> (from <hash>) (to <hash>)) or
;;   (moved <block or key> (from-cut <cut>) (to-cut <cut>)), and the answer
;;   counts them;
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
;; - THE STRUCTURAL DRAFTS (new and delete, working.sc) are frozen beside the
;;   block drafts: listed, dumped, checked in the view by key and version; the
;;   tree is the view's with them laid over it (working.sc,
;;   working-structural-outputs: a new file written, a deleted one left out),
;;   and CHANGED.sexp names each as (path <p> added) or (path <p> removed).
;; A draft written during the freeze is refused (error drafts-changed ...),
;; with nothing written after it; the directory is left for the coder to
;; remove before freezing again. So is a commit during the freeze, which
;; leaves the pinned cut behind the applied one: (error cut-moved (pinned
;; <cut>) (now <cut>)).
;;
;; A refusal prints (error <kind> ...) and exits 1; success prints
;; (ok (frozen <n>) (changed <k>) (dir "<dir>")), followed by (rebased <r>)
;; (moved <m>) when any draft was written, and exits 0. The drafts written
;; stay written when the freeze is refused after them; a coder's edit-export
;; of the old drafts is stale from then on and is exported again.
;;
;; --check reads a freeze already made, before it lands, and writes nothing.
;; Each frozen item is first checked against its own record (bytes against
;; their sha256, the version against its fields, a draft's bytes as text):
;; (error frozen-damaged (block|structural <id>)). Then a frozen draft whose
;; block was committed since is merged as above but only to judge it, and a
;; structural item at another cut is judged as above. Any conflicting one:
;; (error drafts-behind-conflict (blocks ...) [(structural ...)]). Otherwise,
;; any draft behind or only off the cut (a structural one a freeze would
;; move among them), or the applied cut no longer the pinned one: (error
;; refreeze (behind (<block> ...)) (moved (<block or key> ...)) (pinned
;; <cut>) (now <cut>)), since what was gated is not what would land. Else
;; (ok (current (pinned <cut>))).
(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia store) store-state-at-cut open-and-reduce)
        (only (theourgia reduce) reduce-applied-cut reduction? state-read block-hash draft-version structural-version)
        (only (theourgia markers) src-text)
        (only (theourgia working) working-state working-structural-outputs working-structural
              working-move-structural! path-holder holder-baseline)
        (only (theourgia landing-files) raw-projection changed-files-between ambiguous-groups)
        (only (theourgia code-project) export-code-view)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (theourgia ffi) entry-bytes entry-name-type unreadable-entry? overwrite-entry! hold-point!)
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

;; A STRUCTURAL DRAFT'S KEY in a frozen list: its provisional id, or delete-
;; and its holder (the names working.sc files them under)
(define (structural-key-of item)
  (if (eq? (car item) 'new) (clause 'provisional item) (string-append "delete-" (clause 'holder item))))
;; -> ((<block or structural key> . <version>) ...), sorted: the writer's live
;; drafts now
(define (listed store writer)
  (let ((a (rpc-dispatch store (list 'drafts "--writer" writer) actor)))
    (unless (and (pair? a) (eq? (car a) 'ok)) (refuse 'drafts-refused a))
    (let ((items (let ((i (assq 'items (cdr a)))) (if i (cdr i) '()))))
      (list-sort (lambda (x y) (string<? (car x) (car y)))
                 (append
                   (map (lambda (d) (cons (clause 'block d) (clause 'version d)))
                        (filter (lambda (d) (and (pair? d) (eq? (car d) 'draft))) items))
                   (map (lambda (d) (cons (structural-key-of d) (clause 'version d)))
                        (filter (lambda (d) (and (pair? d) (memq (car d) '(new delete)))) items)))))))

;; ---- bringing the drafts onto the applied cut ------------------------------

;; -> the lines of S, each with the newline that ends it; the last one without
;; when S does not end in a newline
(define (lines-of s)
  (let loop ((i 0) (from 0) (out '()))
    (cond ((= i (string-length s))
           (reverse (if (< from i) (cons (substring s from i) out) out)))
          ((char=? (string-ref s i) #\newline)
           (loop (+ i 1) (+ i 1) (cons (substring s from (+ i 1)) out)))
          (else (loop (+ i 1) from out)))))

;; -> the edits that turn the lines BASE into the lines SIDE, from a longest
;; common subsequence: ((<start> <end> <lines>) ...) in order, each replacing
;; base lines [start, end) with lines; between two edits at least one line is
;; kept, so the edits of one side never touch
(define (edits base side)
  (let* ((a (list->vector base)) (b (list->vector side))
         (n (vector-length a)) (m (vector-length b))
         ;; t[i][j]: the length of a longest common subsequence of a[i..] and b[j..]
         (t (let ((v (make-vector (+ n 1))))
              (do ((i 0 (+ i 1))) ((> i n) v) (vector-set! v i (make-vector (+ m 1) 0))))))
    (do ((i (- n 1) (- i 1))) ((< i 0))
      (do ((j (- m 1) (- j 1))) ((< j 0))
        (vector-set! (vector-ref t i) j
                     (if (string=? (vector-ref a i) (vector-ref b j))
                         (+ 1 (vector-ref (vector-ref t (+ i 1)) (+ j 1)))
                         (max (vector-ref (vector-ref t (+ i 1)) j)
                              (vector-ref (vector-ref t i) (+ j 1)))))))
    (let loop ((i 0) (j 0) (si 0) (sj 0) (out '()))
      (define (flush)
        (if (and (= si i) (= sj j))
            out
            (cons (list si i (let take ((k sj) (acc '())) (if (= k j) (reverse acc) (take (+ k 1) (cons (vector-ref b k) acc)))))
                  out)))
      (cond ((and (= i n) (= j m)) (reverse (flush)))
            ((and (< i n) (< j m) (string=? (vector-ref a i) (vector-ref b j))
                  (= (vector-ref (vector-ref t i) j) (+ 1 (vector-ref (vector-ref t (+ i 1)) (+ j 1)))))
             (let ((o (flush))) (loop (+ i 1) (+ j 1) (+ i 1) (+ j 1) o)))
            ((and (< i n) (or (= j m) (>= (vector-ref (vector-ref t (+ i 1)) j) (vector-ref (vector-ref t i) (+ j 1)))))
             (loop (+ i 1) j si sj out))
            (else (loop i (+ j 1) si sj out))))))

;; THE SAME EDITS FROM THE OTHER END: the alignment that keeps the LAST of
;; several equal lines where `edits` keeps the first, read off the reversed
;; texts and turned back into base coordinates
(define (edits-late base side)
  (let ((n (length base)))
    (reverse (map (lambda (e) (list (- n (cadr e)) (- n (car e)) (reverse (caddr e))))
                  (edits (reverse base) (reverse side))))))

;; THE THREE-WAY MERGE OF ONE BLOCK'S TEXT, by lines: BASE is the text the
;; draft was written on, OURS the draft, THEIRS the committed text. -> the
;; merged text, or #f for a conflict.
;; NEVER: A DOUBT IS A CONFLICT. An edit of ours and one of theirs that
;; overlap OR ABUT (two insertions at one place, an insertion against the
;; edge of a change) conflict unless they are the same edit; a text that is
;; not a string is a conflict. A merge that guessed an order for two edits at
;; one place would land a text nobody wrote, with no one told.
;; NEVER: AND AN ALIGNMENT IS A GUESS WHERE LINES REPEAT. Deleting one of two
;; equal runs can be read as deleting either, and whether it meets the other
;; side's edit depends on which. Each side is aligned from both ends (edits,
;; edits-late), and the four pairings must all merge, to one text.
(define (merge3 base ours theirs)
  (and (string? base) (string? ours) (string? theirs)
       (let* ((b (lines-of base)) (o (lines-of ours)) (t (lines-of theirs))
              (texts (apply append
                            (map (lambda (eo) (map (lambda (et) (merge-edits b eo et)) (list (edits b t) (edits-late b t))))
                                 (list (edits b o) (edits-late b o))))))
         (and (for-all string? texts)
              (for-all (lambda (x) (string=? x (car texts))) texts)
              (car texts)))))

;; -> the base lines B with the edits EO and ET applied, or #f when two of
;; them touch and are not the same edit
(define (merge-edits b eo et)
  (let ((touch? (lambda (x y) (and (<= (car x) (cadr y)) (<= (car y) (cadr x))))))
    (and (for-all (lambda (x) (for-all (lambda (y) (or (equal? x y) (not (touch? x y)))) et)) eo)
         (let ((all (list-sort (lambda (x y) (< (car x) (car y)))
                               (append eo (filter (lambda (y) (not (member y eo))) et))))
               (bv (list->vector b)))
           (let loop ((es all) (at 0) (out '()))
             (define (kept from to acc)
               (if (= from to) acc (kept (+ from 1) to (cons (vector-ref bv from) acc))))
             (if (null? es)
                 (apply string-append (reverse (kept at (vector-length bv) out)))
                 (let ((e (car es)))
                   (loop (cdr es) (cadr e) (append (reverse (caddr e)) (kept at (car e) out))))))))))

(define (cut-set c)
  (list-sort (lambda (a b) (string<? (car a) (car b)))
             (filter (lambda (e) (and (pair? e) (integer? (cdr e)) (> (cdr e) 0))) c)))
(define (src-of state id)
  (let* ((b (and (reduction? state) (state-read state id)))
         (fs (and b (assq 'fields b))) (v (and fs (assq 'src (cdr fs)))))
    (cond ((not v) #f) ((string? (cdr v)) (cdr v)) ((bytevector? (cdr v)) (src-text (cdr v))) (else #f))))

;; -> (<id> <version> <based-on> <cut> <text>) for one live draft of WRITER
(define (draft-info store writer id)
  (let ((a (rpc-dispatch store (list 'read id "--working-info" "--writer" writer) actor)))
    (unless (and (pair? a) (eq? (car a) 'ok) (pair? (cadr a)) (eq? (car (cadr a)) 'projection) (eq? (cadr (cadr a)) 'working))
      (refuse 'draft-unreadable (list 'block id) (list 'answer a)))
    (let ((p (cadr a)))
      (list id (list-ref p 4) (list-ref p 5) (list-ref p 6) (list-ref p 7)))))

;; THE JUDGEMENT OF ONE DRAFT (<id> <based-on> <cut> <text>) against NOW, the
;; store as applied: -> (current) when it stands at the applied cut, (moved
;; <text>) when only the cut moved, (rebased <merged text> <committed hash>)
;; when the block was committed since and the two changes merge, (conflict)
;; when they do not.
(define (judge store now id based-on cut text)
  (let ((committed (block-hash now id)))
    (cond ((equal? (cut-set cut) (cut-set (reduce-applied-cut now))) (list 'current))
          ((equal? based-on committed) (list 'moved text))
          (else
           (let* ((past (store-state-at-cut store #f (format "~s" cut)))
                  (merged (and committed (reduction? past) (equal? (block-hash past id) based-on)
                               (merge3 (src-of past id) text (src-of now id)))))
             (if merged (list 'rebased merged committed) (list 'conflict)))))))

;; THE JUDGEMENT OF ONE STRUCTURAL DRAFT I against NOW, the store as applied.
;; I is a live item (working-structural) or a frozen one, (new (provisional
;; ..) (path ..) (cut ..) ...) or (delete (holder ..) (based-on ..) (cut ..)
;; ...). -> current when it stands at the applied cut; moved when it can be
;; written again there unchanged: a new file whose path no live file holds
;; there (path-holder, the test working-new! makes), a delete whose file's
;; baseline there is still its based-on (holder-baseline, the test the
;; commit's preflight makes); conflict otherwise, merged by hand.
(define (judge-structural now i)
  (cond ((equal? (cut-set (clause 'cut i)) (cut-set (reduce-applied-cut now))) 'current)
        ((eq? (car i) 'new) (if (path-holder now (clause 'path i)) 'conflict 'moved))
        (else (let ((b (holder-baseline now (clause 'holder i))))
                (if (and b (equal? b (clause 'based-on i))) 'moved 'conflict)))))
;; -> ((<item> . <verdict>) ...), the writer's live structural drafts judged
(define (structural-judged store writer now)
  (let ((a (working-structural store #f writer)))
    (unless (and (pair? a) (eq? (car a) 'ok)) (refuse 'structural-refused a))
    (map (lambda (i) (cons i (judge-structural now i))) (caddr a))))
;; ONE STRUCTURAL DRAFT I MOVED ONTO THE APPLIED CUT AT, by working.sc's own
;; move: its entry rewritten in place under one draft lock, and only while it
;; is still the version judged here (I's), so a draft rewritten since the
;; judgement is refused (structural-changed) and stands as it is.
;; NEVER: NO DISCARD, NO RE-WRITE. One key holds one entry; a discard then a
;; write is two lock acquisitions, and between them a draft re-created at the
;; same path (its key is a function of the path) was overwritten, and a
;; discard that failed after its unlink lost the only copy. The move removes
;; nothing first; what a refused or failed move leaves is said in the header.
;; -> (moved <key> (from-cut <cut>) (to-cut <cut>)), the to-cut the move's own
(define (move-structural! store writer at i)
  (let ((key (structural-key-of i)))
    (hold-point! 'freeze-before-move)
    (let ((a (working-move-structural! store writer key (clause 'version i) (format "~s" at))))
      (unless (and (pair? a) (eq? (car a) 'ok))
        (refuse 'structural-move-refused (list 'key key) (list 'answer a)))
      (list 'moved key (list 'from-cut (clause 'cut i)) (list 'to-cut (clause 'cut a))))))

(define (conflict-refusal blocks structural)
  (apply refuse 'drafts-behind-conflict (list 'blocks blocks)
         (if (null? structural) '() (list (list 'structural structural)))))

;; EVERY DRAFT ONTO THE APPLIED CUT, or nothing written: every block draft and
;; every structural one is judged before the first write. -> the records,
;; ((rebased <block> (from <hash>) (to <hash>)) | (moved <block or key> (from-cut <cut>) (to-cut <cut>)) ...),
;; the block drafts' first
(define (bring-onto-applied-cut! store writer)
  (let* ((now (open-and-reduce store))
         (at (reduce-applied-cut now))
         (drafts (map (lambda (d) (draft-info store writer (car d))) (block-drafts store writer)))
         (judged (map (lambda (d) (cons d (judge store now (car d) (caddr d) (cadddr d) (list-ref d 4)))) drafts))
         (conflicts (map caar (filter (lambda (j) (eq? (cadr j) 'conflict)) judged)))
         (sjudged (structural-judged store writer now))
         (structural (map (lambda (j) (structural-key-of (car j))) (filter (lambda (j) (eq? (cdr j) 'conflict)) sjudged)))
         (smoves (map car (filter (lambda (j) (eq? (cdr j) 'moved)) sjudged))))
    (unless (and (null? conflicts) (null? structural)) (conflict-refusal conflicts structural))
    (let ((block-records (bring-blocks! store writer at judged)))
      ;; one at a time, in order: a refused move stops the rest
      (let loop ((is smoves) (out '()))
        (if (null? is)
            (append block-records (reverse out))
            (loop (cdr is) (cons (move-structural! store writer at (car is)) out)))))))

;; the block drafts judged moved or rebased, written: -> their records
(define (bring-blocks! store writer at judged)
  (fold-right
    (lambda (j records)
      (let* ((d (car j)) (id (car d)) (verdict (cdr j)))
        (if (eq? (car verdict) 'current)
            records
            (let* ((to (if (eq? (car verdict) 'rebased) (caddr verdict) (caddr d)))
                   (a (rpc-dispatch store (list 'write id "-" "--writer" writer "--rebase"
                                                "--based-on" to "--working-cut" (format "~s" at))
                                    actor #f #f (lambda () (cadr verdict)) #f)))
              (unless (and (pair? a) (eq? (car a) 'ok))
                (refuse 'rebase-refused (list 'block id) (list 'answer a)))
              (cons (if (eq? (car verdict) 'rebased)
                        (list 'rebased id (list 'from (caddr d)) (list 'to to))
                        (list 'moved id (list 'from-cut (cadddr d)) (list 'to-cut at)))
                    records)))))
    '() judged))

;; -> ((<block> . <version>) ...), the writer's live block drafts
(define (block-drafts store writer)
  (let ((a (rpc-dispatch store (list 'drafts "--writer" writer) actor)))
    (unless (and (pair? a) (eq? (car a) 'ok)) (refuse 'drafts-refused a))
    (map (lambda (d) (cons (clause 'block d) (clause 'version d)))
         (filter (lambda (d) (and (pair? d) (eq? (car d) 'draft)))
                 (let ((i (assq 'items (cdr a)))) (if i (cdr i) '()))))))

;; THE FREEZE'S MANIFEST, one `drafts` datum and nothing after it
(define (frozen-manifest dir)
  (let ((m (guard (e (#t #f))
             (let ((p (open-string-input-port (utf8->string (entry-bytes (string-append dir "/DRAFTS.sexp"))))))
               (let* ((m (read p)) (rest (read p))) (and (eof-object? rest) m))))))
    (unless (and (pair? m) (eq? (car m) 'drafts) (list? m)) (refuse 'manifest-unreadable (list 'dir dir)))
    m))
;; -> the frozen bytes of item D, checked against its recorded sha256 and
;; version; a damaged item is refused by name. A version is only matched once
;; it is recorded as a string and rebuilt from the item's fields: two failed
;; readings would otherwise compare equal.
(define (frozen-bytes dir d)
  (let* ((key (case (car d) ((draft) (clause 'block d)) ((new) (clause 'provisional d)) (else (clause 'holder d))))
         (bytes (and (memq (car d) '(draft new)) (string? (clause 'bytes d))
                     (guard (e ((unreadable-entry? e) #f)) (entry-bytes (string-append dir "/" (clause 'bytes d))))))
         (sha-ok? (lambda () (and (bytevector? bytes) (equal? (bytevector->hex (sha256 bytes)) (clause 'sha256 d)))))
         (version-ok? (lambda (rebuild)
                        (let ((v (guard (e (#t #f)) (rebuild))))
                          (and (string? v) (string? (clause 'version d)) (string=? v (clause 'version d)))))))
    (unless (case (car d)
              ((draft) (and (sha-ok?) (src-text bytes)
                            (version-ok? (lambda () (draft-version bytes (clause 'based-on d) (clause 'cut d))))))
              ((new) (and (sha-ok?)
                          (version-ok? (lambda () (structural-version 'new (clause 'path d) (bytevector->hex (sha256 bytes)) (clause 'cut d))))))
              (else (version-ok? (lambda () (structural-version 'delete (clause 'holder d) (clause 'based-on d) (clause 'cut d))))))
      (refuse 'frozen-damaged (list (if (eq? (car d) 'draft) 'block 'structural) key)))
    bytes))

;; --check: a freeze already made, judged against the store now; nothing
;; written. THE FROZEN BYTES ARE JUDGED, not the writer's live drafts: what
;; was gated is what would land. A structural item at the applied cut is
;; current (nothing was committed since, so a delete's file is as it was);
;; one at another is judged by judge-structural, as a freeze judges it.
(define (check store writer dir)
  (let ((m (frozen-manifest dir)))
    (unless (equal? (clause 'writer m) writer)
      (refuse 'other-writer (list 'frozen (clause 'writer m)) (list 'given writer)))
    (let* ((items (filter (lambda (d) (and (pair? d) (memq (car d) '(draft new delete)))) (cdr m)))
           (bytes (map (lambda (d) (frozen-bytes dir d)) items))
           (pinned (clause 'pinned-cut m))
           (now (open-and-reduce store))
           (at (reduce-applied-cut now))
           (judged (filter values
                           (map (lambda (d b)
                                  (and (eq? (car d) 'draft)
                                       (cons (clause 'block d)
                                             (judge store now (clause 'block d) (clause 'based-on d) (clause 'cut d) (src-text b)))))
                                items bytes)))
           (named (lambda (kind) (map car (filter (lambda (j) (eq? (cadr j) kind)) judged))))
           ;; the frozen structural items, judged as a freeze judges the live ones
           (sjudged (map (lambda (d) (cons (structural-key-of d) (judge-structural now d)))
                         (filter (lambda (d) (memq (car d) '(new delete))) items)))
           (skeys (lambda (verdict) (map car (filter (lambda (j) (eq? (cdr j) verdict)) sjudged)))))
      (cond ((not (and (null? (named 'conflict)) (null? (skeys 'conflict))))
             (conflict-refusal (named 'conflict) (skeys 'conflict)))
            ((or (pair? (named 'rebased)) (pair? (named 'moved)) (pair? (skeys 'moved)) (not (equal? (cut-set at) (cut-set pinned))))
             (refuse 'refreeze (list 'behind (named 'rebased)) (list 'moved (append (named 'moved) (skeys 'moved)))
                     (list 'pinned pinned) (list 'now at)))
            (else (list 'ok (list 'current (list 'pinned pinned))))))))

(define (freeze store writer dir)
  ;; anything at the directory's name, asked with an lstat, refuses the freeze
  ;; before any draft is moved, in the words the dump refuses it with
  (unless (eq? 'absent (entry-name-type dir)) (refuse 'dump-refused (list 'error 'dir-exists (list 'dir dir))))
  (let* ((records (strictly store (lambda (key) (bring-onto-applied-cut! key writer))))
         (dumped (run-dump store writer dir)))
    (unless (and (pair? dumped) (eq? (car dumped) 'ok)) (refuse 'dump-refused dumped))
    (overwrite-entry! (string-append dir "/REBASED.sexp")
                      (string->utf8 (string-append (format "~s" (cons 'rebases records)) "\n")))
    (let* ((manifest (read (open-string-input-port (utf8->string (entry-bytes (string-append dir "/DRAFTS.sexp"))))))
           (drafts (filter (lambda (d) (and (pair? d) (memq (car d) '(draft new delete)))) (cdr manifest)))
           (frozen (list-sort (lambda (x y) (string<? (car x) (car y)))
                              (map (lambda (d) (cons (if (eq? (car d) 'draft) (clause 'block d) (structural-key-of d))
                                                     (clause 'version d)))
                                   drafts)))
           (pinned (clause 'pinned-cut manifest)))
      (when (null? drafts) (refuse 'nothing-to-freeze (list 'dir dir)))
      (hold-point! 'freeze-after-dump)
      (let ((answer (strictly store (lambda (key) (freeze-view key writer dir frozen pinned (length drafts))))))
        ;; A COMMIT DURING THE FREEZE leaves the pinned cut behind the applied
        ;; one, whatever it touched, and landing-commit would refuse it: it is
        ;; refused here, the directory left as for drafts-changed, and a new
        ;; freeze brings the drafts onto the cut it moved to
        (let ((now (reduce-applied-cut (open-and-reduce store))))
          (unless (equal? (cut-set now) (cut-set pinned))
            (refuse 'cut-moved (list 'pinned pinned) (list 'now now))))
        ;; the counts follow the answer only when a draft was written: a freeze
        ;; of drafts already at the applied cut answers as it always has
        (append answer
              (if (null? records)
                  '()
                  (list (list 'rebased (length (filter (lambda (r) (eq? (car r) 'rebased)) records)))
                        (list 'moved (length (filter (lambda (r) (eq? (car r) 'moved)) records))))))))))

;; THE FREEZE'S LOADS ARE STRICT, as export-code's are: a store whose history
;; is cut by damage, or missing a writer, is refused rather than frozen from
;; its readable part; the rebase before it and a check read the same way.
;; -> (PROC <key>) with the declaration riding on a copy of the store string
;; for its extent, as the dispatcher sets one for a verb
(define (strictly store proc)
  (let ((key (string-copy store)))
    (dynamic-wind
      (lambda () (load-declaration-set! key incomplete-refused))
      (lambda () (proc key))
      (lambda () (load-declaration-set! key #f)))))

;; -> the freeze's answer, from one read of the working view
(define (freeze-view store writer dir frozen pinned count)
  (let* ((w (working-state store #f writer))
         (view (if (and (pair? w) (eq? (car w) 'ok)) (caddr w) (refuse 'working-view-refused w)))
         ;; the view's own drafts, each (<id> <version> <based-on> <bytes>), and
         ;; its structural entries by key
         (structural (if (> (length w) 4) (list-ref w 4) '()))
         (in-view (list-sort (lambda (x y) (string<? (car x) (car y)))
                             (append (map (lambda (d) (cons (car d) (cadr d))) (cadddr w))
                                     (map (lambda (e) (cons (if (eq? (car e) 'working-new) (list-ref e 3)
                                                                (string-append "delete-" (list-ref e 3)))
                                                            (list-ref e 4)))
                                          structural))))
         ;; THE TREE'S FILES: the view's raw projection with the structural drafts
         ;; laid over it, ((<path> <bytes>) ...)
         (tree-files (working-structural-outputs view structural (raw-projection view)))
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
    (let ((paths (map car tree-files)))
      (for-each (lambda (kind)
                  (let ((groups (ambiguous-groups kind paths)))
                    (unless (null? groups)
                      (refuse 'paths-ambiguous (list 'kind kind) (list 'groups groups)))))
                ;; each folding a volume may apply, and the two together: a
                ;; pair can meet under both and under neither alone
                '(case normalisation both)))
    (let ((e (export-code-view store (string-append dir "/tree") #t (lambda () view)
                               (lambda (outputs) (working-structural-outputs view structural outputs)))))
      (unless (and (pair? e) (eq? (car e) 'ok)) (refuse 'export-refused e)))
    ;; ((<path> <bytes>) ...), the files export-code-view wrote, each read
    ;; back: on a volume that folds case or normalisation two paths can
    ;; land on one file, and the tree would not be the view's
    (let ((files tree-files))
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
      (hold-point! 'freeze-before-list)
      (let ((now (listed store writer)))
        (unless (equal? now frozen)
          (refuse 'drafts-changed (list 'dumped frozen) (list 'now now))))
      ;; A NEW FILE IS ADDED AND A DELETED ONE REMOVED, beside the contents
      ;; changed-files-between compared over the view
      (let* ((news (filter (lambda (e) (eq? (car e) 'working-new)) structural))
             (gone (map (lambda (o) (car o))
                        (filter (lambda (o) (not (assoc (car o) tree-files))) (raw-projection view))))
             (changed (if (and (pair? changed) (eq? (car changed) 'ok) (assq 'items (cdr changed)))
                          (cons 'ok
                                (map (lambda (c) (if (and (pair? c) (eq? (car c) 'items))
                                                     (append c (map (lambda (e) (list 'path (list-ref e 5) 'added)) news)
                                                             (map (lambda (p) (list 'path p 'removed)) gone))
                                                     c))
                                     (cdr changed)))
                          changed)))
        (overwrite-entry! (string-append dir "/CHANGED.sexp")
                          (string->utf8 (string-append (format "~s" changed) "\n")))
        (list 'ok (list 'frozen count)
              (list 'changed (length (let ((i (assq 'items (cdr changed)))) (if i (cdr i) '()))))
              (list 'dir dir))))))

(define (main args)
  ;; `--check` IS NEVER A DIRECTORY: a check whose directory was left off is
  ;; refused, not run as a freeze into a directory of that name
  (if (not (and (or (and (= 4 (length args)) (not (string=? (cadddr args) "--check")))
                    (and (= 5 (length args)) (string=? (cadddr args) "--check")))
                (string=? (cadr args) "--writer")))
      (begin (write '(usage (landing-freeze <store> "--writer" <W> ["--check"] <dir>))) (newline) (exit 2))
      (let ((a (guard (e ((and (pair? e) (eq? (car e) 'error)) e))
                 (if (= 5 (length args))
                     (strictly (car args) (lambda (key) (check key (caddr args) (list-ref args 4))))
                     (freeze (car args) (caddr args) (cadddr args))))))
        (write a)
        (newline)
        (exit (if (eq? (car a) 'ok) 0 1)))))

(main (cdr (command-line)))
