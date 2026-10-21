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

;; Turn an agent's edits to a directory tools/edit-export.sc wrote back into
;; the writer's drafts.
;;
;;   scheme --script tools/edit-sync.sc <store> <dir> --writer <W>
;;
;; THE EDITS ARE READ AGAINST THE PRISTINE COPY in <dir>.edit/, never against
;; the store: each changed file is diffed by lines against the bytes the
;; export wrote, and each hunk's OLD side is mapped through the block spans
;; the export recorded, by locate's own rule (landing-files.sc,
;; locate-pieces): a nonempty range answers every block it intersects; an
;; insertion answers the block that starts where it is, or the last block
;; at the file's end. A hunk must answer exactly one block. Each touched
;; block's new text is its old text with its hunks applied.
;;
;; EVERY CHECK RUNS BEFORE ANY WRITE, and a refusal writes nothing:
;;   missing-sidecar, bad-sidecar, foreign-store, foreign-writer (the
;;     sidecar is another store's or another writer's);
;;   pristine-damaged (a pristine copy no longer has its recorded hash);
;;   unexpected-entry (not a regular file or a directory);
;;   outside-domain (an added file, or a new file the export wrote, outside
;;     the store route's domain: dot-named, executable, not text, a path the
;;     export would not write, or one that a volume folding case or
;;     normalisation would merge with another file of the tree the sync
;;     leaves; landing-files.sc, store-route-domain; or a removed file whose
;;     recorded mode is not 100644): the change takes the git route;
;;   removed-file (a removed file whose sidecar entry names no holder: an
;;     export from before structural drafts), removed-drafted-file (a removed
;;     file holding a draft of one of its blocks: discard it first);
;;   file-mode (a changed file whose git mode is not 100644, or any recorded
;;     file that has become executable on disk, its bytes changed or not:
;;     publication writes 100644 only);
;;   invalid-utf8 (a changed file or a block's new text);
;;   no-block, crossing (a hunk answering no block, or more than one);
;;   not-reconstructed (the blocks' new texts do not give the edited file);
;;   stale (the block's committed hash moved since the export, or the
;;     draft it carried was already based on another version: rebase W's
;;     drafts onto the new cut, `write --rebase --based-on --working-cut`,
;;     then export again and re-apply, since a fresh export of an old draft
;;     still projects the draft's old cut);
;;   draft-changed (W's slot for the block holds a version that is neither
;;     the one the export recorded nor the one this sync would write: another
;;     write under W, which the protocol forbids; and for a structural draft,
;;     one this export does not account for: an add or delete undone in the
;;     export after its sync, or another export's newer draft. The answer
;;     names the file, the key and the remedy: export again, and a delete is
;;     undone by `discard delete-<holder>`. Once a writer holds structural
;;     drafts, one export of it is live at a time);
;;   non-text-block (the slot's text cannot be read as text).
;;
;; FILES ADDED AND REMOVED ARE STRUCTURAL DRAFTS (working.sc): a file on
;; disk the sidecar does not record, a new subdirectory's included, becomes a
;; working-new draft; a recorded file gone (a directory gone is its files)
;; becomes a working-delete draft of its holder; a rename is both; a new file
;; the export wrote from its draft and that is gone again is discarded. Each
;; is predicted -- structural-version, reduce.sc -- and skipped when the
;; store already holds it.
;;
;; A RETRY WRITES NOTHING. A block whose slot already holds exactly the
;; version this sync would write -- sha256 of the new bytes, the based-on
;; and the cut, reduce.sc's draft-version -- is this tool's own earlier
;; write: it is skipped, not refused, and no write call is made for it. A
;; second run over the same files therefore succeeds with (writes 0).
;;
;; THE BASELINE OF A WRITE: an undrafted block is written on its recorded
;; committed hash at the export's cut; a block W had drafted at the export
;; is written with that draft's own based-on and cut, which a write into an
;; occupied slot keeps anyway.
;;
;; A refusal prints (error <kind> ...) and exits 1; a failure after a write
;; adds (written ...). Success prints (ok (files-changed <n>) (writes <n>)
;; (written (block "<id>" (version "<v>")) ...) (skipped "<id>" ...)).
(import (chezscheme)
        (only (theourgia store) open-and-reduce store-state-at-cut)
        (only (theourgia reduce) reduction? reduce-applied-cut block-hash draft-version
              structural-version provisional-id file-baseline)
        (only (theourgia working) working-read working-write! working-new! working-delete! working-discard!
              working-structural)
        (only (theourgia landing-files) locate-pieces store-route-domain)
        (only (theourgia code-project) lang-fields)
        (only (theourgia languages) language-for-path)
        (only (theourgia log) store-id-of)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (theourgia ffi) entry-bytes entry-name-type directory-entries path-executable?))

(define (refuse kind . details) (raise (cons 'error (cons kind details))))
;; the blocks written so far, for a failure after the first write to say
(define written '())

;; ---- small tools -----------------------------------------------------------

(define (prefix? s p) (and (>= (string-length s) (string-length p)) (string=? (substring s 0 (string-length p)) p)))
(define (absolute p) (if (prefix? p "/") p (string-append (current-directory) "/" p)))
(define (without-trailing-slash p)
  (if (and (> (string-length p) 1) (char=? #\/ (string-ref p (- (string-length p) 1))))
      (without-trailing-slash (substring p 0 (- (string-length p) 1)))
      p))
(define (bytes-concat parts)
  (let* ((n (fold-left + 0 (map bytevector-length parts))) (out (make-bytevector n)))
    (let loop ((ps parts) (at 0))
      (if (null? ps) out
          (begin (bytevector-copy! (car ps) 0 out at (bytevector-length (car ps)))
                 (loop (cdr ps) (+ at (bytevector-length (car ps)))))))))
(define (slice bv s e)
  (let ((out (make-bytevector (- e s)))) (bytevector-copy! bv s out 0 (- e s)) out))
(define (hash-of bv) (bytevector->hex (sha256 bv)))
;; valid UTF-8 exactly: a decode replaces every bad sequence, so the round
;; trip gives the bytes back only when there was none
(define (utf8? bv) (bytevector=? bv (string->utf8 (utf8->string bv))))
(define (clause l head) (find (lambda (c) (and (pair? c) (eq? (car c) head))) l))
(define (hex64? s)
  (and (string? s) (= (string-length s) 64)
       (for-all (lambda (c) (or (char<=? #\0 c #\9) (char<=? #\a c #\f))) (string->list s))))
(define (value l head) (let ((c (clause l head))) (and c (pair? (cdr c)) (cadr c))))

;; ---- the sidecar -------------------------------------------------------------
;;
;; -> (values store-id writer cut files), FILES as
;; ((<path> <mode> <pristine-bytes> ((<id> <start> <end> <committed> <draft or #f>) ...)) ...),
;; a draft as (version based-on cut). The sidecar is checked whole here, so a
;; damaged one is refused by name before anything is written, never met
;; halfway: one datum and nothing after it; every entry's shape before it is
;; read into; each cut a nonempty list of (<writer> . <count>); and each
;; file's spans tiling its pristine copy exactly, in order, from 0 to its
;; length. What only the store can confirm (a recorded hash at the recorded
;; cut, a draft's version against its bytes) is checked in `sync`, also
;; before the first write.
(define trailing (list 'trailing-data))
(define (count? x) (and (integer? x) (exact? x) (>= x 0)))
(define (cut? c)
  (and (list? c) (pair? c)
       (for-all (lambda (p) (and (pair? p) (string? (car p)) (> (string-length (car p)) 0) (count? (cdr p)))) c)))
(define (distinct? names)
  (let ((seen (make-hashtable string-hash string=?)))
    (for-all (lambda (n) (and (not (hashtable-ref seen n #f)) (begin (hashtable-set! seen n #t) #t))) names)))
(define (tiles? spans len)
  (let loop ((ss spans) (at 0))
    (if (null? ss) (= at len) (and (= (car (car ss)) at) (loop (cdr ss) (cdr (car ss)))))))
(define (read-sidecar side)
  (let ((path (string-append side "/SIDECAR.sexp")))
    (unless (eq? 'regular (entry-name-type path)) (refuse 'missing-sidecar (list 'path path)))
    (let* ((d (guard (e (#t (refuse 'bad-sidecar (list 'path path) '(reason unreadable))))
                (let* ((port (open-string-input-port (utf8->string (entry-bytes path)))) (first (read port)))
                  (if (eof-object? (read port)) first trailing))))
           (bad (lambda (why) (refuse 'bad-sidecar (list 'path path) (list 'reason why)))))
      (when (eq? d trailing) (bad 'trailing-data))
      (unless (and (list? d) (pair? d) (eq? (car d) 'edit-export) (equal? (clause (cdr d) 'format) '(format 1)))
        (bad 'not-an-edit-sidecar))
      (let ((store-id (value (cdr d) 'store)) (writer (value (cdr d) 'writer)) (cut (clause (cdr d) 'cut)))
        (unless (and (string? store-id) (string? writer) cut (list? cut) (= 2 (length cut)) (cut? (cadr cut))) (bad 'header))
        (let ((files
                (map (lambda (f)
                       (unless (and (list? f) (>= (length f) 5) (string? (cadr f))) (bad 'file))
                       (let* ((rel (cadr f)) (fields (cddr f))
                              (pristine (value fields 'pristine)) (sum (value fields 'sha256))
                              (mode (value fields 'mode)))
                         (unless (and (string? pristine) (string? sum) (or (string? mode) (eq? mode 'none))
                                      (or (not (clause fields 'holder)) (string? (value fields 'holder))))
                           (bad 'file))
                         (let* ((bytes (let ((p (string-append side "/pristine/" pristine)))
                                         (unless (eq? 'regular (entry-name-type p)) (refuse 'pristine-damaged (list 'file rel)))
                                         (entry-bytes p)))
                                (blocks
                                  (map (lambda (b)
                                         ;; the entry's own shape first: nothing below reads into it before
                                         (unless (and (list? b) (>= (length b) 2) (string? (cadr b))) (bad 'block))
                                         (let ((span (clause (cddr b) 'span)) (committed (value (cddr b) 'committed))
                                               (draft (clause (cddr b) 'draft)))
                                           (unless (and span (list? span) (= 3 (length span)) (count? (cadr span))
                                                        (count? (caddr span)) (<= (cadr span) (caddr span)) (hex64? committed)
                                                        (or (not draft)
                                                            (and (list? draft)
                                                                 (hex64? (value (cdr draft) 'version)) (hex64? (value (cdr draft) 'based-on))
                                                                 (let ((c (clause (cdr draft) 'cut)))
                                                                   (and c (list? c) (= 2 (length c)) (cut? (cadr c)))))))
                                             (bad 'block))
                                           (list (cadr b) (cadr span) (caddr span) committed
                                                 (and draft (list (value (cdr draft) 'version) (value (cdr draft) 'based-on)
                                                                  (cadr (clause (cdr draft) 'cut)))))))
                                       (filter (lambda (c) (and (pair? c) (eq? (car c) 'block))) fields))))
                           (unless (equal? (hash-of bytes) sum) (refuse 'pristine-damaged (list 'file rel)))
                           (unless (tiles? (map (lambda (b) (cons (cadr b) (caddr b))) blocks) (bytevector-length bytes))
                             (bad 'spans))
                           (list rel mode bytes blocks (value fields 'holder)))))
                     (filter (lambda (c) (and (pair? c) (eq? (car c) 'file))) (cdr d))))
              ;; A NEW FILE: ((<path> <version> <sha256> <cut> <pristine bytes>) ...),
              ;; a structural draft the export wrote from its entry
              (news
                (map (lambda (f)
                       (unless (and (list? f) (>= (length f) 6) (string? (cadr f))) (bad 'new))
                       (let* ((rel (cadr f)) (fields (cddr f))
                              (version (value fields 'version)) (sum (value fields 'sha256))
                              (pristine (value fields 'pristine)) (c (clause fields 'cut)))
                         (unless (and (hex64? version) (hex64? sum) (string? pristine)
                                      c (list? c) (= 2 (length c)) (cut? (cadr c)))
                           (bad 'new))
                         (let ((bytes (let ((p (string-append side "/pristine/" pristine)))
                                        (unless (eq? 'regular (entry-name-type p)) (refuse 'pristine-damaged (list 'file rel)))
                                        (entry-bytes p))))
                           (unless (equal? (hash-of bytes) sum) (refuse 'pristine-damaged (list 'file rel)))
                           (list rel version sum (cadr c) bytes))))
                     (filter (lambda (c) (and (pair? c) (eq? (car c) 'new))) (cdr d)))))
          ;; every path and every block named once: a second entry for the same
          ;; one would be read as two places for one text
          (unless (distinct? (append (map car files) (map car news))) (bad 'duplicate-file))
          (unless (distinct? (apply append (map (lambda (f) (map car (cadddr f))) files))) (bad 'duplicate-block))
          (values store-id writer (cadr cut) files news))))))

;; ---- the directory ---------------------------------------------------------------

(define (names-in dir) (filter (lambda (n) (not (member n '("." "..")))) (directory-entries dir)))
;; every regular file under DIR: ((<relative path> . <bytes>) ...)
(define (files-under dir)
  (let walk ((rel ""))
    (let ((path (if (string=? rel "") dir (string-append dir "/" rel))))
      (case (entry-name-type path)
        ((directory) (apply append (map (lambda (n) (walk (if (string=? rel "") n (string-append rel "/" n))))
                                        (names-in path))))
        ((regular) (list (cons rel (entry-bytes path))))
        (else (refuse 'unexpected-entry (list 'path rel)))))))

;; ---- the line diff ------------------------------------------------------------------
;;
;; LINES: each ends after its LF; the last one may have none. A hunk is
;; (os oe ns ne), the old bytes [os, oe) replaced by the new bytes [ns, ne).
(define (line-starts bv)
  (let loop ((i 0) (out (if (> (bytevector-length bv) 0) '(0) '())))
    (cond ((= i (bytevector-length bv)) (list->vector (reverse out)))
          ((and (= 10 (bytevector-u8-ref bv i)) (< (+ i 1) (bytevector-length bv))) (loop (+ i 1) (cons (+ i 1) out)))
          (else (loop (+ i 1) out)))))
(define (line-ids bv starts table)
  (let ((n (vector-length starts)))
    (vector-map (lambda (k)
                  (let* ((s (vector-ref starts k)) (e (if (< (+ k 1) n) (vector-ref starts (+ k 1)) (bytevector-length bv)))
                         (line (slice bv s e)))
                    (or (hashtable-ref table line #f)
                        (let ((id (hashtable-size table))) (hashtable-set! table line id) id))))
                (list->vector (iota n)))))

;; THE MATCHED LINE PAIRS of an LCS of A and B (vectors of line ids), in
;; order, by Myers' greedy algorithm. The common head and tail are matched
;; first, so the search runs on the changed middle only; each step keeps
;; its diagonals -d..d for the walk back.
(define (matches a b)
  (let* ((n (vector-length a)) (m (vector-length b))
         (head (let loop ((i 0)) (if (and (< i n) (< i m) (= (vector-ref a i) (vector-ref b i))) (loop (+ i 1)) i)))
         (tail (let loop ((t 0)) (if (and (< (+ head t) n) (< (+ head t) m)
                                          (= (vector-ref a (- n t 1)) (vector-ref b (- m t 1))))
                                     (loop (+ t 1)) t)))
         (a1 head) (a2 (- n tail)) (b1 head) (b2 (- m tail))
         (N (- a2 a1)) (M (- b2 b1)) (max (+ N M)) (off (+ max 1))
         (v (make-vector (+ (* 2 max) 3) 0))
         (eq (lambda (x y) (= (vector-ref a (+ a1 x)) (vector-ref b (+ b1 y))))))
    (define (pairs from to) (map (lambda (i) (cons (+ from i) (+ from i))) (iota (- to from))))
    (define (shifted i j) (cons (+ a1 i) (+ b1 j)))
    ;; -> one slice per step, the last step's first: the diagonals -d..d as
    ;; they were when step d began, which are what step d chose from
    (define (search)
      (let step ((d 0) (trace '()))
        (let ((trace (cons (let ((s (make-vector (+ (* 2 d) 1) 0)))
                             (do ((k (- d) (+ k 1))) ((> k d) s) (vector-set! s (+ k d) (vector-ref v (+ off k)))))
                           trace)))
          (let walk ((k (- d)))
            (cond
              ((> k d) (step (+ d 1) trace))
              (else
               (let* ((x0 (if (or (= k (- d)) (and (not (= k d)) (< (vector-ref v (+ off k -1)) (vector-ref v (+ off k 1)))))
                              (vector-ref v (+ off k 1))
                              (+ (vector-ref v (+ off k -1)) 1)))
                      (x (let slide ((x x0)) (if (and (< x N) (< (- x k) M) (eq x (- x k))) (slide (+ x 1)) x))))
                 (vector-set! v (+ off k) x)
                 (if (and (>= x N) (>= (- x k) M))
                     trace
                     (walk (+ k 2))))))))))
    ;; from the end back to the start, one step at a time: step d's choice of
    ;; the diagonal it came from, then its snake, matched pairs collected in
    ;; ascending order
    (define (walk-back trace)
      (let loop ((d (- (length trace) 1)) (slices trace) (x N) (y M) (out '()))
        (if (< d 0)
            out
            (let* ((s (car slices)) (k (- x y))
                   (at (lambda (k) (vector-ref s (+ k d))))
                   (prev-k (if (or (= k (- d)) (and (not (= k d)) (< (at (- k 1)) (at (+ k 1))))) (+ k 1) (- k 1)))
                   (prev-x (if (= d 0) 0 (at prev-k)))
                   (prev-y (if (= d 0) 0 (- prev-x prev-k))))
              (let snake ((x x) (y y) (out out))
                (if (and (> x prev-x) (> y prev-y))
                    (snake (- x 1) (- y 1) (cons (shifted (- x 1) (- y 1)) out))
                    (loop (- d 1) (cdr slices) prev-x prev-y out)))))))
    (append (pairs 0 head)
            (if (= max 0) '() (walk-back (search)))
            (map (lambda (t) (cons (+ a2 t) (+ b2 t))) (iota tail)))))

;; THE HUNKS of OLD -> NEW in bytes: every run between two matched lines.
(define (hunks old new)
  (let* ((table (make-hashtable equal-hash equal?))
         (os (line-starts old)) (ns (line-starts new))
         (a (line-ids old os table)) (b (line-ids new ns table))
         (oat (lambda (i) (if (< i (vector-length os)) (vector-ref os i) (bytevector-length old))))
         (nat (lambda (j) (if (< j (vector-length ns)) (vector-ref ns j) (bytevector-length new)))))
    (let loop ((ms (append (matches a b) (list (cons (vector-length a) (vector-length b))))) (i 0) (j 0) (out '()))
      (if (null? ms)
          (reverse out)
          (let ((mi (car (car ms))) (mj (cdr (car ms))))
            (loop (cdr ms) (+ mi 1) (+ mj 1)
                  (if (and (= mi i) (= mj j)) out (cons (list (oat i) (oat mi) (nat j) (nat mj)) out))))))))

;; ---- one changed file to its blocks' new texts ----------------------------------------
;;
;; -> ((<block> . <new bytes>) ...) for every block whose text changes.
(define (blocks-of rel old new blocks)
  (let* ((pieces (map (lambda (b) (list 'source (car b) (cadr b) (caddr b) 0 '())) blocks))
         (len (bytevector-length old))
         (assigned
           (map (lambda (h)
                  (let ((items (locate-pieces pieces len (car h) (cadr h))))
                    (cond ((null? items) (refuse 'no-block (list 'file rel) (list 'bytes (car h) (cadr h))))
                          ((pair? (cdr items))
                           (refuse 'crossing (list 'file rel) (list 'bytes (car h) (cadr h)) (cons 'blocks (map cadr items))))
                          (else (cons (cadr (car items)) h)))))
                (hunks old new)))
         (texts
           (map (lambda (b)
                  (let ((mine (filter (lambda (h) (equal? (car h) (car b))) assigned)))
                    (cons (car b)
                          (let loop ((hs mine) (at (cadr b)) (out '()))
                            (if (null? hs)
                                (bytes-concat (reverse (cons (slice old at (caddr b)) out)))
                                (let ((h (cdr (car hs))))
                                  (loop (cdr hs) (cadr h)
                                        (cons (slice new (caddr h) (cadddr h)) (cons (slice old at (car h)) out)))))))))
                blocks)))
    (unless (bytevector=? (bytes-concat (map cdr texts)) new) (refuse 'not-reconstructed (list 'file rel)))
    (filter (lambda (t) (let ((b (assoc (car t) blocks)))
                          (not (bytevector=? (cdr t) (slice old (cadr b) (caddr b))))))
            texts)))

;; ---- the sync -----------------------------------------------------------------------------

(define (sync store dir writer)
  (let ((side (string-append dir ".edit")))
    (let-values (((store-id side-writer cut files news) (read-sidecar side)))
      (unless (equal? store-id (store-id-of store)) (refuse 'foreign-store (list 'sidecar store-id)))
      (unless (equal? side-writer writer) (refuse 'foreign-writer (list 'sidecar side-writer)))
      (unless (eq? 'directory (entry-name-type dir)) (refuse 'removed-file (list 'path dir)))
      (let* ((on-disk (files-under dir))
             (recorded (append (map car files) (map car news)))
             ;; THE PATHS, CLASSIFIED: a file on disk the sidecar does not record
             ;; is ADDED (a new subdirectory's too); a recorded file gone is
             ;; REMOVED (a directory gone is its files); a new file the export
             ;; wrote is KEPT or GONE. A rename is a removal and an addition.
             (added (filter (lambda (f) (not (member (car f) recorded))) on-disk))
             (removed (filter (lambda (f) (not (assoc (car f) on-disk))) files))
             (news-kept (filter (lambda (n) (assoc (car n) on-disk)) news))
             (news-gone (filter (lambda (n) (not (assoc (car n) on-disk))) news))
             ;; A CASE COLLISION IS JUDGED ON THE TREE THE SYNC LEAVES: a path
             ;; being removed is not there, so a.sc renamed A.sc collides with
             ;; nothing, as the landing's own check reads it
             (paths (map car on-disk)))
        ;; EVERY CHECK OF A STRUCTURAL CHANGE BEFORE ANY WRITE. A new file must
        ;; be in the store route's domain (landing-files.sc): outside it, the
        ;; whole item takes the git route, and nothing is written here.
        (for-each (lambda (f)
                    (let* ((rel (car f))
                           (mode (if (path-executable? (string-append dir "/" rel)) "100755" "100644"))
                           (judged (store-route-domain rel mode (cdr f) paths)))
                      (unless (eq? judged 'in) (refuse 'outside-domain (list 'file rel) (list 'reason judged)))))
                  (append added (map (lambda (n) (cons (car n) (cdr (assoc (car n) on-disk)))) news-kept)))
        ;; A REMOVED FILE NAMES ITS HOLDER, and carries no draft of a block: its
        ;; baseline is its committed bytes, which a drafted block is not. Its
        ;; recorded mode is in the store route's domain too: removing an
        ;; executable (or renaming it, a removal and an addition) takes the git
        ;; route, since publication writes 100644 only.
        (for-each (lambda (f)
                    (when (and (string? (cadr f)) (not (equal? (cadr f) "100644")))
                      (refuse 'outside-domain (list 'file (car f)) '(reason mode)))
                    (unless (list-ref f 4) (refuse 'removed-file (list 'file (car f)) '(reason no-holder-recorded)))
                    (when (exists (lambda (b) (car (cddddr b))) (cadddr f))
                      (refuse 'removed-drafted-file (list 'file (car f)))))
                  removed)
        ;; A RECORDED FILE MADE EXECUTABLE ON DISK is a change of mode whether or
        ;; not its bytes changed: the export writes none, and publication writes
        ;; 100644 only, so it takes the git route
        (for-each (lambda (f)
                    (when (and (assoc (car f) on-disk) (path-executable? (string-append dir "/" (car f))))
                      (refuse 'file-mode (list 'file (car f)) '(mode executable-on-disk))))
                  files)
        (let* ((changed (filter (lambda (f) (and (assoc (car f) on-disk)
                                                 (not (bytevector=? (caddr f) (cdr (assoc (car f) on-disk))))))
                                files))
               ;; ((<id> <new bytes> <committed> <draft or #f> <old bytes>) ...), every check of a file passed
               (touched
                 (apply append
                   (map (lambda (f)
                          (let ((rel (car f)) (new (cdr (assoc (car f) on-disk))))
                            (unless (equal? (cadr f) "100644") (refuse 'file-mode (list 'file rel) (list 'mode (cadr f))))
                            (unless (utf8? new) (refuse 'invalid-utf8 (list 'file rel)))
                            (map (lambda (t)
                                   (unless (utf8? (cdr t)) (refuse 'invalid-utf8 (list 'file rel) (list 'block (car t))))
                                   (let ((b (assoc (car t) (cadddr f))))
                                     (list (car t) (cdr t) (cadddr b) (car (cddddr b)) (slice (caddr f) (cadr b) (caddr b)))))
                                 (blocks-of rel (caddr f) new (cadddr f)))))
                        changed)))
               (state (open-and-reduce store))
               ;; THE SIDECAR'S OWN FACTS, CONFIRMED BY THE STORE before any write:
               ;; an undrafted block's recorded hash is its hash at the recorded
               ;; cut (read through the cut path a read uses, which answers an
               ;; unusable cut rather than raising), and a drafted block's
               ;; recorded version is the version of its exported bytes, its
               ;; based-on and its cut. A sidecar that fails either was not
               ;; written by edit-export for this store, and a write planned
               ;; from it could be refused after an earlier one landed.
               (at-cut (and (exists (lambda (t) (not (cadddr t))) touched)
                            (store-state-at-cut store state (format "~s" cut))))
               (confirmed
                 (for-each (lambda (t)
                             (let ((id (car t)) (draft (cadddr t)))
                               (if draft
                                   (unless (equal? (draft-version (list-ref t 4) (cadr draft) (caddr draft)) (car draft))
                                     (refuse 'bad-sidecar (list 'block id) '(reason draft-inconsistent)))
                                   (unless (and (reduction? at-cut) (equal? (block-hash at-cut id) (caddr t)))
                                     (refuse 'bad-sidecar (list 'block id) '(reason committed-not-at-cut))))))
                           touched))
               ;; ((<id> <new bytes> <based-on> <cut-text> <version>) ...) to write, and the skipped ids
               (plan
                 (map (lambda (t)
                        (let* ((id (car t)) (new (cadr t)) (committed (caddr t)) (draft (cadddr t))
                               (now (block-hash state id))
                               (based-on (if draft (cadr draft) committed))
                               ;; an undrafted block's write records the cut the store
                               ;; answers for the recorded one, in its own order, so
                               ;; the version is predicted on that cut too
                               (at (if draft (caddr draft) (reduce-applied-cut at-cut)))
                               (version (draft-version new based-on at)))
                          (unless (equal? now committed) (refuse 'stale (list 'block id) '(reason committed-moved)))
                          (unless (equal? based-on now) (refuse 'stale (list 'block id) '(reason draft-on-another-version)))
                          (let ((a (working-read store state writer id #t)))
                            (unless (and (pair? a) (eq? (car a) 'ok) (pair? (cadr a)) (eq? (car (cadr a)) 'projection))
                              (refuse 'non-text-block (list 'block id) (list 'answer a)))
                            (let* ((p (cadr a)) (kind (cadr p)) (held (list-ref p 4)))
                              (cond
                                ((and (eq? kind 'working) (equal? held version)) (list 'skip id))
                                ((and (eq? kind 'working) draft (equal? held (car draft)))
                                 (list 'write id new based-on (format "~s" at) version))
                                ((and (eq? kind 'committed) (not draft))
                                 (list 'write id new based-on (format "~s" at) version))
                                (else (refuse 'draft-changed (list 'block id) (list 'held held)
                                              (list 'recorded (and draft (car draft))))))))))
                      touched))
               ;; THE STRUCTURAL WRITES, every one predicted before any write:
               ;; (new <path> <pid> <bytes> <version>), (delete <path> <holder>
               ;; <version>), (discard <path> <pid>) for a new file gone from
               ;; disk, or a skip when the store already holds that version.
               ;; A new file not yet drafted, and a removed file, stand on the
               ;; cut the store answers for the sidecar's; a new file the export
               ;; wrote keeps its entry's cut.
               (structural-cut (and (or (pair? added) (pair? removed))
                            (let ((at (store-state-at-cut store state (format "~s" cut))))
                              (unless (reduction? at) (refuse 'cut-unavailable (list 'cut cut)))
                              (reduce-applied-cut at))))
               ;; READ ON EVERY SYNC, not only when a path is classified: a
               ;; structural draft this export does not show (an undone add or
               ;; delete, or another export's) is refused below, never left
               ;; standing under an ok
               (live (let ((a (working-structural store state writer)))
                       (unless (and (pair? a) (eq? (car a) 'ok)) (refuse 'structural-refused (list 'answer a)))
                       (caddr a)))
               (held (lambda (kind key)
                       (let ((e (find (lambda (i) (and (eq? (car i) kind)
                                                       (equal? (value (cdr i) (if (eq? kind 'new) 'provisional 'holder)) key)))
                                      live)))
                         (and e (value (cdr e) 'version)))))
               (structural-plan
                 (append
                   (map (lambda (f)
                          (let* ((rel (car f)) (pid (provisional-id rel))
                                 (v (structural-version 'new rel (hash-of (cdr f)) structural-cut)))
                            (if (equal? (held 'new pid) v) (list 'skip rel 'new pid v) (list 'new rel pid (cdr f) v))))
                        added)
                   (map (lambda (n)
                          (let* ((rel (car n)) (bytes (cdr (assoc rel on-disk))) (pid (provisional-id rel))
                                 (v (structural-version 'new rel (hash-of bytes) (cadddr n))))
                            (if (or (bytevector=? bytes (list-ref n 4)) (equal? (held 'new pid) v))
                                (list 'skip rel 'new pid v)
                                (list 'new rel pid bytes v))))
                        news-kept)
                   (map (lambda (n) (if (held 'new (provisional-id (car n)))
                                        (list 'discard (car n) (provisional-id (car n)))
                                        (list 'skip (car n) 'gone (provisional-id (car n)) #f)))
                        news-gone)
                   (map (lambda (f)
                          (let* ((holder (list-ref f 4))
                                 (v (structural-version 'delete holder (file-baseline (caddr f) (map car (cadddr f))) structural-cut)))
                            (if (equal? (held 'delete holder) v) (list 'skip (car f) 'delete holder v) (list 'delete (car f) holder v))))
                        removed))))
          ;; EVERY LIVE STRUCTURAL DRAFT IS ACCOUNTED FOR BY THIS EXPORT before any
          ;; write, as a block draft is (draft-changed): its slot holds the
          ;; version the export recorded, or the one this sync writes, or
          ;; nothing. A new file the export wrote is its sidecar entry; a new
          ;; file on disk the sidecar does not record holds at most this sync's
          ;; version; a removed file's delete likewise; a delete of a file the
          ;; export did not write was there when it was made. Anything else --
          ;; an undone add or delete, or another export's newer draft -- is
          ;; refused, and the remedy is a fresh export (a delete is undone by
          ;; `discard delete-<holder>`): one live export per writer once
          ;; structural drafts exist.
          ;; (new <path> <pid> <bytes> <v>), (delete <path> <holder> <v>) and
          ;; (skip <path> <kind> <key> <v>) each carry the version predicted
          (let* ((predicted (lambda (kind key)
                              (let ((x (find (lambda (x)
                                               (or (and (eq? (car x) kind) (equal? (caddr x) key))
                                                   (and (eq? (car x) 'skip) (eq? (caddr x) kind) (equal? (cadddr x) key))))
                                             structural-plan)))
                                (and x (cond ((eq? (car x) 'skip) (list-ref x 4))
                                             ((eq? kind 'new) (list-ref x 4))
                                             (else (cadddr x)))))))
                 (allowed
                   (append
                     (map (lambda (n) (let ((pid (provisional-id (car n))))
                                        (list (cons 'new pid) (car n)
                                              (if (assoc (car n) on-disk)
                                                  (list (cadr n) (or (predicted 'new pid) (cadr n)))
                                                  (list (cadr n))))))
                          news)
                     (map (lambda (f) (let ((pid (provisional-id (car f))))
                                        (list (cons 'new pid) (car f) (list (predicted 'new pid)))))
                          added)
                     (map (lambda (f) (list (cons 'delete (list-ref f 4)) (car f) (list (predicted 'delete (list-ref f 4)))))
                          removed)))
                 (exported-holders (filter values (map (lambda (f) (list-ref f 4)) files))))
            (for-each
              (lambda (i)
                (let* ((kind (car i)) (key (value (cdr i) (if (eq? kind 'new) 'provisional 'holder)))
                       (v (value (cdr i) 'version)) (a (assoc (cons kind key) allowed)))
                  (cond
                    ((and a (member v (caddr a))) #t)
                    ((and (not a) (eq? kind 'delete) (not (member key exported-holders))) #t)
                    (else
                     (refuse 'draft-changed
                             (list 'file (if a (cadr a) (value (cdr i) 'path)))
                             (list 'key (if (eq? kind 'new) key (string-append "delete-" key)))
                             (list 'held v)
                             (list 'remedy (if (eq? kind 'new)
                                               'export-again
                                               (list 'export-again (list 'discard (string-append "delete-" key))))))))))
              live)
            ;; AND A NEW FILE THE EXPORT WROTE IS STILL DRAFTED when it is kept:
            ;; one discarded since is not written again from this export (a
            ;; discard's retry, the file still gone, holds nothing and is a skip)
            (for-each (lambda (n)
                        (unless (held 'new (provisional-id (car n)))
                          (refuse 'draft-changed (list 'file (car n)) (list 'key (provisional-id (car n)))
                                  '(held none) '(remedy export-again))))
                      news-kept))
          (for-each
            (lambda (p)
              (when (eq? (car p) 'write)
                (let* ((id (cadr p))
                       (a (working-write! store #f writer id (utf8->string (caddr p)) #f (cadddr p) (list-ref p 4)))
                       (v (and (pair? a) (eq? (car a) 'ok) (value (cdr a) 'version))))
                  (unless v (refuse 'write-refused (list 'block id) (list 'answer a)))
                  (set! written (cons (list 'block id (list 'version v)) written))
                  (unless (equal? v (list-ref p 5))
                    (refuse 'version-unexpected (list 'block id) (list 'version v) (list 'predicted (list-ref p 5)))))))
            plan)
          (for-each
            (lambda (p)
              (case (car p)
                ((new delete)
                 (let* ((a (if (eq? (car p) 'new)
                               (working-new! store #f writer (cadr p) (lang-fields (language-for-path (cadr p)))
                                             (cadddr p) (format "~s" cut))
                               (working-delete! store #f writer (caddr p) (format "~s" cut))))
                        (v (and (pair? a) (eq? (car a) 'ok) (value (cdr a) 'version)))
                        (predicted (if (eq? (car p) 'new) (list-ref p 4) (cadddr p))))
                   (unless v (refuse 'write-refused (list 'file (cadr p)) (list 'answer a)))
                   (set! written (cons (list (car p) (cadr p) (list 'version v)) written))
                   (unless (equal? v predicted)
                     (refuse 'version-unexpected (list 'file (cadr p)) (list 'version v) (list 'predicted predicted)))))
                ((discard)
                 (let ((a (working-discard! store writer (caddr p))))
                   (unless (and (pair? a) (eq? (car a) 'ok)) (refuse 'discard-refused (list 'file (cadr p)) (list 'answer a)))
                   (set! written (cons (list 'discarded (cadr p)) written))))
                (else #f)))
            structural-plan)
          (list 'ok (list 'files-changed (length changed)) (list 'files-added (length added))
                (list 'files-removed (length removed)) (list 'writes (length written))
                (cons 'written (reverse written))
                (cons 'skipped (append (map cadr (filter (lambda (p) (eq? (car p) 'skip)) plan))
                                       (map cadr (filter (lambda (p) (eq? (car p) 'skip)) structural-plan))))))))))

;; ---- the command line ------------------------------------------------------------------

(define (main args)
  (let ((m (member "--writer" args)))
    (if (not (and (= 4 (length args)) m (pair? (cdr m)) (equal? (list-ref args 2) "--writer")
                  (not (prefix? (car args) "--")) (not (prefix? (cadr args) "--"))))
        (begin (write '(usage (edit-sync <store> <dir> "--writer" <W>))) (newline) (exit 2))
        (let ((a (guard (e ((and (pair? e) (eq? (car e) 'error)) e))
                   (sync (without-trailing-slash (absolute (car args))) (without-trailing-slash (absolute (cadr args)))
                         (list-ref args 3)))))
          (write (if (and (eq? (car a) 'error) (pair? written)) (append a (list (cons 'written (reverse written)))) a))
          (newline)
          (exit (if (eq? (car a) 'ok) 0 1))))))

(main (cdr (command-line)))
