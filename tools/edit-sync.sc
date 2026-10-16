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
;;   unexpected-entry (not a regular file or a directory), added-file,
;;     removed-file;
;;   file-mode (a changed file whose git mode is not 100644, or that has
;;     become executable on disk: publication writes 100644 only);
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
;;     write under W, which the protocol forbids);
;;   non-text-block (the slot's text cannot be read as text).
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
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) block-hash draft-version)
        (only (theourgia working) working-read working-write!)
        (only (theourgia landing-files) locate-pieces)
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
;; a draft as (version based-on cut). Its shape is checked whole here, so a
;; damaged sidecar is refused by name rather than met halfway.
(define (read-sidecar side)
  (let ((path (string-append side "/SIDECAR.sexp")))
    (unless (eq? 'regular (entry-name-type path)) (refuse 'missing-sidecar (list 'path path)))
    (let* ((d (guard (e (#t (refuse 'bad-sidecar (list 'path path) '(reason unreadable))))
                (read (open-string-input-port (utf8->string (entry-bytes path))))))
           (bad (lambda (why) (refuse 'bad-sidecar (list 'path path) (list 'reason why)))))
      (unless (and (list? d) (pair? d) (eq? (car d) 'edit-export) (equal? (clause (cdr d) 'format) '(format 1)))
        (bad 'not-an-edit-sidecar))
      (let ((store-id (value (cdr d) 'store)) (writer (value (cdr d) 'writer)) (cut (clause (cdr d) 'cut)))
        (unless (and (string? store-id) (string? writer) cut (pair? (cdr cut))) (bad 'header))
        (values store-id writer (cadr cut)
                (map (lambda (f)
                       (unless (and (list? f) (>= (length f) 5) (string? (cadr f))) (bad 'file))
                       (let* ((rel (cadr f)) (fields (cddr f))
                              (pristine (value fields 'pristine)) (sum (value fields 'sha256))
                              (mode (value fields 'mode)))
                         (unless (and (string? pristine) (string? sum)) (bad 'file))
                         (let ((bytes (let ((p (string-append side "/pristine/" pristine)))
                                        (unless (eq? 'regular (entry-name-type p)) (refuse 'pristine-damaged (list 'file rel)))
                                        (entry-bytes p))))
                           (unless (equal? (hash-of bytes) sum) (refuse 'pristine-damaged (list 'file rel)))
                           (list rel mode bytes
                                 (map (lambda (b)
                                        ;; the entry's own shape first: nothing below reads into it before
                                        (unless (and (list? b) (>= (length b) 2) (string? (cadr b))) (bad 'block))
                                        (let ((span (clause (cddr b) 'span)) (committed (value (cddr b) 'committed))
                                              (draft (clause (cddr b) 'draft)))
                                          (unless (and span (list? span) (= 3 (length span)) (fixnum? (cadr span))
                                                       (fixnum? (caddr span)) (<= 0 (cadr span) (caddr span)) (hex64? committed)
                                                       (or (not draft)
                                                           (and (list? draft)
                                                                (hex64? (value (cdr draft) 'version)) (hex64? (value (cdr draft) 'based-on))
                                                                (let ((c (clause (cdr draft) 'cut))) (and c (list? c) (= 2 (length c)))))))
                                            (bad 'block))
                                          (list (cadr b) (cadr span) (caddr span) committed
                                                (and draft (list (value (cdr draft) 'version) (value (cdr draft) 'based-on)
                                                                 (cadr (clause (cdr draft) 'cut)))))))
                                      (filter (lambda (c) (and (pair? c) (eq? (car c) 'block))) fields))))))
                     (filter (lambda (c) (and (pair? c) (eq? (car c) 'file))) (cdr d))))))))

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
    (let-values (((store-id side-writer cut files) (read-sidecar side)))
      (unless (equal? store-id (store-id-of store)) (refuse 'foreign-store (list 'sidecar store-id)))
      (unless (equal? side-writer writer) (refuse 'foreign-writer (list 'sidecar side-writer)))
      (unless (eq? 'directory (entry-name-type dir)) (refuse 'removed-file (list 'path dir)))
      (let* ((on-disk (files-under dir))
             (recorded (map car files)))
        (for-each (lambda (f) (unless (member (car f) recorded) (refuse 'added-file (list 'file (car f))))) on-disk)
        (for-each (lambda (r) (unless (assoc r on-disk) (refuse 'removed-file (list 'file r)))) recorded)
        (let* ((changed (filter (lambda (f) (not (bytevector=? (caddr f) (cdr (assoc (car f) on-disk))))) files))
               ;; ((<id> <new bytes> <committed> <draft or #f>) ...), every check of a file passed
               (touched
                 (apply append
                   (map (lambda (f)
                          (let ((rel (car f)) (new (cdr (assoc (car f) on-disk))))
                            (unless (equal? (cadr f) "100644") (refuse 'file-mode (list 'file rel) (list 'mode (cadr f))))
                            (when (path-executable? (string-append dir "/" rel))
                              (refuse 'file-mode (list 'file rel) '(mode executable-on-disk)))
                            (unless (utf8? new) (refuse 'invalid-utf8 (list 'file rel)))
                            (map (lambda (t)
                                   (unless (utf8? (cdr t)) (refuse 'invalid-utf8 (list 'file rel) (list 'block (car t))))
                                   (let ((b (assoc (car t) (cadddr f))))
                                     (list (car t) (cdr t) (cadddr b) (car (cddddr b)))))
                                 (blocks-of rel (caddr f) new (cadddr f)))))
                        changed)))
               (state (open-and-reduce store))
               ;; ((<id> <new bytes> <based-on> <cut-text> <version>) ...) to write, and the skipped ids
               (plan
                 (map (lambda (t)
                        (let* ((id (car t)) (new (cadr t)) (committed (caddr t)) (draft (cadddr t))
                               (now (block-hash state id))
                               (based-on (if draft (cadr draft) committed))
                               (at (if draft (caddr draft) cut))
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
                      touched)))
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
          (list 'ok (list 'files-changed (length changed)) (list 'writes (length written))
                (cons 'written (reverse written))
                (cons 'skipped (map cadr (filter (lambda (p) (eq? (car p) 'skip)) plan)))))))))

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
