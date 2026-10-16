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

;; Absorb the files that differ between two git commits into the store, as
;; committed blocks, and tag the result.
;;
;;   scheme --script tools/import-git.sc <store> <repo> <from-rev> <to-rev> --tag <name> [--dry-run]
;;
;; THE DOMAIN is the seed importer's: regular files of mode 100644 or 100755
;; whose bytes are text (import-text?), not dot-named. A changed path outside
;; it is refused, a dot-named one is skipped.
;;
;; THE RANGE comes from `git diff --name-status -M -z`: M is a content change
;; with the same mode at both ends (file-mode otherwise), A an added file at
;; 100644 (file-mode otherwise), D a removed one, R the removal of the old
;; path and the addition of the new (ids do not follow a rename), T and any
;; other letter paths-unsupported; an added path the store cannot hold is
;; unsafe-path. Nothing left is nothing-to-import.
;;
;; THE PRECONDITION is read at the store's applied cut, THE READ CUT, by
;; literal blobs, and accepts one of three states:
;;   fresh             no import block for the range, the export equal to
;;                     <from-rev>'s tree
;;   pending           an import block for the range with status pending
;;                     (an interrupted run), every path outside the range
;;                     equal to <from-rev>
;;   done-without-tag  a done import block for the range whose tag is not
;;                     bound, the export equal to <to-rev>: only the tag is
;;                     written
;; A pending block of another range is pending-import; anything else is
;; base-mismatch, and nothing is written. On fresh and pending a bound
;; <name> is tag-exists.
;;
;; EVERY RANGE PATH IS READ FROM THE STORE, never from the git letter: a
;; path absent at <to-rev> has its live children and its file block
;; deleted; a path with no holder is added; otherwise the holder's current
;; children are matched to suggest-boundaries' pieces of <to-rev>'s bytes.
;; So a run started after an interrupted one converges from whatever prefix
;; that one left.
;;
;; THE MATCHER, per held file, in three passes over the pieces' bytes:
;; (i) the longest common subsequence, equal pieces paired earliest to
;; earliest, keeps those ids; (ii) every remaining old piece equal to a
;; remaining new one is paired with it, earliest to earliest, and keeps its
;; id (a relocation); (iii) between consecutive anchors in target order,
;; the remaining pieces are paired by position: a pair is a set of src,
;; surplus new pieces are inserts and surplus old ones are deletes. An old
;; piece belongs to the gap after the last anchor that precedes it in the
;; current order. PLACEMENT then walks the target order, left to right: a
;; new piece is inserted after the block before it in the target, and a
;; kept block whose current predecessor is not that block is moved after
;; it. With no block before it the predecessor is #f, which appends after
;; the last sibling (the store has no first-child form), and every later
;; block is then placed right after its target predecessor, so the order
;; ends as the target's.
;;
;; THE WRITES are two requests. The first, on a fresh run only, inserts the
;; import block (kind decision, slot "import", from, to, tag, status
;; pending, text the range's commit subjects). The second is one batch: the
;; deletes, sets, inserts and moves, then `set status done` on the import
;; block, then the tag. It carries premises for every live block of every
;; range path at the read cut and for the import block, so a concurrent
;; change to any of them refuses it whole (premise-changed). A batch is not
;; atomic: an item the store refuses stops the rest, with the earlier items
;; on disk, and is answered (error partial (item <k>) (kind ...) (path ...)
;; (store-answer ...) (done <k>)); running the same command again converges.
;; The tag-only run of done-without-tag carries the same premises.
;;
;; POST-VERIFY: the export at the new tag is compared with <to-rev>'s tree
;; in the domain; a difference is (error import-diverged (differing ...)),
;; with the tag made and the block done.
;;
;; --dry-run reads, checks and plans as a run does, and prints the plan
;; instead of writing.
;;
;; TEST SEAMS: THEOURGIA_IMPORT_STOP=<k> sends only the first k items of the
;; batch (none: only the import block) and exits 3, printing (stopped ...),
;; so a fixture can make the prefix an interrupted run leaves. The hold
;; stage import-before-batch (THEOURGIA_HOLD, with THEOURGIA_INJECT=on)
;; waits between the premises' computation and the batch.
;;
;; A refusal prints (error <kind> ...) and exits 1; success prints
;; (ok (tag <name> (cut <cut>)) (rerun <#t|#f>)
;;     (files (modified <n>) (added <n>) (removed <n>))
;;     (blocks (kept <n>) (set (<id> ...)) (inserted (<id> ...))
;;             (deleted (<id> ...)) (moved (<id> ...))))
;; the lists in item order.
(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia store) open-and-reduce store-tags)
        (only (theourgia reduce) reduce-applied-cut block-hash block-id state-read)
        (only (theourgia code-project) code-files code-field code-children projected-files projected-file lang-fields code-safe-path?)
        (only (theourgia code-suggest) suggest-boundaries)
        (only (theourgia languages) language-for-path)
        (only (theourgia markers) import-text? byte-slice bytes-append)
        (only (theourgia query) make-query-session session-answer)
        (only (theourgia ffi) hold-point!))

(define actor "import-git")
(define (refuse kind . details) (raise (cons 'error (cons kind details))))

;; ---- small tools -----------------------------------------------------------

(define (prefix? s p) (and (>= (string-length s) (string-length p)) (string=? (substring s 0 (string-length p)) p)))
(define (split-on s ch)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i (string-length s)) (reverse (cons (substring s start i) out)))
          ((char=? (string-ref s i) ch) (loop (+ i 1) (+ i 1) (cons (substring s start i) out)))
          (else (loop (+ i 1) start out)))))
(define (join parts sep)
  (if (null? parts) ""
      (fold-left (lambda (acc p) (string-append acc sep p)) (car parts) (cdr parts))))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(define (chomp s) (if (and (> (string-length s) 0) (char=? #\newline (string-ref s (- (string-length s) 1))))
                      (substring s 0 (- (string-length s) 1)) s))
(define (hex40? s)
  (and (string? s) (= (string-length s) 40)
       (for-all (lambda (c) (or (char<=? #\0 c #\9) (char<=? #\a c #\f))) (string->list s))))
(define (absolute p) (if (prefix? p "/") p (string-append (current-directory) "/" p)))
(define (dot-named? rel) (exists (lambda (c) (prefix? c ".")) (split-on rel #\/)))
(define (under? p dir) (or (string=? p dir) (prefix? p (string-append dir "/"))))
(define (sorted-unique paths)
  (let loop ((ps (list-sort string<? paths)) (out '()))
    (cond ((null? ps) (reverse out))
          ((and (pair? out) (string=? (car ps) (car out))) (loop (cdr ps) out))
          (else (loop (cdr ps) (cons (car ps) out))))))
(define (text-of v) (cond ((string? v) v) ((symbol? v) (symbol->string v)) (else #f)))

;; ---- git -------------------------------------------------------------------
;;
;; One shell command, its standard output read whole as bytes, its exit
;; status printed after the output and taken off it. Standard error is
;; discarded: a failure is answered by the status and the command.
(define (shell-run command)
  (let-values (((to from errs pid)
                (open-process-ports (string-append command "; printf '\\n%s' \"$?\"") (buffer-mode block) #f)))
    (close-port to)
    (let ((out (let ((b (get-bytevector-all from))) (if (eof-object? b) (make-bytevector 0) b))))
      (get-bytevector-all errs)
      (close-port from) (close-port errs)
      (let loop ((i (- (bytevector-length out) 1)))
        (cond ((< i 0) (values -1 out))
              ((= 10 (bytevector-u8-ref out i))
               (values (or (string->number (utf8->string (byte-slice out (+ i 1) (bytevector-length out)))) -1)
                       (byte-slice out 0 i)))
              (else (loop (- i 1))))))))
(define (git-command repo args)
  (string-append "LC_ALL=C GIT_LITERAL_PATHSPECS=1 git -c core.hooksPath=/dev/null -C " (quoted repo) " "
                 (join (map quoted args) " ")))
(define (git-bytes repo . args)
  (let-values (((rc out) (shell-run (string-append (git-command repo args) " 2>/dev/null"))))
    (unless (= rc 0) (refuse 'repo-unreadable (list 'repo repo) (cons 'command args) (list 'status rc)))
    out))
(define (git-text repo . args) (utf8->string (apply git-bytes repo args)))

(define (commit-id repo rev)
  (let ((id (let-values (((rc out) (shell-run (string-append (git-command repo (list "rev-parse" "--verify" "--quiet" (string-append rev "^{commit}")))
                                                             " 2>/dev/null"))))
              (and (= rc 0) (chomp (utf8->string out))))))
    (unless (hex40? id) (refuse 'repo-unreadable (list 'repo repo) (list 'rev rev)))
    id))

;; path -> (mode type sha), from `git ls-tree -r -z`: "<mode> <type> <sha>\t<path>\0"
(define (tree-of repo id)
  (let ((h (make-hashtable string-hash string=?)))
    (for-each (lambda (rec)
                (when (> (string-length rec) 0)
                  (let* ((tab (let loop ((i 0)) (if (char=? (string-ref rec i) #\tab) i (loop (+ i 1)))))
                         (meta (split-on (substring rec 0 tab) #\space)))
                    (hashtable-set! h (substring rec (+ tab 1) (string-length rec)) meta))))
              (split-on (git-text repo "ls-tree" "-r" "-z" "--full-tree" id) #\nul))
    h))

;; THE BLOBS, read by one `git cat-file --batch` per chunk of object ids and
;; kept by id. Each answer is "<sha> blob <size>\n<bytes>\n"; an object git
;; does not have is "<sha> missing\n" and refuses the run.
(define blobs (make-hashtable string-hash string=?))
(define (read-blobs! repo shas)
  (let ((wanted (sorted-unique (filter (lambda (s) (not (hashtable-contains? blobs s))) shas))))
    (let chunk ((rest wanted))
      (unless (null? rest)
        (let* ((n (min 500 (length rest))) (these (list-head rest n)))
          (let-values (((rc out) (shell-run (string-append "printf '%s\\n' " (join these " ") " | "
                                                           (git-command repo '("cat-file" "--batch")) " 2>/dev/null"))))
            (unless (= rc 0) (refuse 'repo-unreadable (list 'repo repo) '(command "cat-file" "--batch") (list 'status rc)))
            (let loop ((at 0) (ids these))
              (unless (null? ids)
                (let* ((eol (let find ((i at)) (if (= 10 (bytevector-u8-ref out i)) i (find (+ i 1)))))
                       (head (split-on (utf8->string (byte-slice out at eol)) #\space)))
                  (unless (and (= 3 (length head)) (string=? (cadr head) "blob") (string=? (car head) (car ids)))
                    (refuse 'repo-unreadable (list 'repo repo) (list 'object (car ids))))
                  (let ((size (string->number (caddr head))))
                    (hashtable-set! blobs (car ids) (byte-slice out (+ eol 1) (+ eol 1 size)))
                    (loop (+ eol 2 size) (cdr ids)))))))
          (chunk (list-tail rest n)))))))
(define (blob-of entry) (hashtable-ref blobs (caddr entry) #f))

(define (regular? entry) (and (member (car entry) '("100644" "100755")) (string=? (cadr entry) "blob")))

;; THE DOMAIN OF A TREE: path -> bytes for every regular, not dot-named file
;; whose bytes are text, and the list of the other paths that are not
;; dot-named (the importer follows a link, so the store may hold files at or
;; under one; those are not differences).
(define (domain-of repo tree)
  (let-values (((paths entries) (hashtable-entries tree)))
    (read-blobs! repo (filter (lambda (x) x)
                              (map (lambda (p e) (and (not (dot-named? p)) (regular? e) (caddr e)))
                                   (vector->list paths) (vector->list entries))))
    (let ((h (make-hashtable string-hash string=?)) (skipped '()))
      (vector-for-each
        (lambda (p e)
          (unless (dot-named? p)
            (let ((b (and (regular? e) (blob-of e))))
              (if (and b (import-text? b)) (hashtable-set! h p b) (set! skipped (cons p skipped))))))
        paths entries)
      (cons h skipped))))

;; ---- the range -------------------------------------------------------------
;;
;; -> ((<path> modified|added|removed) ...), sorted by path, every path in
;; the domain at the ends it exists at; refused as the letters and modes
;; demand.
(define (range-of repo from to from-tree to-tree)
  (let ((fields (filter (lambda (s) (> (string-length s) 0))
                        (split-on (git-text repo "diff" "--no-ext-diff" "--name-status" "-M" "-z" from to) #\nul)))
        (out '()))
    (define (mode-of tree p) (let ((e (hashtable-ref tree p #f))) (if e (car e) 'none)))
    (define (text! tree p)
      (let ((e (hashtable-ref tree p #f)))
        (read-blobs! repo (list (caddr e)))
        (unless (import-text? (blob-of e)) (refuse 'not-text (list 'path p)))))
    (define (removed! p)
      (unless (dot-named? p)
        (let ((m (mode-of from-tree p)))
          (unless (and (member m '("100644" "100755")) (regular? (hashtable-ref from-tree p #f)))
            (refuse 'file-mode (list 'path p) (list 'from m) '(to none)))
          (text! from-tree p)
          (set! out (cons (list p 'removed) out)))))
    (define (added! p)
      (unless (dot-named? p)
        (let ((m (mode-of to-tree p)))
          (unless (and (equal? m "100644") (regular? (hashtable-ref to-tree p #f)))
            (refuse 'file-mode (list 'path p) '(from none) (list 'to m)))
          ;; a path the store cannot hold (a backslash, a NUL) is refused
          ;; here, before anything is written, not by the export at the end
          (unless (code-safe-path? p) (refuse 'unsafe-path (list 'path p)))
          (text! to-tree p)
          (set! out (cons (list p 'added) out)))))
    (define (modified! p)
      (unless (dot-named? p)
        (let ((a (mode-of from-tree p)) (b (mode-of to-tree p)))
          (unless (and (equal? a b) (member a '("100644" "100755"))
                       (regular? (hashtable-ref from-tree p #f)) (regular? (hashtable-ref to-tree p #f)))
            (refuse 'file-mode (list 'path p) (list 'from a) (list 'to b)))
          (text! from-tree p)
          (text! to-tree p)
          (set! out (cons (list p 'modified) out)))))
    (let loop ((fs fields))
      (unless (null? fs)
        (let ((letter (car fs)))
          (cond ((and (string=? letter "M") (pair? (cdr fs))) (modified! (cadr fs)) (loop (cddr fs)))
                ((and (string=? letter "A") (pair? (cdr fs))) (added! (cadr fs)) (loop (cddr fs)))
                ((and (string=? letter "D") (pair? (cdr fs))) (removed! (cadr fs)) (loop (cddr fs)))
                ((and (prefix? letter "R") (pair? (cdr fs)) (pair? (cddr fs)))
                 (removed! (cadr fs)) (added! (caddr fs)) (loop (cdddr fs)))
                (else (refuse 'paths-unsupported (list 'letter letter)
                              (list 'path (if (pair? (cdr fs)) (cadr fs) ""))))))))
    (when (null? out) (refuse 'nothing-to-import (list 'from from) (list 'to to)))
    (list-sort (lambda (x y) (string<? (car x) (car y))) out)))

;; ---- the store -------------------------------------------------------------

;; THE RAW EXPORT OF A STATE, in memory: path -> its children's srcs run
;; together, as export-code --raw writes them.
(define (export-of state)
  (let ((h (make-hashtable string-hash string=?)))
    (for-each (lambda (f) (hashtable-set! h (car f) (apply bytes-append (map cadr (cadddr f)))))
              (projected-files state))
    h))

;; THE PATHS ON WHICH AN EXPORT AND A TREE'S DOMAIN DIFFER, among those KEEP?
;; accepts: a domain file the export lacks or holds other bytes for, and an
;; exported file the domain lacks that is not dot-named and not at or under
;; a skipped path.
(define (differing export domain keep?)
  (let ((files (car domain)) (skipped (cdr domain)) (out '()))
    (let-values (((paths bytes) (hashtable-entries files)))
      (vector-for-each (lambda (p b)
                         (when (keep? p)
                           (let ((x (hashtable-ref export p #f)))
                             (unless (and x (bytevector=? x b)) (set! out (cons p out))))))
                       paths bytes))
    (let-values (((paths bytes) (hashtable-entries export)))
      (vector-for-each (lambda (p b)
                         (when (and (keep? p) (not (dot-named? p)) (not (hashtable-contains? files p))
                                    (not (exists (lambda (s) (under? p s)) skipped)))
                           (set! out (cons p out))))
                       paths bytes))
    (sorted-unique out)))

(define (field-of state id name)
  (let* ((row (state-read state id)) (fs (and row (assq 'fields row))) (f (and fs (assq name (cdr fs)))))
    (and f (cdr f))))
;; -> ((<id> <from> <to> <status> <tag>) ...) for every live import block
(define (import-blocks state)
  (let ((a (session-answer (make-query-session state) '(field ?b "slot" "import"))))
    (unless (and (pair? a) (eq? (car a) 'ok)) (raise a))
    (filter (lambda (b) (eq? 'decision (field-of state (car b) 'kind)))
            (map (lambda (row)
                   (let ((id (car row)))
                     (list id (text-of (field-of state id 'from)) (text-of (field-of state id 'to))
                           (text-of (field-of state id 'status)) (text-of (field-of state id 'tag)))))
                 (cadr a)))))
(define (tag-bound? store state name) (exists (lambda (t) (equal? (cadr t) name)) (store-tags store state)))

;; THE PRECONDITION -> (values <state-name> <import block id or #f>)
(define (precondition store state export from to name range from-domain to-domain)
  (let* ((blocks (import-blocks state))
         (cut (reduce-applied-cut state))
         (in-range? (lambda (p) (assoc p range)))
         (mine? (lambda (b) (and (equal? (cadr b) from) (equal? (caddr b) to))))
         (other (find (lambda (b) (and (equal? (cadddr b) "pending") (not (mine? b)))) blocks))
         (pending (find (lambda (b) (and (equal? (cadddr b) "pending") (mine? b))) blocks))
         (done (find (lambda (b) (and (equal? (cadddr b) "done") (mine? b) (equal? (list-ref b 4) name))) blocks)))
    (cond
      (other (refuse 'pending-import (list 'from (cadr other)) (list 'to (caddr other)) (list 'block (car other))))
      (pending
       (let ((outside (differing export from-domain (lambda (p) (not (in-range? p))))))
         (when (pair? outside)
           (refuse 'base-mismatch (list 'store-cut cut) (list 'rev from) (cons 'differing outside)))
         (values 'pending (car pending))))
      ((and done (not (tag-bound? store state name)) (null? (differing export to-domain (lambda (p) #t))))
       (values 'done-without-tag (car done)))
      (else
       (let ((d (differing export from-domain (lambda (p) #t))))
         (when (or (pair? d) (exists mine? blocks))
           (refuse 'base-mismatch (list 'store-cut cut) (list 'rev from) (cons 'differing d)))
         (values 'fresh #f))))))

;; ---- the matcher -----------------------------------------------------------
;;
;; OLD: a vector of bytes, the current children's srcs in order; NEW: a
;; vector of bytes, the target pieces in order. -> a vector over NEW, each
;; (kept <old index>), (set <old index>) or (new), and the list of old
;; indices to delete.
(define (match-pieces old new)
  (let* ((n (vector-length old)) (m (vector-length new))
         (same? (lambda (i j) (bytevector=? (vector-ref old i) (vector-ref new j))))
         ;; L[i][j]: the longest common subsequence of old[i..] and new[j..]
         (L (let ((t (make-vector (+ n 1))))
              (do ((i 0 (+ i 1))) ((> i n)) (vector-set! t i (make-vector (+ m 1) 0)))
              (do ((i (- n 1) (- i 1))) ((< i 0))
                (do ((j (- m 1) (- j 1))) ((< j 0))
                  (vector-set! (vector-ref t i) j
                               (if (same? i j)
                                   (+ 1 (vector-ref (vector-ref t (+ i 1)) (+ j 1)))
                                   (max (vector-ref (vector-ref t (+ i 1)) j) (vector-ref (vector-ref t i) (+ j 1)))))))
              t))
         (owner (make-vector m #f))
         (taken (make-vector n #f)))
    ;; (i) the LCS, walked from the front: equal pieces are paired at once,
    ;; which keeps an optimal subsequence and pairs earliest with earliest;
    ;; on a tie the NEW piece is passed over, so an earlier old piece stays
    ;; in the subsequence ([a,b,a] -> [b,a,a] keeps both a's in order)
    (let walk ((i 0) (j 0))
      (when (and (< i n) (< j m))
        (cond ((same? i j) (vector-set! owner j i) (vector-set! taken i #t) (walk (+ i 1) (+ j 1)))
              ((> (vector-ref (vector-ref L (+ i 1)) j) (vector-ref (vector-ref L i) (+ j 1))) (walk (+ i 1) j))
              (else (walk i (+ j 1))))))
    ;; (ii) relocations: each remaining new piece takes the earliest
    ;; remaining old piece with its bytes
    (do ((j 0 (+ j 1))) ((= j m))
      (unless (vector-ref owner j)
        (let find ((i 0))
          (when (< i n)
            (if (and (not (vector-ref taken i)) (same? i j))
                (begin (vector-set! owner j i) (vector-set! taken i #t))
                (find (+ i 1)))))))
    ;; (iii) the gaps between anchors, in target order: gap g lies after
    ;; the g-th anchor (gap 0 before the first). A new piece's gap counts
    ;; the anchors before it in the target; an old piece's is the gap after
    ;; the last anchor before it in the current order.
    (let* ((anchors (filter (lambda (j) (vector-ref owner j)) (iota m)))
           (a (length anchors))
           (rank-of-old (make-vector n #f))
           (gap-of-new (make-vector m 0))
           (gap-of-old (make-vector n 0))
           (olds (make-vector (+ a 1) '()))
           (news (make-vector (+ a 1) '()))
           (result (make-vector m '(new)))
           (dels '()))
      (let loop ((as anchors) (k 1))
        (unless (null? as)
          (vector-set! rank-of-old (vector-ref owner (car as)) k)
          (vector-set! result (car as) (list 'kept (vector-ref owner (car as))))
          (loop (cdr as) (+ k 1))))
      (let loop ((j 0) (g 0))
        (when (< j m)
          (if (vector-ref owner j)
              (loop (+ j 1) (+ g 1))
              (begin (vector-set! gap-of-new j g) (loop (+ j 1) g)))))
      (let loop ((i 0) (g 0))
        (when (< i n)
          (if (vector-ref rank-of-old i)
              (loop (+ i 1) (vector-ref rank-of-old i))
              (begin (vector-set! gap-of-old i g) (loop (+ i 1) g)))))
      (do ((i (- n 1) (- i 1))) ((< i 0))
        (unless (vector-ref taken i)
          (let ((g (vector-ref gap-of-old i))) (vector-set! olds g (cons i (vector-ref olds g))))))
      (do ((j (- m 1) (- j 1))) ((< j 0))
        (unless (vector-ref owner j)
          (let ((g (vector-ref gap-of-new j))) (vector-set! news g (cons j (vector-ref news g))))))
      (do ((g 0 (+ g 1))) ((> g a))
        (let pair ((os (vector-ref olds g)) (ns (vector-ref news g)))
          (cond ((and (pair? os) (pair? ns))
                 (vector-set! result (car ns) (list 'set (car os)))
                 (pair (cdr os) (cdr ns)))
                ((pair? os) (set! dels (append dels os))))))
      (values result (list-sort < dels)))))

;; THE PIECES OF A FILE'S BYTES, as a fresh import splits them: the slices
;; between suggest-boundaries' cuts.
(define (pieces-of path bytes)
  (let ((cuts (car (suggest-boundaries (language-for-path path) bytes))))
    (list->vector (map (lambda (from to) (byte-slice bytes from to))
                       cuts (append (cdr cuts) (list (bytevector-length bytes)))))))

;; ---- the batch -------------------------------------------------------------
;;
;; The items are accumulated in order; each is (intent kind subject), the
;; kind and subject only for the answer. (from k) names item k's block.
(define items '())
(define item-count 0)
(define planning #f)
(define (emit! intent kind subject)
  (set! items (cons (list intent kind subject planning) items))
  (set! item-count (+ item-count 1))
  (- item-count 1))

(define (predecessor S x)
  (let loop ((s S) (prev 'none))
    (cond ((null? s) 'none) ((equal? (car s) x) prev) (else (loop (cdr s) (car s))))))
(define (place S x prev)
  (let ((S (remove x S)))
    (if (eq? prev 'none)
        (append S (list x))
        (let loop ((s S) (out '()))
          (cond ((null? s) (reverse (cons x out)))
                ((equal? (car s) prev) (append (reverse (cons x (cons (car s) out))) (cdr s)))
                (else (loop (cdr s) (cons (car s) out))))))))

;; One range path: its items emitted. -> the number of kept blocks.
(define (plan-path! state path target)
  (set! planning path)
  (let* ((holders (filter (lambda (id) (and (eq? 'text (code-field state id 'mode)) (equal? (code-field state id 'path) path)))
                          (code-files state)))
         ;; projected-file refuses a path two live files hold (duplicate-path)
         (file (and (pair? holders) (projected-file state (car holders))))
         (holder (and file (cadr file)))
         (children (if file (cadddr file) '()))
         (entry (language-for-path path)))
    (cond
      ((not target)
       (for-each (lambda (c) (emit! (list 'del (car c)) 'deleted (car c))) children)
       (when holder (emit! (list 'del holder) 'deleted holder))
       0)
      (else
       (let ((parent (or holder
                         (list 'from (emit! (list 'insert 'root #f (append (list '(kind . file) '(mode . text) (cons 'path path))
                                                                          (lang-fields entry)))
                                            'inserted #f))))
             (old (list->vector (map cadr children)))
             (ids (list->vector (map car children)))
             (new (pieces-of path target)))
         (let-values (((result dels) (match-pieces old new)))
           (for-each (lambda (i) (emit! (list 'del (vector-ref ids i)) 'deleted (vector-ref ids i))) dels)
           (let loop ((j 0) (prev 'none) (S (filter (lambda (id) (not (member id (map (lambda (i) (vector-ref ids i)) dels))))
                                                    (vector->list ids)))
                      (kept 0))
             (if (= j (vector-length new))
                 kept
                 (let ((r (vector-ref result j)) (where (lambda (prev) (if (eq? prev 'none) #f prev))))
                   (if (eq? (car r) 'new)
                       (let ((ref (list 'from (emit! (list 'insert parent (where prev)
                                                           (append (list '(kind . code) '(mode . text) (cons 'src (vector-ref new j)))
                                                                   (lang-fields entry)))
                                                     'inserted #f))))
                         (loop (+ j 1) ref (place S ref prev) kept))
                       (let ((id (vector-ref ids (cadr r))))
                         (when (eq? (car r) 'set)
                           (emit! (list 'set id 'src (vector-ref new j)) 'set id))
                         (if (equal? (predecessor S id) prev)
                             (loop (+ j 1) id S (if (eq? (car r) 'kept) (+ kept 1) kept))
                             (begin
                               (emit! (list 'move id parent (where prev)) 'moved id)
                               (loop (+ j 1) id (place S id prev) (if (eq? (car r) 'kept) (+ kept 1) kept)))))))))))))))

;; THE PREMISES: every live block of every range path at the read cut, and
;; the import block when there is one.
(define (premises-of state range block block-version)
  (let ((ids (apply append
                    (map (lambda (r)
                           (let ((holders (filter (lambda (id) (and (eq? 'text (code-field state id 'mode))
                                                                    (equal? (code-field state id 'path) (car r))))
                                                  (code-files state))))
                             (apply append (map (lambda (h) (cons h (code-children state h))) holders))))
                         range))))
    (append (map (lambda (id) (list 'premise id (block-hash state id))) ids)
            (if block (list (list 'premise block block-version)) '()))))

;; ---- running it --------------------------------------------------------------

(define (ask store . req) (rpc-dispatch store req actor))
(define (batch-text intents) (format "~s" intents))
;; the id of the block an item of a batch answer made
(define (made-id answer)
  (let ((ev (and (pair? answer) (eq? (car answer) 'ok) (assq 'events (cdr answer)))))
    (and ev (pair? (cadr ev)) (block-id (car (car (cadr ev))) (cdr (car (cadr ev)))))))
;; -> the batch answer's item answers, or the refusal
(define (send-batch store intents premises)
  (let ((a (apply ask store 'batch (batch-text intents)
                  (if (pair? premises) (list "--premises" (format "~s" premises)) '()))))
    (cond ((and (pair? a) (eq? (car a) 'batch) (pair? (cdr a)) (list? (cadr a))) a)
          ((and (pair? a) (eq? (car a) 'error)) (raise a))
          (else (refuse 'partial (list 'store-answer a))))))
(define (all-ok? a) (for-all (lambda (x) (and (pair? x) (eq? (car x) 'ok))) (cadr a)))
;; the version a write's answer reports for BLOCK in its (state ((<id> . <v>) ...))
(define (reported-version answer block)
  (let ((st (and (pair? answer) (assq 'state (cdr answer)))))
    (and st (pair? (cdr st)) (list? (cadr st))
         (let ((x (assoc block (cadr st)))) (and x (cdr x))))))
;; A STORE REFUSAL MID-BATCH: the earlier items are on disk. The refusal
;; names the item that was refused, its kind and the path it was planned
;; for, and the store's answer to it (too-deep, for one).
(define (partial-refusal a sent)
  (let loop ((as (cadr a)) (k 0))
    (cond ((null? as) (refuse 'partial (list 'store-answer a)))
          ((and (pair? (car as)) (eq? (car (car as)) 'ok)) (loop (cdr as) (+ k 1)))
          (else (let ((it (list-ref sent k)))
                  (refuse 'partial (list 'item k) (list 'kind (cadr it)) (list 'path (cadddr it))
                          (list 'store-answer (car as)) (list 'done k)))))))

(define (stop-setting)
  (let ((v (getenv "THEOURGIA_IMPORT_STOP")))
    (and v (> (string-length v) 0)
         (let ((k (string->number v)))
           (unless (and k (exact? k) (integer? k) (>= k 0))
             (assertion-violation 'import-git "THEOURGIA_IMPORT_STOP must be an exact non-negative integer" v))
           k))))

(define (run-import store repo from-rev to-rev name dry-run?)
  (let* ((stop (stop-setting))
         (from (commit-id repo from-rev))
         (to (commit-id repo to-rev))
         (from-tree (tree-of repo from))
         (to-tree (tree-of repo to))
         (range (range-of repo from to from-tree to-tree))
         (from-domain (domain-of repo from-tree))
         (to-domain (domain-of repo to-tree))
         (state (open-and-reduce store))
         (cut (reduce-applied-cut state))
         (export (export-of state))
         (count (lambda (kind) (length (filter (lambda (r) (eq? (cadr r) kind)) range))))
         (files (list 'files (list 'modified (count 'modified)) (list 'added (count 'added)) (list 'removed (count 'removed)))))
    (let-values (((how block) (precondition store state export from to name range from-domain to-domain)))
      (when (and (not (eq? how 'done-without-tag)) (tag-bound? store state name))
        (refuse 'tag-exists (list 'tag name)))
      ;; IN ORDER: the paths are planned one after another, in path order,
      ;; and the items read only after every path has emitted its own
      (let* ((kept (if (eq? how 'done-without-tag)
                       0
                       (fold-left (lambda (acc r) (+ acc (plan-path! state (car r) (hashtable-ref (car to-domain) (car r) #f))))
                                  0 range)))
             (content (reverse items)))
        (define (of kind) (filter (lambda (it) (eq? (cadr it) kind)) content))
        (if dry-run?
            (list 'ok '(dry-run #t) (list 'state how) (list 'read-cut cut) files
                  (list 'blocks (list 'kept kept) (list 'set (length (of 'set))) (list 'inserted (length (of 'inserted)))
                        (list 'deleted (length (of 'deleted))) (list 'moved (length (of 'moved))))
                  (list 'items (+ (length content) (if (eq? how 'done-without-tag) 1 2))))
            (let* ((first (and (not block)
                              ;; THE FIRST REQUEST, fresh only: the import block, pending
                              (let ((a (send-batch store (list (list 'insert 'root #f
                                                                     (list '(kind . decision) '(slot . "import")
                                                                           (cons 'from from) (cons 'to to) (cons 'tag name)
                                                                           '(status . "pending")
                                                                           (cons 'src (git-text repo "log" "--format=%s" "--reverse"
                                                                                                (string-append from ".." to))))))
                                                   '())))
                                (unless (all-ok? a) (refuse 'partial (list 'store-answer a)))
                                (car (cadr a)))))
                   (block (or block (made-id first)))
                   ;; THE IMPORT BLOCK'S VERSION: on a fresh run the one its
                   ;; insertion answered (the state report), so an edit made
                   ;; after the insertion is not adopted into the premise; read
                   ;; afterwards only when the store could not report it. On a
                   ;; rerun, at the read cut.
                   (version (or (and first (reported-version first block))
                                (block-hash (if first (open-and-reduce store) state) block)))
                   ;; the tag-only run is premised too: a change under it
                   ;; refuses it before the tag, rather than diverging after
                   (premises (premises-of state range block version))
                   (tail (append (if (eq? how 'done-without-tag) '() (list (list (list 'set block 'status "done") 'done block #f)))
                                 (list (list (list 'tag name) 'tag #f #f))))
                   (all (append content tail))
                   (sent (if stop (list-head all (min stop (length all))) all)))
              (hold-point! 'import-before-batch)
              (let ((a (if (null? sent) '(batch () (done 0)) (send-batch store (map car sent) premises))))
                (when stop
                  (write (list 'stopped (list 'items (length sent)) (list 'answer a)))
                  (newline)
                  (exit 3))
                (unless (all-ok? a) (partial-refusal a sent))
                (let* ((tag-cut (let ((t (find (lambda (t) (equal? (cadr t) name)) (store-tags store))))
                                  (and t (eq? (car t) 'settled) (caddr t))))
                       (ids (lambda (kind) (map (lambda (it ans) (or (caddr it) (made-id ans)))
                                                (filter (lambda (it) (eq? (cadr it) kind)) all)
                                                (let loop ((is all) (as (cadr a)) (out '()))
                                                  (cond ((null? is) (reverse out))
                                                        ((eq? (cadr (car is)) kind) (loop (cdr is) (cdr as) (cons (car as) out)))
                                                        (else (loop (cdr is) (cdr as) out)))))))
                       (tagged (open-and-reduce store tag-cut))
                       (d (differing (export-of tagged) to-domain (lambda (p) #t))))
                  (when (pair? d)
                    (refuse 'import-diverged (cons 'differing d) (list 'tag name (list 'cut tag-cut))))
                  (list 'ok (list 'tag name (list 'cut tag-cut)) (list 'rerun (not (eq? how 'fresh))) files
                        (list 'blocks (list 'kept kept) (list 'set (ids 'set)) (list 'inserted (ids 'inserted))
                              (list 'deleted (ids 'deleted)) (list 'moved (ids 'moved))))))))))))

;; ---- the command line ------------------------------------------------------------------

(define (main args)
  (let* ((dry-run? (and (member "--dry-run" args) #t))
         (args (remove "--dry-run" args))
         (tag (let ((m (member "--tag" args))) (and m (pair? (cdr m)) (cadr m)))))
    (if (not (and (= 6 (length args)) tag (equal? (list-ref args 4) "--tag")
                  (not (exists (lambda (a) (prefix? a "--")) (list-head args 4)))))
        (begin (write '(usage (import-git <store> <repo> <from-rev> <to-rev> "--tag" <name> ["--dry-run"])))
               (newline) (exit 2))
        (let ((a (guard (e ((and (pair? e) (eq? (car e) 'error)) e))
                   (run-import (absolute (car args)) (absolute (cadr args)) (caddr args) (cadddr args) tag dry-run?))))
          (write a)
          (newline)
          (exit (if (eq? (car a) 'ok) 0 1))))))

(main (cdr (command-line)))
