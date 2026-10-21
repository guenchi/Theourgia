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
;; (theourgia landing-files) -- the work behind the landing verbs: a file's
;; byte or line range, or a diff hunk, to the blocks holding it (locate), the
;; files that differ between two cuts (changed-files), and the raw projection
;; and the spelling and volume rules tools/publish-landing.sc publishes with.
;;
;; ENTERED ON FIRST USE, NOT AT START-UP. The verbs stay built-in in rpc.sc,
;; which parses their arguments and then evaluates the entry procedure here,
;; as it enters (theourgia derived); so a start that runs neither verb does
;; not load this library. It never imports rpc, the daemon or the client.
(library (theourgia landing-files)
  (export locate-file locate-pieces changed-files-between raw-projection path-collision spelling-key
          ambiguous-groups volume-insensitivity landing-collision base-entry-judgement
          store-route-domain file-pieces file-new-intents file-delete-intents file-baseline
          structural-outputs)
  (import (rnrs)
          (only (theourgia code-project) code-field code-files projection-outputs projected-file export-code-view
                lang-fields code-safe-path?)
          (only (theourgia code-suggest) suggest-boundaries)
          (only (theourgia languages) language-for-path)
          (rename (only (theourgia code-project) code-project-answer) (code-project-answer answer))
          (only (theourgia reduce) reduce-applied-cut state-path-claimants file-baseline)
          (only (theourgia ffi) directory-entries entry-bytes entry-name-type machine-home mkdir-exclusive!
                mkdir-p! process-id rmdir! unlink!)
          (only (theourgia markers) bytes-append import-text? byte-slice)
          (theourgia degeneracy))

  ;; ---- the raw projection: each file's bytes, its children's srcs run together --
  (define (raw-projection state) (projection-outputs #f state #t))

  ;; ---- what a base is compared over -------------------------------------------
  ;;
  ;; ONE JUDGEMENT OF A BASE TREE'S ENTRY, asked by the publisher
  ;; (tools/publish-landing.sc) and the importer (tools/import-git.sc), so the
  ;; two compare a store's export over the same paths. -> in, or why the entry
  ;; is outside: dot-named (a path component starts with "."), link (mode
  ;; 120000), submodule (160000), mode (any mode but 100644 and 100755), or
  ;; not-text (a regular file whose bytes the importer does not import,
  ;; import-text?). BYTES is read only for a regular entry that is not
  ;; dot-named; a caller passes #f otherwise.
  ;; NOTE: WHAT EACH VERDICT MEANS FOR A COMPARISON IS ONE RULE IN BOTH CALLERS:
  ;; in is compared; link, submodule and mode are skipped, and the export at
  ;; or under them is not a difference (the importer follows a link); not-text
  ;; is neither compared nor exempt, since the store never holds it.
  (define (base-entry-judgement path mode bytes)
    (cond ((exists (lambda (c) (and (> (string-length c) 0) (char=? (string-ref c 0) #\.)))
                   (path-components path))
           'dot-named)
          ((equal? mode "120000") 'link)
          ((equal? mode "160000") 'submodule)
          ((not (member mode '("100644" "100755"))) 'mode)
          ((not (and (bytevector? bytes) (import-text? bytes))) 'not-text)
          (else 'in)))
  ;; THE STORE ROUTE'S DOMAIN: what a structural draft may create. The
  ;; importer's domain (base-entry-judgement) less what the store route cannot
  ;; carry: a mode other than 100644 (publication writes 100644 only; an
  ;; executable keeps the git route), a path the export would not write
  ;; (code-safe-path?), and a path another of OTHERS, the tree's paths, would
  ;; meet on a volume that folds case or normalisation. -> in, or the reason.
  (define (store-route-domain path mode bytes others)
    (let ((judged (base-entry-judgement path mode bytes)))
      (cond ((not (eq? judged 'in)) judged)
            ((not (equal? mode "100644")) 'mode)
            ((not (code-safe-path? path)) 'unsafe-path)
            ((let ((mine (cons path (path-ancestors path))))
               (exists (lambda (g) (exists (lambda (p) (member p g)) mine))
                       (ambiguous-groups 'both (cons path (remove path others)))))
             'case-collision)
            (else 'in))))

  ;; ---- a file as blocks: the one construction --------------------------------
  ;;
  ;; A FILE BECOMES BLOCKS ONE WAY, whoever creates it: the importer
  ;; (tools/import-git.sc) for a path the store does not hold, and a commit of
  ;; a structural draft (working.sc). The bytes are cut at suggest-boundaries'
  ;; cuts for the path's language; a root-level holder carries kind, mode,
  ;; path and language; the pieces are its code children in order. Deleting a
  ;; file deletes its children, then its holder.
  ;; -> the pieces of BYTES, a vector of byte slices
  (define (file-pieces path bytes)
    (let ((cuts (car (suggest-boundaries (language-for-path path) bytes))))
      (list->vector (map (lambda (from to) (byte-slice bytes from to))
                         cuts (append (cdr cuts) (list (bytevector-length bytes)))))))
  ;; -> the intents creating the file, the holder's at index FIRST of the
  ;; batch they join, each child after the one before it
  (define (file-new-intents path bytes first)
    (let ((entry (language-for-path path)))
      (let loop ((pieces (vector->list (file-pieces path bytes))) (k (+ first 1)) (prev #f)
                 (out (list (list 'insert 'root #f
                                  (append (list '(kind . file) '(mode . text) (cons 'path path))
                                          (lang-fields entry))))))
        (if (null? pieces)
            (reverse out)
            (loop (cdr pieces) (+ k 1) (list 'from k)
                  (cons (list 'insert (list 'from first) prev
                              (append (list '(kind . code) '(mode . text) (cons 'src (car pieces)))
                                      (lang-fields entry)))
                        out))))))
  ;; -> the intents deleting the file: its children, then the holder
  (define (file-delete-intents holder children)
    (append (map (lambda (c) (list 'del c)) children) (list (list 'del holder))))
  ;; THE WORKING TREE'S FILES WITH THE STRUCTURAL DRAFTS: OUTPUTS, ((<path>
  ;; <bytes>) ...), less the paths DELETED names, with NEWS, ((<path> <bytes>)
  ;; ...), added. A new file has no blocks yet, so it is its bytes in a marked
  ;; export too.
  (define (structural-outputs outputs news deleted)
    (list-sort (lambda (x y) (string<? (car x) (car y)))
               (append (filter (lambda (o) (not (member (car o) deleted))) outputs) news)))

  ;; -> the components of a relative path, split at "/"
  (define (path-components path)
    (let loop ((i 0) (from 0) (out '()))
      (cond ((= i (string-length path)) (reverse (cons (substring path from i) out)))
            ((char=? (string-ref path i) #\/) (loop (+ i 1) (+ i 1) (cons (substring path from i) out)))
            (else (loop (+ i 1) from out)))))

  ;; ---- a file's byte or line range, or a diff hunk, to the blocks holding it --
  ;;
  ;; IN RAW COORDINATES: the file's bytes are its children's srcs run
  ;; together, what export-code --raw writes, so a range read off a raw
  ;; export or a git diff of one means the same bytes here. No offset is
  ;; stored; the pieces are built from the export's own entries, in its
  ;; order, each (source <id> <start> <end> 0 ()) -- projection-range's
  ;; shape, with no controls and no inserted bytes.
  ;;
  ;; SPEC: (bytes s e), the half-open [s, e); (lines a b), 1-based and
  ;; inclusive, from the start of line a to the end of line b with its LF;
  ;; or (hunk <text>), one unified-diff hunk whose OLD side is used. A file
  ;; ending in LF has no empty last line: "a\n" is one line, "" none.
  ;; -> (ok (items (block <id> <start> <end>) ...)); refusals: unknown-path,
  ;; not-a-text-file, the exporter's own for a file it would not write,
  ;; range-outside (bounds first, before any block rule), not-a-hunk,
  ;; one-hunk-only.
  (define (locate-file state path spec)
    (answer
      (lambda ()
        (let* ((holders (state-path-claimants state 'file 'text path))
               (id (cond ((pair? holders) (car holders))
                         ((exists (lambda (f) (equal? (code-field state f 'path) path)) (code-files state))
                          (raise (list 'error 'not-a-text-file path)))
                         (else (raise (list 'error 'unknown-path path)))))
               (entries (cadddr (projected-file state id)))
               (bytes (apply bytes-append (map cadr entries)))
               (pieces (let loop ((es entries) (at 0) (out '()))
                         (if (null? es) (reverse out)
                             (let ((end (+ at (bytevector-length (cadr (car es))))))
                               (loop (cdr es) end (cons (list 'source (car (car es)) at end 0 '()) out))))))
               (range (spec-range bytes spec)))
          (list 'ok (cons 'items (locate-pieces pieces (bytevector-length bytes) (car range) (cdr range))))))))

  ;; THE BLOCKS A RANGE TOUCHES, EACH WHOLE: -> ((block <id> <start> <end>)
  ;; ...) in document order, with each block's own span, for 0 <= s <= e <= len.
  ;;
  ;; A NONEMPTY [s, e) answers every piece it intersects, so an edit crossing
  ;; a boundary is the list of blocks it touches and is never refused; a
  ;; zero-length piece strictly inside the range is between the bytes the
  ;; edit replaces and is answered too, one at s or e is not.
  ;;
  ;; NEVER: AN EMPTY [o, o) IS NOT projection-range's RULE, AND THE
  ;; DIFFERENCE IS THE POINT. projection-range gives a cursor to the block
  ;; that ENDS at o; an insertion before a block belongs to the block that
  ;; STARTS there, so locate answers every piece starting at o (several
  ;; zero-length ones and the block after them, all of them, in order: the
  ;; caller chooses). Strictly inside a block it answers that block. The one
  ;; exception is the file's end, where nothing starts: an append belongs to
  ;; the last block, which ends there, so every piece ending at len is
  ;; answered (the last block and the zero-length ones after it).
  (define (locate-pieces pieces len s e)
    (define (start-of p) (caddr p))
    (define (end-of p) (cadddr p))
    (define (item p) (list 'block (cadr p) (start-of p) (end-of p)))
    (map item
         (cond
           ((< s e)
            (filter (lambda (p) (if (= (start-of p) (end-of p))
                                    (and (< s (start-of p)) (< (start-of p) e))
                                    (and (< (start-of p) e) (> (end-of p) s))))
                    pieces))
           ((= s len) (filter (lambda (p) (= (end-of p) s)) pieces))
           (else
            (let ((starting (filter (lambda (p) (= (start-of p) s)) pieces)))
              (if (pair? starting)
                  starting
                  (filter (lambda (p) (and (< (start-of p) s) (< s (end-of p)))) pieces)))))))

  ;; SPEC TO A BYTE INTERVAL (s . e) of BYTES, the bounds checked first.
  (define (spec-range bytes spec)
    (let* ((len (bytevector-length bytes))
           (starts (line-starts bytes))
           (count (length starts))
           (outside (lambda () (raise (list 'error 'range-outside (list 'bytes len) (list 'lines count)))))
           (line-start (lambda (k) (list-ref starts (- k 1))))
           (line-end (lambda (k) (if (< k count) (list-ref starts k) len)))
           (lines (lambda (a b)
                    (unless (and (<= 1 a) (<= a b) (<= b count)) (outside))
                    (cons (line-start a) (line-end b)))))
      (case (car spec)
        ((bytes)
         (let ((s (cadr spec)) (e (caddr spec)))
           (unless (and (<= 0 s) (<= s e) (<= e len)) (outside))
           (cons s e)))
        ((lines) (lines (cadr spec) (caddr spec)))
        ((hunk)
         (let* ((old (hunk-old-side (cadr spec))) (n (car old)) (m (cdr old)))
           (if (= m 0)
               ;; an insertion after old line n, n 0 before the first
               (begin (unless (<= 0 n count) (outside))
                      (let ((o (if (= n 0) 0 (line-end n)))) (cons o o)))
               (lines n (+ n m -1)))))
        (else (assertion-violation 'locate-file "unknown range spec" spec)))))

  ;; the offset each line starts at: one per line, none for an empty file,
  ;; and no empty line after a final LF
  (define (line-starts bytes)
    (let ((len (bytevector-length bytes)))
      (let loop ((i 0) (out (if (> len 0) '(0) '())))
        (cond ((= i len) (reverse out))
              ((and (= 10 (bytevector-u8-ref bytes i)) (< (+ i 1) len)) (loop (+ i 1) (cons (+ i 1) out)))
              (else (loop (+ i 1) out))))))

  ;; ONE HUNK'S OLD SIDE, (n . m), from text whose first line is the header
  ;; "@@ -N[,M] +P[,Q] @@" (anything after the closing @@ is a section
  ;; heading); "-N" with no count is "-N,1". The body is not read, except
  ;; that a second header line refuses: one hunk names one range.
  (define (hunk-old-side text)
    (define (refuse what) (raise (list 'error what)))
    (define (digits s i)
      (let loop ((j i)) (if (and (< j (string-length s)) (char<=? #\0 (string-ref s j) #\9)) (loop (+ j 1)) j)))
    (define (number s i j)
      (unless (and (< i j) (<= (- j i) 18)) (refuse 'not-a-hunk))
      (string->number (substring s i j) 10))
    (let* ((lines (split-lines text))
           (head (if (pair? lines) (car lines) ""))
           (h (if (and (> (string-length head) 0) (char=? #\return (string-ref head (- (string-length head) 1))))
                  (substring head 0 (- (string-length head) 1)) head)))
      (unless (and (>= (string-length h) 4) (string=? "@@ -" (substring h 0 4))) (refuse 'not-a-hunk))
      (when (exists (lambda (l) (and (>= (string-length l) 2) (string=? "@@" (substring l 0 2)))) (cdr lines))
        (refuse 'one-hunk-only))
      (let* ((n-end (digits h 4)) (n (number h 4 n-end))
             (m-end (if (and (< n-end (string-length h)) (char=? #\, (string-ref h n-end))) (digits h (+ n-end 1)) n-end))
             (m (if (= m-end n-end) 1 (number h (+ n-end 1) m-end))))
        (unless (and (< (+ m-end 1) (string-length h)) (string=? " +" (substring h m-end (+ m-end 2))))
          (refuse 'not-a-hunk))
        (let* ((p-end (digits h (+ m-end 2)))
               (_ (number h (+ m-end 2) p-end))
               (q-end (if (and (< p-end (string-length h)) (char=? #\, (string-ref h p-end))) (digits h (+ p-end 1)) p-end)))
          (when (> q-end p-end) (number h (+ p-end 1) q-end))
          (unless (and (<= (+ q-end 3) (string-length h)) (string=? " @@" (substring h q-end (+ q-end 3))))
            (refuse 'not-a-hunk))
          (cons n m)))))
  (define (split-lines text)
    (let loop ((i 0) (start 0) (out '()))
      (cond ((= i (string-length text)) (reverse (if (< start i) (cons (substring text start i) out) out)))
            ((char=? #\newline (string-ref text i)) (loop (+ i 1) (+ i 1) (cons (substring text start i) out)))
            (else (loop (+ i 1) start out)))))

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

  ;; WHETHER A PUBLICATION WOULD MERGE TWO SPELLINGS ON ITS VOLUME, a pure
  ;; function of the volume's probe result and the names, so it is asked of
  ;; either result on any machine. CASE? and NORMALISATION? are what the probe
  ;; found; TARGETS are the paths the landing writes (added and content);
  ;; EXISTING are the worktree's entries, files and directories, that the
  ;; landing leaves in place (removed paths are not among them, so a case
  ;; rename lands: removals are applied first). -> #f, or (error
  ;; case-collision|normalisation-collision (paths <p> <q>)), the first pair in
  ;; sorted order.
  ;;
  ;; THE SET IS THE LANDING'S OWN: every target and every directory above one,
  ;; compared with each other and with every existing entry. Two existing
  ;; entries are never compared: an ambiguous pair the landing does not touch
  ;; does not block it. The key is the volume's (spelling-key): case alone,
  ;; NFC alone, or both, where the refusal is case-collision.
  (define (landing-collision case? normalisation? targets existing)
    (let ((kind (cond ((and case? normalisation?) 'both) (case? 'case) (normalisation? 'normalisation) (else #f))))
      (and kind
           (let ((by-key (make-hashtable string-hash string=?))
                 (pairs '()))
             (define (pair! p q) (set! pairs (cons (if (string<? p q) (list p q) (list q p)) pairs)))
             (for-each
               (lambda (s)
                 (let* ((k (spelling-key kind s)) (others (hashtable-ref by-key k '())))
                   (unless (member s others)
                     (for-each (lambda (o) (pair! o s)) others)
                     (hashtable-set! by-key k (cons s others)))))
               (apply append (map (lambda (p) (cons p (path-ancestors p))) targets)))
             (for-each
               (lambda (e)
                 (for-each (lambda (s) (unless (string=? s e) (pair! s e)))
                           (hashtable-ref by-key (spelling-key kind e) '())))
               existing)
             (and (pair? pairs)
                  (list 'error (if (eq? kind 'normalisation) 'normalisation-collision 'case-collision)
                        (cons 'paths (car (list-sort (lambda (x y) (or (string<? (car x) (car y))
                                                                      (and (string=? (car x) (car y))
                                                                           (string<? (cadr x) (cadr y)))))
                                                     pairs)))))))))

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
