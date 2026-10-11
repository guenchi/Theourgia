#!r6rs
;; Copyright 2018 - 2026 guenchi
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

;; THE WHOLE TREE ROUND-TRIPS THROUGH A STORE.
;;
;; The tree this fixture sits in is copied to a scratch directory, every
;; Scheme file (extension ss, sc, scm, sls) is replaced there by the review
;; copy split-suggest writes for it, and the copy is imported in text mode.
;; Then: the raw export is the tree byte for byte (RT-1); the marked export,
;; decoded by the library's own projection-decode, carries the same bytes
;; (RT-1b); both exports hold exactly the compared files (RT-2); check finds
;; nothing (RT-3); the marked export imported again lands on the same blocks
;; (RT-4), and one byte changed in it moves exactly one block (RT-5); each
;; Scheme file has as many children as split-suggest gave boundaries (RT-6);
;; and the files fall into three classes, three of them pinned (RT-7).
;;
;; THE COMPARED SET is the tree's files minus every path with a component
;; that starts with a dot -- the import's walk does not enter such a name, so
;; an export cannot write it; the row names those paths. Under the runner the
;; tree is a git archive, so its files are ls-tree's; where the tree has a
;; .git (a working copy), git ls-files says which files are the tree's, so an
;; untracked file is not compared.
;;
;; NOTHING IS STRIPPED. The marked export is a reversible projection
;; (escaped marker-like lines, framing line feeds, a byte-order mark's own
;; line), so the byte comparison is made on export-code --raw, and the marked
;; form is proven by decoding it and by importing it back.
;;
;; RT-5 imports a directory holding only the changed file: the import of the
;; whole export again is RT-4's, and a second one would double the fixture's
;; longest step. The row asserts the live blocks are the same set afterwards,
;; so a one-file import that deleted anything would show.
;;
;; RT-7'S EXEMPLARS ARE THE TREE AS IT IS: eval-worker.sc splits (a program
;; with top-level defines and no uncertain token), admission.sc is one block
;; with no warning (a library: its defines are inside the library form), and
;; code-project.sc falls back, lexically-uncertain (a library holding a
;; character literal). When the Scheme profile learns library bodies and
;; character literals, the exemplars move and this row is pinned again.
;;
;; AN OPT-IN, FOR ITS COST: the tree, split, imports in about eight minutes
;; here, and RT-4 imports it a second time -- about nineteen minutes in all,
;; past the runner's per-fixture limit. Without THEOURGIA_ROUND_TRIP=1 the
;; fixture says so and runs nothing (rows: 0); a job with its own limit sets
;; it. The split tree's block count is printed after the import, the reading
;; the import's speed is judged by.

(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia reduce) block-hash)
        (only (theourgia store) open-and-reduce)
        (only (theourgia code-project) code-files code-children code-field)
        (only (theourgia languages) language-table language-for-path)
        (only (theourgia code-markers) projection-decode projection-header-wrapper?))

(register-verbs! extension-verbs)

(define bad 0)
(define rows 0)
(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))
(define-syntax tolerant
  (syntax-rules ()
    ((_ e) (guard (x (#t (list 'RAISED (if (and (condition? x) (message-condition? x)) (condition-message x) x)
                               (if (and (condition? x) (irritants-condition? x)) (condition-irritants x) '()))))
             e))))
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (want-1 name (tolerant got) expected))))

;; ---- paths and bytes ------------------------------------------------------------------------------

(define tree (let ((here (current-directory)))
               (let loop ((i (- (string-length here) 1)))
                 (cond ((< i 0) "..")
                       ((char=? (string-ref here i) #\/) (substring here 0 i))
                       (else (loop (- i 1)))))))
(unless (equal? (getenv "THEOURGIA_ROUND_TRIP") "1")
  (printf "SKIP: THEOURGIA_ROUND_TRIP is not 1: this fixture imports the whole tree, split, twice -- about nineteen minutes, past the runner's per-fixture limit; set THEOURGIA_ROUND_TRIP=1 to run it (an opt-in, not a failure)~%")
  (printf "\n0 failures\nrows: 0\ntree-round-trip complete\n")
  (exit 0))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/tree-round-trip-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define scratch (string-append root "/tree"))
(define reviews (string-append root "/reviews"))
(define raw-dir (string-append root "/export-raw"))
(define marked-dir (string-append root "/export-marked"))
(define one-dir (string-append root "/export-one"))
(define store (string-append root "/store"))
(define (run . args) (rpc-dispatch store args "tester"))

;; NOTHING AT THE TOP LEVEL RAISES past the rows: a file that cannot be read
;; is #f, the listing and the steps below are guarded, and a failure reads as
;; a red row while the cleanup at the end still runs.
(define (bytes-of path)
  (guard (e (#t #f))
   (and (file-exists? path)
       (call-with-port (open-file-input-port path)
         (lambda (i) (let ((b (get-bytevector-all i))) (if (eof-object? b) (make-bytevector 0) b)))))))
(define (parent-of path)
  (let loop ((i (- (string-length path) 1)))
    (cond ((< i 0) ".") ((char=? (string-ref path i) #\/) (substring path 0 i)) (else (loop (- i 1))))))
(define (write-bytes! path b)
  (system (string-append "mkdir -p '" (parent-of path) "'"))
  (when (file-exists? path) (delete-file path))
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (o) (put-bytevector o b))))
(define (split-path p)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i (string-length p)) (reverse (cons (substring p start i) out)))
          ((char=? (string-ref p i) #\/) (loop (+ i 1) (+ i 1) (cons (substring p start i) out)))
          (else (loop (+ i 1) start out)))))
(define (dot-named? p) (exists (lambda (c) (and (> (string-length c) 0) (char=? (string-ref c 0) #\.))) (split-path p)))
(define (extension p)
  (let loop ((i (- (string-length p) 1)))
    (cond ((< i 0) "") ((char=? (string-ref p i) #\/) "") ((char=? (string-ref p i) #\.) (substring p (+ i 1) (string-length p)))
          (else (loop (- i 1))))))
(define (scheme-file? p) (member (extension p) '("ss" "sc" "scm" "sls")))

;; Every regular file under DIR, relative, not entering a dot-named directory
;; when SKIP-DOTS? (an export's own walk), sorted.
(define (files-under dir skip-dots?)
  (define (walk rel out)
    (let ((d (if (string=? rel "") dir (string-append dir "/" rel))))
      (fold-left (lambda (out name)
                   (let* ((r (if (string=? rel "") name (string-append rel "/" name)))
                          (p (string-append dir "/" r)))
                     (cond ((member name '("." "..")) out)
                           ((and skip-dots? (char=? (string-ref name 0) #\.)) out)
                           ((file-symbolic-link? p) out)
                           ((file-directory? p) (walk r out))
                           ((file-regular? p) (cons r out))
                           (else out))))
                 out (directory-list d))))
  (if (file-directory? dir) (list-sort string<? (walk "" '())) '()))

(define (lines-of text)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i (string-length text)) (reverse (if (= start i) out (cons (substring text start i) out))))
          ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring text start i) out)))
          (else (loop (+ i 1) start out)))))

;; THE TREE'S FILES: git ls-files where the tree is a working copy, else the
;; files on disk (a git archive under the runner).
(define all-files
  (guard (e (#t '()))
   (if (file-exists? (string-append tree "/.git"))
      (let ((out (string-append root "/ls-files")))
        (system (string-append "git -C '" tree "' ls-files > '" out "' 2>/dev/null"))
        (list-sort string<? (lines-of (call-with-input-file out get-string-all))))
      (files-under tree #f))))
(define dot-paths (filter dot-named? all-files))
(define compared (filter (lambda (p) (not (dot-named? p))) all-files))
(define originals (map (lambda (p) (cons p (bytes-of (string-append tree "/" p)))) compared))
(define (original p) (cdr (assoc p originals)))

;; ---- the scratch copy, split, imported ------------------------------------------------------------

;; Under the guard, so a failure here reads in the rows and the cleanup at
;; the end still runs.
(tolerant (for-each (lambda (e) (write-bytes! (string-append scratch "/" (car e)) (cdr e))) originals))
(tolerant (run 'init))
(define scheme-files (filter scheme-file? compared))
;; Each Scheme file's split-suggest answer, (path . answer); its review copy
;; replaces the file in the scratch copy.
(define suggestions
  (map (lambda (p)
         (let* ((out (string-append reviews "/" p))
                (_ (system (string-append "mkdir -p '" (parent-of out) "'")))
                (a (tolerant (run 'split-suggest (string-append scratch "/" p) "--output" out))))
           (tolerant (when (and (pair? a) (eq? (car a) 'ok) (file-exists? out))
                       (write-bytes! (string-append scratch "/" p) (bytes-of out))))
           (cons p a)))
       scheme-files))
(define (clause a name) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) name))) (cdr a))))
;; A suggestion that answered ok, with its boundaries, and wrote its review
;; copy; anything else is listed and fails RT-6, never counted as one block.
(define (suggested? p a)
  (and (pair? a) (eq? (car a) 'ok) (clause a 'boundaries) (pair? (cadr (clause a 'boundaries)))
       (clause a 'warnings) (file-exists? (string-append reviews "/" p))))
(define failed-suggestions (filter (lambda (s) (not (suggested? (car s) (cdr s)))) suggestions))
(define (boundaries-of a) (let ((c (clause a 'boundaries))) (if c (cadr c) '())))
(define (fallback-of a)
  (let ((c (clause a 'warnings)))
    (and c (find (lambda (w) (and (pair? w) (eq? (car w) 'code))) (cadr c)))))

(define t0 (real-time))
(define imported (tolerant (run 'import-code scratch)))
(define import-ms (- (real-time) t0))
(printf "import of the tree: ~a ms, ~a compared files, ~a Scheme files\n" import-ms (length compared) (length scheme-files))

;; Each file's children as the store holds them, by path, read once after the import.
(define store-children
  (tolerant (let ((st (open-and-reduce store)))
              (map (lambda (id) (cons (code-field st id 'path) (length (code-children st id)))) (code-files st)))))
(define (store-child-count p)
  (guard (x (#t #f))
    (let ((e (and (list? store-children) (pair? store-children) (pair? (car store-children)) (assoc p store-children))))
      (and e (cdr e)))))

(want "RT-0 the import answers ok with no skipped clause: every compared file was read"
      (in-order (and (pair? imported) (car imported)) (clause imported 'skipped))
      '(ok #f))

;; ---- the exports ---------------------------------------------------------------------------------

(define raw-answer (tolerant (run 'export-code raw-dir "--raw")))
(define marked-answer (tolerant (run 'export-code marked-dir)))

(define unequal (filter (lambda (p) (let ((b (bytes-of (string-append raw-dir "/" p))) (o (original p)))
                                      (not (and (bytevector? b) (bytevector? o) (bytevector=? b o)))))
                        compared))
(want "RT-1 the raw export is the tree byte for byte: every compared path equal (the dot-named paths not compared are listed)"
      (in-order (and (pair? raw-answer) (car raw-answer))
                (- (length compared) (length unequal))
                (length compared)
                unequal
                dot-paths)
      (list 'ok (length compared) (length compared) '() '(".gitattributes" ".gitignore")))

;; The entry import chooses for a path: the language whose header wrapper the
;; bytes carry, else the one the path's extension selects.
(define (decode-entry p b)
  (or (find (lambda (candidate) (projection-header-wrapper? candidate b)) (language-table)) (language-for-path p)))
(define decoded
  (map (lambda (p)
         (let ((b (bytes-of (string-append marked-dir "/" p))))
           (cons p (and b (tolerant (projection-decode (decode-entry p b) b))))))
       compared))
(define (decoded-ok? d) (and (list? d) (= 2 (length d)) (list? (cadr d))
                             (for-all (lambda (c) (and (list? c) (= 2 (length c)) (bytevector? (cadr c)))) (cadr d))))
(define (children-of p) (let ((d (cdr (assoc p decoded)))) (if (decoded-ok? d) (cadr d) '())))
(define undecodable (map car (filter (lambda (e) (not (decoded-ok? (cdr e)))) decoded)))
(define (decoded-bytes p) (apply bytevector-append-all (map cadr (children-of p))))
(define (bytevector-append-all . bs)
  (let* ((n (apply + (map bytevector-length bs))) (out (make-bytevector n)))
    (let loop ((bs bs) (i 0))
      (if (null? bs) out
          (begin (bytevector-copy! (car bs) 0 out i (bytevector-length (car bs)))
                 (loop (cdr bs) (+ i (bytevector-length (car bs)))))))))
(define undecoded (filter (lambda (p) (not (equal? (decoded-bytes p) (bytes-of (string-append raw-dir "/" p))))) compared))
(want "RT-1b the marked export decodes, by projection-decode, for every compared path, and carries the raw export's bytes"
      (in-order (and (pair? marked-answer) (car marked-answer)) undecodable (- (length compared) (length undecoded)) undecoded)
      (list 'ok '() (length compared) '()))

(want "RT-2 each export, written to a fresh directory, holds exactly the compared files"
      (in-order (equal? (files-under raw-dir #f) compared) (equal? (files-under marked-dir #f) compared))
      '(#t #t))

;; ---- check ---------------------------------------------------------------------------------------

(define (clauses-named x name)
  (cond ((and (pair? x) (eq? (car x) name)) (list x))
        ((pair? x) (append (clauses-named (car x) name) (clauses-named (cdr x) name)))
        (else '())))
(define checked (tolerant (run 'check)))
(define bom-files (filter (lambda (p) (let ((b (original p))) (and (bytevector? b) (>= (bytevector-length b) 3)
                                                                  (= (bytevector-u8-ref b 0) #xEF) (= (bytevector-u8-ref b 1) #xBB)
                                                                  (= (bytevector-u8-ref b 2) #xBF))))
                          compared))
(define integrity-clauses (if (pair? checked) (clauses-named checked 'integrity) '()))
(want "RT-3 check: every writer's integrity is (integrity ()), no undecodable text; the ten files that begin with a byte-order mark are compared and equal"
      (in-order (and (pair? checked) (car checked))
                (and (pair? integrity-clauses) (for-all (lambda (c) (equal? c '(integrity ()))) integrity-clauses))
                (clauses-named checked 'undecodable-text)
                (length bom-files)
                (filter (lambda (p) (member p unequal)) bom-files))
      '(check #t () 10 ()))
(printf "files with a byte-order mark: ~a ~s\n" (length bom-files) bom-files)

;; ---- the marked export imported again --------------------------------------------------------------

(define (live-ids) (let ((a (run 'query "(kind ?b ?k)")))
                     (list-sort string<? (map cadr (filter (lambda (r) (and (pair? r) (eq? (car r) 'row)) )
                                                           (let ((items (clause a 'items))) (if items (cdr items) '())))))))
(define (versions ids) (let ((st (open-and-reduce store))) (map (lambda (id) (cons id (block-hash st id))) ids)))
(define ids-before (tolerant (live-ids)))
(printf "blocks after the import of the split tree: ~a (~a ms)\n" (if (list? ids-before) (length ids-before) ids-before) import-ms)
(define reimported (tolerant (run 'import-code marked-dir)))
(define ids-after (tolerant (live-ids)))
(want "RT-4 the marked export imported again lands on the same blocks: ok, no skipped, symbols-ignored or symbols-refused, and the live block ids and their count unchanged"
      (in-order (and (pair? reimported) (car reimported))
                (map (lambda (n) (clause reimported n)) '(skipped symbols-ignored symbols-refused))
                (and (list? ids-before) (> (length ids-before) (length compared)))
                (equal? ids-before ids-after))
      '(ok (#f #f #f) #t #t))

;; ---- one byte changed ----------------------------------------------------------------------------

;; A child of eval-worker.sc whose bytes appear verbatim in the marked file,
;; and in it the first lower-case letter of a line that is not a marker line:
;; changed to another letter, the text stays UTF-8 and the framing is not touched.
(define (index-of-bytes hay needle)
  (let ((n (bytevector-length needle)) (m (bytevector-length hay)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((let check ((k 0)) (or (= k n) (and (= (bytevector-u8-ref hay (+ i k)) (bytevector-u8-ref needle k)) (check (+ k 1))))) i)
            (else (loop (+ i 1)))))))
(define target-path (and (member "eval-worker.sc" compared) "eval-worker.sc"))
(define target
  (let ((t (tolerant
  (and target-path
       (let ((file (bytes-of (string-append marked-dir "/" target-path))))
         (let loop ((cs (if (pair? (children-of target-path)) (cdr (children-of target-path)) '())))
           (and (pair? cs)
                (let* ((id (car (car cs))) (b (cadr (car cs))) (at (and (> (bytevector-length b) 0) (index-of-bytes file b))))
                  (let ((k (and at (let find-letter ((k 0))
                                     (cond ((= k (bytevector-length b)) #f)
                                           ((<= 97 (bytevector-u8-ref b k) 122) k)
                                           (else (find-letter (+ k 1))))))))
                    (if k (list id (+ at k) file b k) (loop (cdr cs))))))))))))
    (and (list? t) (not (eq? (car t) 'RAISED)) t)))
(define changed
  (tolerant
   (and target
       (let* ((file (bytevector-copy (caddr target))) (at (cadr target)) (c (bytevector-u8-ref file at)))
         (bytevector-u8-set! file at (if (= c 122) 121 (+ c 1)))
         (write-bytes! (string-append one-dir "/" target-path) file)
         (let* ((before (versions ids-after))
                (a (run 'import-code one-dir))
                (ids (live-ids))
                (after (versions ids))
                ;; the child's bytes with the one byte changed, as the store must now hold them
                (meant (let ((m (bytevector-copy (list-ref target 3))) (k (list-ref target 4)))
                         (bytevector-u8-set! m k (bytevector-u8-ref file at)) m))
                (held (code-field (open-and-reduce store) (car target) 'src)))
           (list (and (pair? a) (car a))
                 (equal? ids ids-after)
                 (map car (filter (lambda (b) (let ((e (assoc (car b) after))) (not (and e (equal? (cdr b) (cdr e)))))) before))
                 (and (bytevector? held) (bytevector=? held meant))))))))
(want "RT-5 CONTROL: one byte changed in one child of eval-worker.sc's marked export moves that block's version and no other's, and the block holds exactly the changed bytes"
      (in-order (and target #t) (and (list? changed) (car changed)) (and (list? changed) (cadr changed)) (and (list? changed) (caddr changed))
                (and (list? changed) (= 4 (length changed)) (cadddr changed)))
      (list #t 'ok #t (if target (list (car target)) '()) #t))

;; ---- the splitting -------------------------------------------------------------------------------

(define child-mismatch
  (filter (lambda (s) (not (eqv? (store-child-count (car s)) (length (boundaries-of (cdr s))))))
          suggestions))
(want "RT-6 every Scheme file's suggestion succeeded, and its children in the store are as many as its boundaries (one when it fell back)"
      (in-order (length suggestions) (map car failed-suggestions) (map car child-mismatch))
      (list (length scheme-files) '() '()))

;; A suggestion that failed, or one boundary with some other warning, is in
;; none of the three classes, and the sum says so.
(define (class-of s)
  (let ((a (cdr s)))
    (cond ((not (suggested? (car s) a)) 'failed)
          ((fallback-of a) (if (equal? (boundaries-of a) '(0)) 'fell-back 'other))
          ((and (= 1 (length (boundaries-of a))) (null? (cadr (clause a 'warnings)))) 'one-block)
          ((> (length (boundaries-of a)) 1) 'split)
          (else 'other))))
(define fell-back (filter (lambda (s) (eq? (class-of s) 'fell-back)) suggestions))
(define one-block (filter (lambda (s) (eq? (class-of s) 'one-block)) suggestions))
(define split (filter (lambda (s) (eq? (class-of s) 'split)) suggestions))
;; The exemplars are the tree's root files of those names.
(define (named file) (assoc file suggestions))
(printf "RT-7 classes: fell back ~a, one block ~a, split ~a\n" (length fell-back) (length one-block) (length split))
(printf "RT-7 fell back: ~s\n" (map (lambda (s) (list (car s) (cadr (fallback-of (cdr s))))) fell-back))
(printf "RT-7 one block: ~s\n" (map car one-block))
(want "RT-7 the three classes sum to the Scheme files scanned; eval-worker.sc splits into more than one child, admission.sc is one block with no warning, code-project.sc falls back lexically-uncertain"
      (in-order (= (+ (length fell-back) (length one-block) (length split)) (length scheme-files))
                (> (length scheme-files) 0)
                (let ((s (named "eval-worker.sc"))) (and s (list (class-of s) (> (or (store-child-count (car s)) 0) 1))))
                (let ((s (named "admission.sc"))) (and s (class-of s)))
                (let ((s (named "code-project.sc"))) (and s (list (class-of s) (cadr (fallback-of (cdr s)))))))
      '(#t #t (split #t) one-block (fell-back lexically-uncertain)))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\ntree-round-trip complete\n" bad rows)
(exit (if (= bad 0) 0 1))
