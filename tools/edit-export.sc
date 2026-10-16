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

;; Export a writer's working view as files for an agent to edit, with what
;; tools/edit-sync.sc needs to turn the edits back into that writer's drafts.
;;
;;   scheme --script tools/edit-export.sc <store> <dir> --writer <W> --git <repo> --base <rev>
;;
;; THE FILES ARE THE RAW WORKING EXPORT, what `export-code <dir> --working
;; --raw --writer W` writes: each text file is its children's srcs run
;; together, the writer's drafts laid over the committed state at the join
;; of the drafts' cuts. Raw, because a block's span is then plain byte
;; arithmetic and locate's coordinates are raw bytes; the marked export
;; embeds markers and its header carries the cut.
;;
;; THE SIDECAR IS BESIDE THE DIRECTORY, NOT IN IT: <dir>.edit/, holding
;; SIDECAR.sexp and a pristine copy of every exported file. Inside the
;; directory it would be one more file for the agent to edit and for
;; edit-sync to read as an added one. SIDECAR.sexp is one datum:
;;
;;   (edit-export (format 1) (store "<id>") (writer "<W>") (cut <cut>)
;;                (base "<40-hex>")
;;     (file "<path>" (mode "<git mode>" | none) (pristine "<n>.bytes")
;;                    (sha256 "<64-hex>")
;;       (block "<id>" (span <start> <end>) (committed "<64-hex>")
;;              [(draft (version "<64-hex>") (based-on "<64-hex>") (cut <cut>))])
;;       ...)
;;     ...)
;;
;; - cut: the committed cut the view stands on (the export's working cut).
;; - mode: the file's mode in the git tree the store was published from,
;;   since the export writes every file 0644 and keeps no mode; `none` for a
;;   path that tree does not hold.
;; - span: the block's half-open byte range in the exported file, from the
;;   same state the files are written from, zero-length blocks included.
;; - committed: the block's hash in the committed state at the cut, the
;;   based-on a write of an undrafted block names.
;; - draft: where W already has a draft on the block, its version, its own
;;   based-on and its own cut, which a later write into that slot keeps.
;;
;; ONE STATE FOR EVERYTHING. The files, the spans and the draft list all
;; come from one call of working-state; the drafts' cuts, which its list
;; does not carry, are read per drafted block afterwards and refused as
;; drafts-moved when that read's version is not the state's. The committed
;; hashes come from the committed state at the same cut.
;;
;; THE DIRECTORY AND THE SIDECAR MUST NOT EXIST: the export removes nothing,
;; so a file left from an earlier export would be edited and synced as if
;; the store held it. SIDECAR.sexp is written last, under a temporary name
;; renamed into place, so an export that dies leaves no sidecar and
;; edit-sync refuses missing-sidecar.
;;
;; A refusal prints (error <kind> ...) and exits 1; success prints
;; (ok (files <n>) (blocks <n>) (drafts <n>) (cut <cut>) (sidecar "<path>")).
(import (chezscheme)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) reduction? reduce-applied-cut block-hash)
        (only (theourgia working) working-state working-read)
        (only (theourgia code-project) projected-files)
        (only (theourgia log) store-id-of)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (theourgia ffi) entry-bytes entry-name-type mkdir-p! overwrite-entry! rename-over! unlink!))

(define (refuse kind . details) (raise (cons 'error (cons kind details))))

;; ---- small tools -----------------------------------------------------------

(define (prefix? s p) (and (>= (string-length s) (string-length p)) (string=? (substring s 0 (string-length p)) p)))
(define (split-on s ch)
  (let loop ((i 0) (start 0) (out '()))
    (cond ((= i (string-length s)) (reverse (cons (substring s start i) out)))
          ((char=? (string-ref s i) ch) (loop (+ i 1) (+ i 1) (cons (substring s start i) out)))
          (else (loop (+ i 1) start out)))))
(define (absolute p) (if (prefix? p "/") p (string-append (current-directory) "/" p)))
(define (without-trailing-slash p)
  (if (and (> (string-length p) 1) (char=? #\/ (string-ref p (- (string-length p) 1))))
      (without-trailing-slash (substring p 0 (- (string-length p) 1)))
      p))
(define (parent-of path)
  (let loop ((i (- (string-length path) 1)))
    (cond ((< i 0) ".") ((char=? (string-ref path i) #\/) (substring path 0 i)) (else (loop (- i 1))))))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(define (hex40? s)
  (and (string? s) (= (string-length s) 40)
       (for-all (lambda (c) (or (char<=? #\0 c #\9) (char<=? #\a c #\f))) (string->list s))))
(define (bytes-concat parts)
  (let* ((n (fold-left + 0 (map bytevector-length parts))) (out (make-bytevector n)))
    (let loop ((ps parts) (at 0))
      (if (null? ps) out
          (begin (bytevector-copy! (car ps) 0 out at (bytevector-length (car ps)))
                 (loop (cdr ps) (+ at (bytevector-length (car ps)))))))))
(define (src-bytes src) (if (string? src) (string->utf8 src) src))
(define (hash-of bv) (bytevector->hex (sha256 bv)))

;; ---- the git base: its full id, and every path's mode ---------------------------
;;
;; Each git command's output goes to a file in the sidecar directory, read
;; afterwards: one process, no pipe to drain while another fills, and its
;; exit status read from system. Pathspecs never appear, so nothing is a
;; pattern; no hook runs for these reads.
(define (git-to-file repo args out err)
  (system (string-append "LC_ALL=C git -c core.hooksPath=/dev/null -C " (quoted repo) " "
                         (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
                         "> " (quoted out) " 2> " (quoted err))))
(define (git-output side repo . args)
  (let ((out (string-append side "/git.out")) (err (string-append side "/git.err")))
    (let ((rc (git-to-file repo args out err)))
      (let ((bytes (entry-bytes out)) (why (utf8->string (entry-bytes err))))
        (unlink! out) (unlink! err)
        (unless (= rc 0) (refuse 'git-failed (cons 'command args) (list 'status rc) (list 'stderr why)))
        bytes))))
(define (base-id side repo rev)
  (let* ((text (utf8->string (git-output side repo "rev-parse" "--verify" (string-append rev "^{commit}"))))
         (id (if (and (> (string-length text) 0) (char=? #\newline (string-ref text (- (string-length text) 1))))
                 (substring text 0 (- (string-length text) 1))
                 text)))
    (unless (hex40? id) (refuse 'bad-base (list 'rev rev)))
    id))
;; path -> mode, from `git ls-tree -r -z`: "<mode> <type> <sha>\t<path>\0"
(define (base-modes side repo id)
  (let ((h (make-hashtable string-hash string=?)))
    (for-each (lambda (rec)
                (when (> (string-length rec) 0)
                  (let* ((tab (let loop ((i 0)) (if (char=? (string-ref rec i) #\tab) i (loop (+ i 1)))))
                         (meta (split-on (substring rec 0 tab) #\space)))
                    (hashtable-set! h (substring rec (+ tab 1) (string-length rec)) (car meta)))))
              (split-on (utf8->string (git-output side repo "ls-tree" "-r" "-z" id)) #\nul))
    h))

;; ---- the store -------------------------------------------------------------------

(define (answer-ok a) (unless (and (pair? a) (eq? (car a) 'ok)) (raise a)) a)

;; A DRAFTED BLOCK'S OWN FACTS: (version based-on cut), read with
;; --working-info, which answers (projection working W id version based-on
;; cut text prefix). VERSION is the one working-state overlaid; another
;; means a write under W landed between the two reads.
(define (draft-facts store state writer id version)
  (let* ((a (answer-ok (working-read store state writer id #t)))
         (p (cadr a)))
    (unless (and (pair? p) (eq? (car p) 'projection) (eq? (cadr p) 'working) (equal? (list-ref p 4) version))
      (refuse 'drafts-moved (list 'block id) (list 'version version)))
    (list (list-ref p 4) (list-ref p 5) (list-ref p 6))))

;; ---- the export --------------------------------------------------------------------

(define (export store dir writer repo rev)
  (let ((side (string-append dir ".edit")))
    (unless (eq? 'absent (entry-name-type dir)) (refuse 'export-exists (list 'path dir)))
    (unless (eq? 'absent (entry-name-type side)) (refuse 'export-exists (list 'path side)))
    (let ((store-id (store-id-of store)))
      (when (equal? store-id "unknown") (refuse 'not-a-store (list 'path store)))
      (mkdir-p! (string-append side "/pristine"))
      (let* ((base (base-id side repo rev))
             (modes (base-modes side repo base))
             (w (working-state store #f writer)))
        (unless (and (pair? w) (eq? 'ok (car w))) (raise w))
        (let* ((view (caddr w))
               (drafts (cadddr w))
               (cut (reduce-applied-cut view))
               (committed (if (null? drafts) view (open-and-reduce store cut)))
               (files (projected-files view))
               ;; the blocks the files hold; a draft on any other block
               ;; (one in no text file) has no span and is not read
               (exported (let ((h (make-hashtable string-hash string=?)))
                           (for-each (lambda (f) (for-each (lambda (c) (hashtable-set! h (car c) #t)) (cadddr f))) files)
                           h))
               (mine (filter (lambda (d) (hashtable-ref exported (car d) #f)) drafts))
               ;; id -> (version based-on cut) for every exported block W
               ;; has drafted, all read against one reduction
               (drafted (let ((h (make-hashtable string-hash string=?)) (now (if (null? mine) #f (open-and-reduce store))))
                          (for-each (lambda (d) (hashtable-set! h (car d) (draft-facts store now writer (car d) (cadr d))))
                                    mine)
                          h)))
          (unless (reduction? committed) (refuse 'cut-unavailable (list 'cut cut)))
          (let loop ((fs files) (k 1) (blocks 0) (entries '()))
            (if (pair? fs)
                (let* ((f (car fs)) (rel (car f)) (children (cadddr f))
                       (parts (map (lambda (c) (src-bytes (cadr c))) children))
                       (bytes (bytes-concat parts))
                       (pristine (string-append (number->string k) ".bytes"))
                       (path (string-append dir "/" rel)))
                  (mkdir-p! (parent-of path))
                  (overwrite-entry! path bytes)
                  (overwrite-entry! (string-append side "/pristine/" pristine) bytes)
                  (let spans ((cs children) (ps parts) (at 0) (out '()))
                    (if (pair? cs)
                        (let* ((id (car (car cs))) (end (+ at (bytevector-length (car ps))))
                               (hash (block-hash committed id))
                               (d (hashtable-ref drafted id #f)))
                          (unless hash (refuse 'no-committed-block (list 'block id) (list 'cut cut)))
                          (spans (cdr cs) (cdr ps) end
                                 (cons (append (list 'block id (list 'span at end) (list 'committed hash))
                                               (if d (list (list 'draft (list 'version (car d)) (list 'based-on (cadr d))
                                                                 (list 'cut (caddr d))))
                                                   '()))
                                       out)))
                        (loop (cdr fs) (+ k 1) (+ blocks (length children))
                              (cons (append (list 'file rel
                                                  (list 'mode (hashtable-ref modes rel 'none))
                                                  (list 'pristine pristine)
                                                  (list 'sha256 (hash-of bytes)))
                                            (reverse out))
                                    entries)))))
                (let ((datum (append (list 'edit-export '(format 1) (list 'store store-id) (list 'writer writer)
                                           (list 'cut cut) (list 'base base))
                                     (reverse entries)))
                      (tmp (string-append side "/SIDECAR.sexp.tmp")))
                  (overwrite-entry! tmp (string->utf8 (format "~s\n" datum)))
                  (rename-over! tmp (string-append side "/SIDECAR.sexp"))
                  (list 'ok (list 'files (length files)) (list 'blocks blocks) (list 'drafts (length drafts))
                        (list 'cut cut) (list 'sidecar (string-append side "/SIDECAR.sexp")))))))))))

;; ---- the command line ------------------------------------------------------------------

(define (option args name)
  (let ((m (member name args))) (and m (pair? (cdr m)) (cadr m))))
(define (main args)
  (let ((writer (option args "--writer")) (repo (option args "--git")) (rev (option args "--base")))
    (if (not (and (= 8 (length args)) writer repo rev
                  (not (prefix? (car args) "--")) (not (prefix? (cadr args) "--"))))
        (begin (write '(usage (edit-export <store> <dir> "--writer" <W> "--git" <repo> "--base" <rev>)))
               (newline) (exit 2))
        (let ((a (guard (e ((and (pair? e) (eq? (car e) 'error)) e))
                   (export (without-trailing-slash (absolute (car args)))
                           (without-trailing-slash (absolute (cadr args)))
                           writer (absolute repo) rev))))
          (write a)
          (newline)
          (exit (if (eq? (car a) 'ok) 0 1))))))

(main (cdr (command-line)))
