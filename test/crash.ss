#!chezscheme
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

;; The crash device (design 13', the ruling on open question 6).
;;
;; Power loss is checked against TWO states, and a store must recover
;; from both:
;;
;;   (i)  the kill state -- SIGKILL at a barrier, so the disk holds
;;        everything the process had written, buffered or not
;;   (ii) the durable-only state -- this device rewrites a copy so that
;;        only what was fsynced survives
;;
;; The second is the one that needs a device, and its rules are:
;;   a file keeps only the bytes it had at the last fsync OF THAT PATH,
;;   and a directory entry made by create / rename / link / unlink
;;   survives only if that DIRECTORY was fsynced afterwards -- otherwise
;;   it goes back to what it was (rename returns to the old name, a
;;   link's new name is gone, an unlinked name is present again).
;;
;; IT READS THE TRACE, THE CRASHED TREE, AND A BEFORE-IMAGE. It never
;; consults the product's own state -- a device that did would agree
;; with the product by construction, which is the one thing this must
;; not do. The before-image is a copy of the tree taken BEFORE the run,
;; and it is not product state: it is what the disk held, which is
;; exactly what a real recovery would find under the parts the run never
;; made durable.
;;
;; THE BEFORE-IMAGE IS NOT OPTIONAL, and the first version of this
;; device tried to do without it. Byte counts and the crashed tree
;; cannot reconstruct:
;;   * a file that was truncated after its last flush -- the durable
;;     bytes are longer than anything still on disk
;;   * the contents an unlink removed
;;   * the contents a rename overwrote
;; Each of those was silently answered with an empty file or with the
;; wrong length, which is a device that reports a store as recoverable
;; when the model says otherwise.
;;
;; ONE ASSUMPTION REMAINS: writes are appends. This library's log writes
;; are O_APPEND and its whole-file replacements go to a fresh temporary,
;; so a write event's byte count places it. A seeking writer would need
;; offsets in the trace, and this device would be wrong about it.
(import (chezscheme))

;; THE WORK DIRECTORY IS DECIDED AT RUN TIME. Every fixture used to name
;; an absolute path under one session's scratchpad. That is green only
;; while that particular directory happens to still exist: tmp is swept,
;; and another machine has no such path at all -- so the whole suite
;; would go red for a reason with nothing to do with the code under test.
;; THEOURGIA_TEST_ROOT overrides the default; the pid keeps two runs, or
;; two fixtures, out of each other's way. Directories are left behind
;; deliberately, as evidence.
(define (test-dir name)
  (let* ((root (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                 (if (and (string? v) (> (string-length v) 0)) v "/tmp/theourgia-test")))
         (path (string-append root "/" name "-" (number->string (get-process-id)))))
    ;; A ROOT THAT DOES NOT SURVIVE THE ROUND TRIP IS REFUSED HERE. Trace
    ;; lines are written with display and read back as data, and paths go
    ;; into generated scripts and shell commands unquoted -- so a root
    ;; with a space or a bracket in it makes the crash device read no
    ;; events at all and rewrite nothing, which reads exactly like a tree
    ;; that needed no rewriting. Refusing is the one answer that cannot
    ;; be mistaken for success.
    (let loop ((i 0))
      (when (< i (string-length path))
        (let ((c (string-ref path i)))
          (unless (or (char-alphabetic? c) (char-numeric? c)
                      (memv c '(#\/ #\. #\- #\_)))
            (assertion-violation 'test-dir
              "THEOURGIA_TEST_ROOT may use only letters, digits, / . - and _"
              root)))
        (loop (+ i 1))))
    (system (string-append "mkdir -p " path))
    path))


(define (crash-lines-of text)
  (let ((n (string-length text)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((>= i n) (reverse (if (> i start) (cons (substring text start i) acc) acc)))
            ((char=? (string-ref text i) #\newline)
             (loop (+ i 1) (+ i 1) (cons (substring text start i) acc)))
            (else (loop (+ i 1) start acc))))))

(define (crash-has-substring? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) sub) #t)
            (else (loop (+ i 1)))))))

;; THE TRACE IS WRITTEN WITH display, SO PATHS COME BACK AS SYMBOLS.
;; Reading a line yields (trace write /some/path 4) -- a bare symbol, not
;; a string. The first version tested string? on it, matched nothing, and
;; rewrote nothing at all; an empty rewrite reads exactly like a tree
;; that needed no rewriting.
(define (crash-path x)
  (cond ((string? x) x)
        ((symbol? x) (symbol->string x))
        (else #f)))

(define (crash-events text)
  (let loop ((ls (crash-lines-of text)) (acc '()))
    (cond
      ((null? ls) (reverse acc))
      ((crash-has-substring? (car ls) "(trace ")
       (let ((d (guard (e (#t #f)) (read (open-string-input-port (car ls))))))
         (if (and (list? d) (= 4 (length d)) (eq? (car d) 'trace))
             (loop (cdr ls)
                   (cons (list (cadr d)
                               (let ((s (caddr d)))
                                 (if (pair? s)
                                     (cons (crash-path (car s)) (crash-path (cdr s)))
                                     (crash-path s)))
                               (cadddr d))
                         acc))
             (loop (cdr ls) acc))))
      (else (loop (cdr ls) acc)))))

(define (crash-op e) (car e))
(define (crash-subject e) (cadr e))
(define (crash-bytes e) (caddr e))

(define (crash-parent-of path)
  (let loop ((i (- (string-length path) 1)))
    (cond ((< i 1) "/")
          ((char=? (string-ref path i) #\/) (substring path 0 i))
          (else (loop (- i 1))))))

;; #f FOR ANYTHING THAT IS NOT A READABLE FILE, directories included. A
;; directory fsync is an ordinary event whose subject is a directory, and
;; reading it as a file ends the device with an exception rather than a
;; verdict.
(define (crash-read path)
  (guard (e (#t #f))
    (and (file-exists? path)
         (not (file-directory? path))
         (call-with-port (open-file-input-port path)
           (lambda (p) (let ((b (get-bytevector-all p)))
                         (if (eof-object? b) (make-bytevector 0) b)))))))

(define (crash-write! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p bv))))

(define (crash-take bv n)
  (let* ((have (bytevector-length bv)) (k (min n have)) (o (make-bytevector k)))
    (bytevector-copy! bv 0 o 0 k)
    o))

;; Where the same path lives in the before-image.
(define (crash-in-before root before path)
  (let ((rn (string-length root)))
    (if (and (>= (string-length path) rn) (string=? (substring path 0 rn) root))
        (string-append before (substring path rn (string-length path)))
        #f)))

;; THE DURABLE LENGTH OF EVERY PATH THE TRACE TOUCHED, simulated forward
;; from the before-image. The trace carries byte COUNTS, not bytes, so a
;; simulation can know how long a file was at each flush and can never
;; know what was in it -- an earlier version built a bytevector as it
;; went and wrote that back, filling every rotated segment with
;; uninitialised memory and reporting the store as corrupt.
;;
;; The bytes themselves come from the trees: the crashed one where it
;; still holds enough of them, the before-image where a truncation after
;; the last flush means the durable file is longer than anything left.
(define (crash-durable-lengths root before events)
  (let ((state '()) (durable '()))
    (define (get p)
      (let ((e (assoc p state)))
        (if e (cdr e)
            (let* ((b (crash-in-before root before p))
                   (bytes (and b (crash-read b))))
              (if bytes (bytevector-length bytes) 'absent)))))
    (define (put! p v) (set! state (cons (cons p v) (remp (lambda (e) (equal? (car e) p)) state))))
    (define (seal! p v) (set! durable (cons (cons p v) (remp (lambda (e) (equal? (car e) p)) durable))))
    (define (unseal! p) (set! durable (remp (lambda (e) (equal? (car e) p)) durable)))
    (for-each
      (lambda (e)
        (let ((op (crash-op e)) (s (crash-subject e)))
          (cond
            ;; A NAME THAT IS REMOVED OR REMADE STARTS OVER. The flush
            ;; that sealed the old file says nothing about the new one.
            ((and (eq? op 'create) (string? s)) (put! s 0) (unseal! s))
            ((and (eq? op 'unlink) (string? s)) (put! s 'absent) (unseal! s))
            ((and (eq? op 'link) (string? s)) (put! s 0) (unseal! s))
            ;; DURABILITY TRAVELS WITH THE CONTENT ACROSS A RENAME.
            ((and (eq? op 'rename) (pair? s))
             (let ((from (car s)) (to (cdr s)))
               (put! to (get from))
               (put! from 'absent)
               (let ((d (assoc from durable)))
                 (if d (seal! to (cdr d)) (unseal! to)))
               (unseal! from)))
            ((and (eq? op 'write) (string? s))
             (let ((cur (get s)))
               (put! s (+ (if (number? cur) cur 0) (crash-bytes e)))))
            ((and (eq? op 'ftruncate) (string? s)) (put! s (crash-bytes e)))
            ;; A directory flush seals entries, not contents.
            ((and (eq? op 'fsync) (string? s) (not (file-directory? s)))
             (seal! s (get s)))
            (else (if #f #f)))))
      events)
    durable))

;; Enough bytes to make LENGTH, taken from whichever tree still has them.
(define (crash-bytes-for root before path length)
  (let* ((now (crash-read path))
         (old (let ((b (crash-in-before root before path))) (and b (crash-read b))))
         (source (cond
                   ((and now (>= (bytevector-length now) length)) now)
                   ((and old (>= (bytevector-length old) length)) old)
                   (now now)
                   (else old))))
    (and source (crash-take source length))))

;; Directory operations that were never sealed by an fsync of their own
;; directory, newest first -- which is the order they must be undone in.
;; Undoing forward turned "create a then unlink a" into an empty a, and
;; two chained renames into the middle name.
(define (crash-unsealed-entries events)
  (let loop ((es (reverse events)) (flushed '()) (undo '()))
    (cond
      ((null? es) (reverse undo))
      (else
       (let* ((e (car es)) (op (crash-op e)) (s (crash-subject e)))
         (cond
           ((and (eq? op 'fsync) (string? s)) (loop (cdr es) (cons s flushed) undo))
           ((memq op '(create link unlink))
            (loop (cdr es) flushed
                  (if (member (crash-parent-of s) flushed) undo (cons (list op s) undo))))
           ((eq? op 'rename)
            ;; BOTH DIRECTORIES. Flushing the destination cannot seal the
            ;; source's loss of its entry, and checking only the
            ;; destination left a cross-directory rename half-applied.
            (let* ((from (car s)) (to (cdr s))
                   (sealed (and (member (crash-parent-of to) flushed)
                                (member (crash-parent-of from) flushed))))
              (loop (cdr es) flushed
                    (if sealed undo (cons (list 'rename from to) undo)))))
           (else (loop (cdr es) flushed undo))))))))

;; Rewrite the tree at ROOT into the durable-only state. BEFORE is a copy
;; of that tree as it stood before the traced run.
(define (crash-durable-only! root before trace-text)
  (let* ((events (crash-events trace-text))
         (durable (crash-durable-lengths root before events)))
    (for-each
      (lambda (u)
        (case (car u)
          ((create link)
           (when (file-exists? (cadr u)) (delete-file (cadr u))))
          ((unlink)
           ;; RESTORED FROM THE BEFORE-IMAGE, not as an empty file. The
           ;; trace does not carry the bytes an unlink removed, and
           ;; substituting nothing reports a store as recoverable that
           ;; the model says has lost data.
           (let* ((b (crash-in-before root before (cadr u)))
                  (bytes (and b (crash-read b))))
             (crash-write! (cadr u) (or bytes (make-bytevector 0)))))
          ((rename)
           (let* ((from (cadr u)) (to (caddr u))
                  (over (crash-in-before root before to))
                  (overwritten (and over (crash-read over))))
             (when (file-exists? to)
               (let ((carried (crash-read to)))
                 (delete-file to)
                 (crash-write! from carried)))
             ;; A rename that replaced an existing file also has to give
             ;; that file back.
             (when overwritten (crash-write! to overwritten))))))
      (crash-unsealed-entries events))
    ;; Contents last, so that a file undone back to a name still gets the
    ;; length it was durable at.
    (for-each
      (lambda (e)
        (let ((path (car e)) (len (cdr e)))
          (cond
            ((eq? len 'absent) (when (file-exists? path) (delete-file path)))
            ((and (number? len) (or (file-exists? path) (> len 0)))
             (let ((bytes (crash-bytes-for root before path len)))
               (when bytes (crash-write! path bytes))))
            (else (if #f #f)))))
      durable)
    ;; Anything the trace wrote but never sealed, and whose name still
    ;; stands, goes back to what the before-image held -- or disappears.
    (for-each
      (lambda (p)
        (unless (assoc p durable)
          (let* ((b (crash-in-before root before p))
                 (bytes (and b (crash-read b))))
            (cond
              ((and bytes (file-exists? p)) (crash-write! p bytes))
              ((and (not bytes) (file-exists? p)) (delete-file p))
              (else (if #f #f))))))
      (let loop ((es events) (acc '()))
        (cond ((null? es) acc)
              ((and (memq (crash-op (car es)) '(write ftruncate))
                    (string? (crash-subject (car es)))
                    (not (member (crash-subject (car es)) acc)))
               (loop (cdr es) (cons (crash-subject (car es)) acc)))
              (else (loop (cdr es) acc)))))
    root))

;; ---- self-check ------------------------------------------------------------
;; THE DEVICE IS THE JUDGE FOR EVERY CRASH CASE, so it is checked first
;; against answers worked out on paper. A judge that is wrong makes each
;; case it presides over worthless, and worthless in the direction that
;; reads as success.
;;
;; Every row here exists because an earlier version of this device got it
;; wrong: the last flush rather than the first, a truncation after a
;; flush, contents carried by a rename, a name reused after an unlink,
;; two operations on one name undone in the wrong order, a rename across
;; two directories sealed by flushing only one, and an unlink whose
;; contents were replaced with nothing.
(define (crash-self-check)
  (let ((bad 0))
    (define (want label got expect)
      (let ((ok (equal? got expect)))
        (unless ok (set! bad (+ bad 1)))
        (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
                (if ok "" (format "   WANT ~s" expect)))))
    (define base (test-dir "crashwork"))
    (define d (string-append base "/tree"))
    (define b (string-append base "/before"))
    ;; fresh! LAYS OUT THE BEFORE STATE and copies it; crashed! then puts
    ;; the tree into the state the run left it in. Keeping them separate
    ;; matters: the first version wrote only the before state and handed
    ;; the device a trace describing a tree that was never there, so its
    ;; answers were about nothing.
    (define (fresh! . files)
      (system (string-append "rm -rf " base "; mkdir -p " d))
      (for-each (lambda (f) (crash-write! (string-append d "/" (car f))
                                          (string->utf8 (cdr f))))
                files)
      (system (string-append "cp -R " d " " b)))
    (define (crashed! . files)
      (system (string-append "rm -f " d "/*"))
      (for-each (lambda (f) (crash-write! (string-append d "/" (car f))
                                          (string->utf8 (cdr f))))
                files))
    (define (after! trace) (crash-durable-only! d b trace))
    (define (contents f)
      (let ((x (crash-read (string-append d "/" f))))
        (if x (utf8->string x) 'absent)))
    (define (listing) (list-sort string<? (directory-list d)))
    (define (p f) (string-append d "/" f))

    ;; 1. The LAST flush decides, not the first.
    (fresh! '("a" . ""))
    (crashed! '("a" . "AAAABBBBCCCC"))
    (after! (string-append "(trace write " (p "a") " 4)\n"
                           "(trace fsync " (p "a") " #f)\n"
                           "(trace write " (p "a") " 4)\n"
                           "(trace fsync " (p "a") " #f)\n"
                           "(trace write " (p "a") " 4)\n"))
    ;; The bytes, not just the length: the device takes them from the
    ;; crashed tree, and taking them from the wrong end would keep a
    ;; length that looks right.
    (want "the last flush decides, not the first" (contents "a") "AAAABBBB")

    ;; 2. A truncation AFTER the last flush is undone -- the durable file
    ;;    is LONGER than anything left on disk, which no byte count in
    ;;    the trace can tell you.
    (fresh! '("a" . "AAAABBBB"))
    (after! (string-append "(trace fsync " (p "a") " #f)\n"
                           "(trace ftruncate " (p "a") " 4)\n"))
    (want "a truncation after the last flush is undone from the before-image"
          (contents "a") "AAAABBBB")

    ;; 3. A truncation BEFORE the last flush stands, and later appends go.
    (fresh! '("a" . ""))
    (crashed! '("a" . "BBBB"))
    (after! (string-append "(trace write " (p "a") " 4)\n"
                           "(trace fsync " (p "a") " #f)\n"
                           "(trace ftruncate " (p "a") " 0)\n"
                           "(trace fsync " (p "a") " #f)\n"
                           "(trace write " (p "a") " 4)\n"))
    (want "a truncation before the last flush stands" (string-length (contents "a")) 0)

    ;; 4. Contents travel with a rename: the flush that sealed four bytes
    ;;    under the old name still governs them under the new one.
    (fresh! '("a" . ""))
    (crashed! '("b" . "AAAABBBB"))
    (after! (string-append "(trace write " (p "a") " 4)\n"
                           "(trace fsync " (p "a") " #f)\n"
                           "(trace write " (p "a") " 4)\n"
                           "(trace rename (" (p "a") " . " (p "b") ") #f)\n"
                           "(trace fsync " d " #f)\n"))
    (want "durability follows the content across a rename" (string-length (contents "b")) 4)

    ;; 5. A name reused after an unlink does not inherit the old file's
    ;;    flush.
    (fresh! '("a" . ""))
    (crashed! '("a" . "BBBB"))
    (after! (string-append "(trace write " (p "a") " 4)\n"
                           "(trace fsync " (p "a") " #f)\n"
                           "(trace unlink " (p "a") " #f)\n"
                           "(trace create " (p "a") " #f)\n"
                           "(trace write " (p "a") " 4)\n"
                           "(trace fsync " d " #f)\n"))
    (want "a reused name does not inherit the old incarnation's flush"
          (string-length (contents "a")) 0)

    ;; 6. Two operations on one name are undone newest first.
    (fresh!)
    (crashed!)
    (after! (string-append "(trace create " (p "a") " #f)\n"
                           "(trace unlink " (p "a") " #f)\n"))
    (want "create then unlink, unsealed, leaves the name absent" (listing) '())
    (fresh! '("a" . "X"))
    (system (string-append "mv " (p "a") " " (p "c")))
    (after! (string-append "(trace rename (" (p "a") " . " (p "b") ") #f)\n"
                           "(trace rename (" (p "b") " . " (p "c") ") #f)\n"))
    (want "two chained renames, unsealed, return to the first name" (listing) '("a"))

    ;; 7. A cross-directory rename needs BOTH directories sealed.
    (system (string-append "rm -rf " base "; mkdir -p " d "/s " d "/t"))
    (crash-write! (string-append d "/t/b") (string->utf8 "X"))
    (system (string-append "cp -R " d " " b))
    (system (string-append "rm -f " b "/t/b; printf X > " b "/s/a"))
    (after! (string-append "(trace rename (" d "/s/a . " d "/t/b) #f)\n"
                           "(trace fsync " d "/t #f)\n"))
    (want "flushing only the destination does not seal a cross-directory rename"
          (list (list-sort string<? (directory-list (string-append d "/s")))
                (list-sort string<? (directory-list (string-append d "/t"))))
          '(("a") ()))

    ;; 8. An unlink gives back the contents, not an empty file.
    (fresh! '("a" . "DATA"))
    (crashed!)
    (after! (string-append "(trace unlink " (p "a") " #f)\n"))
    (want "an unsealed unlink restores the contents, not an empty file"
          (contents "a") "DATA")

    ;; 9. A rename that overwrote a file gives that file back too.
    (fresh! '("a" . "OLD") '("tmp" . "NEW"))
    (system (string-append "rm -f " (p "tmp") "; printf NEW > " (p "a")))
    (after! (string-append "(trace rename (" (p "tmp") " . " (p "a") ") #f)\n"))
    (want "an unsealed rename gives back the file it replaced"
          (list (contents "a") (contents "tmp")) '("OLD" "NEW"))

    ;; 10. Sealed operations stand -- the controls, one per operation.
    (fresh!)
    (crashed! '("a" . "X"))
    (after! (string-append "(trace create " (p "a") " #f)\n"
                           "(trace fsync " d " #f)\n"))
    (want "CONTROL: a sealed create stands" (listing) '("a"))
    (fresh! '("a" . "X"))
    (crashed! '("b" . "X"))
    (after! (string-append "(trace rename (" (p "a") " . " (p "b") ") #f)\n"
                           "(trace fsync " d " #f)\n"))
    (want "CONTROL: a sealed rename stands" (listing) '("b"))
    (fresh!)
    (crashed! '("linked" . "X"))
    (after! (string-append "(trace link " (p "linked") " #f)\n"
                           "(trace fsync " d " #f)\n"))
    (want "CONTROL: a sealed link stands" (listing) '("linked"))
    (fresh!)
    (after! (string-append "(trace unlink " (p "gone") " #f)\n"
                           "(trace fsync " d " #f)\n"))
    (want "CONTROL: a sealed unlink stands" (listing) '())

    ;; 11. A directory flush BEFORE the operation seals nothing.
    (fresh!)
    (crashed! '("a" . "X"))
    (after! (string-append "(trace fsync " d " #f)\n"
                           "(trace create " (p "a") " #f)\n"))
    (want "a directory flush before the operation seals nothing" (listing) '())

    (printf "\n~a failures\n" bad)
    (printf "crash complete\n")
    bad))

;; Run the self-check only when this file IS the script, and exit non-zero
;; if it fails: a device that reports its own failures only in text can be
;; loaded by a case that never looks.
(let ((argv (command-line)))
  (when (and (pair? argv) (crash-has-substring? (car argv) "crash.ss"))
    (let ((failures (crash-self-check)))
      (when (> failures 0) (exit 1)))))
