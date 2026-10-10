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

;; Small batch 2, the main session's rows: F73 (the outline rows are
;; sorted, and callers rely on it), F79 (a writers/ directory that cannot be
;; listed is answered by name), F81 (an interned actor of illegal shape is
;; refused by the shape rule), F82 and F83 (export-md and import-md on a
;; directory that does not exist answer the tree's directory refusal).
(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) state-outline)
        (only (theourgia wire) encode-record))

(define bad 0)
(define rows 0)
(define (want-1 label got expect)
  (set! rows (+ rows 1))
  (if (equal? got expect)
      (printf "ok   ~a -> ~s\n" label got)
      (begin (set! bad (+ bad 1))
             (printf "FAIL ~a -> ~s   WANT ~s\n" label got expect))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) x)))))

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                   (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/sb2-" (number->string (get-process-id))))
(when (file-exists? root)
  (assertion-violation 'sb2 "scratch directory already exists" root))
(system (string-append "mkdir -p " root))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(putenv "THEOURGIA_RUN" (string-append root "/run"))

(define n 0)
(define (fresh-store!)
  (set! n (+ n 1))
  (let ((s (string-append root "/s" (number->string n))))
    (system (string-append "mkdir -p " s))
    (rpc-dispatch s '(init) "test")
    s))
(define (ask store . req) (rpc-dispatch store req "test"))
(define (head a) (and (pair? a) (car a)))
(define (field a name)
  (let ((f (and (pair? a) (list? a) (find (lambda (x) (and (pair? x) (eq? (car x) name))) (cdr a)))))
    (and f (pair? (cdr f)) (cadr f))))
(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (chmod! mode path) (system (string-append "chmod " mode " " path)))
(define (file-text path)
  (if (not (file-exists? path)) ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))
;; THE NEW ID IS READ FROM THE OUTLINE, as the F85 cells do: the insert
;; answer does not carry it.
(define (ids-in-outline s) (map caddr (state-outline (open-and-reduce s))))
(define (insert! s . args)
  (let ((before (ids-in-outline s)))
    (apply ask s 'insert args)
    (let loop ((xs (ids-in-outline s)))
      (cond ((null? xs) #f)
            ((member (car xs) before) (loop (cdr xs)))
            (else (car xs))))))

;; ---- F73: the outline rows are sorted by (parent, ord, id) -------------------
(printf "== F73: the outline rows are sorted, and the sort is the promise ==\n")
(define (parent-key p) (if (symbol? p) (symbol->string p) p))
(define (row<? x y)
  (if (equal? (car x) (car y))
      (if (= (cadr x) (cadr y)) (string<? (caddr x) (caddr y)) (< (cadr x) (cadr y)))
      (string<? (parent-key (car x)) (parent-key (car y)))))
(define (sorted? rows)
  (or (null? rows) (null? (cdr rows))
      (and (not (row<? (cadr rows) (car rows))) (sorted? (cdr rows)))))
(let* ((s (fresh-store!))
       (a (insert! s "--under" "root" "--title" "A"))
       (b (insert! s "--under" "root" "--title" "B"))
       (c (insert! s "--under" a "--title" "C"))
       (d (insert! s "--under" a "--title" "D"))
       ;; inserted last, placed between C and D: its id is the newest, its
       ;; ord is between theirs, so id order and ord order disagree
       (e (insert! s "--under" a "--title" "E" "--after" c))
       (f (insert! s "--under" b "--title" "F"))
       (rows (state-outline (open-and-reduce s))))
  (want "CONTROL F73-1 the store has six placed blocks under two parents and the root"
        (list (length rows) (and (string? a) (string? b) (string? c) (string? d) (string? e) (string? f)))
        '(6 #t))
  (want "F73-1 the outline rows come sorted by (parent, ord, id): under A the order is C E D, not the insertion order C D E"
        (let ((under-a (filter (lambda (r) (equal? (car r) a)) rows)))
          (list (sorted? rows) (equal? (map caddr under-a) (list c e d))))
        '(#t #t)))

;; ---- F79: writers/ itself unlistable ------------------------------------------
(printf "== F79: a writers/ directory this process cannot list ==\n")
(let* ((s (fresh-store!))
       (writers (string-append s "/writers")))
  (ask s 'insert "--under" "root" "--title" "A")
  (chmod! "000" writers)
  (let ((a (caught (ask s 'check))))
    (chmod! "700" writers)
    (want "F79-1 check with writers/ at 000 answers unreadable by name, naming writers/, and neither internal nor a raise"
          (cond ((and (pair? a) (eq? (car a) 'RAISED)) (list 'raised (cadr a)))
                ((and (eq? (head a) 'error) (pair? (cdr a)) (eq? (cadr a) 'unreadable))
                 (list 'unreadable (and (string? (field a 'path)) (contains? (field a 'path) "/writers"))))
                (else (list 'answered (head a) (and (pair? (cdr a)) (cadr a)))))
          '(unreadable #t))
    (want "F79-1 TWIN: readable again, check's verdict is ok"
          (let ((c (ask s 'check))) (list (head c) (field c 'verdict)))
          '(check ok))))

;; ---- F81: an interned actor of illegal shape ----------------------------------
(printf "== F81: the shape rule, told apart from the intern check ==\n")
(define (six who identity sub fp plan after) (list who identity sub fp plan after))
(define (encodes? actor)
  (guard (e (#t #f)) (encode-record 0 0 actor '() '(a)) #t))
(want "CONTROL F81-1 a well-shaped actor encodes"
      (encodes? (six "agent:claude" (cons "w" "req-1") 'single "fp" #f (cons "w" 3)))
      #t)
(want "F81-1 an actor whose symbols are all interned but whose sub slot is neither single, plan nor an index is refused: the shape rule, not the intern check"
      (encodes? (six "agent:claude" (cons "w" "req-1") 'nope "fp" #f (cons "w" 3)))
      #f)
(want "F81-1 TWIN: an interned symbol where the pair of writer and request id must be is refused too"
      (encodes? (six "agent:claude" 'w-req-1 'single "fp" #f (cons "w" 3)))
      #f)

;; ---- F82 / F83: a directory that does not exist -------------------------------
(printf "== F82 / F83: export-md and import-md on an absent directory ==\n")
(let* ((s (fresh-store!))
       (absent (string-append root "/absent-" (number->string (get-process-id))))
       (present (string-append root "/present")))
  ;; a document, so the export has a file to write
  (let ((a (insert! s "--under" "root" "--title" "A" "--text" "hello")))
    (ask s 'set a "kind" "doc")
    (ask s 'set a "path" "a.md"))
  (system (string-append "mkdir -p " present))
  (want "CONTROL F82-1 export-md to an existing empty directory answers ok and writes a file"
        (let ((a (ask s 'export-md present)))
          (list (head a) (> (length (directory-list present)) 0)))
        '(ok #t))
  (want "F82-1 export-md to a directory that does not exist answers projection-invalid not-a-directory naming it, and creates nothing"
        (let ((a (ask s 'export-md absent)))
          (list (head a) (and (pair? (cdr a)) (cadr a)) (field a 'reason) (equal? (field a 'dir) absent)
                (file-exists? absent)))
        '(error projection-invalid not-a-directory #t #f))
  (want "F83-1 import-md from a directory that does not exist answers the same refusal, not internal"
        (let ((a (ask s 'import-md absent)))
          (list (head a) (and (pair? (cdr a)) (cadr a)) (field a 'reason) (equal? (field a 'dir) absent)))
        '(error projection-invalid not-a-directory #t))
  ;; A REGULAR FILE WHERE A DIRECTORY IS NAMED is the same defect: a path that
  ;; cannot hold files answered (ok (files 0)) on the base. Ruled L with this
  ;; row (small batch 2, code session's reading on 877f0da).
  (let ((file (string-append root "/a-file")))
    (call-with-output-file file (lambda (p) (put-string p "not a directory\n")) 'replace)
    (want "F82-1 TWIN: export-md to a path that is a regular file answers the same refusal, and the file's bytes are untouched"
          (let ((a (ask s 'export-md file)))
            (list (head a) (and (pair? (cdr a)) (cadr a)) (field a 'reason) (equal? (field a 'dir) file)
                  (file-text file)))
          '(error projection-invalid not-a-directory #t "not a directory\n"))
    (want "F83-1 TWIN: import-md from a path that is a regular file answers the same refusal"
          (let ((a (ask s 'import-md file)))
            (list (head a) (and (pair? (cdr a)) (cadr a)) (field a 'reason) (equal? (field a 'dir) file)))
          '(error projection-invalid not-a-directory #t)))
  ;; A PATH WITH A COMPONENT THIS PROCESS CANNOT SEARCH is answered by name,
  ;; not as ok-with-nothing or internal: the directory test is R1's type
  ;; question, stat of <sealed>/inner fails EACCES, and unreadable-entry is
  ;; answered by every route (review r1 C-N1, ruled L). The target itself at
  ;; 000 is NOT this case: stat needs search permission on the parent, so
  ;; the test passes and the failure comes later at the write or the listing
  ;; (raw, internal, as on the base): queued as F98.
  (let* ((sealed (string-append root "/sealed"))
         (inner (string-append sealed "/inner")))
    (system (string-append "mkdir -p " inner))
    (chmod! "000" sealed)
    (let ((e (ask s 'export-md inner))
          (i (ask s 'import-md inner)))
      (chmod! "700" sealed)
      (want "F82-1 TWIN: export-md to a directory under one this process cannot search answers unreadable naming the path, and writes nothing"
            (list (head e) (and (pair? (cdr e)) (cadr e))
                  (and (string? (field e 'path)) (contains? (field e 'path) inner))
                  (null? (directory-list inner)))
            '(error unreadable #t #t))
      (want "F83-1 TWIN: import-md from a directory under one this process cannot search answers unreadable naming the path"
            (list (head i) (and (pair? (cdr i)) (cadr i))
                  (and (string? (field i 'path)) (contains? (field i 'path) inner)))
            '(error unreadable #t)))))

;; ---- F98: a target that stats but cannot be listed or written ---------------
(printf "== F98: the target directory itself at 000 ==\n")
;; The directory test passes (stat needs search on the parent); the failure
;; comes at the write (export) or the listing (import) and must be named,
;; never internal.
(let* ((s (fresh-store!))
       (target (string-append root "/target000")))
  (let ((a (insert! s "--under" "root" "--title" "A" "--text" "hello")))
    (ask s 'set a "kind" "doc")
    (ask s 'set a "path" "a.md"))
  (system (string-append "mkdir -p " target))
  (chmod! "000" target)
  (let ((e (ask s 'export-md target))
        (i (ask s 'import-md target)))
    (chmod! "700" target)
    (want "F98-1 export-md to a directory it cannot write into answers unreadable naming the file it could not write, not internal"
          (list (head e) (and (pair? (cdr e)) (cadr e))
                (and (string? (field e 'path)) (contains? (field e 'path) target)))
          '(error unreadable #t))
    (want "F98-2 import-md from a directory it cannot list answers unreadable naming it, not internal"
          (list (head i) (and (pair? (cdr i)) (cadr i))
                (and (string? (field i 'path)) (contains? (field i 'path) target)))
          '(error unreadable #t))))

;; AN EXPORT STOPPED PART WAY SAYS HOW FAR IT GOT (F98, the brief's "with the
;; files written so far named in the answer"). Two documents, one at the top
;; and one under a sub-directory at 000; whichever the export reaches first,
;; the answer's `written` must be exactly the files that are on disk
;; afterwards, and `path` the file it could not write.
(let* ((s (fresh-store!))
       (target (string-append root "/partial"))
       (sub (string-append target "/sub")))
  ;; The export writes documents in id order, so the one with the smaller id
  ;; gets the writable path: the file written first is then always there to
  ;; be named, and an answer that named nothing could not pass.
  (let* ((x (insert! s "--under" "root" "--title" "A" "--text" "top"))
         (y (insert! s "--under" "root" "--title" "B" "--text" "below"))
         (first (if (string<? x y) x y))
         (second (if (string<? x y) y x)))
    (for-each (lambda (id) (ask s 'set id "kind" "doc")) (list x y))
    (ask s 'set first "path" "a.md")
    (ask s 'set second "path" "sub/b.md"))
  (system (string-append "mkdir -p " sub))
  ;; MODE 500, NOT 000 (the merge onto F100a): the export lists the target
  ;; before it writes, and a directory at 000 cannot be listed, so the
  ;; answer named nothing written and nothing was. At 500 the listing
  ;; succeeds and the write into it is the one refused.
  (chmod! "500" sub)
  (let ((e (ask s 'export-md target)))
    (chmod! "700" sub)
    (let ((on-disk (filter (lambda (f) (file-exists? (string-append target "/" f)))
                           '("a.md" "sub/b.md")))
          (named (let ((w (and (pair? e) (list? e)
                               (find (lambda (x) (and (pair? x) (eq? (car x) 'written))) (cdr e)))))
                   (and w (cadr w)))))
      (want "F98-3 export-md stopped at a file it cannot write names the files it had already written, and they are exactly the ones on disk"
            (list (head e) (and (pair? (cdr e)) (cadr e))
                  (and (string? (field e 'path)) (contains? (field e 'path) "sub/b.md"))
                  on-disk
                  (and (list? named) (equal? named on-disk)))
            '(error unreadable #t ("a.md") #t)))))

(system (string-append "chmod -R u+rwx " root " 2>/dev/null; rm -rf " root))
(printf "\n~a failures\nrows: ~a\nsb2 complete\n" bad rows)
