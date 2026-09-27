#!r6rs
(import (chezscheme) (theourgia store) (theourgia rpc) (theourgia reduce) (theourgia ffi)
        (theourgia code-project) (theourgia code-markers) (theourgia languages) (theourgia log) (theourgia datum-code))
(define bad 0)
(define (want label got expected)
  (if (equal? got expected) (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/datum-import-" (number->string (get-process-id))))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define counter 0)
(define scheme-entry (language-for-name 'scheme))
(define (write! path text)
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p (string->utf8 text)))))
(define (fresh)
  (set! counter (+ counter 1))
  (let* ((area (string-append root "/case-" (number->string counter)))
         (store (string-append area "/store")) (input (string-append area "/input")) (edit (string-append area "/edit")))
    (mkdir-p! input) (mkdir-p! edit) (rpc-dispatch store '(init) "test")
    (write! (string-append input "/a.sc") "(library (a) (export x y) (import (rnrs))\n;; x docs\n(define x 1)\n(define y 2)\n(use A)\n(use B)\n(use C))\n")
    (write! (string-append input "/b.sc") "(library (b) (export x) (import (rnrs)) (define x 99))\n")
    (unless (rpc-ok? (rpc-dispatch store (list 'import-code input "--datum") "test")) (error 'fixture "Initial import failed"))
    (let* ((state (open-and-reduce store))
           (a (find (lambda (id) (equal? (code-field state id 'name) '(a))) (map cadr (state-datum state))))
           (b (find (lambda (id) (equal? (code-field state id 'name) '(b))) (map cadr (state-datum state)))))
      (list store edit a (code-children state a) b state))))
(define (old-rows c)
  (map (lambda (id) (list id (code-field (list-ref c 5) id 'body) (code-field (list-ref c 5) id 'doc))) (list-ref c 3)))
(define (header c) (list (store-id-of (car c)) (list-ref c 2) (reduce-applied-cut (list-ref c 5))))
(define (render head rows exports imports)
  (string-append
    (if head (utf8->string (projection-header-line scheme-entry head 'datum 0)) "")
    "(library (a)\n" (datum-print (cons 'export exports)) (datum-print (cons 'import imports))
    (apply string-append
      (map (lambda (row)
             (string-append (if (car row) (utf8->string (marker-line scheme-entry (string-append "@block " (car row)))) "")
                            (caddr row) (datum-print (cadr row)))) rows)) ")\n"))
(define (edit! c rows . rest)
  (write! (string-append (cadr c) "/a.sc")
          (render (if (pair? rest) (car rest) (header c)) rows
                  (if (> (length rest) 1) (cadr rest) '(x y))
                  (if (> (length rest) 2) (caddr rest) '((rnrs))))))
(define (run c) (rpc-dispatch (car c) (list 'import-code (cadr c) "--datum") "test"))
(define (reason a) (let ((p (and (list? a) (assq 'reason (filter pair? a))))) (and p (cadr p))))
(define (body c id) (code-field (open-and-reduce (car c)) id 'body))
(for-each
  (lambda (cause)
    (let* ((c (fresh)) (ids (list-ref c 3)) (rows (old-rows c)) (h (header c))
           (last (case cause ((duplicate-id) (car ids)) ((foreign-library) (car (code-children (list-ref c 5) (list-ref c 4)))) (else (car (reverse ids))))))
      (when (eq? cause 'tombstone)
        (with-store-write (car c) (lambda (s v) (list (list 'del last))) "test")
        (set! h (list (car h) (cadr h) (reduce-applied-cut (open-and-reduce (car c))))))
      (when (eq? cause 'foreign-store) (set! h (cons "wrong-store" (cdr h))))
      (when (eq? cause 'unavailable-cut) (set! h (list (car h) (cadr h) '(("missing" . 999)))))
      (edit! c (append (list (list (car ids) '(define x 42) ";; x docs\n"))
                       (list-head (cdr rows) 3) (list (list last '(use C) ""))) h)
      (let* ((before (reduce-applied-cut (open-and-reduce (car c)))) (a (run c)))
        (want (string-append "CD-13 exact preflight reason " (symbol->string cause)) (reason a) cause)
        (want (string-append "CD-13 no earlier event on " (symbol->string cause)) (reduce-applied-cut (open-and-reduce (car c))) before)
        (want "CD-13 earlier valid body remains untouched" (body c (car ids)) '(define x 1)))))
  '(duplicate-id foreign-library foreign-store tombstone unavailable-cut))
(let* ((c (fresh)) (ids (list-ref c 3)) (rows (old-rows c)))
  (edit! c (cons (list #f '(define x 42) ";; x docs\n") (cdr rows)))
  (want "CD-09 unmarked primary name updates within target library" (rpc-ok? (run c)) #t)
  (want "CD-09 target name keeps original identity" (body c (car ids)) '(define x 42))
  (want "CD-09 same name in another library remains unchanged" (body c (car (code-children (list-ref c 5) (list-ref c 4)))) '(define x 99)))
(let* ((c (fresh)) (ids (list-ref c 3)) (rows (old-rows c)))
  (edit! c (cons (list (car ids) '(define y 42) ";; x docs\n") (cdr rows)))
  (want "CD-09 explicit ID wins when new name is already used" (rpc-ok? (run c)) #t)
  (want "CD-09 explicit rename edits the declared identity" (body c (car ids)) '(define y 42)))
(for-each
  (lambda (related?)
    (let* ((c (fresh)) (rows (old-rows c))
           (target (if related? (list-ref c 2) (list-ref c 4))))
      (edit! c (cons (list (caar rows) '(define x 42) ";; x docs\n") (cdr rows)))
      (with-store-write (car c) (lambda (s v) (list (list 'set target 'exports '(changed)))) "test")
      (let* ((before (reduce-applied-cut (open-and-reduce (car c)))) (a (run c)))
        (want (if related? "CD-14 changed library metadata refuses entire import" "CD-14 unrelated library metadata permits import") (rpc-ok? a) (not related?))
        (when related?
          (want "CD-14 stale import has no event" (reduce-applied-cut (open-and-reduce (car c))) before)
          (want "CD-14 carried exports cannot erase local edit" (code-field (open-and-reduce (car c)) target 'exports) '(changed)))))) '(#t #f))
(let* ((c (fresh)) (rows (old-rows c)) (ids (list-ref c 3)))
  (edit! c (append (list-head rows 2) (list (list #f '(use A) "") (list #f '(use X) "") (list #f '(use B) "") (list #f '(use C) ""))))
  (want "CD-10 real middle insertion succeeds" (rpc-ok? (run c)) #t)
  (let ((children (code-children (open-and-reduce (car c)) (list-ref c 2))))
    (want "CD-10 anonymous A identity survives" (list-ref children 2) (list-ref ids 2))
    (want "CD-10 anonymous B identity survives middle insertion" (list-ref children 4) (list-ref ids 3))
    (want "CD-10 anonymous C identity survives middle insertion" (list-ref children 5) (list-ref ids 4))))
(let* ((c (fresh)) (rows (old-rows c)) (ids (list-ref c 3)))
  (edit! c (list (cadr rows) (list (car ids) '(define x 1) ";; changed doc\n") (list-ref rows 4)) (header c) '(y) '((rnrs) (only (chezscheme) pretty-print)))
  (want "CD-12 doc deletion order imports and exports commit together" (rpc-ok? (run c)) #t)
  (let ((state (open-and-reduce (car c))))
    (want "CD-12 marker order is authoritative" (code-children state (list-ref c 2)) (list (cadr ids) (car ids) (list-ref ids 4)))
    (want "CD-12 doc-only edit is retained" (code-field state (car ids) 'doc) ";; changed doc\n")
    (want "CD-12 imports retain source order" (code-field state (list-ref c 2) 'imports) '((rnrs) (only (chezscheme) pretty-print)))
    (want "CD-12 exports update" (code-field state (list-ref c 2) 'exports) '(y))))
(let* ((c (fresh)) (ids (list-ref c 3)) (lib (list-ref c 2)))
  (edit! c '((#f (define x 42) "")) #f)
  (let* ((a (run c)) (state (open-and-reduce (car c))))
    (want "CD-15 a file without a header, of a path the store holds, updates that library: one library at the path, the same id"
          (list (and (pair? a) (car a)) (state-path-claimants state 'library 'datum "a.sc"))
          (list 'ok (list lib)))
    (want "CD-15 its one form updates the child it names, and every child it omits is kept, in place after it, and named"
          (list (code-field state (car ids) 'body) (code-children state lib)
                (let ((k (and (pair? a) (list? a) (assq 'kept (cdr a))))) k))
          (list '(define x 42) ids (list 'kept (list 'ids (cdr ids)))))))
(let* ((c (fresh)) (id (list-ref c 2)))
  (want "CD-17 explicit library def creates one form"
        (rpc-ok? (rpc-dispatch (car c) (list 'def "fresh" "--under" id "(define fresh 7)") "test")) #t)
  (want "CD-17 occupied name reports its existing identity"
        (cadr (rpc-dispatch (car c) (list 'def "x" "--under" id "(define x 8)") "test")) 'name-exists)
  (want "CD-17 extra active form refuses entire def"
        (reason (rpc-dispatch (car c) (list 'def "other" "--under" id "(define other 9) (define ignored 10)") "test")) 'expected-one-form)
  (want "CD-17 mismatched requested primary name is refused"
        (reason (rpc-dispatch (car c) (list 'def "other" "--under" id "(define actual 9)") "test")) 'name-mismatch))
(let* ((c (fresh)) (id (list-ref c 2)))
  (want "CD-17 record secondary binding cannot shadow existing name"
        (cadr (rpc-dispatch (car c) (list 'def "record" "--under" id "(define-record-type (record x record?) (fields))") "test")) 'name-exists))
(let* ((c (fresh)) (before (reduce-applied-cut (open-and-reduce (car c)))))
  (write! (string-append (cadr c) "/a.sc") "(library (short))")
  (want "CD-13 short library has a concrete refusal" (reason (run c)) 'invalid-library)
  (want "CD-13 short library produces no events" (reduce-applied-cut (open-and-reduce (car c))) before))
(let* ((c (fresh)))
  (write! (string-append (cadr c) "/plain.sc") "(import (rnrs) (only (chezscheme) pretty-print)) (define plain 1)")
  (want "CD-15 plain import declaration creates library metadata" (rpc-ok? (run c)) #t)
  (let* ((state (open-and-reduce (car c)))
         (lib (find (lambda (id) (equal? (code-field state id 'path) "plain.sc")) (map cadr (state-datum state)))))
    (want "CD-15 plain imports preserve declared order" (code-field state lib 'imports) '((rnrs) (only (chezscheme) pretty-print)))
    (want "CD-15 plain import declaration is not a code block" (length (code-children state lib)) 1)))
;; F19: THE ANSWER NAMES A COMMENT INSIDE A FORM, AND NOTHING FOR ONE ABOVE IT.
;; The README's import-code section and the catalogue's description say so in
;; one sentence; these rows hold the sentence to what the answer does. They
;; are a GUARD, green on the base: the answer already carried this before the
;; sentence was written, and the rows are here so that it cannot stop doing
;; so while the sentence stays.
(define (warnings-of answer)
  (let ((clause (and (list? answer) (assq 'warnings (filter pair? answer)))))
    (and clause (cadr clause))))
(define (import-one! name text)
  (let* ((area (string-append root "/" name)) (store (string-append area "/store"))
         (input (string-append area "/input")))
    (mkdir-p! input)
    (rpc-dispatch store '(init) "test")
    (write! (string-append input "/lib.sc") text)
    (rpc-dispatch store (list 'import-code input "--datum") "test")))
(want "F19 GUARD (green on the base): import-code --datum of a comment inside a form answers one internal-comment warning with its line and column"
      (warnings-of (import-one! "f19-inner"
                                "(library (demo inner)\n  (export f)\n  (import (rnrs))\n  (define (f x)\n    ;; inside a form\n    (+ x 1)))\n"))
      '((warning internal-comment (byte-offset 73) (line 5) (column 5))))
(want "F19 GUARD TWIN (green on the base): a comment only above the form answers no warning"
      (warnings-of (import-one! "f19-lead"
                                ";; leading comment\n(library (demo lead)\n  (export g)\n  (import (rnrs))\n  (define (g x) (* x 2)))\n"))
      '())
;; ---- which files a datum import reads -------------------------------------
;;
;; NEVER: A DATUM IMPORT READS SCHEME FILES ONLY, AND SAYS WHICH IT DID NOT.
;; A file is Scheme when the language table gives its path the scheme entry:
;; ss, sc, scm, sls, matched exactly and case-sensitively, the suffix after
;; the last dot of the file's own name. Every other regular file the walk
;; returns (it does not enter a name that starts with a dot) is listed in
;; `(skipped ...)`, in the walk's order, and the clause is absent when
;; nothing was skipped. A file the reader refuses is named by `(path ...)`.
(define (sel-area! name)
  (let* ((area (string-append root "/" name)) (store (string-append area "/store")) (input (string-append area "/input")))
    (mkdir-p! (string-append input "/sub"))
    (rpc-dispatch store '(init) "test")
    (cons store input)))
;; a.sc and sub/d.sls are Scheme; b.md, c.txt, e.SC and an extensionless f
;; are not -- e.SC and f hold Scheme text, so only the name decides.
(define (mixed-dir! input)
  (write! (string-append input "/a.sc") "(library (sel a) (export x) (import (rnrs)) (define x 1))\n")
  (write! (string-append input "/b.md") "# A heading\n\ntext\n")
  (write! (string-append input "/c.txt") "plain text\n")
  (write! (string-append input "/sub/d.sls") "(library (sel d) (export y) (import (rnrs)) (define y 2))\n")
  (write! (string-append input "/e.SC") "(library (sel e) (export z) (import (rnrs)) (define z 3))\n")
  (write! (string-append input "/f") "(library (sel f) (export w) (import (rnrs)) (define w 4))\n"))
(define (clause a head)
  (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) head))) (cdr a))))
(define (head-of a)
  (cond ((not (pair? a)) a)
        ((and (eq? (car a) 'error) (pair? (cdr a))) (list 'error (cadr a)))
        (else (car a))))
(define (datum-library-paths store)
  (let ((state (open-and-reduce store)))
    (list-sort string<?
      (map (lambda (id) (code-field state id 'path))
           (filter (lambda (id) (and (eq? (code-field state id 'kind) 'library) (eq? (code-field state id 'mode) 'datum)))
                   (map cadr (state-datum state)))))))
(define (block-count store) (length (state-datum (open-and-reduce store))))
(define (cut-of store) (reduce-applied-cut (open-and-reduce store)))

;; D1: the Scheme files are imported, the rest listed.
(let* ((sa (sel-area! "sel-d1")) (store (car sa)) (input (cdr sa)))
  (mixed-dir! input)
  (let ((a (rpc-dispatch store (list 'import-code input "--datum") "test")))
    (want "D1 a datum import of a mixed directory answers ok, imports a.sc and sub/d.sls, and lists b.md, c.txt, e.SC and f as skipped"
          (list (head-of a) (datum-library-paths store) (clause a 'skipped))
          '(ok ("a.sc" "sub/d.sls") (skipped ("b.md" "c.txt" "e.SC" "f"))))))

;; D2: a refusal names its file, and nothing is imported (the refusal comes
;; before any write).
(let* ((sa (sel-area! "sel-d2")) (store (car sa)) (input (cdr sa))
       (bad-text ";; a comment\n(define x 1)\n# not a datum\n")
       (k (let loop ((i 0)) (if (char=? (string-ref bad-text i) #\#) i (loop (+ i 1))))))
  (write! (string-append input "/a.sc") bad-text)
  (write! (string-append input "/b.sc") "(library (sel b) (export x) (import (rnrs)) (define x 1))\n")
  (let* ((before (list (block-count store) (cut-of store)))
         (a (rpc-dispatch store (list 'import-code input "--datum") "test")))
    (want "D2 a file the reader refuses is named: bad-source with its reason, offset and (path \"a.sc\"), and nothing is imported"
          (list a (equal? before (list (block-count store) (cut-of store))) (datum-library-paths store))
          (list (list 'error 'bad-source '(reason unsupported-reader-dispatch) (list 'character-offset k) '(path "a.sc"))
                #t '()))))

;; D3: def reads its source from an argument, so its refusal has no file to
;; name, and keeps the lexer's shape. PIN: the base answers the same.
(let* ((sa (sel-area! "sel-d3")) (store (car sa))
       (a (rpc-dispatch store (list 'def "x" ";; a comment\n(define x 1)\n# not a datum\n") "test")))
  (want "D3 PIN def with the same bad source refuses bad-source with no path clause"
        (list (head-of a) (clause a 'path))
        '((error bad-source) #f)))

;; D4: nothing to read. WITHOUT a request identity: ok with no items, every
;; file listed, and the store unchanged -- the no-op is visible by its list.
;; (With --req an empty plan is written; that is another row's.)
(let* ((sa (sel-area! "sel-d4")) (store (car sa)) (input (cdr sa)))
  (write! (string-append input "/b.md") "# A heading\n")
  (write! (string-append input "/c.txt") "plain text\n")
  (let* ((before (list (block-count store) (cut-of store)))
         (a (rpc-dispatch store (list 'import-code input "--datum") "test")))
    (want "D4 a datum import of a directory with no Scheme file answers ok, no items, every file skipped, and writes nothing"
          (list (head-of a) (clause a 'items) (clause a 'skipped) (equal? before (list (block-count store) (cut-of store))))
          '(ok (items) (skipped ("b.md" "c.txt")) #t))))

;; D5: nothing skipped, no clause. The ok and the imports are asserted first,
;; so an answer that refused cannot pass for one without the clause.
(let* ((sa (sel-area! "sel-d5")) (store (car sa)) (input (cdr sa)))
  (write! (string-append input "/a.sc") "(library (sel a) (export x) (import (rnrs)) (define x 1))\n")
  (write! (string-append input "/b.sc") "(library (sel b) (export y) (import (rnrs)) (define y 2))\n")
  (let ((a (rpc-dispatch store (list 'import-code input "--datum") "test")))
    (want "D5 PIN a directory of Scheme files only answers ok, imports each, and carries no skipped clause"
          (list (head-of a) (datum-library-paths store) (clause a 'skipped))
          '(ok ("a.sc" "b.sc") #f))))

;; D7: THE SELECTION READS THE TABLE AS IT IS NOW. Scheme registered again,
;; its extensions kept and one more property added, is still Scheme: a.sc is
;; read. The table is put back as it was, whatever the import answered.
(let* ((sa (sel-area! "sel-d7")) (store (car sa)) (input (cdr sa)) (original (language-for-name 'scheme)))
  (write! (string-append input "/a.sc") "(library (sel a) (export x) (import (rnrs)) (define x 1))\n")
  (let ((a (dynamic-wind
             (lambda () (register-language! (cons '(review-note #t) original)))
             (lambda () (rpc-dispatch store (list 'import-code input "--datum") "test"))
             (lambda () (register-language! original)))))
    (want "D7 with Scheme registered again, extensions unchanged and a property added, a.sc is still read and imported"
          (list (head-of a) (datum-library-paths store) (clause a 'skipped))
          '(ok ("a.sc") #f))))

;; D6: PIN, the text import is not this rule's: over the same mixed
;; directory it imports every file, with the language the table gives the
;; path, and none for a path the table does not know.
(let* ((sa (sel-area! "sel-d6")) (store (car sa)) (input (cdr sa)))
  (mixed-dir! input)
  (let* ((a (rpc-dispatch store (list 'import-code input) "test"))
         (state (open-and-reduce store))
         (files (filter (lambda (id) (and (eq? (code-field state id 'kind) 'file) (eq? (code-field state id 'mode) 'text)))
                        (map cadr (state-datum state)))))
    (want "D6 PIN the text import over the mixed directory imports every file, lang scheme or markdown by the table, none for c.txt, e.SC and f"
          (list (head-of a)
                (list-sort (lambda (x y) (string<? (car x) (car y)))
                           (map (lambda (id) (list (code-field state id 'path) (code-field state id 'lang))) files)))
          '(ok (("a.sc" scheme) ("b.md" markdown) ("c.txt" #f) ("e.SC" #f) ("f" #f) ("sub/d.sls" scheme))))))

;; ---- a raw re-import of a path the store holds ------------------------------
;;
;; NEVER: A FILE WITHOUT A HEADER, OF A PATH THE STORE HOLDS, UPDATES THAT
;; LIBRARY. None holds it: a new library. One holds it: that library, its
;; forms matched as a headed file's are, nothing deleted -- what the file
;; omits is kept and named in `(kept (ids (...)))`. Several hold it: refused
;; by name, path and ids, nothing written. The holders are captured and
;; checked again at the write. Every duplicate-path refusal names its path
;; and ids; check and conflicts report a duplicated path.
(define (safely thunk)
  (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e)))) (thunk)))
(define (answer-clause a head)
  (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) head))) (cdr a))))
(define (answer-head a)
  (cond ((not (pair? a)) a)
        ((and (eq? (car a) 'error) (pair? (cdr a))) (list 'error (cadr a)))
        (else (car a))))
(define (deep-member? x tree)
  (or (equal? x tree) (and (pair? tree) (or (deep-member? x (car tree)) (deep-member? x (cdr tree))))))
;; -> (store input writer)
(define (raw-area! name)
  (let* ((area (string-append root "/" name)) (store (string-append area "/store")) (input (string-append area "/input")))
    (mkdir-p! input)
    (let ((init (rpc-dispatch store '(init) "test")))
      (list store input (cadr (assq 'writer (cdr init)))))))
(define p-text "(library (p) (export f g) (import (rnrs))\n(define (f) 1)\n(define (g) 2))\n")
(define q-text "(library (q) (export h) (import (rnrs))\n(define (h) 3))\n")
(define (import-raw! store dir . extra)
  (safely (lambda () (rpc-dispatch store (append (list 'import-code dir "--datum") extra) "test"))))
(define (holders store rel) (state-path-claimants (open-and-reduce store) 'library 'datum rel))
(define (children-of store lib) (code-children (open-and-reduce store) lib))
(define (seq-of store writer)
  (let ((p (assoc writer (reduce-applied-cut (open-and-reduce store))))) (if p (cdr p) 0)))
(define (cursor-after store writer) (string-append writer ":" (number->string (+ 1 (seq-of store writer)))))
(define (second-library! store rel)
  (with-store-write store
    (lambda (s v) (list (list 'insert 'root #f (list '(kind . library) '(mode . datum) '(lang . chez) (cons 'path rel)))))
    "other"))

;; U1: the same raw directory imported twice is one library per path.
(let* ((ra (raw-area! "u1")) (store (car ra)) (input (cadr ra)))
  (write! (string-append input "/p.sc") p-text)
  (write! (string-append input "/q.sc") q-text)
  (import-raw! store input)
  (let* ((first (list (holders store "p.sc") (holders store "q.sc")))
         (outline-before (state-outline (open-and-reduce store)))
         (a (import-raw! store input))
         (w (safely (lambda () (rpc-dispatch store '(whereis "f") "test")))))
    (want "U1 a second raw import of the same directory answers ok, keeps one library per path by id, the outline as it was, no kept clause, and whereis finds one def"
          (list (answer-head a) (equal? first (list (holders store "p.sc") (holders store "q.sc"))) (length (car first))
                (equal? outline-before (state-outline (open-and-reduce store))) (answer-clause a 'kept)
                (let ((it (answer-clause w 'items))) (and it (length (filter (lambda (r) (and (pair? r) (eq? (car r) 'def))) (cdr it))))))
          '(ok #t 1 #t #f 1))))

;; U2: an edited raw file: one body changed, one form added, one omitted.
(let* ((ra (raw-area! "u2")) (store (car ra)) (input (cadr ra)))
  (write! (string-append input "/p.sc") p-text)
  (import-raw! store input)
  (let* ((lib (car (holders store "p.sc"))) (old (children-of store lib)) (f-id (car old)) (g-id (cadr old)))
    (write! (string-append input "/p.sc") "(library (p) (export f k) (import (rnrs))\n(define (f) 10)\n(define (k) 4))\n")
    (let* ((a (import-raw! store input)) (state (open-and-reduce store)) (now (code-children state lib)))
      (want "U2 the library keeps its id; f keeps its identity with the new body; k is a new child; g is kept, named, and last"
            (list (answer-head a) (holders store "p.sc") (code-field state f-id 'body) (length now) (car now)
                  (and (pair? (cdr now)) (not (member (cadr now) old)) (code-field state (cadr now) 'body))
                  (list-ref now (- (length now) 1)) (answer-clause a 'kept))
            (list 'ok (list lib) '(define (f) 10) 3 f-id '(define (k) 4) g-id (list 'kept (list 'ids (list g-id))))))))

;; U1b: a path is held per kind. (i) PIN: a text file block of p.sc does
;; not make a raw datum import of p.sc an update: the first datum library
;; is created, the text block left. (ii) with both, the datum import
;; updates the library and leaves the text block.
(let* ((ra (raw-area! "u1b")) (store (car ra)) (input (cadr ra)))
  (write! (string-append input "/p.sc") p-text)
  (safely (lambda () (rpc-dispatch store (list 'import-code input) "test")))
  (let* ((text-files (state-path-claimants (open-and-reduce store) 'file 'text "p.sc"))
         (a (import-raw! store input)))
    (want "U1b (i) PIN a store holding only a text file of p.sc: the datum import creates the first datum library, and the text file stays"
          (list (answer-head a) (length (holders store "p.sc")) (state-path-claimants (open-and-reduce store) 'file 'text "p.sc"))
          (list 'ok 1 text-files))
    (let* ((lib (holders store "p.sc")) (b (import-raw! store input)))
      (want "U1b (ii) with both, the datum import updates the library and leaves the text file"
            (list (answer-head b) (holders store "p.sc") (state-path-claimants (open-and-reduce store) 'file 'text "p.sc"))
            (list 'ok lib text-files)))))

;; U3, U4, U5: a store already holding two datum libraries at one path.
(let* ((ra (raw-area! "u3")) (store (car ra)) (input (cadr ra)) (out (string-append root "/u3-out")))
  (write! (string-append input "/p.sc") p-text)
  (import-raw! store input)
  (second-library! store "p.sc")
  (let* ((two (holders store "p.sc")) (before (reduce-applied-cut (open-and-reduce store)))
         (a (import-raw! store input)))
    (want "U3 a raw import of a path two libraries hold refuses duplicate-path with the path and both ids, and writes nothing"
          (list a (equal? before (reduce-applied-cut (open-and-reduce store))) (length two))
          (list (list 'error 'projection-invalid '(reason duplicate-path) '(path "p.sc") (list 'ids two)) #t 2))
    (mkdir-p! out)
    (want "U4 export-code --datum of that store refuses with the path and both ids"
          (safely (lambda () (rpc-dispatch store (list 'export-code out "--datum") "test")))
          (list 'error 'projection-invalid '(reason duplicate-path) '(path "p.sc") (list 'ids two)))
    (let ((conflicts (safely (lambda () (rpc-dispatch store '(conflicts) "test"))))
          (check (safely (lambda () (rpc-dispatch store '(check) "test")))))
      (want "U5 conflicts lists the duplicated path; check lists it under paths, and its verdict is still ok"
            (list (deep-member? (list 'duplicate-path "p.sc" (list 'ids two)) conflicts)
                  (deep-member? (list 'paths (list (list 'duplicate-path "p.sc" (list 'ids two)))) check)
                  (deep-member? '(verdict ok) check))
            '(#t #t #t)))))

;; U4b: two TEXT files sharing a path: the text export names the path and
;; both ids.
(let* ((ra (raw-area! "u4b")) (store (car ra)) (input (cadr ra)) (out (string-append root "/u4b-out")))
  (write! (string-append input "/x.py") "def one():\n    return 1\n")
  (safely (lambda () (rpc-dispatch store (list 'import-code input) "test")))
  (with-store-write store
    (lambda (s v) (list (list 'insert 'root #f (list '(kind . file) '(mode . text) '(path . "x.py") '(lang . python)))))
    "other")
  (let ((two (state-path-claimants (open-and-reduce store) 'file 'text "x.py")))
    (mkdir-p! out)
    (want "U4b export-code of two text files sharing a path refuses with the path and both ids"
          (list (safely (lambda () (rpc-dispatch store (list 'export-code out) "test"))) (length two))
          (list (list 'error 'projection-invalid '(reason duplicate-path) '(path "x.py") (list 'ids two)) 2))))

;; U6 PIN: the headed round trip -- export, then import the export -- is
;; still idempotent.
(let* ((ra (raw-area! "u6")) (store (car ra)) (input (cadr ra)) (out (string-append root "/u6-out")))
  (write! (string-append input "/p.sc") p-text)
  (import-raw! store input)
  (mkdir-p! out)
  (safely (lambda () (rpc-dispatch store (list 'export-code out "--datum") "test")))
  (want "U6 PIN importing the export back answers ok with no items and no warnings"
        (import-raw! store out)
        '(ok (items) (warnings ()))))

;; U7, U7b, U7c: the capture and the write, apart. The first call carries a
;; request identity and a cursor one past the writer's end, so it captures
;; and is then refused before writing (the cursor cannot be reached); the
;; change between the two is what makes the cursor reachable; the retry,
;; the same request, writes from the captured packet. Its preflight runs
;; against the CHANGED state -- the write session's own reduction, after the
;; change -- while its baselines and holders are the capture's: that
;; difference is what these rows measure.
(define (interleaved! name setup! between! dir-text)
  (let* ((ra (raw-area! name)) (store (car ra)) (input (cadr ra)) (w (caddr ra)))
    (setup! store input)
    (write! (string-append input (car dir-text)) (cadr dir-text))
    (let* ((cursor (cursor-after store w))
           (first (import-raw! store input "--req" name "--cursor" cursor)))
      (between! store input)
      (list first (import-raw! store input "--req" name "--cursor" cursor) store))))
;; U7: another write to the matched library between capture and write.
(let ((r (interleaved! "u7"
           (lambda (store input) (write! (string-append input "/p.sc") p-text) (import-raw! store input))
           (lambda (store input)
             (let ((f-id (car (children-of store (car (holders store "p.sc"))))))
               (with-store-write store (lambda (s v) (list (list 'set f-id 'doc ";; changed\n"))) "other")))
           (list "/p.sc" "(library (p) (export f g) (import (rnrs))\n(define (f) 100)\n(define (g) 2))\n"))))
  (want "U7 a raw re-import whose matched library changed between capture and write is refused stale-baseline; still one library"
        (list (answer-head (car r)) (answer-head (cadr r)) (length (holders (caddr r) "p.sc")))
        '((error cursor-unreachable) (error stale-baseline) 1)))
;; U7b: a second library takes the path between capture and write.
(let ((r (interleaved! "u7b"
           (lambda (store input) (write! (string-append input "/p.sc") p-text) (import-raw! store input))
           (lambda (store input) (second-library! store "p.sc"))
           (list "/p.sc" "(library (p) (export f g) (import (rnrs))\n(define (f) 100)\n(define (g) 2))\n"))))
  (want "U7b a second library taking the path between capture and write refuses stale-baseline, changed-claimants"
        (list (answer-head (car r)) (cadr r))
        '((error cursor-unreachable) (error stale-baseline (path "p.sc") (reason changed-claimants)))))
;; U7c: two imports of one NEW path: both capture no holder, B writes, then A.
(let ((r (interleaved! "u7c"
           (lambda (store input) #t)
           (lambda (store input) (import-raw! store input))
           (list "/n.sc" "(library (n) (export m) (import (rnrs))\n(define (m) 5))\n"))))
  (want "U7c of two imports of one new path, the second to write refuses stale-baseline, changed-claimants, and one library holds it"
        (list (answer-head (car r)) (cadr r) (length (holders (caddr r) "n.sc")))
        '((error cursor-unreachable) (error stale-baseline (path "n.sc") (reason changed-claimants)) 1)))

;; U2c: a raw file that holds only the library's SECOND form. That form is
;; placed first and the kept first form follows it: the file's order, then
;; what it kept.
(let* ((ra (raw-area! "u2c")) (store (car ra)) (input (cadr ra)))
  (write! (string-append input "/p.sc") p-text)
  (import-raw! store input)
  (let* ((lib (car (holders store "p.sc"))) (old (children-of store lib)) (f-id (car old)) (g-id (cadr old)))
    (write! (string-append input "/p.sc") "(library (p) (export g) (import (rnrs))\n(define (g) 20))\n")
    (let ((a (import-raw! store input)))
      (want "U2c a raw file holding only the second form: that form first, the kept first form after it, and named"
            (list (answer-head a) (children-of store lib) (answer-clause a 'kept))
            (list 'ok (list g-id f-id) (list 'kept (list 'ids (list f-id))))))))

;; U2d: a raw file that holds only a NEW form. The new form comes first, and
;; both old forms are kept after it, in their order.
(let* ((ra (raw-area! "u2d")) (store (car ra)) (input (cadr ra)))
  (write! (string-append input "/p.sc") p-text)
  (import-raw! store input)
  (let* ((lib (car (holders store "p.sc"))) (old (children-of store lib)))
    (write! (string-append input "/p.sc") "(library (p) (export k) (import (rnrs))\n(define (k) 4))\n")
    (let* ((a (import-raw! store input)) (now (children-of store lib)))
      (want "U2d a raw file holding only a new form: the new form first, both old forms kept after it, in order, and named"
            (list (answer-head a) (length now) (and (pair? now) (not (member (car now) old))) (and (pair? now) (cdr now))
                  (answer-clause a 'kept))
            (list 'ok 3 #t old (list 'kept (list 'ids old)))))))

;; U8 PIN: an import of headed files loads nothing it did not load before.
;; A headed file of ANOTHER store, imported where the writers cannot be
;; read, is refused foreign-store, before any read of the writers.
(let* ((ra (raw-area! "u8a")) (store-a (car ra)) (input (cadr ra)) (out (string-append root "/u8-out"))
       (rb (raw-area! "u8b")) (store-b (car rb)))
  (write! (string-append input "/p.sc") p-text)
  (import-raw! store-a input)
  (mkdir-p! out)
  (safely (lambda () (rpc-dispatch store-a (list 'export-code out "--datum") "test")))
  (system (string-append "chmod 000 " store-b "/writers"))
  (let ((a (import-raw! store-b out)))
    (system (string-append "chmod 755 " store-b "/writers"))
    (want "U8 PIN a headed file of another store, where the writers cannot be read, is refused foreign-store, not unreadable"
          (list (answer-head a) (answer-clause a 'reason))
          '((error projection-invalid) (reason foreign-store)))))

(printf "~a failures\ndatum-import complete\n" bad)
(exit (if (zero? bad) 0 1))
