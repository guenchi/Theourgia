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

;; `supply`: facts an editor computed from an export-code projection, kept
;; beside the store in <store>/derived/ and never in it.
;;
;; Every digest a supply file carries here is the sha256 of a file a real
;; `export-code` wrote, of the committed store or of a writer's working
;; view; every id is one `grep` answered. A row that expects a refusal
;; changes ONE thing from a supply the fixture has seen accepted, so the
;; refusal is the answer to that one thing.
;;
;; These rows read the table's own file where no reader exists yet: the
;; answer, the file's name and its body are the interface this commit
;; makes. The readers that judge the facts fresh or stale come with their
;; own rows.
(import (chezscheme) (theourgia rpc) (theourgia ffi)
        (only (theourgia store) open-and-reduce)
        (only (theourgia code-project) code-files code-field code-children)
        (only (theourgia render) render-human)
        (only (theourgia languages) register-language!)
        (only (theourgia derived) percent-encode)
        (only (theourgia reduce) state-hash reduce-applied-cut)
        (only (theourgia evidence-index) index-checkpoint! index-forget-memory!)
        (only (theourgia wire) storable-decode storable-encode string->sexpr-extended sexpr->string-extended)
        (only (theourgia log) store-id-of)
        (only (theourgia crc32) crc32-hex)
        (only (theourgia digest) sha256 bytevector->hex))

(include "forge-record.ss")

;; THE VALUES A ROW READS ARE TAKEN IN THE ORDER THEY ARE WRITTEN. A row
;; that writes and then reads names both in one form, and R6RS leaves the
;; order of a procedure call's arguments open (Chez takes them as it
;; pleases), so `list` would let a read run before the write it follows.
(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))
(define bad 0)
(define rows 0)
(define (want name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/supply-" (number->string (get-process-id))))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))

;; The parent directory is made when it is missing: a mutant that writes no
;; table leaves no derived/, and a row that plants a table there then
;; compares what the readers make of it rather than stopping the fixture.
(define (write-bytes! path bv)
  (let loop ((i (- (string-length path) 1)))
    (cond ((< i 1) #f)
          ((char=? (string-ref path i) #\/) (mkdir-p! (substring path 0 i)))
          (else (loop (- i 1)))))
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p bv))))
(define (write! path s) (write-bytes! path (string->utf8 s)))
;; #f when there is no file; #vu8() when it is empty, where get-bytevector-all
;; answers the eof object.
(define (bytes-of path)
  (and (file-exists? path)
       (let ((b (call-with-port (open-file-input-port path) get-bytevector-all)))
         (if (eof-object? b) (make-bytevector 0) b))))
(define (text-of path) (let ((b (bytes-of path))) (if (bytevector? b) (utf8->string b) (and b ""))))
(define (sha-of path) (bytevector->hex (sha256 (bytes-of path))))
(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

;; The first list anywhere in `x` headed by `head`, or #f.
(define (find-headed x head)
  (cond ((and (pair? x) (eq? (car x) head)) x)
        ((pair? x) (or (find-headed (car x) head) (find-headed (cdr x) head)))
        (else #f)))
(define (head-of a)
  (cond ((not (pair? a)) a)
        ((and (eq? (car a) 'error) (pair? (cdr a))) (list 'error (cadr a)))
        (else (car a))))

(define counter 0)
(define (fresh-dir! name)
  (set! counter (+ counter 1))
  (let ((d (string-append root "/" name "-" (number->string counter)))) (mkdir-p! d) d))

;; ---- the store --------------------------------------------------------------
;; Two javascript files and an empty one: a.js with alpha and beta, b.js with
;; gamma, empty.js with no block at all.
;;
;; IMPORT MAKES ONE BLOCK PER FILE unless the file carries marker lines, so
;; a.js is written with a `// @block new` line before each function (the
;; boundary an import respects, code-markers1 CT-16/CT-18); the markers are
;; not part of any block's src. An empty file is imported as one empty
;; block, and deleting that block leaves a file with no blocks.
(define store (string-append root "/store"))
(define src (fresh-dir! "src"))
(write! (string-append src "/a.js") "// @block new\nfunction alpha() {\n  return 1;\n}\n// @block new\nfunction beta() {\n  return 2;\n}\n")
(write! (string-append src "/b.js") "function gamma() {\n  return alpha();\n}\n")
(write! (string-append src "/empty.js") "")
(define (run . args) (rpc-dispatch store args "test"))
(run 'init)
;; What init wrote, read before any supply could have touched it.
(define ignore-at-init (text-of (string-append store "/.gitignore")))
(define imported (run 'import-code src))
(define (holder needle)
  (let ((m (find-headed (run 'grep needle) 'match))) (and m (string? (cadr m)) (cadr m))))
;; A block's own src bytes, as a plain read gives its fields (a code block's
;; src is bytes; `read --md` answers text fields only).
(define (src-bytes-of answer)
  (let* ((b (and (pair? answer) (eq? (car answer) 'ok) (cadr answer)))
         (fs (and b (assq 'fields b)))
         (e (and fs (assq 'src (cdr fs)))))
    (cond ((not e) #f) ((bytevector? (cdr e)) (cdr e)) ((string? (cdr e)) (string->utf8 (cdr e))) (else #f))))
(define alpha (holder "function alpha"))
(define beta (holder "function beta"))
(define gamma (holder "function gamma"))
;; The empty file's one block, and its deletion.
(define empty-children
  (let* ((s (open-and-reduce store))
         (f (find (lambda (id) (equal? (code-field s id 'path) "empty.js")) (code-files s))))
    (if f (code-children s f) '())))
(define empty-deleted
  (map (lambda (id) (head-of (run 'del id))) empty-children))

;; -> the directory the export wrote.
(define (export! . options)
  (let ((d (fresh-dir! "export")))
    (apply run 'export-code d options)
    d))
(define e0 (export!))
(define (sha0 name) (sha-of (string-append e0 "/" name)))
(want "S0 setup: import answered ok; alpha, beta and gamma are three distinct blocks; empty.js's one block deleted; the export wrote a.js, b.js and empty.js, and empty.js holds no @block line"
      (in-order (head-of imported) (and alpha beta gamma (not (equal? alpha beta)) (not (equal? beta gamma)) (not (equal? alpha gamma)))
            empty-deleted
            (map (lambda (n) (file-exists? (string-append e0 "/" n))) '("a.js" "b.js" "empty.js"))
            (let ((t (text-of (string-append e0 "/empty.js"))))
              (and (string? t)
                   (let loop ((i 0))
                     (cond ((> (+ i 6) (string-length t)) #f)
                           ((string=? (substring t i (+ i 6)) "@block") #t)
                           (else (loop (+ i 1))))))))
      (list 'ok #t '(ok) '(#t #t #t) #f))

;; ---- supply files -----------------------------------------------------------
(define (line x) (format "~s\n" x))
(define (header kind writer files replaces . language)
  (line (list 'supply kind (list 'writer writer)
              (list 'language (if (pair? language) (car language) "javascript"))
              '(source (vscode "1.140.0")) (list 'files files) (list 'replaces replaces))))
(define (files-of . names) (map (lambda (n) (list n (sha0 n))) names))
(define (signature id . depends) (line (list 'signature id "() => number" '(kind function) (list 'depends (cons id depends)))))

;; -> the answer to `supply` of a file holding `text`, in store `st`.
(define (supply-in st text . args)
  (let ((path (string-append (fresh-dir! "supply") "/supply.sexp")))
    (write! path text)
    (rpc-dispatch st (append (list 'supply) args (list path)) "test")))
(define (supply text . args) (apply supply-in store text args))
(define derived (string-append store "/derived"))
(define (table name) (string-append derived "/" name))
(define js-table (table "signatures-%2D-javascript.sexp"))
;; The table's datum: (derived-table 1 <store-id> <digest> (<kind> <writer> <language> (<fact> ...))).
;; #f when there is no table, and #f when the bytes are not a datum: a
;; table that does not read is a reading for the row, not a stop.
(define (table-datum path)
  (let ((b (bytes-of path)))
    (and b (let ((d (guard (e (#t #f)) (storable-decode (string->sexpr-extended (utf8->string b))))))
             (and (list? d) (= 5 (length d)) d)))))
;; A TABLE THAT DOES NOT READ ANSWERS (unreadable-table), NOT A CONDITION.
;; The rows that take facts apart would otherwise stop the fixture at the
;; first table a mutant corrupted, and every row after it -- the one aimed
;; at that mutant included -- would never run. '() when there is no table.
(define (table-facts path)
  (let ((d (table-datum path)))
    (cond ((not (bytes-of path)) '())
          ((and (list? d) (= 5 (length d)) (list? (list-ref d 4)) (= 4 (length (list-ref d 4)))
                (list? (cadddr (list-ref d 4))))
           (cadddr (list-ref d 4)))
          (else '(unreadable-table)))))
(define (facts? x) (and (list? x) (for-all (lambda (f) (and (list? f) (= 6 (length f)) (eq? (car f) 'fact))) x)))
;; `f` of each fact of the table at `path`, or the table's refusal value as it is.
(define (fact-column f path)
  (let ((fs (table-facts path))) (if (facts? fs) (map f fs) fs)))
(define (malformed line reason) (list 'error 'supply-malformed (list 'line line) (list 'reason reason)))

;; ---- D2: a stale file is refused before anything is written ------------------------
(let ((a (supply (string-append (header 'signatures "-" (list (list "a.js" (sha0 "b.js"))) '("a.js"))
                                (signature alpha beta))
                 "signatures")))
  (want "D2 a.js listed with another file's digest: supply-stale naming a.js, and no derived directory is made"
        (in-order a (file-exists? derived))
        (list '(error supply-stale (file "a.js")) #f)))

;; ---- A1: an accepted supply -----------------------------------------------------------
(define a1-text (string-append (header 'signatures "-" (files-of "a.js" "b.js") '("a.js" "b.js"))
                               (signature alpha beta) (signature gamma)))
(let ((a (supply a1-text "signatures")))
  (want "A1 a signatures supply of the committed store is kept: two facts, two replaced files"
        a '(ok (supplied (facts 2) (files 2)))))
(want "A1 the table is derived/signatures-%2D-javascript.sexp: this store's id, the sha256 of its body's encoding, the kind, writer and language, beside derived/lock"
      (let ((d (table-datum js-table)))
        (list (and d (car d)) (and d (cadr d)) (and d (equal? (caddr d) (store-id-of store)))
              (and d (equal? (cadddr d) (bytevector->hex (sha256 (string->utf8 (sexpr->string-extended
                                                                                   (storable-encode (list-ref d 4))))))))
              (and d (let ((body (list-ref d 4))) (list (car body) (cadr body) (caddr body) (length (cadddr body)))))
              (file-exists? (string-append derived "/lock"))))
      (list 'derived-table 1 #t #t (list 'signatures "-" "javascript" 2) #t))
(want "A1 each fact is kept against its own file, with the depends stamped by id and the file by path, and the provenance"
      (fact-column (lambda (f) (list (cadr f) (caddr f) (map car (list-ref f 3)) (map car (list-ref f 4)) (list-ref f 5)))
           js-table)
      (list (list "a.js" (list 'signature alpha "() => number" '(kind function)) (list alpha beta) '("a.js")
                  '(vscode "1.140.0" "javascript"))
            (list "b.js" (list 'signature gamma "() => number" '(kind function)) (list gamma) '("b.js")
                  '(vscode "1.140.0" "javascript"))))
(want "A1 a dependency's stamp is the sha256 of the block's src bytes as read; a file's stamp is not the file's digest"
      (let ((f (let ((fs (table-facts js-table)))
                 (if (and (facts? fs) (pair? fs)) (car fs) '(fact "" () (("" "")) (("" "")) ())))))
        (list (equal? (cadr (car (list-ref f 3)))
                      (bytevector->hex (sha256 (src-bytes-of (run 'read alpha)))))
              (= 64 (string-length (cadr (car (list-ref f 4)))))
              (equal? (cadr (car (list-ref f 4))) (sha0 "a.js"))))
      '(#t #t #f))
(want "A1 a table with bytes after its datum: table-facts answers (unreadable-table) and table-datum #f, rather than stopping the fixture"
      (let ((p (string-append (fresh-dir! "trailing") "/t.sexp")))
        (in-order (begin (write! p "(derived-table 1 \"s\" \"d\" (signatures \"-\" \"javascript\" ())) (trailing)") 'written)
                  (table-facts p) (table-datum p) (fact-column cadr p)))
      (list 'written '(unreadable-table) #f '(unreadable-table)))

;; ---- D12: only a dependency file's digest is wrong ------------------------------------
(let* ((before (bytes-of js-table))
       (a (supply (string-append (header 'signatures "-" (list (list "a.js" (sha0 "a.js")) (list "b.js" (sha0 "a.js")))
                                         '("a.js"))
                                 (signature alpha beta))
                  "signatures")))
  (want "D12 files (a.js b.js), replaces (a.js), b.js's digest wrong: supply-stale naming b.js, the table byte-identical"
        (in-order a (equal? (bytes-of js-table) before))
        (list '(error supply-stale (file "b.js")) #t)))

;; ---- D31: a file with no blocks is digest-checked like any other ---------------------
(let* ((before (bytes-of js-table))
       (wrong (supply (string-append (header 'signatures "-" (list (list "a.js" (sha0 "a.js")) (list "empty.js" (sha0 "a.js")))
                                             '("a.js"))
                                     (signature alpha beta))
                      "signatures"))
       (after-wrong (bytes-of js-table))
       (right (supply (string-append (header 'signatures "-" (files-of "a.js" "empty.js") '("a.js" "empty.js"))
                                     (signature alpha beta))
                      "signatures")))
  (want "D31 empty.js with a wrong digest is supply-stale naming it (table unchanged); with its own digest the supply is kept"
        (in-order wrong (equal? after-wrong before) right)
        (list '(error supply-stale (file "empty.js")) #t '(ok (supplied (facts 1) (files 2))))))

;; ---- R8: replacement is per file --------------------------------------------------------
(supply a1-text "signatures")
(let ((a (supply (header 'signatures "-" (files-of "a.js" "b.js") '("a.js")) "signatures")))
  (want "R8 an empty supply replacing a.js keeps b.js's fact and drops a.js's"
        (in-order a (fact-column cadr js-table))
        (list '(ok (supplied (facts 0) (files 1))) '("b.js"))))
(supply a1-text "signatures")

;; ---- the order of the checks, and every malformed reason ------------------------------
;; Each row starts from a1-text, accepted above, and changes one thing.
(define (refused-in tbl name text expected . args)
  (let* ((before (bytes-of tbl)) (a (apply supply text (if (null? args) '("signatures") args))))
    (want (string-append name ", and the table is byte-identical") (list a (equal? (bytes-of tbl) before))
          (list expected #t))))
(define (refused name text expected . args) (apply refused-in js-table name text expected args))
(refused "M1 line 1 not a header: header" (string-append (line '(nonsense)) (signature alpha beta)) (malformed 1 'header))
(refused "M2 an empty file: header" "" (malformed 1 'header))
(refused "M3 the header's kind is calls, the command's signatures: kind-mismatch"
         (string-append (header 'calls "-" (files-of "a.js" "b.js") '("a.js" "b.js")) (signature alpha beta))
         (malformed 1 'kind-mismatch))
(refused "M4 the header's writer is w-1 and there is no --for: writer-mismatch"
         (string-append (header 'signatures "w-1" (files-of "a.js" "b.js") '("a.js" "b.js")) (signature alpha beta))
         (malformed 1 'writer-mismatch))
(refused "M5 the header says - and --for is w-1: writer-mismatch"
         a1-text (malformed 1 'writer-mismatch) "signatures" "--for" "w-1")
(refused "M6 replaces names a file files does not list: replaces-not-listed"
         (string-append (header 'signatures "-" (files-of "a.js") '("a.js" "b.js")) (signature alpha beta))
         (malformed 1 'replaces-not-listed))
(refused "M7 line 2 does not read: unreadable"
         (string-append (header 'signatures "-" (files-of "a.js" "b.js") '("a.js" "b.js")) "(signature \"x\"\n")
         (malformed 2 'unreadable))
(refused "M8 a calls line in a signatures supply: fact-shape"
         (string-append (header 'signatures "-" (files-of "a.js" "b.js") '("a.js" "b.js"))
                        (line (list 'calls gamma alpha (list 'depends (list gamma alpha)))))
         (malformed 2 'fact-shape))
(refused "M9 depends led by another block: depends-not-led-by-subject"
         (string-append (header 'signatures "-" (files-of "a.js" "b.js") '("a.js" "b.js"))
                        (line (list 'signature alpha "t" '(kind function) (list 'depends (list beta alpha)))))
         (malformed 2 'depends-not-led-by-subject))
(refused "M10 an id no projected file holds: unknown-id"
         (string-append (header 'signatures "-" (files-of "a.js" "b.js") '("a.js" "b.js")) (signature "nope.1"))
         (malformed 2 'unknown-id))
(refused "M11 a dependency in b.js, which files does not list: dependency-file-not-listed"
         (string-append (header 'signatures "-" (files-of "a.js") '("a.js")) (signature alpha gamma))
         (malformed 2 'dependency-file-not-listed))
(refused "M11b depends naming a block of an unlisted file, then an unknown id: unknown-id, whatever the order"
         (string-append (header 'signatures "-" (files-of "a.js") '("a.js")) (signature alpha gamma "nope.1"))
         (malformed 2 'unknown-id))
(refused "M12 a fact on gamma, whose file replaces does not name: own-file-not-replaced"
         (string-append (header 'signatures "-" (files-of "a.js" "b.js") '("a.js")) (signature gamma))
         (malformed 2 'own-file-not-replaced))
(refused "M13 an empty line is skipped and the numbering is the file's: the bad fact on line 4"
         (string-append (header 'signatures "-" (files-of "a.js" "b.js") '("a.js" "b.js")) (signature alpha beta) "\n"
                        (signature "nope.1"))
         (malformed 4 'unknown-id))
(refused "M14 a wrong digest and an unknown id: supply-stale, which precedes the facts' reasons"
         (string-append (header 'signatures "-" (list (list "a.js" (sha0 "b.js")) (list "b.js" (sha0 "b.js"))) '("a.js" "b.js"))
                        (signature "nope.1"))
         '(error supply-stale (file "a.js")))
(refused "M15 a wrong digest and a fact line that does not read: the line's shape is judged first"
         (string-append (header 'signatures "-" (list (list "a.js" (sha0 "b.js")) (list "b.js" (sha0 "b.js"))) '("a.js" "b.js"))
                        "(signature\n")
         (malformed 2 'unreadable))
;; A diagnostic's range is judged against its own file's projection.
(define a-size (bytevector-length (bytes-of (string-append e0 "/a.js"))))
(define a-text (utf8->string (bytes-of (string-append e0 "/a.js"))))
(define (a-offset needle)
  (let loop ((i 0)) (if (string=? (substring a-text i (+ i (string-length needle))) needle) i (loop (+ i 1)))))
(define (diagnostic id s e) (line (list 'diagnostic id 'error "m" (list 'range s e) (list 'depends (list id)))))
(define diag-table (table "diagnostics-%2D-javascript.sexp"))
(refused-in diag-table "M16 a diagnostic whose range ends past a.js: range-outside-file"
         (string-append (header 'diagnostics "-" (files-of "a.js" "b.js") '("a.js")) (diagnostic alpha 0 (+ a-size 1)))
         (malformed 2 'range-outside-file) "diagnostics")
(refused-in diag-table "M17 a diagnostic on beta whose range lies in alpha's body: id-range-mismatch"
         (let ((s (a-offset "return 1"))) (string-append (header 'diagnostics "-" (files-of "a.js" "b.js") '("a.js"))
                                                         (diagnostic beta s (+ s 6))))
         (malformed 2 'id-range-mismatch) "diagnostics")
(let* ((s (a-offset "return 1"))
       (a (supply (string-append (header 'diagnostics "-" (files-of "a.js" "b.js") '("a.js")) (diagnostic alpha s (+ s 6)))
                  "diagnostics")))
  (want "M18 control: the same range on alpha is kept, stored in alpha's own src"
        (in-order a (fact-column caddr diag-table))
        (list '(ok (supplied (facts 1) (files 1)))
              (list (list 'diagnostic alpha 'error "m" (list 'at 21 27))))))

;; ---- the view: the committed store, or the writer's working view -------------------
;; w-2 drafts beta; its working projection of a.js differs from the committed one.
(run 'write beta "function beta() {\n  return 3;\n}\n" "--writer" "w-2")
(define e2 (export! "--working" "--writer" "w-2"))
(define (sha2 name) (sha-of (string-append e2 "/" name)))
(define w2-files (list (list "a.js" (sha2 "a.js")) (list "b.js" (sha2 "b.js"))))
(want "V0 setup: w-2's working a.js differs from the committed one, b.js does not"
      (in-order (equal? (sha2 "a.js") (sha0 "a.js")) (equal? (sha2 "b.js") (sha0 "b.js")))
      '(#f #t))
(want "V1 --for w-2 with the committed digests: supply-stale naming a.js"
      (supply (string-append (header 'signatures "w-2" (files-of "a.js" "b.js") '("a.js")) (signature alpha beta))
              "signatures" "--for" "w-2")
      '(error supply-stale (file "a.js")))
(want "V2 --for w-2 with its working digests: kept, in derived/signatures-w%2D2-javascript.sexp"
      (in-order (supply (string-append (header 'signatures "w-2" w2-files '("a.js")) (signature alpha beta))
                    "signatures" "--for" "w-2")
            (and (table-datum (table "signatures-w%2D2-javascript.sexp")) #t))
      (list '(ok (supplied (facts 1) (files 1))) #t))
(want "V3 the committed store with w-2's working digests: supply-stale naming a.js"
      (supply (string-append (header 'signatures "-" w2-files '("a.js")) (signature alpha beta)) "signatures")
      '(error supply-stale (file "a.js")))

;; ---- tables per language, and --clear ------------------------------------------------
(let* ((before (bytes-of js-table))
       (a (supply (string-append (header 'signatures "-" (files-of "a.js" "b.js") '("a.js") "python") (signature alpha beta))
                  "signatures")))
  (want "C1 the same kind and writer in another language is another table; the javascript one is byte-identical"
        (in-order a (and (table-datum (table "signatures-%2D-python.sexp")) #t) (equal? (bytes-of js-table) before))
        (list '(ok (supplied (facts 1) (files 1))) #t #t)))
(let ((a (supply (header 'signatures "-" '() '() "python") "signatures" "--clear")))
  (want "C2 --clear removes the table its header names, and only that one"
        (in-order a (file-exists? (table "signatures-%2D-python.sexp")) (file-exists? js-table))
        (list '(ok (cleared (kind signatures) (writer "-") (language "python"))) #f #t)))
(want "C3 --clear of a table that is not there answers the same"
      (supply (header 'signatures "-" '() '() "python") "signatures" "--clear")
      '(ok (cleared (kind signatures) (writer "-") (language "python"))))
(want "C4 --clear judges the header as a supply does: writer-mismatch"
      (supply (header 'signatures "w-2" '() '() "python") "signatures" "--clear")
      (malformed 1 'writer-mismatch))
(want "C5 an unknown kind is answered with the usage form"
      (supply a1-text "types")
      '(usage (supply <kind> <file> ["--for" <writer>] ["--clear"])))

;; ---- .gitignore --------------------------------------------------------------------------
(define ignore (string-append store "/.gitignore"))
(define ignore-init ignore-at-init)
(supply a1-text "signatures")
(want "G1 init's .gitignore, read before any supply, holds the /derived/ line, and a supply leaves it alone"
      (in-order (contains? ignore-at-init "\n/derived/\n") (text-of ignore))
      (list #t ignore-at-init))
(write! ignore "custom")
(supply a1-text "signatures")
(want "G2 a .gitignore without the line, and without a final newline, keeps what it held and gains the line"
      (text-of ignore) "custom\n/derived/\n")
(delete-file ignore)
(supply a1-text "signatures")
(want "G3 with no .gitignore, supply makes one holding the line" (text-of ignore) "/derived/\n")
(write! ignore ignore-init)

;; ---- D11: nothing a supply does reaches the log ----------------------------------------
(define (writers-snapshot)
  (let walk ((dir (string-append store "/writers")))
    (apply append
      (map (lambda (n)
             (let ((p (string-append dir "/" n)))
               (if (file-directory? p) (walk p) (list (cons p (bytes-of p))))))
           (list-sort string<? (directory-list dir))))))
(define (reading)
  (let ((s (open-and-reduce store))) (list (state-hash s) (reduce-applied-cut s))))
(index-forget-memory!)
(index-checkpoint! store)
(define checkpoint-before (bytes-of (string-append store "/request-index.sexp")))
(define log-before (writers-snapshot))
(define reading-before (reading))
(supply a1-text "signatures")
(supply (header 'signatures "-" (files-of "a.js" "b.js") '("a.js")) "signatures")
(supply (header 'signatures "-" '() '()) "signatures" "--clear")
(index-forget-memory!)
(index-checkpoint! store)
(want "D11 after a supply, a replacement and a clear: every file under writers/ byte-identical, the state hash and cut unchanged, the evidence checkpoint byte-identical"
      (in-order (equal? (writers-snapshot) log-before) (equal? (reading) reading-before)
            (equal? (bytes-of (string-append store "/request-index.sexp")) checkpoint-before))
      '(#t #t #t))

;; ---- D10: the reserved relation names ------------------------------------------------------
(define (edge verb rel) (head-of (run verb alpha rel beta)))
(want "D10 control before: link and unlink under explains are accepted"
      (in-order (edge 'link "explains") (edge 'unlink "explains")) '(ok ok))
(want "D10 link refuses ref, uses, calls and guards by name"
      (map (lambda (r) (run 'link alpha r beta)) '("ref" "uses" "calls" "guards"))
      (map (lambda (r) (list 'error 'reserved-relation (list 'relation r))) '(ref uses calls guards)))
(want "D10 unlink refuses the same four"
      (map (lambda (r) (run 'unlink alpha r beta)) '("ref" "uses" "calls" "guards"))
      (map (lambda (r) (list 'error 'reserved-relation (list 'relation r))) '(ref uses calls guards)))
(want "D10 control after: link under explains is accepted"
      (edge 'link "explains") 'ok)

;; ---- the exporter's refusal is the supply's -------------------------------------------
;; A block that is not code, placed in a file, makes the file unexportable;
;; supply re-projects and answers what export-code answers.
(define file-a
  (let ((s (open-and-reduce store)))
    (find (lambda (id) (equal? (code-field s id 'path) "a.js")) (code-files s))))
(define note (run 'insert "--under" (or file-a "root") "--title" "a note" "--text" "not code"))
(let ((x (run 'export-code (fresh-dir! "refused")))
      (s (supply a1-text "signatures")))
  (want "X1 a file holding a block that is not code: supply answers export-code's own refusal, unexportable-block"
        (in-order (and file-a #t) (head-of note) (head-of s) (equal? s x) (and (pair? s) (assq 'reason (cddr s))))
        '(#t ok (error projection-invalid) #t (reason unexportable-block))))

;; ==== the readers: read --signature, outline --with-signatures, search ====
;;
;; Each scenario has a store of its own, three javascript files -- a.js with
;; alpha and beta, b.js with gamma, c.js with delta -- so no row depends on
;; what another did to its store. A supply here is always made the way the
;; editor makes one: the view is exported, every projected file is listed
;; with its digest, and the header names the writer whose view it was.
(define js-a "// @block new\nfunction alpha() {\n  return 1;\n}\n// @block new\nfunction beta() {\n  return 2;\n}\n")
(define js-b "function gamma() {\n  return alpha();\n}\n")
(define js-c "function delta() {\n  return 4;\n}\n")
;; THE RECEIPT A READ NOW ENDS WITH -- a trailing (versions ...), then a
;; trailing (cut ...) -- is read-receipt.sc's to check; these rows compare
;; what the answer said before it, so they read the answer without it.
(define (without-receipt a)
  (let* ((drop (lambda (a head)
                 (if (and (pair? a) (list? a) (pair? (cdr a))
                          (let ((l (list-ref a (- (length a) 1)))) (and (pair? l) (eq? (car l) head))))
                     (list-head a (- (length a) 1))
                     a))))
    (drop (drop a 'versions) 'cut)))
(define (ask st . args) (without-receipt (rpc-dispatch st args "test")))
(define (src-bytes st id) (src-bytes-of (ask st 'read id)))
;; EACH SCENARIO STORE'S OWN WRITER, by name, as the one directory under
;; writers/ when the store is made. A record is forged for that writer by
;; name: a working view opened later for another writer ("-" included)
;; makes a directory of its own, and the first entry is then whichever the
;; listing puts first.
(define own-writers (make-hashtable string-hash string=?))
(define (own-writer st) (hashtable-ref own-writers st #f))
;; The name for a path: a store made with other than one writer directory
;; has no own writer, and a row naming its segment then reads no file
;; rather than stopping the fixture.
(define (own-writer-name st) (or (own-writer st) "no-own-writer"))
(define (forge-own! st payload-text) (forge-record-as! st (own-writer st) payload-text))
;; 'forged, or the refusal forge-record-as! answered.
(define (forged st payload-text)
  (let ((n (forge-own! st payload-text))) (if (integer? n) 'forged n)))
(define (make-store! name)
  (let ((st (string-append root "/" name)) (src (fresh-dir! (string-append name "-src"))))
    (write! (string-append src "/a.js") js-a)
    (write! (string-append src "/b.js") js-b)
    (write! (string-append src "/c.js") js-c)
    (let* ((i (ask st 'init)) (m (ask st 'import-code src))
           (ids (map (lambda (n) (id-in st (string-append "function " n))) '("alpha" "beta" "gamma" "delta"))))
      ;; EVERY SCENARIO'S STORE SAYS IT WAS MADE AS THE ROWS ASSUME, so a
      ;; failed import or a different split is red here, by name.
      (want (string-append "setup " name ": init and import answer ok; alpha, beta, gamma and delta are four distinct blocks")
            (list (head-of i) (head-of m) (length (filter string? ids))
                  (let loop ((xs ids)) (or (null? xs) (and (not (member (car xs) (cdr xs))) (loop (cdr xs))))))
            '(ok ok 4 #t)))
    (let ((wdir (string-append st "/writers")))
      (hashtable-set! own-writers st
                      (and (file-directory? wdir)
                           (let ((ws (directory-list wdir))) (and (= 1 (length ws)) (car ws))))))
    st))
(define (id-in st needle)
  (let ((m (find-headed (ask st 'grep needle) 'match))) (and m (string? (cadr m)) (cadr m))))
(define (file-in st path)
  (let ((s (open-and-reduce st)))
    (find (lambda (id) (equal? (code-field s id 'path) path)) (code-files s))))
(define (export-in st . options)
  (let ((d (fresh-dir! "x"))) (apply ask st 'export-code d options) d))
(define (projection-files d)
  (list-sort string<? (filter (lambda (n) (not (file-directory? (string-append d "/" n)))) (directory-list d))))
;; -> the answer to a supply of `lines` made from the view `writer` names:
;; every file that view projects, listed with its digest.
(define (supply-now st writer lines replaces . opts)
  (let* ((version (if (pair? opts) (car opts) "1.140.0"))
         (language (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) "javascript"))
         (d (if (equal? writer "-") (export-in st) (export-in st "--working" "--writer" writer)))
         (files (map (lambda (n) (list n (sha-of (string-append d "/" n)))) (projection-files d))))
    (apply supply-in st
           (apply string-append
                  (line (list 'supply 'signatures (list 'writer writer) (list 'language language)
                              (list 'source (list 'vscode version)) (list 'files files) (list 'replaces replaces)))
                  lines)
           "signatures" (if (equal? writer "-") '() (list "--for" writer)))))
(define (sig id text . depends) (line (list 'signature id text '(kind function) (list 'depends (cons id depends)))))
(define (kw id words . depends) (line (list 'keywords id words (list 'depends (cons id depends)))))
(define (sig-of st id . options) (apply ask st 'read id "--signature" options))
(define (via version . language) (list 'via (list 'vscode version (if (pair? language) (car language) "javascript"))))
;; The clauses of an answer that consulted a table: (stale <n>), then the via.
(define (present text . version) (list 'ok (list 'signature text) '(stale 0) (via (if (pair? version) (car version) "1.140.0"))))
(define absent-stale-1 '(ok (signature absent) (stale 1) (via)))
(define absent-consulted '(ok (signature absent) (stale 0) (via)))
(define (lines-of s)
  (let loop ((cs (string->list s)) (cur '()) (acc '()))
    (cond ((null? cs) (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
          ((char=? (car cs) #\newline) (loop (cdr cs) '() (cons (list->string (reverse cur)) acc)))
          (else (loop (cdr cs) (cons (car cs) cur) acc)))))
(define (has-substring? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
;; The outline row drawn for `id`, or #f.
(define (outline-row text id)
  (find (lambda (l) (has-substring? l (string-append "- " id "  "))) (lines-of text)))
(define (outline-with-signatures st) (ask st 'outline "--with-signatures"))
(define (text-of-answer a) (and (pair? a) (pair? (cdr a)) (pair? (cadr a)) (cadr (cadr a))))
(define (clauses-after-text a) (if (and (pair? a) (list? a) (> (length a) 2)) (cddr a) '()))
(define (row-signature text id)
  (let ((r (outline-row text id)))
    (and r (let loop ((i 0))
             (cond ((> (+ i 4) (string-length r)) 'none)
                   ((string=? (substring r i (+ i 4)) "  ::") (substring r (+ i 5) (string-length r)))
                   (else (loop (+ i 1))))))))

;; ---- D1, D9: one signature, and no provenance without a table --------------------
(define r1 (make-store! "r1"))
(define r1-alpha (id-in r1 "function alpha"))
(define r1-beta (id-in r1 "function beta"))
(define r1-gamma (id-in r1 "function gamma"))
(define r1-delta (id-in r1 "function delta"))
(define r1-plain (ask r1 'read r1-alpha))
(want "D9 control: before any supply, --signature answers absent with no via and no stale; outline --with-signatures carries no clause"
      (in-order (and r1-alpha r1-beta r1-gamma r1-delta #t) (sig-of r1 r1-alpha)
            (clauses-after-text (outline-with-signatures r1)))
      (list #t '(ok (signature absent)) '()))
;; alpha's signature depends on delta in c.js (a dependency of alpha only);
;; gamma's comes from another supply, of another version.
(want "D1 two supplies: alpha's (1.140.0, a.js and c.js) and gamma's (1.141.0, b.js)"
      (in-order (supply-now r1 "-" (list (sig r1-alpha "alpha(): number" r1-beta r1-delta)) '("a.js" "c.js"))
            (supply-now r1 "-" (list (sig r1-gamma "gamma(): number")) '("b.js") "1.141.0"))
      '((ok (supplied (facts 1) (files 2))) (ok (supplied (facts 1) (files 1)))))
(want "D1 read --signature answers each block's text, (stale 0) and its own provenance; beta, with no fact, is absent with (stale 0) and an empty via"
      (in-order (sig-of r1 r1-alpha) (sig-of r1 r1-gamma) (sig-of r1 r1-beta))
      (list (present "alpha(): number") (present "gamma(): number" "1.141.0") absent-consulted))
(want "D1 a plain read of alpha is what it was before any supply"
      (ask r1 'read r1-alpha) r1-plain)
(want "D1 --signature with --md, --recursive or --working-info is refused; of an unknown id, unknown-id"
      (in-order (ask r1 'read r1-alpha "--signature" "--md") (ask r1 'read r1-alpha "--signature" "--recursive")
            (ask r1 'read r1-alpha "--signature" "--working-info") (head-of (sig-of r1 "nope.1")))
      (list '(error bad-request incompatible-signature-options) '(error bad-request incompatible-signature-options)
            '(error bad-request incompatible-signature-options) '(error unknown-id)))
(want "D8 setup: outline draws a.js's rows (alpha) before b.js's (gamma), the order the via below lists them in"
      (let* ((t (text-of-answer (ask r1 'outline))) (ls (lines-of t))
             (at (lambda (id) (let loop ((ls ls) (i 0))
                                (cond ((null? ls) #f)
                                      ((has-substring? (car ls) (string-append "- " id "  ")) i)
                                      (else (loop (cdr ls) (+ i 1))))))))
        (and (at r1-alpha) (at r1-gamma) (< (at r1-alpha) (at r1-gamma))))
      #t)
(want "D1 the writer named - is the committed tables' name: read --signature --working and diagnostics refuse it"
      (in-order (sig-of r1 r1-alpha "--working" "--writer" "-") (ask r1 'diagnostics "--writer" "-"))
      '((error reserved-writer (writer "-")) (error reserved-writer (writer "-"))))
(let* ((a (outline-with-signatures r1)) (t (text-of-answer a)))
  (want "D8/D9 outline --with-signatures: alpha's and gamma's rows end in their signatures, beta's has none, and the via names both provenances"
        (in-order (row-signature t r1-alpha) (row-signature t r1-gamma) (row-signature t r1-beta)
              (clauses-after-text a))
        (list "alpha(): number" "gamma(): number" 'none
              (list '(stale 0) (list 'via '(vscode "1.140.0" "javascript") '(vscode "1.141.0" "javascript"))))))
(want "D8 the human rendering of that outline prints the via line after the listing"
      (let ((h (render-human (outline-with-signatures r1))))
        (has-substring? h "\n(via (vscode \"1.140.0\" \"javascript\") (vscode \"1.141.0\" \"javascript\"))\n"))
      #t)
(want "D9 outline without --with-signatures carries no clause and no signature column"
      (let ((a (ask r1 'outline))) (list (clauses-after-text a) (has-substring? (text-of-answer a) "  ::")))
      '(() #f))

;; ---- D3, D15: a changed dependency, through read and outline --------------------
(want "D3 setup: delta's src is changed and committed" (head-of (ask r1 'set r1-delta "src" "function delta() {\n  return 5;\n}\n")) 'ok)
(want "D3 alpha, which lists delta, is absent and counted; gamma, which does not, still answers"
      (in-order (sig-of r1 r1-alpha) (sig-of r1 r1-gamma))
      (list absent-stale-1 (present "gamma(): number" "1.141.0")))
(let* ((a (outline-with-signatures r1)) (t (text-of-answer a)))
  (want "D15 outline omits alpha's signature, counts it, and names only gamma's provenance"
        (in-order (row-signature t r1-alpha) (row-signature t r1-gamma) (clauses-after-text a))
        (list 'none "gamma(): number" (list '(stale 1) (via "1.141.0")))))
(want "D3 a fresh supply of alpha's answers again"
      (in-order (supply-now r1 "-" (list (sig r1-alpha "alpha(): number" r1-beta r1-delta)) '("a.js" "c.js"))
            (sig-of r1 r1-alpha))
      (list '(ok (supplied (facts 1) (files 2))) (present "alpha(): number")))

;; ---- D8: a table that cannot be trusted is not used at all ---------------------------
(define r1-table (string-append r1 "/derived/signatures-%2D-javascript.sexp"))
;; #vu8() when there is no table: the rows below then read red rather
;; than stopping the fixture.
(define r1-table-bytes (or (bytes-of r1-table) #vu8()))
(define (readings st id) (list (ask st 'read id) (ask st 'check) (ask st 'outline)))
(define r1-readings (readings r1 r1-alpha))
(define (replace-all text from to)
  (let loop ((i 0) (out '()))
    (cond ((> (+ i (string-length from)) (string-length text))
           (apply string-append (reverse (cons (substring text i (string-length text)) out))))
          ((string=? (substring text i (+ i (string-length from))) from)
           (loop (+ i (string-length from)) (cons to out)))
          (else (loop (+ i 1) (cons (string (string-ref text i)) out))))))
(write! r1-table (replace-all (utf8->string r1-table-bytes) "alpha(): number" "alpha(): string"))
(want "D8 (i) a table whose fact was changed under its old checksum: neither alpha's nor gamma's fact is used, outline --with-signatures prints no signature and no clause, and read, check and outline answer as before"
      (in-order (sig-of r1 r1-alpha) (sig-of r1 r1-gamma) (equal? (readings r1 r1-alpha) r1-readings)
            (has-substring? (text-of r1-table) "alpha(): string")
            (let ((a (outline-with-signatures r1)))
              (list (row-signature (text-of-answer a) r1-alpha) (row-signature (text-of-answer a) r1-gamma)
                    (clauses-after-text a))))
      (list '(ok (signature absent)) '(ok (signature absent)) #t #t '(none none ())))
(write! r1-table "(derived-table 1")
(want "D8 (ii) a table that does not read: nothing is used, and read, check and outline answer as before"
      (in-order (sig-of r1 r1-alpha) (sig-of r1 r1-gamma) (equal? (readings r1 r1-alpha) r1-readings))
      (list '(ok (signature absent)) '(ok (signature absent)) #t))
;; A TABLE WHOSE CHECKSUM IS RIGHT AND WHOSE CONTENT IS NOT this library's:
;; the body changed and the checksum recomputed over it, as a hand-edited
;; or foreign table would be.
;; -> rewritten, or not-a-table when `bytes` is not a table's datum (empty,
;; as it is when there is no table to copy): those bytes are then written
;; as they are, so the rows after read a table that does not read rather
;; than the fixture stopping here.
(define (rewrite-table-from! bytes path change)
  (let ((d (guard (e (#t #f)) (storable-decode (string->sexpr-extended (utf8->string bytes))))))
    (if (not (and (list? d) (= 5 (length d))
                  (let ((body (list-ref d 4))) (and (list? body) (= 4 (length body)) (list? (cadddr body))))))
        (begin (write-bytes! path bytes) 'not-a-table)
        (let ((body (change (list-ref d 4))))
          (write-bytes! path (string->utf8 (sexpr->string-extended
                                             (storable-encode
                                               (list (car d) (cadr d) (caddr d)
                                                     (bytevector->hex (sha256 (string->utf8 (sexpr->string-extended
                                                                                              (storable-encode body)))))
                                                     body)))))
          'rewritten))))
(define (rewrite-table! path change) (rewrite-table-from! r1-table-bytes path change))
(want "D8 a rewrite of a table that is not there writes the empty bytes and says so, rather than stopping the fixture"
      (let ((p (string-append (fresh-dir! "rewrite-empty") "/t.sexp")))
        (in-order (rewrite-table-from! #vu8() p (lambda (body) body)) (bytes-of p)))
      (list 'not-a-table #vu8()))
(rewrite-table! r1-table (lambda (body) (list (car body) (cadr body) (caddr body)
                                              (map (lambda (f) (list (car f) (cadr f) (caddr f) '() (list-ref f 4) (list-ref f 5)))
                                                   (cadddr body)))))
(want "D8 (iii) a checksummed table whose facts carry no dependency stamps: none of them is used"
      (in-order (sig-of r1 r1-alpha) (sig-of r1 r1-gamma))
      (list '(ok (signature absent)) '(ok (signature absent))))
(rewrite-table! r1-table (lambda (body) (list (car body) (cadr body) 123 (cadddr body))))
(want "D8 (iv) a checksummed table whose language is not a string: nothing is used, and the readers answer"
      (in-order (sig-of r1 r1-alpha) (head-of (outline-with-signatures r1)))
      (list '(ok (signature absent)) 'ok))
(rewrite-table! r1-table (lambda (body) (list (car body) (cadr body) (caddr body)
                                              (map (lambda (f) (let ((p (caddr f)))
                                                                 (if (eq? (car p) 'signature)
                                                                     (list (car f) (cadr f) (list (car p) (cadr p) 123 (cadddr p))
                                                                           (list-ref f 3) (list-ref f 4) (list-ref f 5))
                                                                     f)))
                                                   (cadddr body)))))
(want "D8 (v) a checksummed table whose signature is not text: nothing is used"
      (in-order (sig-of r1 r1-alpha) (sig-of r1 r1-gamma))
      (list '(ok (signature absent)) '(ok (signature absent))))
(write-bytes! r1-table r1-table-bytes)
;; A TABLE IS USED ONLY BY THE STORE AND THE WRITER IT NAMES ITSELF FOR: the
;; same bytes under another store, or under another writer's file name, are
;; not consulted.
(define r1c-bare (make-store! "r1c-bare"))
(define r1c-bare-alpha (id-in r1c-bare "function alpha"))
(want "D8 identity: r1's table copied into another store answers nothing there; copied under w1's file name in r1, w1 reads nothing from it"
      (in-order
        (begin (mkdir-p! (string-append r1c-bare "/derived"))
               (write-bytes! (string-append r1c-bare "/derived/signatures-%2D-javascript.sexp") r1-table-bytes)
               (sig-of r1c-bare r1c-bare-alpha))
        (begin (write-bytes! (string-append r1 "/derived/signatures-w1-javascript.sexp") r1-table-bytes)
               (sig-of r1 r1-alpha "--working" "--writer" "w1")))
      (list '(ok (signature absent)) '(ok (signature absent))))
(delete-file (string-append r1 "/derived/signatures-w1-javascript.sexp"))
(want "D8 a table's file name escapes every byte outside [A-Za-z0-9_.], a per cent sign and a multibyte character included"
      (in-order (percent-encode "a%b/\xE9;") (percent-encode "-") (percent-encode "w_1.x"))
      '("a%25b%2F%C3%A9" "%2D" "w_1.x"))
(want "D8 control: the table's own bytes put back, both answer again"
      (in-order (sig-of r1 r1-alpha) (sig-of r1 r1-gamma))
      (list (present "alpha(): number") (present "gamma(): number" "1.141.0")))
(want "D8 an empty supply replacing a.js clears alpha's fact and keeps gamma's"
      (in-order (supply-now r1 "-" '() '("a.js")) (sig-of r1 r1-alpha) (sig-of r1 r1-gamma))
      (list '(ok (supplied (facts 0) (files 1))) absent-consulted (present "gamma(): number" "1.141.0")))
(want "D8 a python table for the same writer: delta answers from it, gamma still from the javascript one"
      (in-order (supply-now r1 "-" (list (sig r1-delta "delta()")) '("c.js") "1.140.0" "python")
            (sig-of r1 r1-delta) (sig-of r1 r1-gamma))
      (list '(ok (supplied (facts 1) (files 1))) (list 'ok '(signature "delta()") '(stale 0) (via "1.140.0" "python"))
            (present "gamma(): number" "1.141.0")))

;; ---- D5, D14: an editor's keywords in search --------------------------------------
(define r2 (make-store! "r2"))
(define r2-alpha (id-in r2 "function alpha"))
(define r2-beta (id-in r2 "function beta"))
(define r2-gamma (id-in r2 "function gamma"))
(ask r2 'set r2-beta "keywords" "zebrafish")
(ask r2 'set r2-gamma "keywords" "authored-word")
(define (hits a) (map (lambda (h) (list (list-ref h 1) (list-ref h 2) (cadr (list-ref h 4))))
                      (cdr (assq 'items (cdr a)))))
(define (scanned-fields a) (let ((c (assq 'scanned (cdr a)))) (and c (cadr (assq 'fields (cdr c))))))
(define r2-authored-before (hits (ask r2 'search "authored")))
(define r2-zebra-before (ask r2 'search "zebrafish"))
(want "D5 control: with no table, the scan names no derived-keywords and the answer carries neither clause; zebrafish finds beta alone"
      (in-order (scanned-fields r2-zebra-before) (assq 'stale (cdr r2-zebra-before)) (assq 'via (cdr r2-zebra-before))
            (hits r2-zebra-before))
      (list '(title keywords src names doc body) #f #f (list (list r2-beta 4 '(keywords)))))
(want "D5 setup: keywords supplied for alpha (no author keywords), beta (author keyword zebrafish; the editor's zebrafish and narwhal) and gamma (author keywords)"
      (supply-now r2 "-" (list (kw r2-alpha '("okapi" "zebrafish") r2-beta) (kw r2-beta '("zebrafish" "narwhal"))
                               (kw r2-gamma '("quokka")))
                  '("a.js" "b.js"))
      '(ok (supplied (facts 3) (files 2))))
(let ((a (ask r2 'search "okapi")))
  (want "D5 a word only an editor supplied finds alpha at the src tier, the field named derived-keywords, with the via; the scan names derived-keywords"
        (in-order (hits a) (assq 'stale (cdr a)) (assq 'via (cdr a)) (scanned-fields a))
        (list (list (list r2-alpha 2 '(derived-keywords))) '(stale 0) (via "1.140.0")
              '(title keywords derived-keywords src names doc body))))
(want "D5 zebrafish: beta by its author's keyword first (4), alpha by the editor's second (2)"
      (hits (ask r2 'search "zebrafish"))
      (list (list r2-beta 4 '(keywords)) (list r2-alpha 2 '(derived-keywords))))
(want "D14 gamma has author keywords: the editor's quokka does not find it, and its own word scores as before"
      (in-order (hits (ask r2 'search "quokka")) (hits (ask r2 'search "authored")))
      (list '() r2-authored-before))
(want "D14 beta has the author's zebrafish and the editor's zebrafish and narwhal: narwhal finds nothing, and zebrafish scores beta at the author's tier alone"
      (in-order (hits (ask r2 'search "narwhal")) (assoc r2-beta (hits (ask r2 'search "zebrafish"))))
      (list '() (list r2-beta 4 '(keywords))))
(want "D5 the human rendering of a search that read a table is hit lines only: no stale and no via line"
      (let ((ls (filter (lambda (l) (> (string-length l) 0)) (lines-of (render-human (ask r2 'search "okapi"))))))
        (list (length ls) (for-all (lambda (l) (has-substring? l "(hit ")) ls)))
      '(1 #t))
(want "D5 an editor's words for alpha from a second provenance (a python table): a hit found by them names that provenance only, and one found by the first names the first only"
      (in-order (supply-now r2 "-" (list (kw r2-alpha '("tapir"))) '("a.js") "1.141.0" "python")
            (assq 'via (cdr (ask r2 'search "tapir"))) (assq 'via (cdr (ask r2 'search "okapi"))))
      (list '(ok (supplied (facts 1) (files 1))) (via "1.141.0" "python") (via "1.140.0")))
(want "D14 gamma's keyword field reads back as its author wrote it"
      (let ((b (ask r2 'read r2-gamma))) (cdr (assq 'keywords (cdr (assq 'fields (cadr b))))))
      "authored-word")
(want "D5 alpha's src changed: its editor words (two facts, both listing alpha) are stale; okapi finds nothing and the search counts both, naming no provenance"
      (in-order (head-of (ask r2 'set r2-alpha "src" "function alpha() {\n  return 7;\n}\n"))
                (let ((a (ask r2 'search "okapi"))) (list (hits a) (assq 'stale (cdr a)) (assq 'via (cdr a)))))
      (list 'ok (list '() '(stale 2) '(via))))

;; ---- D13: the writer's working view, pinned ----------------------------------------
;; w1 drafts beta; alpha is then changed and committed, so the latest
;; committed alpha is not the one w1's view shows.
(define r3 (make-store! "r3"))
(define r3-alpha (id-in r3 "function alpha"))
(define r3-beta (id-in r3 "function beta"))
(ask r3 'write r3-beta "function beta() {\n  return 20;\n}\n" "--writer" "w1")
;; THE PINNED VIEW IS READ THROUGH ITS PROJECTION: `read --working` of a
;; block the writer has no draft of answers the committed state as it is
;; now, not the view's baseline.
(define (working-text st writer name) (text-of (string-append (export-in st "--working" "--writer" writer) "/" name)))
(want "D13 setup: alpha is changed and committed after w1's draft; w1's projection still holds the old alpha, the committed store the new one"
      (in-order (head-of (ask r3 'set r3-alpha "src" "function alpha() {\n  return 10;\n}\n"))
            (let ((t (working-text r3 "w1" "a.js"))) (list (contains? t "return 1;") (contains? t "return 10;")))
            (src-bytes r3 r3-alpha))
      (list 'ok '(#t #f) (string->utf8 "function alpha() {\n  return 10;\n}\n")))
(want "D13 a supply for w1 made from the committed projection is supply-stale on a.js"
      (let* ((d (export-in r3)) (files (map (lambda (n) (list n (sha-of (string-append d "/" n)))) (projection-files d))))
        (supply-in r3 (string-append (header 'signatures "w1" files '("a.js")) (sig r3-alpha "w1-alpha" r3-beta))
                   "signatures" "--for" "w1"))
      '(error supply-stale (file "a.js")))
(want "D13 made from w1's working projection it is kept, and read --signature --working answers it"
      (in-order (supply-now r3 "w1" (list (sig r3-alpha "w1-alpha" r3-beta)) '("a.js"))
            (sig-of r3 r3-alpha "--working" "--writer" "w1"))
      (list '(ok (supplied (facts 1) (files 1))) (present "w1-alpha")))
(want "D13 the committed table has nothing for alpha: w1's facts are w1's"
      (sig-of r3 r3-alpha) '(ok (signature absent)))
(want "D13 a second draft of beta under w1, and no supply: alpha's fact is stale in w1's view"
      (in-order (head-of (ask r3 'write r3-beta "function beta() {\n  return 21;\n}\n" "--writer" "w1"))
            (sig-of r3 r3-alpha "--working" "--writer" "w1"))
      (list 'ok absent-stale-1))

;; ---- D19, D20: a commit of the same bytes keeps the writer's fact ---------------------
(define r4 (make-store! "r4"))
(define r4-gamma (id-in r4 "function gamma"))
(ask r4 'write r4-gamma "function gamma() {\n  return 30;\n}\n" "--writer" "w2")
(want "D19 setup: a committed-store fact and a w2 fact on gamma, each from its own view, read back"
      (in-order (supply-now r4 "-" (list (sig r4-gamma "committed-gamma")) '("b.js"))
            (supply-now r4 "w2" (list (sig r4-gamma "w2-gamma")) '("b.js"))
            (sig-of r4 r4-gamma) (sig-of r4 r4-gamma "--working" "--writer" "w2"))
      (list '(ok (supplied (facts 1) (files 1))) '(ok (supplied (facts 1) (files 1)))
            (present "committed-gamma") (present "w2-gamma")))
(want "D19 w2 commits its draft (other bytes than the committed src): w2's fact stays fresh, the committed table's goes stale"
      (in-order (head-of (ask r4 'commit r4-gamma "--writer" "w2"))
            (sig-of r4 r4-gamma "--working" "--writer" "w2") (sig-of r4 r4-gamma))
      (list 'ok (present "w2-gamma") absent-stale-1))
(want "D20 a title change of gamma leaves w2's fact fresh"
      (in-order (head-of (ask r4 'set r4-gamma "title" "renamed gamma")) (sig-of r4 r4-gamma "--working" "--writer" "w2"))
      (list 'ok (present "w2-gamma")))

;; ---- D21: what a fact lists, and the file's order ---------------------------------------
(define r5 (make-store! "r5"))
(define r5-alpha (id-in r5 "function alpha"))
(define r5-beta (id-in r5 "function beta"))
(define r5-delta (id-in r5 "function delta"))
(define r5-a (file-in r5 "a.js"))
(supply-now r5 "-" (list (sig r5-alpha "fine") (sig r5-delta "d")) '("a.js" "c.js"))
(want "D21 alpha lists only itself: an edit of beta, its unlisted sibling, leaves the fact fresh"
      (in-order (head-of (ask r5 'set r5-beta "src" "function beta() {\n  return 22;\n}\n")) (sig-of r5 r5-alpha))
      (list 'ok (present "fine")))
(want "D21 delta moved into a.js after alpha: a.js's blocks changed, alpha's fact is stale, and delta's, stamped on c.js, is stale too"
      (in-order (head-of (ask r5 'move r5-delta r5-a "--after" r5-alpha)) (sig-of r5 r5-alpha) (sig-of r5 r5-delta)
            (let ((t (text-of-answer (ask r5 'outline))))
              (let ((pos (lambda (id) (let loop ((ls (lines-of t)) (i 0))
                                        (cond ((null? ls) #f) ((has-substring? (car ls) (string-append "- " id "  ")) i)
                                              (else (loop (cdr ls) (+ i 1))))))))
                (and (pos r5-alpha) (pos r5-delta) (pos r5-beta)
                     (< (pos r5-alpha) (pos r5-delta) (pos r5-beta))))))
      (list 'ok absent-stale-1 absent-stale-1 #t))
(want "D21 a fresh supply, then a reorder of a.js (delta moved after beta): stale again"
      (in-order (supply-now r5 "-" (list (sig r5-alpha "fine")) '("a.js" "c.js")) (sig-of r5 r5-alpha)
            (head-of (ask r5 'move r5-delta r5-a "--after" r5-beta)) (sig-of r5 r5-alpha))
      (list '(ok (supplied (facts 1) (files 2))) (present "fine") 'ok absent-stale-1))

;; ---- D22: the file's language -----------------------------------------------------------
(define r6 (make-store! "r6"))
(define r6-gamma (id-in r6 "function gamma"))
(define r6-b (file-in r6 "b.js"))
(supply-now r6 "-" (list (sig r6-gamma "g")) '("b.js"))
(want "D22 (a) b.js set to python, whose comments differ: the export's marker lines are python's, and gamma's fact is stale"
      (in-order (head-of (ask r6 'set r6-b "lang" "python"))
            (let ((t (text-of (string-append (export-in r6) "/b.js")))) (list (contains? t "# @file") (contains? t "// @file")))
            (sig-of r6 r6-gamma))
      (list 'ok '(#t #f) absent-stale-1))
(want "D22 (b) b.js set to a language no entry names: it still projects, and a supply against that projection is fresh"
      (in-order (head-of (ask r6 'set r6-b "lang" "nonesuch"))
            (supply-now r6 "-" (list (sig r6-gamma "g2")) '("b.js")) (sig-of r6 r6-gamma))
      (list 'ok '(ok (supplied (facts 1) (files 1))) (present "g2")))

;; ---- D23, D29: a child that no longer projects ------------------------------------------
(define r7 (make-store! "r7"))
(define r7-alpha (id-in r7 "function alpha"))
(define r7-beta (id-in r7 "function beta"))
(define r7-a (file-in r7 "a.js"))
(supply-now r7 "-" (list (sig r7-alpha "a")) '("a.js"))
(want "D23 beta's kind set to section: a.js does not project, alpha's fact is stale; set back to code, it is fresh again"
      (in-order (head-of (ask r7 'set r7-beta "kind" "section")) (sig-of r7 r7-alpha)
            (head-of (ask r7 'set r7-beta "kind" "code")) (sig-of r7 r7-alpha))
      (list 'ok absent-stale-1 'ok (present "a")))
(define r7-note (ask r7 'insert "--under" r7-a "--title" "a note" "--text" "not code"))
(want "D29 a block that is not code inserted into a.js: alpha's fact is stale, and export-code refuses unexportable-block"
      (in-order (head-of r7-note) (sig-of r7 r7-alpha)
            (let ((x (ask r7 'export-code (fresh-dir! "x")))) (and (pair? x) (assq 'reason (cddr x)))))
      (list 'ok absent-stale-1 '(reason unexportable-block)))
(define r7b (make-store! "r7b"))
(define r7b-alpha (id-in r7b "function alpha"))
(define r7b-beta (id-in r7b "function beta"))
(supply-now r7b "-" (list (sig r7b-alpha "a")) '("a.js"))
;; A MODE IS NOT A FIELD THE CALLER'S PATH CHANGES (set answers mode-mismatch),
;; so the change arrives as a record from elsewhere would.
(want "D23 beta's mode changed by a record (its kind still code): a.js does not project, alpha's fact is stale, and export-code refuses unexportable-block"
      (in-order (forged r7b (format "(set ~s mode datum)" r7b-beta)) (sig-of r7b r7b-alpha)
            (let ((x (ask r7b 'export-code (fresh-dir! "x")))) (and (pair? x) (assq 'reason (cddr x)))))
      (list 'forged absent-stale-1 '(reason unexportable-block)))
(want "D23 a record forged for a writer with no segment is refused by name, and nothing is appended elsewhere"
      (let ((before (bytes-of (string-append r7b "/writers/" (own-writer-name r7b) "/000001.sexp"))))
        (in-order (forge-record-as! r7b "nobody" (format "(set ~s title ~s)" r7b-beta "t"))
                  (equal? (bytes-of (string-append r7b "/writers/" (own-writer-name r7b) "/000001.sexp")) before)))
      (list '(forge-refused (writer "nobody")) #t))

;; ---- D30, D24: who holds the path ---------------------------------------------------------
(define r8 (make-store! "r8"))
(define r8-gamma (id-in r8 "function gamma"))
(define r8-delta (id-in r8 "function delta"))
(define r8-b (file-in r8 "b.js"))
(define r8-c (file-in r8 "c.js"))
(supply-now r8 "-" (list (sig r8-gamma "g") (sig r8-delta "d")) '("b.js" "c.js"))
(want "D30 c.js's file block given the path b.js: two holders, gamma's fact is stale and export-code refuses duplicate-path; the path put back, fresh again"
      (in-order (head-of (ask r8 'set r8-c "path" "b.js")) (sig-of r8 r8-gamma)
            (let ((x (ask r8 'export-code (fresh-dir! "x")))) (and (pair? x) (assq 'reason (cddr x))))
            (head-of (ask r8 'set r8-c "path" "c.js")) (sig-of r8 r8-gamma))
      (list 'ok absent-stale-1 '(reason duplicate-path) 'ok (present "g")))
(want "D30 b.js's mode changed by a record: nothing holds b.js as a text file, gamma's fact is stale"
      (in-order (forged r8 (format "(set ~s mode datum)" r8-b)) (sig-of r8 r8-gamma))
      (list 'forged absent-stale-1))
;; With no derived/ to move away, the thunk already sees none.
(define (without-derived st thunk)
  (let ((d (string-append st "/derived")) (away (string-append st "/derived-away")))
    (if (not (file-directory? d))
        (thunk)
        (begin (rename-file d away)
               (let ((v (thunk))) (rename-file away d) v)))))
(want "D24 c.js's file block deleted: delta's fact is stale; check and a read of delta answer as they do with no table at all"
      (in-order (head-of (ask r8 'del r8-c)) (sig-of r8 r8-delta)
            (equal? (list (ask r8 'check) (ask r8 'read r8-delta))
                    (without-derived r8 (lambda () (list (ask r8 'check) (ask r8 'read r8-delta))))))
      (list 'ok absent-stale-1 #t))

;; ---- D25, D27: the working view after a commit ---------------------------------------------
(define r9 (make-store! "r9"))
(define r9-alpha (id-in r9 "function alpha"))
(define r9-beta (id-in r9 "function beta"))
(define r9-beta-before (src-bytes r9 r9-beta))
(ask r9 'write r9-alpha "function alpha() {\n  return 11;\n}\n" "--writer" "w3")
(ask r9 'write r9-beta "function beta() {\n  return 12;\n}\n" "--writer" "w3")
(want "D25 w3 drafts alpha and beta; a fact on beta (listing beta only) from w3's view answers"
      (in-order (supply-now r9 "w3" (list (sig r9-beta "w3-beta")) '("a.js")) (sig-of r9 r9-beta "--working" "--writer" "w3"))
      (list '(ok (supplied (facts 1) (files 1))) (present "w3-beta")))
(want "D25 w3 commits beta and keeps alpha's draft: its view stays at alpha's baseline, where w3's projection holds beta's old bytes, so the fact is stale"
      (in-order (head-of (ask r9 'commit r9-beta "--writer" "w3"))
            (let ((t (working-text r9 "w3" "a.js"))) (list (contains? t (utf8->string r9-beta-before)) (contains? t "return 12;")))
            (sig-of r9 r9-beta "--working" "--writer" "w3"))
      (list 'ok '(#t #f) absent-stale-1))
(define r10 (make-store! "r10"))
(define r10-alpha (id-in r10 "function alpha"))
(define r10-beta (id-in r10 "function beta"))
(ask r10 'write r10-alpha "function alpha() {\n  return 13;\n}\n" "--writer" "w4")
(want "D27 w4's only draft is alpha; a fact on alpha listing beta answers, and still does after beta is changed and committed by the store's writer"
      (in-order (supply-now r10 "w4" (list (sig r10-alpha "w4-alpha" r10-beta)) '("a.js"))
            (sig-of r10 r10-alpha "--working" "--writer" "w4")
            (head-of (ask r10 'set r10-beta "src" "function beta() {\n  return 14;\n}\n"))
            (sig-of r10 r10-alpha "--working" "--writer" "w4"))
      (list '(ok (supplied (facts 1) (files 1))) (present "w4-alpha") 'ok (present "w4-alpha")))
(want "D27 w4 commits alpha, its last draft: its view becomes the committed state, where beta changed, and the fact is stale"
      (in-order (head-of (ask r10 'commit r10-alpha "--writer" "w4")) (sig-of r10 r10-alpha "--working" "--writer" "w4"))
      (list 'ok absent-stale-1))

;; ---- D28: the language entry itself ------------------------------------------------------
;; A language this fixture registers, so replacing it touches no other row.
(define tl-entry '((lang "tl") (extensions ("tl")) (line-comment "#") (def-heads ())))
(register-language! tl-entry)
(define r11 (string-append root "/r11"))
(define r11-src (fresh-dir! "r11-src"))
(write! (string-append r11-src "/x.tl") "one\ntwo\n")
(ask r11 'init)
(ask r11 'import-code r11-src)
(define r11-one (id-in r11 "one"))
(want "D28 setup: a tl file's block, and a fact on it that answers"
      (in-order (and r11-one #t) (supply-now r11 "-" (list (sig r11-one "tl-sig")) '("x.tl") "1.140.0" "tl") (sig-of r11 r11-one))
      (list #t '(ok (supplied (facts 1) (files 1))) (list 'ok '(signature "tl-sig") '(stale 0) (via "1.140.0" "tl"))))
(register-language! '((lang "tl") (extensions ("tl" "tl2")) (line-comment "#") (def-heads ())))
(want "D28 the entry replaced with another extension and the same comments: the fact is fresh"
      (sig-of r11 r11-one) (list 'ok '(signature "tl-sig") '(stale 0) (via "1.140.0" "tl")))
(register-language! '((lang "tl") (extensions ("tl" "tl2")) (line-comment "//") (def-heads ())))
(want "D28 the entry replaced under the same name with other comments: the export's marker lines are the new ones, and the fact is stale"
      (in-order (let ((t (text-of (string-append (export-in r11) "/x.tl")))) (list (contains? t "// @file") (contains? t "# @file")))
            (sig-of r11 r11-one))
      (list '(#t #f) absent-stale-1))
(register-language! tl-entry)

;; ==== calls: refs and reach ====
;; -> the answer to a calls supply of `lines` made from the committed projection.
(define (supply-calls st lines replaces)
  (let* ((d (export-in st))
         (files (map (lambda (n) (list n (sha-of (string-append d "/" n)))) (projection-files d))))
    (supply-in st
               (apply string-append
                      (line (list 'supply 'calls '(writer "-") '(language "javascript") '(source (vscode "1.140.0"))
                                  (list 'files files) (list 'replaces replaces)))
                      lines)
               "calls")))
(define (calls from to . depends) (line (list 'calls from to (list 'depends (cons from depends)))))
(define (refs-items a) (and (pair? a) (eq? (car a) 'ok) (cdr (assq 'items (cdr a)))))
(define (refs-clauses a) (filter (lambda (c) (and (pair? c) (memq (car c) '(via stale)))) (cdr a)))
(define supplied-via '(via (vscode "1.140.0" "javascript")))

;; ---- D6: a cycle, and depth -----------------------------------------------------------
(define r12 (make-store! "r12"))
(define r12-alpha (id-in r12 "function alpha"))
(define r12-gamma (id-in r12 "function gamma"))
(define r12-delta (id-in r12 "function delta"))
(want "D6 setup: alpha -> gamma -> delta -> alpha supplied as calls"
      (supply-calls r12 (list (calls r12-alpha r12-gamma r12-gamma) (calls r12-gamma r12-delta r12-delta)
                              (calls r12-delta r12-alpha r12-alpha))
                    '("a.js" "b.js" "c.js"))
      '(ok (supplied (facts 3) (files 3))))
(want "D6 reach alpha --depth 5 ends on the cycle: each block once, at the fewest hops, with the via"
      (ask r12 'reach r12-alpha "--rel" "calls" "--depth" "5")
      (list 'ok (list 'reached (list (list r12-alpha 0) (list r12-gamma 1) (list r12-delta 2))) '(stale 0) supplied-via))
(want "D6 --depth 1 reaches gamma only; --depth 0 is alpha alone, with an empty via"
      (in-order (ask r12 'reach r12-alpha "--rel" "calls" "--depth" "1") (ask r12 'reach r12-alpha "--depth" "0"))
      (list (list 'ok (list 'reached (list (list r12-alpha 0) (list r12-gamma 1))) '(stale 0) supplied-via)
            (list 'ok (list 'reached (list (list r12-alpha 0))) '(stale 0) '(via))))
(want "D6/D9 refs gamma shows the supplied row from alpha with its provenance, and the answer carries the via"
      (let ((a (ask r12 'refs r12-gamma))) (list (refs-items a) (refs-clauses a)))
      (list (list (list 'ref (list 'from r12-alpha) '(rel calls) '(via (vscode "1.140.0" "javascript"))))
            (list '(stale 0) supplied-via)))
(want "D6 reach with no --depth walks one hop: alpha and gamma"
      (ask r12 'reach r12-alpha)
      (list 'ok (list 'reached (list (list r12-alpha 0) (list r12-gamma 1))) '(stale 0) supplied-via))
(want "D6 the human rendering of refs with a supplied row is item lines only"
      (let ((ls (filter (lambda (l) (> (string-length l) 0)) (lines-of (render-human (ask r12 'refs r12-gamma))))))
        (list (length ls) (for-all (lambda (l) (has-substring? l "(ref ")) ls)))
      '(1 #t))
(want "D6 reach refuses a depth that is not a count, and answers unknown-id for an id the store does not hold"
      (in-order (head-of (ask r12 'reach r12-alpha "--depth" "two")) (head-of (ask r12 'reach "nope.1")))
      '(usage (error unknown-id)))

;; ---- D16: direction and relation ------------------------------------------------------
;; gamma -> alpha and alpha -> beta supplied as calls; alpha explains delta,
;; linked by hand.
(define r13 (make-store! "r13"))
(define r13-alpha (id-in r13 "function alpha"))
(define r13-beta (id-in r13 "function beta"))
(define r13-gamma (id-in r13 "function gamma"))
(define r13-delta (id-in r13 "function delta"))
(want "D16 setup: two supplied calls and one linked edge"
      (in-order (supply-calls r13 (list (calls r13-gamma r13-alpha r13-alpha) (calls r13-alpha r13-beta r13-beta))
                          '("a.js" "b.js"))
            (head-of (ask r13 'link r13-alpha "explains" r13-delta)))
      '((ok (supplied (facts 2) (files 2))) ok))
(want "D16 reach alpha --depth 2 follows calls outward: beta, not gamma (which calls alpha), not delta (a linked edge)"
      (ask r13 'reach r13-alpha "--rel" "calls" "--depth" "2")
      (list 'ok (list 'reached (list (list r13-alpha 0) (list r13-beta 1))) '(stale 0) supplied-via))
(want "D16 reach beta, which calls nothing, is beta alone; --rel explains, a relation no supply produces, is refused by name"
      (in-order (ask r13 'reach r13-beta) (ask r13 'reach r13-alpha "--rel" "explains" "--depth" "2"))
      (list (list 'ok (list 'reached (list (list r13-beta 0))) '(stale 0) '(via))
            '(error unknown-relation (rel explains))))
;; A branch: alpha calls beta and delta, delta calls beta; beta is one hop
;; away, not two.
(define r13c (make-store! "r13c"))
(define r13c-alpha (id-in r13c "function alpha"))
(define r13c-beta (id-in r13c "function beta"))
(define r13c-delta (id-in r13c "function delta"))
(want "D6 in a branching graph each block is at its fewest hops: beta and delta at 1, beta not again at 2"
      (in-order (supply-calls r13c (list (calls r13c-alpha r13c-beta r13c-beta) (calls r13c-alpha r13c-delta r13c-delta)
                                         (calls r13c-delta r13c-beta r13c-beta))
                              '("a.js" "c.js"))
                (ask r13c 'reach r13c-alpha "--depth" "2"))
      (list '(ok (supplied (facts 3) (files 2)))
            (list 'ok (list 'reached (cons (list r13c-alpha 0)
                                           (list-sort (lambda (x y) (string<? (car x) (car y)))
                                                      (list (list r13c-beta 1) (list r13c-delta 1)))))
                  '(stale 0) supplied-via)))
(define r13b (make-store! "r13b"))
(define r13b-alpha (id-in r13b "function alpha"))
(want "D16 with no calls table, reach answers the block alone and carries neither clause; an unknown relation is refused there too"
      (in-order (ask r13b 'reach r13b-alpha) (ask r13b 'reach r13b-alpha "--rel" "uses"))
      (list (list 'ok (list 'reached (list (list r13b-alpha 0)))) '(error unknown-relation (rel uses))))

;; ---- D3: a supplied edge whose third dependency changed ------------------------------------
(define r14 (make-store! "r14"))
(define r14-alpha (id-in r14 "function alpha"))
(define r14-gamma (id-in r14 "function gamma"))
(define r14-delta (id-in r14 "function delta"))
(supply-calls r14 (list (calls r14-alpha r14-gamma r14-gamma r14-delta)) '("a.js"))
(want "D3 before: refs gamma shows the edge from alpha, and reach alpha reaches gamma"
      (in-order (refs-items (ask r14 'refs r14-gamma)) (ask r14 'reach r14-alpha))
      (list (list (list 'ref (list 'from r14-alpha) '(rel calls) '(via (vscode "1.140.0" "javascript"))))
            (list 'ok (list 'reached (list (list r14-alpha 0) (list r14-gamma 1))) '(stale 0) supplied-via)))
(want "D3 delta, listed by the edge, changed and committed: refs gamma and reach alpha drop it and count it"
      (in-order (head-of (ask r14 'set r14-delta "src" "function delta() {\n  return 5;\n}\n"))
            (let ((a (ask r14 'refs r14-gamma))) (list (refs-items a) (refs-clauses a)))
            (ask r14 'reach r14-alpha))
      (list 'ok (list '() '((stale 1) (via)))
            (list 'ok (list 'reached (list (list r14-alpha 0))) '(stale 1) '(via))))
(want "D3 a fresh calls supply answers again"
      (begin (supply-calls r14 (list (calls r14-alpha r14-gamma r14-gamma r14-delta)) '("a.js"))
             (ask r14 'reach r14-alpha))
      (list 'ok (list 'reached (list (list r14-alpha 0) (list r14-gamma 1))) '(stale 0) supplied-via))
(define r14-b (file-in r14 "b.js"))
(want "D6 (v4) an edge listing alpha and gamma only; delta, from c.js which it does not list, moved into b.js beside gamma: b.js's blocks changed, so the edge is stale although no listed block's bytes did"
      (in-order (supply-calls r14 (list (calls r14-alpha r14-gamma r14-gamma)) '("a.js"))
            (ask r14 'reach r14-alpha)
            (head-of (ask r14 'move r14-delta r14-b "--after" r14-gamma))
            (let ((a (ask r14 'refs r14-gamma))) (list (refs-items a) (refs-clauses a)))
            (ask r14 'reach r14-alpha))
      (list '(ok (supplied (facts 1) (files 1)))
            (list 'ok (list 'reached (list (list r14-alpha 0) (list r14-gamma 1))) '(stale 0) supplied-via)
            'ok (list '() '((stale 1) (via)))
            (list 'ok (list 'reached (list (list r14-alpha 0))) '(stale 1) '(via))))

;; ---- D4: the edge's target deleted ----------------------------------------------------------
(define r15 (make-store! "r15"))
(define r15-alpha (id-in r15 "function alpha"))
(define r15-gamma (id-in r15 "function gamma"))
(want "D4 the base oracle: with no reference of any kind, refs alpha answers (ok (items))"
      (ask r15 'refs r15-alpha) '(ok (items)))
(supply-calls r15 (list (calls r15-alpha r15-gamma r15-gamma)) '("a.js"))
(want "D4 before the deletion: refs gamma shows the edge from alpha"
      (refs-items (ask r15 'refs r15-gamma))
      (list (list 'ref (list 'from r15-alpha) '(rel calls) '(via (vscode "1.140.0" "javascript")))))
(ask r15 'del r15-gamma)
(let ((a (ask r15 'refs r15-gamma)))
  (want "D4 gamma deleted: refs gamma answers the rows it answers with no table at all (none), then the dropped edge counted, whole"
        (in-order a (without-derived r15 (lambda () (ask r15 'refs r15-gamma))))
        '((ok (items) (stale 1) (via)) (ok (items)))))
(want "D4 reach alpha reaches nothing and counts the edge; refs alpha has the base's items, and consulted a table with nothing into alpha"
      (in-order (ask r15 'reach r15-alpha) (ask r15 'refs r15-alpha))
      (list (list 'ok (list 'reached (list (list r15-alpha 0))) '(stale 1) '(via)) '(ok (items) (stale 0) (via))))

;; ==== diagnostics ====
;; -> (values <supply text> <projected bytes of `name`>): a diagnostics supply
;; of `lines` for `writer`, made from that writer's working projection.
(define (diagnostics-supply st writer language lines replaces name)
  (let* ((d (export-in st "--working" "--writer" writer))
         (files (map (lambda (n) (list n (sha-of (string-append d "/" n)))) (projection-files d))))
    (values (apply string-append
                   (line (list 'supply 'diagnostics (list 'writer writer) (list 'language language)
                               '(source (vscode "1.140.0")) (list 'files files) (list 'replaces replaces)))
                   lines)
            (bytes-of (string-append d "/" name)))))
(define (diag id s e . depends) (line (list 'diagnostic id 'error "m" (list 'range s e) (list 'depends (cons id depends)))))
(define (diag-answer st writer) (ask st 'diagnostics "--writer" writer))
(define (diag-items st writer) (cdr (assq 'items (cdr (diag-answer st writer)))))
(define (bytes-offset b needle)
  (let ((n (string->utf8 needle)))
    (let loop ((i 0))
      (cond ((> (+ i (bytevector-length n)) (bytevector-length b)) #f)
            ((let check ((k 0)) (or (= k (bytevector-length n))
                                    (and (= (bytevector-u8-ref b (+ i k)) (bytevector-u8-ref n k)) (check (+ k 1)))))
             i)
            (else (loop (+ i 1)))))))

;; ---- D18: the range map, read back through diagnostics ------------------------------
;; A python file whose first block opens with a #! line and a coding cookie,
;; and whose second block holds a marker-like line (escaped when projected).
;; The `# @block new` line cuts the file into the two blocks on import. The
;; marker-like line is found by its line, not by its count of "@": whether
;; the import keeps it as written or reads it as escaped, the projection
;; inserts one "@" after its "# ".
(define py-src "#!/usr/bin/env python\n# coding: utf-8\ndef one():\n    return 1\n# @block new\ndef two():\n# @@block writer.q\n    return 2\n")
(define (line-start-of b needle)
  (let ((i (bytes-offset b needle)))
    (and i (let loop ((j i)) (if (or (= j 0) (= 10 (bytevector-u8-ref b (- j 1)))) j (loop (- j 1)))))))
(define r16 (string-append root "/r16"))
(define r16-src (fresh-dir! "r16-src"))
(write! (string-append r16-src "/m.py") py-src)
(ask r16 'init)
(ask r16 'import-code r16-src)
(define b1 (id-in r16 "def one"))
(define b2 (id-in r16 "def two"))
(define src1 (src-bytes r16 b1))
(define src2 (src-bytes r16 b2))
(define k (bytes-offset src1 "def one"))
(define r (line-start-of src2 "block writer.q"))
(define len1 (bytevector-length src1))
(define len2 (bytevector-length src2))
(define-values (ignored-text P) (diagnostics-supply r16 "wd" "python" '() '() "m.py"))
(define p1 (bytes-offset P "def one"))
(define p2 (bytes-offset P "def two"))
(define q (line-start-of P "block writer.q"))
(define f0 (bytes-offset P "# @file"))
(define m2 (bytes-offset P (string-append "# @block " b2)))
(want "D18 setup: two blocks; b1's prefix (the #! line and the cookie) is 38 bytes and stays above the @file line; b1 ends in LF, so b2's marker follows it directly; b2's marker-like line is escaped"
      (in-order (and b1 b2 (not (equal? b1 b2))) k f0 (bytevector-u8-ref src1 (- len1 1)) (= m2 (+ p1 (- len1 k))) (and q r #t)
            (and q r (= (bytevector-u8-ref P (+ q 2)) 64)
                 (- (- (bytes-offset P "block writer.q") q) (- (bytes-offset src2 "block writer.q") r))))
      (list #t 38 38 10 #t #t 1))
;; -> (supply answer, the one item diagnostics then lists)
(define (map-case id s e)
  (let-values (((text bytes) (diagnostics-supply r16 "wd" "python" (list (diag id s e)) '("m.py") "m.py")))
    (let ((a (supply-in r16 text "diagnostics" "--for" "wd")))
      (list a (diag-items r16 "wd")))))
(define (item id at) (list (list 'diagnostic id 'error "m" at)))
(define kept '(ok (supplied (facts 1) (files 1))))
(want "D18 (1) a range inside the prefix: b1 at the same offsets"
      (map-case b1 2 5) (list kept (item b1 '(at 2 5))))
(want "D18 (2) from the prefix into b1's body, across the @file and @block lines: contiguous in b1"
      (map-case b1 2 (+ p1 3)) (list kept (item b1 (list 'at 2 (+ k 3)))))
(want "D18 (3) around the escape: the byte before it, the escape alone (empty), the byte after it"
      (in-order (map-case b2 (+ q 1) (+ q 2)) (map-case b2 (+ q 2) (+ q 3)) (map-case b2 (+ q 3) (+ q 4)))
      (list (list kept (item b2 (list 'at (+ r 1) (+ r 2)))) (list kept (item b2 (list 'at (+ r 2) (+ r 2))))
            (list kept (item b2 (list 'at (+ r 2) (+ r 3))))))
(want "D18 (4) b1's body to its end (b2's marker start): b1 from the prefix's size to its length"
      (map-case b1 p1 m2) (list kept (item b1 (list 'at k len1))))
(want "D18 (5) from b1's body into b2's: unmappable on b1"
      (map-case b1 (+ p1 1) (+ p2 1)) (list kept (item b1 '(at unmappable))))
(want "D18 (6) inside b2's @block line: unmappable on b2"
      (map-case b2 (+ m2 1) (+ m2 3)) (list kept (item b2 '(at unmappable))))
(want "D18 (7) empty at b1's end (b2's marker start): b1 at its length; empty at EOF: b2 at its length"
      (in-order (map-case b1 m2 m2) (map-case b2 (bytevector-length P) (bytevector-length P)))
      (list (list kept (item b1 (list 'at len1 len1))) (list kept (item b2 (list 'at len2 len2)))))
(want "D18 an empty range strictly inside b1's body: b1 at that offset of its src"
      (map-case b1 (+ p1 3) (+ p1 3)) (list kept (item b1 (list 'at (+ k 3) (+ k 3)))))
(want "D18 (10) empty inside the @file line: b1 at the prefix's size; inside b2's @block line: b2 at 0"
      (in-order (map-case b1 (+ f0 3) (+ f0 3)) (map-case b2 (+ m2 3) (+ m2 3)))
      (list (list kept (item b1 (list 'at k k))) (list kept (item b2 '(at 0 0)))))
(want "D18 (8) a fact on b2 with a range in b1, and (9) one on b2 with a range unmappable on b1: id-range-mismatch, line 2"
      (in-order (car (map-case b2 2 5)) (car (map-case b2 (+ p1 1) (+ p2 1))))
      (list (malformed 2 'id-range-mismatch) (malformed 2 'id-range-mismatch)))

;; ---- D7, D17: diagnostics per writer, and in drafts ---------------------------------------
(define r17 (make-store! "r17"))
(define r17-alpha (id-in r17 "function alpha"))
(define r17-beta (id-in r17 "function beta"))
(define r17-gamma (id-in r17 "function gamma"))
(ask r17 'write r17-alpha "function alpha() {\n  return 11;\n}\n" "--writer" "w1")
(ask r17 'write r17-beta "function beta() {\n  return 12;\n}\n" "--writer" "w1")
(ask r17 'write r17-gamma "function gamma() {\n  return 13;\n}\n" "--writer" "w2")
(define (by-id-then-start items)
  (list-sort (lambda (a b) (or (string<? (cadr a) (cadr b))
                               (and (string=? (cadr a) (cadr b)) (< (cadr (list-ref a 4)) (cadr (list-ref b 4))))))
             items))
(define (draft-counts st writer)
  (map (lambda (x) (list (cadr (assq 'block (cdr x))) (let ((c (assq 'diagnostics (cdr x)))) (and c (cadr c)))))
       (filter (lambda (x) (and (pair? x) (eq? (car x) 'draft))) (cdr (assq 'items (cdr (ask st 'drafts "--writer" writer)))))))
(define (sorted-counts cs) (list-sort (lambda (a b) (string<? (car a) (car b))) cs))
(define-values (w1-text w1-a) (diagnostics-supply r17 "w1" "javascript" '() '() "a.js"))
(define s11 (bytes-offset w1-a "return 11"))
(define s12 (bytes-offset w1-a "return 12"))
(define-values (w1-text* ignored-bytes)
  (diagnostics-supply r17 "w1" "javascript"
                      (list (diag r17-alpha s11 (+ s11 9) r17-beta) (diag r17-beta s12 (+ s12 9) r17-alpha)
                            (diag r17-beta (+ s12 7) (+ s12 9) r17-alpha))
                      '("a.js") "a.js"))
(want "D7 w1's supply of three diagnostics on its drafts of alpha and beta is kept"
      (supply-in r17 w1-text* "diagnostics" "--for" "w1") '(ok (supplied (facts 3) (files 1))))
(want "D7 diagnostics --writer w1 lists them by block and start, each at its block's own offsets, with the via"
      (diag-answer r17 "w1")
      (append (list 'ok (cons 'items (by-id-then-start
                                       (list (list 'diagnostic r17-alpha 'error "m" '(at 21 30))
                                             (list 'diagnostic r17-beta 'error "m" '(at 20 29))
                                             (list 'diagnostic r17-beta 'error "m" '(at 27 29))))))
              (list '(stale 0) supplied-via)))
(want "D7 w2 has none: (ok (items)), no clause; and its drafts carry no diagnostics count"
      (in-order (diag-answer r17 "w2") (draft-counts r17 "w2"))
      (list '(ok (items)) (list (list r17-gamma #f))))
(want "D7 w1's drafts carry their counts: alpha 1, beta 2"
      (sorted-counts (draft-counts r17 "w1"))
      (sorted-counts (list (list r17-alpha 1) (list r17-beta 2))))
(define-values (w2-text w2-b) (diagnostics-supply r17 "w2" "javascript" '() '() "b.js"))
(define s13 (bytes-offset w2-b "return 13"))
(want "D17 w2 supplies its own diagnostic: each writer reads back only its own, with its own draft counts"
      (let-values (((text bytes) (diagnostics-supply r17 "w2" "javascript" (list (diag r17-gamma s13 (+ s13 9))) '("b.js") "b.js")))
        (in-order (supply-in r17 text "diagnostics" "--for" "w2")
              (diag-items r17 "w2") (length (diag-items r17 "w1")) (draft-counts r17 "w2")))
      (list '(ok (supplied (facts 1) (files 1)))
            (list (list 'diagnostic r17-gamma 'error "m" '(at 21 30))) 3 (list (list r17-gamma 1))))
(ask r17 'write r17-alpha "function alpha() {\n  let x = 1;\n  return 11;\n}\n" "--writer" "w1")
(want "D7 w1 grows alpha: the earlier supply file is refused supply-stale on a.js, and every diagnostic on a.js is stale"
      (in-order (supply-in r17 w1-text* "diagnostics" "--for" "w1") (diag-answer r17 "w1"))
      (list '(error supply-stale (file "a.js")) '(ok (items) (stale 3) (via))))
(let-values (((text bytes) (diagnostics-supply r17 "w1" "javascript" '() '() "a.js")))
  (let ((s12* (bytes-offset bytes "return 12")))
    (want "D7 a fresh supply after alpha grew: beta's range moved in the projected file, and its at is the same block-relative one"
          (let-values (((text* ignored) (diagnostics-supply r17 "w1" "javascript"
                                                             (list (diag r17-beta s12* (+ s12* 9) r17-alpha)) '("a.js") "a.js")))
            (in-order (> s12* s12) (supply-in r17 text* "diagnostics" "--for" "w1") (diag-items r17 "w1")))
          (list #t '(ok (supplied (facts 1) (files 1))) (list (list 'diagnostic r17-beta 'error "m" '(at 20 29)))))))

(want "D7 w2 discards its only draft: drafts counts no stale diagnostic, since it consulted none (gamma is not drafted), while diagnostics counts the one on gamma"
      (in-order (head-of (ask r17 'discard r17-gamma "--writer" "w2"))
            (ask r17 'drafts "--writer" "w2") (diag-answer r17 "w2"))
      (list 'ok '(ok (items) (stale 0) (via)) '(ok (items) (stale 1) (via))))
(define r17-w1-table (string-append r17 "/derived/diagnostics-w1-javascript.sexp"))
(define r17-w1-bytes (or (bytes-of r17-w1-table) #vu8()))
(want "D7 w1's own table does not read: drafts gives its drafts no count and the answer no clause, as with no table"
      (in-order (begin (write! r17-w1-table "(derived-table 1") 'damaged)
                (let ((a (ask r17 'drafts "--writer" "w1"))) (list (sorted-counts (draft-counts r17 "w1")) (length a))))
      (list 'damaged (list (sorted-counts (list (list r17-alpha #f) (list r17-beta #f))) 2)))
(write-bytes! r17-w1-table r17-w1-bytes)
;; The committed state's tables are named for "-", and drafts --writer -
;; does not read them: its answer is the one it gave before there was one.
(define r17-drafts-dash (ask r17 'drafts "--writer" "-"))
(want "D7 a diagnostics table for the committed store (writer -): drafts --writer - answers as it did before the table"
      (in-order (let* ((d (export-in r17))
                       (files (map (lambda (n) (list n (sha-of (string-append d "/" n)))) (projection-files d))))
                  (supply-in r17 (line (list 'supply 'diagnostics '(writer "-") '(language "javascript")
                                             '(source (vscode "1.140.0")) (list 'files files) '(replaces ("b.js"))))
                             "diagnostics"))
                (file-exists? (string-append r17 "/derived/diagnostics-%2D-javascript.sexp"))
                (equal? (ask r17 'drafts "--writer" "-") r17-drafts-dash))
      (list '(ok (supplied (facts 0) (files 1))) #t #t))
(want "D7 diagnostics of the information and hint severities are kept and listed by start"
      (let-values (((text bytes) (diagnostics-supply r17 "w2" "javascript" '() '() "b.js")))
        (let ((s (bytes-offset bytes "return alpha")))
          (let-values (((text* ignored)
                        (diagnostics-supply r17 "w2" "javascript"
                                            (list (line (list 'diagnostic r17-gamma 'hint "h" (list 'range (+ s 7) (+ s 12)) (list 'depends (list r17-gamma))))
                                                  (line (list 'diagnostic r17-gamma 'information "i" (list 'range s (+ s 6)) (list 'depends (list r17-gamma)))))
                                            '("b.js") "b.js")))
            (in-order (supply-in r17 text* "diagnostics" "--for" "w2") (diag-items r17 "w2")))))
      (list '(ok (supplied (facts 2) (files 1)))
            (list (list 'diagnostic r17-gamma 'information "i" '(at 21 27))
                  (list 'diagnostic r17-gamma 'hint "h" '(at 28 33)))))
(want "D7 the human rendering of diagnostics is item lines only"
      (let ((ls (filter (lambda (l) (> (string-length l) 0)) (lines-of (render-human (diag-answer r17 "w2"))))))
        (list (length ls) (for-all (lambda (l) (has-substring? l "(diagnostic ")) ls)))
      '(2 #t))

;; ==== what the exporter refuses, a fact's file has no key for ====
;; An unsafe path, and a child whose src is not bytes: the exporter refuses
;; both, so a fact on the file is stale and a supply answers the exporter's
;; own refusal (not supply-stale).
(define r18 (make-store! "r18"))
(define r18-gamma (id-in r18 "function gamma"))
(define r18-b (file-in r18 "b.js"))
(supply-now r18 "-" (list (sig r18-gamma "g")) '("b.js"))
(want "V11 b.js's path set to ../b.js: gamma's fact is stale, and a supply listing ../b.js answers what export-code answers, unsafe-path, not supply-stale"
      (let* ((set-answer (ask r18 'set r18-b "path" "../b.js"))
             (x (ask r18 'export-code (fresh-dir! "unsafe")))
             (s (supply-in r18 (string-append
                                 (line (list 'supply 'signatures '(writer "-") '(language "javascript")
                                             '(source (vscode "1.140.0"))
                                             (list 'files (list (list "../b.js" (make-string 64 #\a))))
                                             '(replaces ("../b.js"))))
                                 (sig r18-gamma "g"))
                           "signatures")))
        (list (head-of set-answer) (sig-of r18 r18-gamma) (equal? s x) (and (pair? s) (assq 'reason (cddr s)))))
      (list 'ok absent-stale-1 #t '(reason unsafe-path)))
;; A src that is text rather than bytes has no producer on the caller's
;; path (a text src is stored as bytes); a record from elsewhere is the only
;; way it arrives, so it is written as one.
(define r19 (make-store! "r19"))
(define r19-alpha (id-in r19 "function alpha"))
(define r19-beta (id-in r19 "function beta"))
(supply-now r19 "-" (list (sig r19-alpha "a")) '("a.js"))
(want "V11 beta's src arrives as text in a record: a.js does not project (unexportable-block), and alpha's fact, which does not list beta, is stale"
      (begin (forge-own! r19 (format "(set ~s src ~s)" r19-beta "function beta() {}\n"))
             (let ((x (ask r19 'export-code (fresh-dir! "text-src"))))
               (list (sig-of r19 r19-alpha) (head-of x) (and (pair? x) (assq 'reason (cddr x))))))
      (list absent-stale-1 '(error projection-invalid) '(reason unexportable-block)))

;; ==== a failed table write leaves the table a reader had ====
;; The write goes to a temporary, is flushed, and only then renamed over the
;; table; a flush that fails leaves the table as it was. Measured in a
;; process of its own, built with fault injection, where the flush of the
;; table's write fails. A control runs the same process with no fault.
(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
;; -> the process's whole output (stderr's banner lines, then the answer).
(define (child-supply st text env)
  (let* ((d (fresh-dir! "child")) (f (string-append d "/supply.sexp")) (out (string-append d "/out.txt")))
    (write! f text)
    (system (string-append env " CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
                           "scheme --script ../core.sc supply signatures '" f "' --store '" st "' --wire > '" out
                           "' 2>&1 < /dev/null"))
    (or (text-of out) "")))
(define r20 (make-store! "r20"))
(define r20-alpha (id-in r20 "function alpha"))
(define (r20-text sig-text)
  (let* ((d (export-in r20)) (files (map (lambda (n) (list n (sha-of (string-append d "/" n)))) (projection-files d))))
    (string-append (line (list 'supply 'signatures '(writer "-") '(language "javascript") '(source (vscode "1.140.0"))
                               (list 'files files) '(replaces ("a.js"))))
                   (sig r20-alpha sig-text))))
(define r20-table (string-append r20 "/derived/signatures-%2D-javascript.sexp"))
(want "D-I control: a supply in a process built with injection and no fault is kept, and alpha reads sig-2"
      (let ((out (child-supply r20 (r20-text "sig-2") "THEOURGIA_INJECT=on")))
        (list (has-substring? out "(ok (supplied (facts 1) (files 1)))") (sig-of r20 r20-alpha)))
      (list #t (present "sig-2")))
(define r20-before (bytes-of r20-table))
(want "D-I the table's flush fails: the supply is refused, the table is byte-identical, and alpha still reads sig-2"
      (let ((out (child-supply r20 (r20-text "sig-3") "THEOURGIA_INJECT=on THEOURGIA_FAULT=fsync-fail@derived:file=signatures")))
        (list (has-substring? out "fault-injection-armed") (has-substring? out "(ok (supplied")
              (equal? (bytes-of r20-table) r20-before) (sig-of r20 r20-alpha)))
      (list #t #f #t (present "sig-2")))

;; ==== old records under the reserved relation names ====
;; As a store written before the names were reserved holds them: they still
;; apply, and check names them. Forged into a store with one writer
;; directory and no drafts, so the helper appends to that writer's segment.
(define (clause a key) (let ((c (and (pair? a) (list? a) (assq key (filter pair? (cdr a)))))) (and c (cadr c))))
(define rf (make-store! "rf"))
(define rf-alpha (id-in rf "function alpha"))
(define rf-beta (id-in rf "function beta"))
(define rf-writer (own-writer-name rf))
(define rf-check-before (ask rf 'check))
(want "D10 setup: the store has one writer directory, holding the segment the helper appends to"
      (in-order (length (directory-list (string-append rf "/writers")))
            (file-exists? (string-append rf "/writers/" rf-writer "/000001.sexp")))
      '(1 #t))
(define rf-seq-1 (forge-own! rf (format "(link ~s calls ~s)" rf-alpha rf-beta)))
(define rf-seq-2 (forge-own! rf (format "(link ~s uses ~s surplus)" rf-beta rf-alpha)))
(define rf-check-after (ask rf 'check))
(want "D10 check: no clause before; after an old calls record and an old uses record with a surplus argument, the clause names both with their events, and the verdict is the same"
      (in-order (clause rf-check-before 'reserved-relations) (clause rf-check-before 'verdict)
            (clause rf-check-after 'reserved-relations) (clause rf-check-after 'verdict))
      (list #f 'ok
            (list (list rf-alpha 'calls rf-beta (list 'event rf-writer rf-seq-1))
                  (list rf-beta 'uses rf-alpha (list 'event rf-writer rf-seq-2)))
            'ok))
(want "D10 control: the old records apply -- refs of beta shows the edge from alpha under calls, refs of alpha the one from beta under uses"
      (let ((has (lambda (a from rel)
                   (and (pair? a) (eq? (car a) 'ok)
                        (exists (lambda (r) (and (pair? r) (member (list 'from from) r) (member (list 'rel rel) r) #t))
                                (cdr (assq 'items (cdr a))))))))
        (list (has (ask rf 'refs rf-beta) rf-alpha 'calls) (has (ask rf 'refs rf-alpha) rf-beta 'uses)))
      '(#t #t))

;; ==== the command line loads the facts' library only when it needs it ====
;; A process of its own: on a store with no table, read, outline (with
;; --with-signatures), search, refs and drafts answer without loading
;; (theourgia derived); read --signature, an option that asks for a fact,
;; loads it. The library list of the process says which.
(define (probe-load-text st)
  (string-append
    "(import (chezscheme) (theourgia rpc))\n"
    "(define st " (format "~s" st) ")\n"
    "(define (ask . a) (rpc-dispatch st a \"t\"))\n"
    "(define (loaded?) (and (member '(theourgia derived) (library-list)) #t))\n"
    "(ask 'init)\n"
    "(define ev (cdr (assq 'events (cdr (ask 'insert \"--under\" \"root\" \"--title\" \"x\" \"--text\" \"a word\")))))\n"
    "(define id (string-append (car (car (car ev))) \".\" (number->string (cdr (car (car ev))))))\n"
    "(ask 'read id) (ask 'outline \"--with-signatures\") (ask 'search \"word\")\n"
    "(ask 'refs id) (ask 'drafts \"--writer\" \"w1\")\n"
    "(define before (loaded?))\n"
    "(ask 'read id \"--signature\")\n"
    "(write (list before (loaded?)))\n"))
(want "startup: with no table, read, outline --with-signatures, search, refs and drafts load no (theourgia derived); read --signature does"
      (let* ((d (fresh-dir! "load")) (f (string-append d "/probe.ss")) (out (string-append d "/out.txt")))
        (write! f (probe-load-text (string-append d "/store")))
        (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' scheme --script '" f "' > '" out
                               "' 2>&1 < /dev/null"))
        (let ((t (or (text-of out) "")))
          (guard (e (#t (list 'UNREADABLE t)))
            (let ((p (open-string-input-port t)))
              (let loop ((last #f))
                (let ((x (read p))) (if (eof-object? x) last (loop x))))))))
      '(#f #t))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\nsupply complete\n" bad rows)
(exit (if (= bad 0) 0 1))
