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
        (only (theourgia code-project) code-files code-field)
        (only (theourgia reduce) state-hash reduce-applied-cut)
        (only (theourgia evidence-index) index-checkpoint! index-forget-memory!)
        (only (theourgia wire) storable-decode string->sexpr-extended)
        (only (theourgia crc32) crc32-hex)
        (only (theourgia digest) sha256 bytevector->hex))

(include "forge-record.ss")

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

(define (write-bytes! path bv)
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p bv))))
(define (write! path s) (write-bytes! path (string->utf8 s)))
(define (bytes-of path)
  (and (file-exists? path) (call-with-port (open-file-input-port path) get-bytevector-all)))
(define (text-of path) (let ((b (bytes-of path))) (if (bytevector? b) (utf8->string b) (and b ""))))
(define (sha-of path) (bytevector->hex (sha256 (bytes-of path))))

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
(define store (string-append root "/store"))
(define src (fresh-dir! "src"))
(write! (string-append src "/a.js") "function alpha() {\n  return 1;\n}\n\nfunction beta() {\n  return 2;\n}\n")
(write! (string-append src "/b.js") "function gamma() {\n  return alpha();\n}\n")
(write! (string-append src "/empty.js") "")
(define (run . args) (rpc-dispatch store args "test"))
(run 'init)
(define imported (run 'import-code src))
(define (holder needle)
  (let ((m (find-headed (run 'grep needle) 'match))) (and m (string? (cadr m)) (cadr m))))
(define alpha (holder "function alpha"))
(define beta (holder "function beta"))
(define gamma (holder "function gamma"))

;; -> the directory the export wrote.
(define (export! . options)
  (let ((d (fresh-dir! "export")))
    (apply run 'export-code d options)
    d))
(define e0 (export!))
(define (sha0 name) (sha-of (string-append e0 "/" name)))
(want "S0 setup: import answered ok; three blocks found; the export wrote a.js, b.js and empty.js, and empty.js holds no @block line"
      (list (head-of imported) (and alpha beta gamma #t)
            (map (lambda (n) (file-exists? (string-append e0 "/" n))) '("a.js" "b.js" "empty.js"))
            (let ((t (text-of (string-append e0 "/empty.js"))))
              (and (string? t)
                   (let loop ((i 0))
                     (cond ((> (+ i 6) (string-length t)) #f)
                           ((string=? (substring t i (+ i 6)) "@block") #t)
                           (else (loop (+ i 1))))))))
      (list 'ok #t '(#t #t #t) #f))

;; ---- supply files -----------------------------------------------------------
(define (line x) (format "~s\n" x))
(define (header kind writer files replaces . language)
  (line (list 'supply kind (list 'writer writer)
              (list 'language (if (pair? language) (car language) "javascript"))
              '(source (vscode "1.140.0")) (list 'files files) (list 'replaces replaces))))
(define (files-of . names) (map (lambda (n) (list n (sha0 n))) names))
(define (signature id . depends) (line (list 'signature id "() => number" '(kind function) (list 'depends (cons id depends)))))

;; -> the answer to `supply` of a file holding `text`.
(define (supply text . args)
  (let ((path (string-append (fresh-dir! "supply") "/supply.sexp")))
    (write! path text)
    (apply run 'supply (append args (list path)))))
(define derived (string-append store "/derived"))
(define (table name) (string-append derived "/" name))
(define js-table (table "signatures-%2D-javascript.sexp"))
;; The table's datum: (derived-table 1 <store-id> <digest> (<kind> <writer> <language> (<fact> ...))).
(define (table-datum path)
  (let ((b (bytes-of path))) (and b (storable-decode (string->sexpr-extended (utf8->string b))))))
(define (table-facts path)
  (let ((d (table-datum path))) (and d (cadddr (list-ref d 4)))))
(define (malformed line reason) (list 'error 'supply-malformed (list 'line line) (list 'reason reason)))

;; ---- D2: a stale file is refused before anything is written ------------------------
(let ((a (supply (string-append (header 'signatures "-" (list (list "a.js" (sha0 "b.js"))) '("a.js"))
                                (signature alpha beta))
                 "signatures")))
  (want "D2 a.js listed with another file's digest: supply-stale naming a.js, and no derived directory is made"
        (list a (file-exists? derived))
        (list '(error supply-stale (file "a.js")) #f)))

;; ---- A1: an accepted supply -----------------------------------------------------------
(define a1-text (string-append (header 'signatures "-" (files-of "a.js" "b.js") '("a.js" "b.js"))
                               (signature alpha beta) (signature gamma)))
(let ((a (supply a1-text "signatures")))
  (want "A1 a signatures supply of the committed store is kept: two facts, two replaced files"
        a '(ok (supplied (facts 2) (files 2)))))
(want "A1 the table is derived/signatures-%2D-javascript.sexp, a checksummed datum naming this store, its kind, writer and language, beside derived/lock"
      (let ((d (table-datum js-table)))
        (list (and d (car d)) (and d (cadr d)) (and d (string? (caddr d)))
              (and d (let ((body (list-ref d 4))) (list (car body) (cadr body) (caddr body) (length (cadddr body)))))
              (file-exists? (string-append derived "/lock"))))
      (list 'derived-table 1 #t (list 'signatures "-" "javascript" 2) #t))
(want "A1 each fact is kept against its own file, with the depends stamped by id and the file by path, and the provenance"
      (map (lambda (f) (list (cadr f) (caddr f) (map car (list-ref f 3)) (map car (list-ref f 4)) (list-ref f 5)))
           (table-facts js-table))
      (list (list "a.js" (list 'signature alpha "() => number" '(kind function)) (list alpha beta) '("a.js")
                  '(vscode "1.140.0" "javascript"))
            (list "b.js" (list 'signature gamma "() => number" '(kind function)) (list gamma) '("b.js")
                  '(vscode "1.140.0" "javascript"))))
(want "A1 a dependency's stamp is the sha256 of its src; a file's stamp is not the file's digest"
      (let ((f (car (table-facts js-table))))
        (list (= 64 (string-length (cadr (car (list-ref f 3)))))
              (equal? (cadr (car (list-ref f 4))) (sha0 "a.js"))))
      '(#t #f))

;; ---- D12: only a dependency file's digest is wrong ------------------------------------
(let* ((before (bytes-of js-table))
       (a (supply (string-append (header 'signatures "-" (list (list "a.js" (sha0 "a.js")) (list "b.js" (sha0 "a.js")))
                                         '("a.js"))
                                 (signature alpha beta))
                  "signatures")))
  (want "D12 files (a.js b.js), replaces (a.js), b.js's digest wrong: supply-stale naming b.js, the table byte-identical"
        (list a (equal? (bytes-of js-table) before))
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
        (list wrong (equal? after-wrong before) right)
        (list '(error supply-stale (file "empty.js")) #t '(ok (supplied (facts 1) (files 2))))))

;; ---- R8: replacement is per file --------------------------------------------------------
(supply a1-text "signatures")
(let ((a (supply (header 'signatures "-" (files-of "a.js" "b.js") '("a.js")) "signatures")))
  (want "R8 an empty supply replacing a.js keeps b.js's fact and drops a.js's"
        (list a (map cadr (table-facts js-table)))
        (list '(ok (supplied (facts 0) (files 1))) '("b.js"))))
(supply a1-text "signatures")

;; ---- the order of the checks, and every malformed reason ------------------------------
;; Each row starts from a1-text, accepted above, and changes one thing.
(define (refused name text expected . args)
  (let* ((before (bytes-of js-table)) (a (apply supply text (if (null? args) '("signatures") args))))
    (want (string-append name ", and the table is byte-identical") (list a (equal? (bytes-of js-table) before))
          (list expected #t))))
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
(refused "M16 a diagnostic whose range ends past a.js: range-outside-file"
         (string-append (header 'diagnostics "-" (files-of "a.js" "b.js") '("a.js")) (diagnostic alpha 0 (+ a-size 1)))
         (malformed 2 'range-outside-file) "diagnostics")
(refused "M17 a diagnostic on beta whose range lies in alpha's body: id-range-mismatch"
         (let ((s (a-offset "return 1"))) (string-append (header 'diagnostics "-" (files-of "a.js" "b.js") '("a.js"))
                                                         (diagnostic beta s (+ s 6))))
         (malformed 2 'id-range-mismatch) "diagnostics")
(let* ((s (a-offset "return 1"))
       (a (supply (string-append (header 'diagnostics "-" (files-of "a.js" "b.js") '("a.js")) (diagnostic alpha s (+ s 6)))
                  "diagnostics")))
  (want "M18 control: the same range on alpha is kept, stored in alpha's own src"
        (list a (map caddr (table-facts diag-table)))
        (list '(ok (supplied (facts 1) (files 1)))
              (list (list 'diagnostic alpha 'error "m" (list 'at 21 27))))))

;; ---- the view: the committed store, or the writer's working view -------------------
;; w-2 drafts beta; its working projection of a.js differs from the committed one.
(run 'write beta "function beta() {\n  return 3;\n}\n" "--writer" "w-2")
(define e2 (export! "--working" "--writer" "w-2"))
(define (sha2 name) (sha-of (string-append e2 "/" name)))
(define w2-files (list (list "a.js" (sha2 "a.js")) (list "b.js" (sha2 "b.js"))))
(want "V0 setup: w-2's working a.js differs from the committed one, b.js does not"
      (list (equal? (sha2 "a.js") (sha0 "a.js")) (equal? (sha2 "b.js") (sha0 "b.js")))
      '(#f #t))
(want "V1 --for w-2 with the committed digests: supply-stale naming a.js"
      (supply (string-append (header 'signatures "w-2" (files-of "a.js" "b.js") '("a.js")) (signature alpha beta))
              "signatures" "--for" "w-2")
      '(error supply-stale (file "a.js")))
(want "V2 --for w-2 with its working digests: kept, in derived/signatures-w%2D2-javascript.sexp"
      (list (supply (string-append (header 'signatures "w-2" w2-files '("a.js")) (signature alpha beta))
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
        (list a (and (table-datum (table "signatures-%2D-python.sexp")) #t) (equal? (bytes-of js-table) before))
        (list '(ok (supplied (facts 1) (files 1))) #t #t)))
(let ((a (supply (header 'signatures "-" '() '() "python") "signatures" "--clear")))
  (want "C2 --clear removes the table its header names, and only that one"
        (list a (file-exists? (table "signatures-%2D-python.sexp")) (file-exists? js-table))
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
(define ignore-init (text-of ignore))
(supply a1-text "signatures")
(want "G1 a store init made already ignores /derived/, and a supply leaves its .gitignore alone"
      (text-of ignore) ignore-init)
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
      (list (equal? (writers-snapshot) log-before) (equal? (reading) reading-before)
            (equal? (bytes-of (string-append store "/request-index.sexp")) checkpoint-before))
      '(#t #t #t))

;; ---- D10: the reserved relation names ------------------------------------------------------
(define (edge verb rel) (head-of (run verb alpha rel beta)))
(want "D10 control before: link and unlink under explains are accepted"
      (list (edge 'link "explains") (edge 'unlink "explains")) '(ok ok))
(want "D10 link refuses ref, uses, calls and guards by name"
      (map (lambda (r) (run 'link alpha r beta)) '("ref" "uses" "calls" "guards"))
      (map (lambda (r) (list 'error 'reserved-relation (list 'relation r))) '(ref uses calls guards)))
(want "D10 unlink refuses the same four"
      (map (lambda (r) (run 'unlink alpha r beta)) '("ref" "uses" "calls" "guards"))
      (map (lambda (r) (list 'error 'reserved-relation (list 'relation r))) '(ref uses calls guards)))
(want "D10 control after: link under explains is accepted"
      (edge 'link "explains") 'ok)

;; An old record under a reserved name, as a store written before the names
;; were reserved holds it: it still applies, and check names it.
(define (clause a key) (let ((c (and (pair? a) (list? a) (assq key (filter pair? (cdr a)))))) (and c (cadr c))))
(define check-before (run 'check))
(define forged-seq (forge-record! store (format "(link ~s calls ~s)" alpha beta)))
(define check-after (run 'check))
(want "D10 check: no reserved-relations clause before; after an old calls record, the clause names it with its event, and the verdict is the same"
      (list (clause check-before 'reserved-relations) (clause check-before 'verdict)
            (clause check-after 'reserved-relations) (clause check-after 'verdict))
      (list #f 'ok
            (list (list alpha 'calls beta
                        (list 'event (car (directory-list (string-append store "/writers"))) forged-seq)))
            'ok))
(want "D10 control: the old record applies -- refs of beta shows the edge from alpha under calls"
      (let ((a (run 'refs beta)))
        (and (pair? a) (eq? (car a) 'ok)
             (exists (lambda (r) (and (pair? r) (member (list 'from alpha) r) (member '(rel calls) r) #t))
                     (cdr (assq 'items (cdr a))))))
      #t)

;; ---- the exporter's refusal is the supply's -------------------------------------------
;; A block that is not code, placed in a file, makes the file unexportable;
;; supply re-projects and answers what export-code answers.
(define file-a
  (let ((s (open-and-reduce store)))
    (find (lambda (id) (equal? (code-field s id 'path) "a.js")) (code-files s))))
(define note (run 'insert "--under" (or file-a "root") "--title" "a note" "--text" "not code"))
(let ((x (run 'export-code (fresh-dir! "refused")))
      (s (supply a1-text "signatures")))
  (want "X1 a file holding a block that is not code: supply answers export-code's own refusal"
        (list (and file-a #t) (head-of note) (head-of s) (equal? s x))
        '(#t ok (error projection-invalid) #t)))

(system (string-append "rm -rf '" root "'"))
(printf "\n~a failures\nrows: ~a\nsupply complete\n" bad rows)
(exit (if (= bad 0) 0 1))
