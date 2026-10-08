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

;; The project template: `init --template`, `template apply` and `export`,
;; `tasks`, the template's place in describe, conflicts and the MCP tools,
;; and a task's part in discharging a decision.
;;
;; Every store here is made through the same dispatcher a caller reaches,
;; in this process; the MCP rows start the shell, which starts a daemon.
(import (chezscheme) (theourgia rpc) (theourgia ffi)
        (only (theourgia crc32) crc32-hex)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia reduce) reduce-empty reduce-apply! state-read state-block-ids)
        (only (theourgia store) open-and-reduce seal-state)
        (only (theourgia log) unreadable-behind)
        (only (theourgia commitments) commitments-answer)
        (only (theourgia template-read) store-template template-problem)
        (only (theourgia templates) built-in-template))

(include "forge-record.ss")

(define-syntax in-order
  (syntax-rules ()
    ((_) '())
    ((_ e rest ...) (let ((v e)) (cons v (in-order rest ...))))))
(define bad 0)
(define rows 0)
(define (want-1 name got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a\n" name)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" name got expected))))
;; A ROW THAT RAISES IS A FAIL LINE, and the rows after it still run.
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (want-1 name (caught got) expected))))

(define (string-contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (file-text path)
  (if (file-exists? path)
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))
      ""))
(define (write-file! path text)
  (call-with-output-file path (lambda (p) (put-string p text)) 'replace))

(register-verbs! extension-verbs)

(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/project-template-" (number->string (get-process-id))))
(define sock-root (string-append (or (getenv "THEOURGIA_TEST_SOCK") "/tmp") "/pt" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "' '" sock-root "'; mkdir -p '" root "/home' '" sock-root "/run'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(putenv "THEOURGIA_RUN" (string-append sock-root "/run"))

(define counter 0)
(define (fresh-store)
  (set! counter (+ counter 1))
  (string-append root "/s" (number->string counter)))
(define (run store . args) (rpc-dispatch store args "test"))
(define (state-of store) (open-and-reduce store))
(define (field row name) (let ((e (assq name (cdr (assq 'fields row))))) (and e (cdr e))))
(define (row-of store id) (state-read (state-of store) id))
;; describe as the daemon answers it: handed the store's published state. With
;; no state handed, describe never opens a store (describe.sc DS-2c).
(define (describe-of store) (rpc-dispatch store '(describe) "test" (open-and-reduce store)))
(define (id-by-title store title)
  (let ((s (state-of store)))
    (find (lambda (id) (equal? (field (state-read s id) 'title) title)) (state-block-ids s))))
;; Insert a block under a parent and set fields on it; -> its id.
(define (make! store under title . fields)
  (run store 'insert "--under" under "--title" title)
  (let ((id (id-by-title store title)))
    (let loop ((fs fields))
      (unless (null? fs)
        (run store 'set id (car fs) (cadr fs))
        (loop (cddr fs))))
    id))
(define (items-of a) (if (and (pair? a) (eq? (car a) 'ok) (pair? (cdr a)) (pair? (cadr a)) (eq? (caadr a) 'items)) (cdadr a) (list 'NOT-ITEMS a)))
;; The ids of the decision and task rows, leaving out a trailing scope item.
(define (ids-of a) (map cadr (filter (lambda (x) (and (pair? x) (memq (car x) '(decision task)))) (items-of a))))
(define (scope-of a) (let ((x (find (lambda (x) (and (pair? x) (eq? (car x) 'scope))) (items-of a)))) (and x (cdr x))))
(define (clause-of answer name) (let ((c (and (pair? answer) (assq name (cdr answer))))) (and c (cdr c))))

;; WHAT A REFUSAL MUST NOT CHANGE: the log's length and the store's files.
(define (log-bytes store)
  (let ((f (string-append root "/size.txt")))
    (system (string-append "find '" store "' -name '*.sexp' -path '*writers*' -exec cat {} + | wc -c | tr -d ' ' > '" f "'"))
    (file-text f)))
(define (file-list store)
  (let ((f (string-append root "/files.txt")))
    (system (string-append "cd '" store "' && find . | LC_ALL=C sort > '" f "'"))
    (file-text f)))
(define (pins store) (in-order (log-bytes store) (file-list store)))

;; ---- plain init is what it was ------------------------------------------------

(define plain (fresh-store))
(define plain-answer (run plain 'init))
(want "INIT plain init answers (ok (store <id>) (writer <w>)) and nothing more"
      (list (car plain-answer) (map car (cdr plain-answer)))
      '(ok (store writer)))
(want "INIT a store made plainly has no template block, and describe adds no clause"
      (list (store-template (state-of plain)) (clause-of (describe-of plain) 'template))
      '(#f #f))

;; ---- T1: init --template project -------------------------------------------------

(define s1 (fresh-store))
(define t1 (run s1 'init "--template" "project"))
(define created (let ((c (clause-of t1 'created))) (or c '())))
(define (created-id slug) (let ((e (assq slug created))) (and e (cadr e))))
(want "T1 init --template project answers init's clauses, the template's name, and what it created"
      (list (car t1) (map car (cdr t1)) (clause-of t1 'template) (map car created))
      '(ok (store writer template created) ("project") (design tasks template)))
(define design (created-id 'design))
(define tasks-root (created-id 'tasks))
(define template-id (created-id 'template))
(want "T1 design and tasks are top-level docs at the fixed paths, with their slugs"
      (map (lambda (id) (let ((r (row-of s1 id)))
                          (list (field r 'kind) (field r 'path) (field r 'slug) (car (cdr (assq 'position r))))))
           (list design tasks-root))
      '((doc "design.md" "design" root) (doc "tasks.md" "tasks" root)))
(want "T1 one template block, the created one, whose datum is the project template"
      (let ((s (state-of s1)))
        (list (filter (lambda (id) (eq? (field (state-read s id) 'kind) 'template)) (state-block-ids s))
              (equal? (store-template s) (built-in-template "project"))))
      (list (list template-id) #t))
(want "T1 describe carries the template's roots and relations"
      (let ((t (clause-of (describe-of s1) 'template)))
        (list (map car (cdr (assq 'roots t))) (map car (cdr (assq 'relations t)))))
      '((design tasks docs code) (implements depends-on supersedes refutes verifies conflicts-with documents)))
(define dropped (make! s1 design "A dropped decision" "kind" "decision" "status" "dropped"))
(define open-d (make! s1 design "An open decision" "kind" "decision"))
(want "T1 commitments --open lists the open decision and not the dropped one"
      (ids-of (run s1 'commitments))
      (list open-d))

;; ---- T2: apply on an existing store ------------------------------------------------

(define (store-with . setups)
  (let ((s (fresh-store)))
    (run s 'init)
    (for-each (lambda (f) (f s)) setups)
    s))
(define (doc-root! slug path) (lambda (s) (make! s "root" (string-append "doc " slug path) "kind" "doc" "path" path "slug" slug)))

(define s2 (store-with (doc-root! "design" "design.md")))
(define s2-design (id-by-title s2 "doc designdesign.md"))
(define s2-design-before (row-of s2 s2-design))
(define t2 (run s2 'template "apply" "project"))
(want "T2 apply on a store that has design creates tasks and the template block only"
      (list (car t2) (map car (clause-of t2 'created)))
      '(ok (tasks template)))
(want "T2 the existing design block is unchanged, field for field"
      (equal? (row-of s2 s2-design) s2-design-before)
      #t)
(define (refusal-of store . args)
  (let* ((before (pins store)) (a (apply run store args)) (after (pins store)))
    (list (and (pair? a) (car a)) (and (pair? a) (pair? (cdr a)) (cadr a)) (equal? before after))))
(want "T2 a second apply refuses template-present and writes nothing"
      (refusal-of s2 'template "apply" "project")
      '(error template-present #t))
(want "T2 a section slugged tasks: template-mismatch, nothing written"
      (refusal-of (store-with (lambda (s) (make! s "root" "a section" "kind" "section" "slug" "tasks")))
                  'template "apply" "project")
      '(error template-mismatch #t))
(want "T2 a doc slugged design at another path: template-mismatch, nothing written"
      (refusal-of (store-with (doc-root! "design" "other.md")) 'template "apply" "project")
      '(error template-mismatch #t))
(want "T2 a doc slugged roadmap at design.md: path-occupied, nothing written"
      (refusal-of (store-with (doc-root! "roadmap" "design.md")) 'template "apply" "project")
      '(error path-occupied #t))
(want "T2 two live blocks slugged tasks: slug-conflict, nothing written"
      (refusal-of (store-with (doc-root! "tasks" "tasks.md")
                              (lambda (s) (make! s "root" "another" "kind" "section" "slug" "tasks")))
                  'template "apply" "project")
      '(error slug-conflict #t))
(define s2t (store-with (doc-root! "design" "design.md")))
(run s2t 'del (id-by-title s2t "doc designdesign.md"))
(want "T2 a deleted doc slugged design does not count: design is created afresh"
      (let ((a (run s2t 'template "apply" "project"))) (list (car a) (map car (clause-of a 'created))))
      '(ok (design tasks template)))
;; A SLUG SPELT LIKE AN UNRELATED BLOCK'S ID is found by the slug scan, not by
;; id: the block with that id carries no such slug, so the root is created.
(define s2i (store-with (lambda (s) (make! s "root" "unrelated" "kind" "section"))))
(define unrelated (id-by-title s2i "unrelated"))
(define custom (string-append root "/custom.sexp"))
(write-file! custom (string-append "(template 1 (roots (" unrelated " doc \"x.md\" \"An id-shaped root.\")))"))
(define unrelated-before (row-of s2i unrelated))
(want "T2 a root slug spelt like an existing block's id is created, not matched to that block"
      (let ((a (run s2i 'template "apply" "--file" custom)))
        (list (car a) (map car (clause-of a 'created))
              (equal? (row-of s2i unrelated) unrelated-before)))
      (list 'ok (list (string->symbol unrelated) 'template) #t))
(want "T2 a positional that looks like a path is a name, refused as no such built-in, nothing written"
      (refusal-of (store-with) 'template "apply" custom)
      '(error unknown-template #t))
(want "T2 --template-file and --template together, and an unknown name, refuse init without making a store"
      (let ((a (fresh-store)) (b (fresh-store)))
        (list (car (run a 'init "--template" "project" "--template-file" custom)) (file-exists? a)
              (cadr (run b 'init "--template" "nonesuch")) (file-exists? b)))
      '(usage #f unknown-template #f))

;; ---- T3: tasks -------------------------------------------------------------------

(define (task! store under title . fields) (apply make! store under title "kind" "task" fields))
(define ta (task! s1 tasks-root "Write the reader" "status" "todo"))
(define tb (task! s1 tasks-root "Ship it" "status" "done" "batch" "b1"))
(define tc (task! s1 tasks-root "Odd status" "status" "later"))
(define elsewhere (task! s1 "root" "A task at the top"))
(define (task-row store id) (find (lambda (x) (and (pair? x) (equal? (cadr x) id))) (items-of (run store 'tasks "--under" "root"))))
(want "T3 exact rows: status read leniently, batch or absent, no edge so (unlinked)"
      (items-of (run s1 'tasks))
      (list (list 'task ta '(title "Write the reader") '(status todo) '(batch absent) '(implements) '(unlinked))
            (list 'task tb '(title "Ship it") '(status done) '(batch "b1") '(implements) '(unlinked))
            (list 'task tc '(title "Odd status") '(status unreadable "later") '(batch absent) '(implements) '(unlinked))
            (list 'scope tasks-root '(outside 1))))
(run s1 'link ta "implements" open-d)
(run s1 'link tb "ref" open-d)
(define gone (make! s1 design "A decision to delete" "kind" "decision"))
(run s1 'link tc "implements" gone)
(run s1 'del gone)
(want "T3 linked to a live decision: no marker; another relation, or a deleted target: still unlinked"
      (map (lambda (id) (list-tail (task-row s1 id) 5)) (list ta tb tc))
      (list (list (list 'implements (list open-d 'live)))
            (list '(implements) '(unlinked))
            (list (list 'implements (list gone 'tombstoned)) '(unlinked))))
(run s1 'unlink ta "implements" open-d)
(want "T3 unlinking restores the marker"
      (list-tail (task-row s1 ta) 5)
      '((implements) (unlinked)))
(define td (task! s1 tasks-root "Done as a symbol"))
(run s1 'batch (string-append "((set \"" td "\" status done))"))
(want "T3 --status done finds the string and the symbol; --batch matches exactly; a word outside the four refuses"
      (list (ids-of (run s1 'tasks "--status" "done")) (ids-of (run s1 'tasks "--batch" "b1"))
            (ids-of (run s1 'tasks "--batch" "b")) (cadr (run s1 'tasks "--status" "bogus")))
      (list (list tb td) (list tb) '() 'bad-request))
(want "T3 the default scope is the template's tasks root: the task at the top is absent, and present under root"
      (list (and (member elsewhere (ids-of (run s1 'tasks))) #t)
            (and (member elsewhere (ids-of (run s1 'tasks "--under" "root"))) #t)
            (and (member ta (ids-of (run s1 'tasks))) #t))
      '(#f #t #t))
;; THE TEMPLATE IS DATA: point its tasks query at design and the scope follows.
(define project-text (call-with-string-output-port (lambda (p) (write (built-in-template "project") p))))
(define (replace text from to)
  (let ((n (string-length from)))
    (let loop ((i 0) (out ""))
      (cond ((> (+ i n) (string-length text)) (string-append out (substring text i (string-length text))))
            ((string=? (substring text i (+ i n)) from) (string-append out to (substring text (+ i n) (string-length text))))
            (else (loop (+ i 1) (string-append out (string (string-ref text i)))))))))
(define in-design (task! s1 design "A task under design"))
(run s1 'set template-id "src" (replace project-text "(tasks tasks)" "(tasks design)"))
(want "T3 the queries entry moved to design: the task under design is listed, the ones under tasks are not"
      (let ((ids (ids-of (run s1 'tasks)))) (list (and (member in-design ids) #t) (and (member ta ids) #t)))
      '(#t #f))
(run s1 'set template-id "src" project-text)
(define s3 (store-with))
(define s3-a (task! s3 "root" "First"))
(define s3-s (make! s3 "root" "Holder" "kind" "section"))
(define s3-b (task! s3 s3-s "Second"))
(want "T3 with no template block the scope is the whole store"
      (ids-of (run s3 'tasks))
      (list s3-a s3-b))

;; ---- the scope the template chose is said --------------------------------------------

(define outside-d (make! s1 "root" "A decision outside design" "kind" "decision"))
(want "SCOPE in a templated store the default scope is named, with what it leaves out; --under root lists it"
      (list (scope-of (run s1 'commitments)) (and (member outside-d (ids-of (run s1 'commitments))) #t)
            (and (member outside-d (ids-of (run s1 'commitments "--under" "root"))) #t)
            (scope-of (run s1 'commitments "--under" "root")))
      (list (list design '(outside 1)) #f #t #f))
(want "SCOPE tasks likewise: named, the task at the top and the one under design counted outside"
      (list (scope-of (run s1 'tasks)) (scope-of (run s1 'tasks "--under" "root")))
      (list (list tasks-root '(outside 2)) #f))
;; OUTSIDE COUNTS THE ROWS THE SAME REQUEST WOULD LIST WITH --under root, filters
;; and all: a done decision outside design is not an open row, and is counted only
;; by the request that lists done ones.
(define done-outside (make! s1 "root" "A done decision outside design" "kind" "decision" "status" "done"))
;; AND A SKIPPED ROW OUTSIDE: a kind spelt as a string, written below the
;; caller's checks as a record from elsewhere arrives. It is a row of every
;; commitments answer that can see it, whatever the filters.
(define s1-writer (let ((id design)) (let loop ((i (- (string-length id) 1))) (if (char=? (string-ref id i) #\.) (substring id 0 i) (loop (- i 1))))))
(forge-record-as! s1 s1-writer "(put ((kind . \"decision\") (title . \"legacy outside design\")))")
(define (outside-of a) (let ((sc (scope-of a))) (and sc (cadr (cadr sc)))))
;; Every row but the scope item: decision, task and skipped rows alike.
(define (rows-of a) (filter (lambda (x) (and (pair? x) (not (eq? (car x) 'scope)))) (items-of a)))
(define (difference . args)
  (- (length (rows-of (apply run s1 (append args '("--under" "root"))))) (length (rows-of (apply run s1 args)))))
(want "SCOPE commitments: --open does not count the done decision outside, --all does, both count the skipped row; each equals rows(--under root) minus rows"
      (list (outside-of (run s1 'commitments)) (difference 'commitments)
            (outside-of (run s1 'commitments "--all")) (difference 'commitments "--all"))
      '(2 2 3 3))
(run s1 'set elsewhere "status" "todo")
(want "SCOPE tasks with a filter: outside counts only the rows the filter keeps"
      (list (outside-of (run s1 'tasks "--status" "todo")) (difference 'tasks "--status" "todo")
            (outside-of (run s1 'tasks "--status" "done")) (difference 'tasks "--status" "done"))
      '(1 1 0 0))
(run s1 'set elsewhere "status")
(run s1 'del done-outside)
(run s1 'del outside-d)

;; ---- T4: projection ---------------------------------------------------------------

(define out-md (string-append root "/md"))
(system (string-append "mkdir -p '" out-md "'"))
(define stray (task! s1 "root" "A stray task"))
(define md-answer (run s1 'export-md out-md))
(define (has-line? text line)
  (or (and (>= (string-length text) (string-length line)) (string=? (substring text 0 (string-length line)) line))
      (string-contains? text (string-append "\n" line "\n"))))
(want "T4 export-md writes design.md and tasks.md with the decisions and tasks as section headings"
      (list (has-line? (file-text (string-append out-md "/design.md")) "# An open decision")
            (has-line? (file-text (string-append out-md "/tasks.md")) "# Write the reader"))
      '(#t #t))
(want "T4 a task outside every document is reported as not in any document"
      (let find ((x md-answer))
        (cond ((and (pair? x) (member stray x) (memq 'not-in-any-document x)) #t)
              ((pair? x) (or (find (car x)) (find (cdr x))))
              (else #f)))
      #t)

(define s4 (store-with))
(define guide (make! s4 "root" "Reader guide" "kind" "doc" "path" "docs/x.md"))
(make! s4 guide "Reading the store" "kind" "section")
(define lib-src (string-append root "/lib-src"))
(system (string-append "mkdir -p '" lib-src "/code'"))
(write-file! (string-append lib-src "/code/tools.sc") "(library (tools) (export gnarlwick) (import (rnrs)) (define gnarlwick 1))\n")
(run s4 'import-code lib-src "--datum")
(define code-src (string-append root "/code-src"))
(system (string-append "mkdir -p '" code-src "/code'"))
(write-file! (string-append code-src "/code/a.js") "function alpha() {\n  return 1;\n}\n")
(write-file! (string-append code-src "/code/b.js") "function beta() {\n  return 2;\n}\n")
(run s4 'import-code code-src)
(define out4 (string-append root "/out4"))
(system (string-append "mkdir -p '" out4 "/md' '" out4 "/code'"))
(run s4 'export-md (string-append out4 "/md"))
(run s4 'export-code (string-append out4 "/code"))
(define (md5-of path)
  (let ((f (string-append root "/md5.txt")))
    (system (string-append "md5 -q '" path "' > '" f "' 2>/dev/null"))
    (file-text f)))
;; EVERY EXPORTED FILE'S FIRST LINE IS ITS `// @file` HEADER, which carries the
;; store's cut at the export, so it changes in every file when any block does.
;; What one block's change must leave alone is the rest of the other files.
(define (body-of path)
  (let ((t (file-text path)))
    (let loop ((i 0)) (cond ((>= i (string-length t)) "")
                            ((char=? (string-ref t i) #\newline) (substring t (+ i 1) (string-length t)))
                            (else (loop (+ i 1)))))))
(define (header-of path)
  (let ((t (file-text path)))
    (let loop ((i 0)) (cond ((>= i (string-length t)) t)
                            ((char=? (string-ref t i) #\newline) (substring t 0 i))
                            (else (loop (+ i 1)))))))
(define a-before (body-of (string-append out4 "/code/code/a.js")))
(define b-before (body-of (string-append out4 "/code/code/b.js")))
(define b-header-before (header-of (string-append out4 "/code/code/b.js")))
;; The code block under the file block for code/a.js, as import made it.
(define a-code
  (let* ((st (state-of s4))
         (file (find (lambda (id) (equal? (field (state-read st id) 'path) "code/a.js")) (state-block-ids st))))
    (find (lambda (id) (let ((r (state-read st id)))
                         (and (eq? (field r 'kind) 'code) (equal? (car (cdr (assq 'position r))) file))))
          (state-block-ids st))))
(run s4 'set a-code "src" "function alpha() {\n  return 3;\n}\n")
(system (string-append "mkdir -p '" out4 "/code2'"))
(define export2 (run s4 'export-code (string-append out4 "/code2")))
(system (string-append "mkdir -p '" out4 "/datum'"))
(run s4 'export-code (string-append out4 "/datum") "--datum")
(want "T4 a document under docs/ is written at docs/x.md with its section, code files and a datum library at code/"
      (list (has-line? (file-text (string-append out4 "/md/docs/x.md")) "# Reading the store")
            (file-exists? (string-append out4 "/code/code/a.js")) (file-exists? (string-append out4 "/code/code/b.js"))
            (let ((t (file-text (string-append out4 "/datum/code/tools.sc"))))
              (list (string-contains? t "(library (tools)") (string-contains? t "(export gnarlwick)")
                    (string-contains? t "(define gnarlwick 1)"))))
      '(#t #t #t (#t #t #t)))
(want "T4 changing one code block changes its file's body and no other file's; every header moves with the cut"
      (list (string? a-code) export2
            (equal? (body-of (string-append out4 "/code2/code/a.js")) a-before)
            (equal? (body-of (string-append out4 "/code2/code/b.js")) b-before)
            (> (string-length b-before) 0)
            (equal? (header-of (string-append out4 "/code2/code/b.js")) b-header-before))
      '(#t (ok (files 2)) #f #t #t #f))

;; ---- T5: a task's part in discharging a decision ------------------------------------

(define (state-of-records . events)
  (let ((r (reduce-empty))) (for-each (lambda (e) (apply reduce-apply! r e)) events) r))
(define (open-ids r) (map cadr (filter (lambda (x) (eq? (car x) 'decision)) (cdadr (commitments-answer r 'open #f #f "root")))))
(define (decision-with status-records)
  (apply state-of-records
         (append (list '("a" 1 () (put ((kind . decision) (title . "D"))))
                       '("a" 2 () (put ((kind . task) (title . "T"))))
                       '("a" 3 () (link "a.2" implements "a.1")))
                 status-records)))
(want "T5 a todo task implementing a decision does not discharge it; the same task done does"
      (list (open-ids (decision-with '(("a" 4 () (set "a.2" status "todo")))))
            (open-ids (decision-with '(("a" 4 () (set "a.2" status "todo")) ("a" 5 () (set "a.2" status "done"))))))
      '(("a.1") ()))
(want "T5 done as a symbol discharges; doing, dropped, a status in conflict and no status do not"
      (list (open-ids (decision-with '(("a" 4 () (set "a.2" status done)))))
            (open-ids (decision-with '(("a" 4 () (set "a.2" status "doing")))))
            (open-ids (decision-with '(("a" 4 () (set "a.2" status dropped)))))
            (open-ids (decision-with '(("b" 1 (("a" . 3)) (set "a.2" status "done")) ("a" 4 () (set "a.2" status "todo")))))
            (open-ids (decision-with '())))
      '(() ("a.1") ("a.1") ("a.1") ("a.1")))
(want "T5 a code block implementing a decision discharges it whatever its status"
      (open-ids (state-of-records '("a" 1 () (put ((kind . decision) (title . "D"))))
                                  '("a" 2 () (put ((kind . code) (title . "C") (status . "todo"))))
                                  '("a" 3 () (link "a.2" implements "a.1"))))
      '())
(define s5 (store-with))
(define s5-d (make! s5 "root" "Decide" "kind" "decision"))
(define s5-t (task! s5 "root" "Do it" "status" "todo"))
(run s5 'link s5-t "implements" s5-d)
(want "T5 through the command line: open while the task is todo, discharged once it is set done"
      (in-order (ids-of (run s5 'commitments)) (begin (run s5 'set s5-t "status" "done") (ids-of (run s5 'commitments))))
      (list (list s5-d) '()))

(want "SCOPE in a store without a template no scope item is added to either answer"
      (list (scope-of (run s3 'tasks)) (scope-of (run s5 'commitments)))
      '(#f #f))

;; ---- a write from the reduction refuses a load that missed a writer ----------------

(define sd (store-with))
(make! sd "root" "Something")
(define own-writer (car (directory-list (string-append sd "/writers"))))
(forge-writer-record! sd "zzbroken" "(put ((kind . section) (title . \"theirs\")))")
(define seg (string-append sd "/writers/zzbroken/000001.sexp"))
(call-with-port (open-file-output-port seg (file-options no-fail no-truncate))
  (lambda (o) (set-port-position! o 0) (put-bytevector o (string->utf8 "00000000"))))
(define export-dir (string-append root "/export-sd"))
(system (string-append "mkdir -p '" export-dir "'"))
(define builtin-refusal (run sd 'export-md export-dir))
(want "DECLARE CONTROL: with a writer unreadable, the built-in undeclared export-md refuses"
      (car builtin-refusal)
      'error)
(want "DECLARE template apply answers what export-md answers there, and writes nothing"
      (let* ((before (pins sd)) (a (run sd 'template "apply" "project")) (after (pins sd)))
        (list (equal? (list (car a) (cadr a)) (list (car builtin-refusal) (cadr builtin-refusal))) (equal? before after)))
      '(#t #t))
(want "DECLARE tasks and template export still answer, and say what the load was missing"
      (map (lambda (a) (list (car a) (and (find (lambda (x) (and (pair? x) (eq? (car x) 'incomplete))) (cdr a)) #t)))
           (list (run sd 'tasks) (run sd 'template "export")))
      '((ok #t) (error #t)))
(system (string-append "rm -rf '" sd "/writers/zzbroken'"))
(want "DECLARE the same store with the writer gone applies"
      (car (run sd 'template "apply" "project"))
      'ok)

;; ---- T6: a fifth root, a renamed relation ------------------------------------------

(define fifth (replace project-text "(code path \"code/\"" "(notes doc \"notes.md\" \"Keep notes under notes.\") (code path \"code/\""))
(run s1 'set template-id "src" fifth)
(define notes (make! s1 "root" "notes" "kind" "doc" "path" "notes.md" "slug" "notes"))
(want "T6 describe lists five roots; tasks' and commitments' scopes are unchanged"
      (list (map car (cdr (assq 'roots (clause-of (describe-of s1) 'template))))
            (and (member ta (ids-of (run s1 'tasks))) #t)
            (ids-of (run s1 'commitments)))
      (list '(design tasks docs notes code) #t (list open-d)))
(system (string-append "rm -rf '" out-md "'; mkdir -p '" out-md "'"))
(run s1 'export-md out-md)
(want "T6 export-md writes notes.md for the new root"
      (file-exists? (string-append out-md "/notes.md"))
      #t)
(run s1 'set template-id "src" (replace fifth "(verifies " "(checks "))
(want "T6 a relation renamed in the template is renamed in describe"
      (map car (cdr (assq 'relations (clause-of (describe-of s1) 'template))))
      '(implements depends-on supersedes refutes checks conflicts-with documents))
(run s1 'set template-id "src" project-text)

;; ---- R: every relation the template names links ------------------------------------
;;
;; A relation the template names that `link` refuses -- a reserved name --
;; is advice an agent cannot follow. Each one is linked and read back.

(define r5 (fresh-store))
(define r5-init (run r5 'init "--template" "project"))
(define (r5-created slug) (let ((e (assq slug (or (clause-of r5-init 'created) '())))) (and e (cadr e))))
(define r5-design (r5-created 'design))
(define r5-tasks (r5-created 'tasks))
(define (edges-of store id)
  (let ((row (row-of store id))) (if row (cdr (assq 'edges row)) '())))
(define named-relations (map car (cdr (assq 'relations (clause-of (describe-of r5) 'template)))))
(want "R1 the project template names seven relations, none of them reserved"
      named-relations
      '(implements depends-on supersedes refutes verifies conflicts-with documents))
(want "R1 every relation the template names is linked, and the link reads back"
      (map (lambda (rel)
             (let* ((from (make! r5 r5-design (string-append "from " (symbol->string rel)) "kind" "decision"))
                    (to (make! r5 r5-design (string-append "to " (symbol->string rel)) "kind" "decision"))
                    (answer (run r5 'link from (symbol->string rel) to)))
               (list rel (car answer) (and (member (cons rel to) (edges-of r5 from)) #t))))
           named-relations)
      (map (lambda (rel) (list rel 'ok #t)) named-relations))

;; ---- DR: bookkeeping after the link is not drift ------------------------------------
;;
;; The frontier of an implementer is the join of its CONTENT field events:
;; status, batch, keywords, class and slug record where a block stands, not
;; what it says. Each pair is a decision and a task with a text, linked, and
;; then one kind of write.

(define (implemented-pair! name)
  (let* ((d (make! r5 r5-design (string-append "decision " name) "kind" "decision"))
         (t (make! r5 r5-tasks (string-append "task " name) "kind" "task" "status" "doing" "src" "the work")))
    (run r5 'link t "implements" d)
    (cons d t)))
(define (drifted-ids) (ids-of (run r5 'commitments "--all" "--drifted")))
(define dr-status (implemented-pair! "status"))
(run r5 'set (cdr dr-status) "status" "done")
(define dr-src (implemented-pair! "src"))
(run r5 'set (cdr dr-src) "src" "the work, changed")
(define dr-books (implemented-pair! "books"))
(run r5 'set (cdr dr-books) "keywords" "later")
(run r5 'set (cdr dr-books) "batch" "b2")
;; THE DECISION'S OWN FRONTIER LEAVES BOOKKEEPING OUT TOO: its status set
;; after the implementer changed does not attest the change.
(define dr-decision (implemented-pair! "decision"))
(run r5 'set (cdr dr-decision) "src" "the work, changed again")
(run r5 'set (car dr-decision) "status" "open")
(define drifted-now (drifted-ids))
(want "DR1 a task set to done after it was linked has not drifted"
      (and (member (car dr-status) drifted-now) #t)
      #f)
(want "DR2 a task whose text changed after it was linked has drifted"
      (and (member (car dr-src) drifted-now) #t)
      #t)
(want "DR3 keywords and batch set after the link are not drift either"
      (and (member (car dr-books) drifted-now) #t)
      #f)
(want "DR4 a decision's status set after its implementer changed does not attest the change: still drifted"
      (and (member (car dr-decision) drifted-now) #t)
      #t)

;; ---- T7: the reader ----------------------------------------------------------------

;; Each store has the project's design and tasks roots, one task and one
;; decision in them, and one of each at the top: a readable template scopes
;; to 1, and every unreadable one leaves the whole store's 2.
(define (store-with-template-src . srcs)
  (let ((s (store-with)))
    (for-each (lambda (src i) (make! s "root" (string-append "template " (number->string i)) "kind" "template" "src" src))
              srcs (iota (length srcs)))
    (let ((d (make! s "root" "design root" "kind" "doc" "path" "design.md" "slug" "design"))
          (t (make! s "root" "tasks root" "kind" "doc" "path" "tasks.md" "slug" "tasks")))
      (task! s t "A task in tasks")
      (make! s d "A decision in design" "kind" "decision"))
    (task! s "root" "A task anywhere")
    (make! s "root" "A decision anywhere" "kind" "decision")
    s))
(define (reader-reading s)
  (list (and (store-template (state-of s)) #t)
        (filter (lambda (x) (and (pair? x) (eq? (car x) 'template))) (items-of (run s 'conflicts)))
        (length (ids-of (run s 'tasks)))
        (length (ids-of (run s 'commitments)))))
(want "T7 CONTROL: a valid datum is read with no conflict, and the scopes are its roots"
      (reader-reading (store-with-template-src project-text))
      '(#t () 1 1))
(want "T7 CONTROL: so an unreadable template is seen by the counts as well as by conflicts"
      (map (lambda (r) (list-tail r 2)) (list (reader-reading (store-with-template-src project-text))
                                              (reader-reading (store-with-template-src "(template 1)"))))
      '((1 1) (2 2)))
(want "T7 two template blocks: not read, conflicts names it, the verbs answer for the whole store"
      (reader-reading (store-with-template-src project-text project-text))
      '(#f ((template several-template-blocks)) 2 2))
(want "T7 a src holding two datums"
      (reader-reading (store-with-template-src (string-append project-text " " project-text)))
      '(#f ((template src-not-one-datum)) 2 2))
(want "T7 a src holding a string datum"
      (reader-reading (store-with-template-src (call-with-string-output-port (lambda (p) (write project-text p)))))
      '(#f ((template not-a-template-datum)) 2 2))
(want "T7 a datum lacking roots"
      (reader-reading (store-with-template-src "(template 1 (relations) (queries))"))
      '(#f ((template roots-absent)) 2 2))

(want "T7 roots repeating a slug or a path, or a root slugged template, are not a template"
      (map (lambda (src) (cadr (reader-reading (store-with-template-src src))))
           '("(template 1 (roots (a doc \"a.md\" \"s\") (a doc \"b.md\" \"s\")))"
             "(template 1 (roots (a doc \"a.md\" \"s\") (b doc \"a.md\" \"s\")))"
             "(template 1 (roots (template doc \"t.md\" \"s\")))"))
      '(((template roots-repeated)) ((template roots-repeated)) ((template roots-repeated))))

(want "T7 a roots clause that is not a list is unreadable, not absent; export refuses an unreadable template"
      (list (cadr (reader-reading (store-with-template-src "(template 1 (roots . broken))")))
            (let ((a (run (store-with-template-src "1 2") 'template "export"))) (list (car a) (cadr a))))
      '(((template roots-unreadable)) (error template-unreadable)))
(define bad-slug (string-append root "/bad-slug.sexp"))
(write-file! bad-slug "(template 1 (roots (design doc \"design.md\" \"s\") (|bad\nslug| doc \"x.md\" \"s\")))")
(want "T7 a root slug the store would refuse as a title: init refuses before making a store, apply writes nothing"
      (let ((a (fresh-store)))
        (list (cadr (run a 'init "--template-file" bad-slug)) (file-exists? a)
              (refusal-of (store-with) 'template "apply" "--file" bad-slug)))
      '(template-unreadable #f (error template-unreadable #t)))

;; A BLOCK OF KIND TEMPLATE THAT IS NOT AT THE TOP LEVEL is not the store's
;; template: describe, apply and conflicts do not see it.
(define sn (store-with))
(define sn-holder (make! sn "root" "Holder" "kind" "section"))
(make! sn sn-holder "Nested template" "kind" "template" "src" project-text)
(want "M1 a nested block of kind template: no describe clause, no conflict, and apply succeeds"
      (list (clause-of (describe-of sn) 'template)
            (filter (lambda (x) (and (pair? x) (eq? (car x) 'template))) (items-of (run sn 'conflicts)))
            (car (run sn 'template "apply" "project")))
      '(#f () ok))

;; ---- describe without a template is what it was ---------------------------------------

(define bare (run (string-append root "/no-such-store") 'describe))
(define (corrupt! s)
  (let* ((w (car (directory-list (string-append s "/writers"))))
         (seg (string-append s "/writers/" w "/000001.sexp")))
    (call-with-port (open-file-output-port seg (file-options no-fail no-truncate))
      (lambda (o) (set-port-position! o 0) (put-bytevector o (string->utf8 "garbage that is not a record\n"))))))
(define unreadable (store-with))
(corrupt! unreadable)
(want "D describe in process is the same answer with no store, an empty directory, an unreadable store, a store without a template and a templated one: it opens none of them"
      (list (equal? bare (run (begin (system (string-append "mkdir -p '" root "/empty'")) (string-append root "/empty")) 'describe))
            (equal? bare (run unreadable 'describe))
            (equal? bare (run plain 'describe))
            (equal? bare (run s1 'describe))
            (clause-of bare 'template))
      '(#t #t #t #t #f))

;; A STATE HANDED TO DESCRIBE THAT MISSED A WRITER adds no clause: a template
;; read from part of a store is not the store's template.
(define (break-writer! store)
  (forge-writer-record! store "zzbroken" "(put ((kind . section) (title . \"theirs\")))")
  (call-with-port (open-file-output-port (string-append store "/writers/zzbroken/000001.sexp") (file-options no-fail no-truncate))
    (lambda (o) (set-port-position! o 0) (put-bytevector o (string->utf8 "00000000")))))
(define sh (store-with))
(run sh 'template "apply" "project")
(define sh-whole (and (clause-of (describe-of sh) 'template) #t))
(break-writer! sh)
(want "D a handed state with every writer read adds the clause; one that missed a writer adds none"
      (list sh-whole (clause-of (describe-of sh) 'template))
      '(#t #f))
;; AND A COMPLETE REDUCTION SEALED WITH A NOTE FROM ELSEWHERE -- what the
;; daemon's probe adds when a writer became unreadable after the publication --
;; is not complete: its notes are all the sealed state's notes.
(define probe-notes (unreadable-behind (open-and-reduce sh)))
(define s1-complete (open-and-reduce s1))
(want "D a complete reduction sealed with no note adds the clause; sealed with a probe's note it adds none"
      (list (pair? probe-notes)
            (and (clause-of (rpc-dispatch s1 '(describe) "test" (seal-state s1-complete '())) 'template) #t)
            (clause-of (rpc-dispatch s1 '(describe) "test" (seal-state s1-complete probe-notes)) 'template))
      '(#t #t #f))

;; THROUGH THE DAEMON: describe answers the clause from the publication and opens
;; nothing of its own. The daemon's trace lines are counted before and after,
;; leaving out its two bookkeeping lines for each request (daemon-dispatch, and
;; routed, which names the route the request took); `conflicts`, which
;; the daemon answers by loading the store, is the control that the count does
;; see a verb that touches the store. (`outline` would not be: the daemon
;; answers it from the publication too.)
(define (count-lines pattern)
  (let ((f (string-append root "/acts.txt")))
    (system (string-append "cat '" sock-root "'/run/*/serve.log 2>/dev/null | grep -c '" pattern "' > '" f "' || true"))
    (let ((t (file-text f))) (if (> (string-length t) 0) (string->number (substring t 0 (- (string-length t) 1))) 0))))
(define (daemon-acts) (- (count-lines "(trace ") (count-lines "daemon-dispatch") (count-lines "(trace routed ")))
(define (client store . verb)
  (let ((out (string-append root "/client.out")))
    (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_HOME=" root "/home "
                           "THEOURGIA_RUN=" sock-root "/run THEOURGIA_TRACE=1 scheme --script ../theourgia.sc "
                           (apply string-append verb) " --store '" store "' --wire > '" out "' 2>/dev/null < /dev/null"))
    (let ((t (file-text out))) (guard (e (#t (list 'UNREADABLE t))) (read (open-string-input-port t))))))
(client s1 "conflicts")
(define acts-0 (daemon-acts))
(define routed-0 (count-lines "routed describe connection"))
(define via-daemon (client s1 "describe"))
(define acts-1 (daemon-acts))
(define routed-1 (count-lines "routed describe connection"))
(client s1 "conflicts")
(define acts-2 (daemon-acts))
(system (string-append "pkill -f 'serve " s1 "' 2>/dev/null; sleep 1"))
(want "D through the daemon describe is answered at the connection, carries the template clause and adds no traced load, lock or write of its own (the probe's file-size reads are not traced)"
      (list (and (clause-of via-daemon 'template) #t) (- acts-1 acts-0) (- routed-1 routed-0))
      '(#t 0 1))
(want "D CONTROL: the same count sees a verb that does touch the store"
      (> acts-2 acts-1)
      #t)
;; THE STORE'S CLAUSES COME FROM THE STATE THE DAEMON HANDS DESCRIBE: the
;; template, and the declared table, which a store made from the project
;; template has (documents, as nothing). The rest is the catalogue.
(want "D through the daemon describe carries the declared table: documents, as nothing"
      (clause-of via-daemon 'declared-relations)
      '((documents nothing)))
(define (strip-store-clauses a)
  (if (pair? a) (filter (lambda (x) (not (and (pair? x) (memq (car x) '(template declared-relations))))) a) a))
(want "D the daemon's catalogue is the in-process one, registered verbs included, byte for byte"
      (equal? (strip-store-clauses via-daemon) (run s1 'describe))
      #t)
;; A WRITER THE DAEMON COULD NOT READ: describe at the connection adds neither the
;; template (read from part of a store) nor an incomplete clause (it did not ask
;; about the store).
(break-writer! s1)
(define via-daemon-broken (client s1 "describe"))
(system (string-append "pkill -f 'serve " s1 "' 2>/dev/null; sleep 1"))
(system (string-append "rm -rf '" s1 "/writers/zzbroken'"))
(want "D through the daemon, a store with an unreadable writer: describe is the answer with no store"
      (equal? via-daemon-broken bare)
      #t)
;; A TEMPLATED STORE WHOSE PUBLICATION BECOMES INCOMPLETE: the clause it had is
;; withheld, and describe answers the catalogue.
(define incomplete (store-with))
(run incomplete 'template "apply" "project")
(define incomplete-whole (client incomplete "describe"))
(system (string-append "pkill -f 'serve " incomplete "' 2>/dev/null; sleep 1"))
(break-writer! incomplete)
(define via-daemon-incomplete (client incomplete "describe"))
(system (string-append "pkill -f 'serve " incomplete "' 2>/dev/null; sleep 1"))
(want "D through the client, a templated store whose publication is incomplete: the clause is withheld and describe answers the catalogue"
      (list (and (clause-of incomplete-whole 'template) #t)
            (and (pair? via-daemon-incomplete) (car via-daemon-incomplete)) (equal? via-daemon-incomplete bare))
      '(#t ok #t))
;; A FOLD THAT FAILS: no daemon starts, and describe is refused as every verb is.
(define unstartable (store-with))
(system (string-append "chmod 000 '" unstartable "/meta.sexp'"))
(define refused-describe (client unstartable "describe"))
(define refused-outline (client unstartable "outline"))
;; Read BEFORE anything is stopped: whether a daemon for this store is running.
(define unstartable-daemons
  (let ((f (string-append root "/daemons.txt")))
    (system (string-append "pgrep -f 'serve " unstartable "' | wc -l | tr -d ' ' > '" f "'"))
    (string->number (let ((t (file-text f))) (substring t 0 (- (string-length t) 1))))))
(system (string-append "chmod 644 '" unstartable "/meta.sexp'; pkill -f 'serve " unstartable "' 2>/dev/null"))
(printf "   (fold failure: describe answered ~s)\n" refused-describe)
(want "D a store whose fold fails: no daemon is running, and describe is refused with outline's error, not an internal one"
      (list (and (pair? refused-describe) (car refused-describe)) (and (pair? refused-outline) (car refused-outline))
            (and (pair? refused-describe) (pair? (cdr refused-describe)) (cadr refused-describe))
            (and (pair? refused-describe) (pair? refused-outline) (equal? (cadr refused-describe) (cadr refused-outline)))
            unstartable-daemons)
      '(error error serve-start-failed #t 0))
;; WHY THE DAEMON'S DESCRIBE IS A READ OF THE PUBLICATION: a template applied
;; from outside the daemon -- in this process -- reaches its describe as it
;; reaches every read there. The probe that notices the outside write starts the
;; reload and the read it ran for is answered from the publication it had
;; (measured: the first describe after the write is the old one, the next has
;; the clause), so the row asks within a few describes, not the next one.
(define outside (store-with))
(define outside-before (client outside "describe"))
(define outside-apply (run outside 'template "apply" "project"))
(define outside-after
  (let loop ((i 0))
    (let ((a (client outside "describe")))
      (if (or (clause-of a 'template) (= i 9)) a (loop (+ i 1))))))
(system (string-append "pkill -f 'serve " outside "' 2>/dev/null; sleep 1"))
(want "D a template applied from outside the daemon reaches the daemon's describe within a few reads"
      (list (clause-of outside-before 'template) (car outside-apply) (and (clause-of outside-after 'template) #t))
      '(#f ok #t))

;; ---- D1: README and the MCP tools ------------------------------------------------------

(define readme (file-text "../README.md"))
(want "D1 the first code line after README's Quick start heading is the template command"
      (let* ((at (let find ((i 0)) (cond ((> (+ i 15) (string-length readme)) #f)
                                         ((string=? (substring readme i (+ i 15)) "## Quick start\n") i)
                                         (else (find (+ i 1))))))
             (lines (and at (let split ((i (+ at 15)) (start (+ at 15)) (out '()))
                              (cond ((>= i (string-length readme)) (reverse out))
                                    ((char=? (string-ref readme i) #\newline)
                                     (split (+ i 1) (+ i 1) (cons (substring readme start i) out)))
                                    (else (split (+ i 1) start out)))))))
        (and lines (find (lambda (l) (and (> (string-length l) 4) (string=? (substring l 0 4) "    "))) lines)))
      "    theourgia init --template project")

(define (tools-list store name)
  (let ((in (string-append root "/" name "-in.jsonl")) (out (string-append root "/" name "-out.jsonl")))
    (write-file! in (string-append
                      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":"
                      "{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"clientInfo\":{\"name\":\"p\",\"version\":\"1\"}}}\n"
                      "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}\n"
                      "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\",\"params\":{}}\n"))
    (system (string-append "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_HOME=" root "/home "
                           "THEOURGIA_RUN=" sock-root "/run scheme --script ../mcp/server.sc --store '" store "' < '" in
                           "' > '" out "' 2>/dev/null"))
    (system (string-append "pkill -f 'serve " store "' 2>/dev/null; sleep 1"))
    (file-text out)))
(define sentences (map (lambda (r) (list-ref r 3)) (cdr (assq 'roots (cddr (built-in-template "project"))))))
;; One tool's description, out of the listing, by name.
(define (description-in listing tool)
  (let* ((key (string-append "\"name\":\"" tool "\",\"description\":\""))
         (n (string-length key))
         (at (let find ((i 0)) (cond ((> (+ i n) (string-length listing)) #f)
                                     ((string=? (substring listing i (+ i n)) key) (+ i n))
                                     (else (find (+ i 1)))))))
    (and at (let loop ((i at) (acc '()))
              (cond ((>= i (string-length listing)) #f)
                    ((char=? (string-ref listing i) #\\)
                     (let ((c (string-ref listing (+ i 1))))
                       (loop (+ i 2) (cons (if (char=? c #\n) #\newline c) acc))))
                    ((char=? (string-ref listing i) #\") (list->string (reverse acc)))
                    (else (loop (+ i 1) (cons (string-ref listing i) acc))))))))
(define (sentences-per-tool listing)
  (map (lambda (tool)
         (let ((d (description-in listing tool)))
           (and d (map (lambda (s) (string-contains? d s)) sentences))))
       '("theourgia_insert" "theourgia_write")))
(want "D1 on a templated store each writing tool carries all four root sentences; on a plain store neither carries any"
      (list (sentences-per-tool (tools-list s1 "templated")) (sentences-per-tool (tools-list plain "plain")))
      '(((#t #t #t #t) (#t #t #t #t)) ((#f #f #f #f) (#f #f #f #f))))

(system (string-append "rm -rf '" root "' '" sock-root "'"))
(printf "\n~a failures\nrows: ~a\nproject-template complete\n" bad rows)
(exit (if (= bad 0) 0 1))
