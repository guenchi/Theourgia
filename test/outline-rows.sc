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

;; `outline` answers its rows: `(ok (text "...") (rows <row> ...) <the
;; clauses it carried before>)`, each row
;; `(row <id> <kind> <mode> <path> <label> <depth> <mark> <orphan> <group>
;; (conflicts (<field> ...)) [(keywords "<s>")] [(signature "<s>")])`, and
;; the text printed from the rows alone, byte for byte what it was.
;;
;; EVERY ROW THAT READS ROWS FIRST ASKS FOR THE CLAUSE: an answer without
;; one decodes to the symbol no-rows-clause, which no expected value is,
;; so a tree without the clause reds the row rather than passing it empty.
;;
;; TWO STORES. The main one holds a document with sections (keywords on
;; two, one of them the empty string), a pathless section, a text file, a
;; datum file with a library and two code blocks (one unnamed, so labelled
;; scheme:<n>), a JavaScript file an editor supplied a signature for, an
;; orphan with a child (its parent deleted), a block with a contested
;; title and one with a contested path (a peer's concurrent writes). The
;; second is cli1's: a parent chain that closes on itself through a deleted
;; block, so its root is a conflict and an orphan, with a child.
;;
;; THE DAEMON ROWS START A DAEMON WITH --idle-exit, which leaves by itself
;; once the rows are done: this fixture sends no signal to any process.
;;
;; A WORKING ANSWER IS TOLD FROM A COMMITTED ONE BY A NEW FILE. The working
;; writer's first draft changes a section's text, which no row shows, so the
;; rows that read --working compare it only with itself; the last OR-5 row
;; drafts a new file and finds its holder and pieces in --working's rows
;; and not in the committed ones.
(import (chezscheme) (only (theourgia rpc) rpc-dispatch register-verbs! outline-text) (theourgia json)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) state-read state-block-ids reduce-applied-cut block-id provisional-id)
        (only (theourgia working) working-new!)
        (only (theourgia render) render-human)
        (only (theourgia log) log-publish! segment-sha writer-directory segment-file-name)
        (only (theourgia wire) encode-record)
        (only (theourgia digest) sha256 bytevector->hex))

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
(include "expected.ss")
(include "pid-state.ss")
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (with-expected name expected (x) (want-1 name (caught got) x)))))

(register-verbs! extension-verbs)

(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define socket-base
  (let ((v (getenv "THEOURGIA_TEST_SOCK")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define pid-text (number->string (get-process-id)))
(define root (string-append scratch-base "/outline-rows-" pid-text))
(define sock-here (string-append socket-base "/orw-" pid-text))
(system (string-append "rm -rf '" root "' '" sock-here "'; mkdir -p '" root "/home' '" sock-here "'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (or (getenv "CHEZSCHEMELIBEXTS") ".sc:.ss"))
(define tree (string-append (current-directory) "/.."))

;; ---- helpers ------------------------------------------------------------------------

;; THE TWO FUNCTIONS ARE LOOKED UP WHEN A ROW CALLS THEM, not imported: a
;; tree without them then reds the rows that use them and runs the rest,
;; where an import would stop the whole file before its first row.
(define (outline-rows . args)
  (apply (eval 'outline-rows (environment '(theourgia rpc))) args))
(define (render-outline-rows . args)
  (apply (eval 'render-outline-rows (environment '(theourgia rpc))) args))

;; The clause headed `name` among an answer's elements, or #f.
(define (clause answer name)
  (and (pair? answer) (list? answer)
       (find (lambda (x) (and (pair? x) (eq? (car x) name))) (cdr answer))))
(define (heads answer) (and (pair? answer) (list? answer) (map (lambda (c) (and (pair? c) (car c))) (cdr answer))))
(define (text-of answer) (let ((t (clause answer 'text))) (and t (cadr t))))
;; The rows an answer carries, or the symbol no-rows-clause.
(define (rows-of answer) (let ((r (clause answer 'rows))) (if r (cdr r) 'no-rows-clause)))
(define (row-for rs id) (and (list? rs) (find (lambda (r) (equal? (cadr r) id)) rs)))
;; A row's elements by name.
(define (row-kind r) (list-ref r 2))
(define (row-mode r) (list-ref r 3))
(define (row-path r) (list-ref r 4))
(define (row-label r) (list-ref r 5))
(define (row-depth r) (list-ref r 6))
(define (row-mark r) (list-ref r 7))
(define (row-orphan r) (list-ref r 8))
(define (row-group r) (list-ref r 9))
(define (row-conflicts r) (cadr (list-ref r 10)))
(define (row-extra r name) (let ((e (assq name (list-tail r 11)))) (and e (cadr e))))
(define (string-count text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0) (k 0))
      (cond ((> (+ i n) m) k)
            ((string=? (substring text i (+ i n)) needle) (loop (+ i n) (+ k 1)))
            (else (loop (+ i 1) k))))))
(define (lines-of s)
  (let loop ((cs (string->list s)) (cur '()) (acc '()))
    (cond ((null? cs) (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc))))
          ((char=? (car cs) #\newline) (loop (cdr cs) '() (cons (list->string (reverse cur)) acc)))
          (else (loop (cdr cs) (cons (car cs) cur) acc)))))
;; The text line drawn for `id`, or #f; and what it prints after the id.
(define (line-for text id)
  (find (lambda (l) (let ((k (string-count l (string-append "- " id "  ")))) (> k 0))) (lines-of text)))
(define (after-id line id)
  (let* ((needle (string-append "- " id "  ")) (n (string-length needle)))
    (let loop ((i 0))
      (cond ((> (+ i n) (string-length line)) #f)
            ((string=? (substring line i (+ i n)) needle) (substring line (+ i n) (string-length line)))
            (else (loop (+ i 1)))))))
;; What the line prints after `:: `, or #f.
(define (signature-in line)
  (let loop ((i 0))
    (cond ((> (+ i 4) (string-length line)) #f)
          ((string=? (substring line i (+ i 4)) "  ::") (substring line (+ i 5) (string-length line)))
          (else (loop (+ i 1))))))
(define (events-of answer)
  (if (and (pair? answer) (eq? (car answer) 'batch))
      (apply append (map events-of (cadr answer)))
      (cadr (assq 'events (cdr answer)))))
(define (made answer)
  (let ((e (car (events-of answer)))) (block-id (car e) (cdr e))))
(define (last-seq answer) (cdr (car (reverse (events-of answer)))))
(define (write-file! path s)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (p) (put-bytevector p (string->utf8 s)))))
(define (file-text path)
  (if (not (file-exists? path)) ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))
(define (sh-out cmd)
  (let* ((p (process cmd)) (t (get-string-all (car p))))
    (close-port (car p)) (close-port (cadr p))
    (if (eof-object? t) "" t)))
(define (datum-of text) (guard (e (#t (list 'unreadable text))) (read (open-string-input-port text))))

(define (store-at name)
  (let ((s (string-append root "/" name)))
    (lambda args (rpc-dispatch s args "test"))))
(define (ins! run parent pred fields)
  (made (run 'batch (format "((insert ~s ~s ~s))" parent pred fields))))
(define peer "zzzzzzzz")
;; A peer writer's records, published with a dependency on the local
;; writer's sequence it had seen: a write made after that sequence is
;; concurrent with it.
(define (publish-peer! store records)
  (let loop ((rs records) (k 1))
    (unless (null? rs)
      (let ((bytes (encode-record k (+ 1757300000000 k) "peer" (list (car (car rs))) (cdr (car rs)))))
        (log-publish! store peer k bytes (segment-sha bytes)))
      (loop (cdr rs) (+ k 1)))))

;; ---- the main store -----------------------------------------------------------------

(define main-store (string-append root "/s"))
(define run (store-at "s"))
(define WR (cadr (assq 'writer (cdr (run 'init)))))
;; A JAVASCRIPT FILE, imported as the editor's projection is, so a supply of
;; signatures can name its blocks.
(define src-dir (string-append root "/js"))
(system (string-append "mkdir -p '" src-dir "'"))
(write-file! (string-append src-dir "/a.js") "// @block new\nfunction alpha() {\n  return 1;\n}\n// @block new\nfunction beta() {\n  return 2;\n}\n")
(define imported (run 'import-code src-dir))
(define (holder needle)
  (let* ((g (run 'grep needle))
         (m (let walk ((x g)) (cond ((and (pair? x) (eq? (car x) 'match)) x)
                                    ((pair? x) (or (walk (car x)) (walk (cdr x))))
                                    (else #f)))))
    (and m (string? (cadr m)) (cadr m))))
(define ALPHA (holder "function alpha"))
(define BETA (holder "function beta"))
;; THE SUPPLY THE EDITOR MAKES: the view exported, every projected file
;; listed with its digest, one signature for alpha.
(define export-dir (string-append root "/x"))
(run 'export-code export-dir)
(define (sha-of path) (bytevector->hex (sha256 (call-with-port (open-file-input-port path) get-bytevector-all))))
(define supply-path (string-append root "/supply.sexp"))
(write-file! supply-path
             (string-append
               (format "~s\n" (list 'supply 'signatures (list 'writer "-") (list 'language "javascript")
                                    '(source (vscode "1.140.0"))
                                    (list 'files (list (list "a.js" (sha-of (string-append export-dir "/a.js")))))
                                    '(replaces ("a.js"))))
               (format "~s\n" (list 'signature ALPHA "alpha(): number" '(kind function) (list 'depends (list ALPHA))))))
(define supplied (run 'supply "signatures" supply-path))

(define DOC (ins! run 'root #f '((kind . doc) (title . "Guide") (src . ""))))
(define S1 (ins! run DOC #f '((kind . section) (title . "Intro") (level . 2) (keywords . "start, here") (src . "intro\n"))))
(define S2 (ins! run DOC S1 '((kind . section) (title . "Usage") (level . 2) (keywords . "") (src . "usage\n"))))
(define PS (ins! run 'root #f '((kind . section) (src . "no title, no path\n"))))
(define TXT (ins! run 'root #f '((kind . file) (mode . text) (path . "notes.txt"))))
(define FD (ins! run 'root #f '((kind . file) (mode . datum) (path . "lib.sc"))))
(define L (ins! run FD #f '((kind . library) (mode . datum) (lang . chez) (name . (lib l)) (imports . ((rnrs))))))
(define KD1 (ins! run L #f '((kind . code) (mode . datum) (lang . scheme) (body . (define y 2)))))
(define KD2 (ins! run L KD1 '((kind . code) (mode . datum) (lang . scheme) (body . (display y)))))
(define V (ins! run 'root #f '((kind . section) (title . "Victim") (src . "v\n"))))
(define OP (ins! run V #f '((kind . section) (title . "Orphan") (src . "o\n"))))
(define OC (ins! run OP #f '((kind . section) (title . "Orphan child") (src . "oc\n"))))
(define CT-answer (run 'batch (format "((insert root #f ~s))" '((kind . section) (title . "Before") (src . "t\n")))))
(define CT (made CT-answer))
(define CP-answer (run 'batch (format "((insert root #f ~s))" '((kind . section) (title . "Pathy") (path . "p0") (src . "p\n")))))
(define CP (made CP-answer))
(run 'set CT "title" "Ours")
(run 'set CP "path" "ours.txt")
(publish-peer! main-store
               (list (cons (cons WR (last-seq CT-answer)) (list 'set CT 'title "Theirs"))
                     (cons (cons WR (last-seq CP-answer)) (list 'set CP 'path "theirs.txt"))))
(run 'del V)

(define st (open-and-reduce main-store))
(define (view-fields id) (cdr (assq 'fields (cadr (run 'read id)))))
(define (field-of id k) (let ((e (assq k (view-fields id)))) (and e (cdr e))))
(define live-ids
  (filter (lambda (id) (not (cdr (assq 'deleted (cdr (state-read st id)))))) (state-block-ids st)))

(want "SETUP the main store: the import, the supply, every block made, the title and the path contested"
      (in-order (car imported) (car supplied)
                (map string? (list ALPHA BETA DOC S1 S2 PS TXT FD L KD1 KD2 V OP OC CT CP))
                (and (pair? (field-of CT 'title)) (car (field-of CT 'title)))
                (and (pair? (field-of CP 'path)) (car (field-of CP 'path))))
      (list 'ok 'ok (map (lambda (x) #t) (iota 16)) 'conflict 'conflict))

;; ---- cli1's store: a cycle through a deleted block ------------------------------------

(define cyc-store (string-append root "/c"))
(define runc (store-at "c"))
(define WC (cadr (assq 'writer (cdr (runc 'init)))))
(define cA (ins! runc 'root #f '((title . "A"))))
(define cB (ins! runc cA #f '((title . "B"))))
(define cC-answer (runc 'batch (format "((insert ~s #f ~s))" cB '((title . "C")))))
(define cC (made cC-answer))
(publish-peer! cyc-store (list (cons (cons WC (last-seq cC-answer)) (list 'move cA cB 1))))
(runc 'del cA)
(define cyc-pinned (string-append "- " cB "  B  conflict  orphan\n" "  - " cC "  C\n"))

;; ---- OR-1 text == rows -------------------------------------------------------------------

(define plain (run 'outline))
(define depth1 (run 'outline "--depth" "1"))
(define with-kw (run 'outline "--with-keywords"))
(define with-sig (run 'outline "--with-signatures"))
(define WW "w1")
(rpc-dispatch main-store (list 'write S1 "intro, drafted\n") "test" #f WW)
(define working (rpc-dispatch main-store '(outline "--working") "test" #f WW))
(define cyc-plain (runc 'outline))

(want "OR-1 the text clause is the rows rendered, plain, --depth 1, --with-keywords, --with-signatures, --working, and on cli1's store"
      (map (lambda (a) (let ((r (rows-of a))) (and (list? r) (equal? (text-of a) (render-outline-rows r)))))
           (list plain depth1 with-kw with-sig working cyc-plain))
      '(#t #t #t #t #t #t))
(want "OR-1 the same rows from outline-rows itself, in process, and outline-text is their rendering"
      (let ((rs (outline-rows st #f #f)))
        (list (equal? rs (rows-of plain)) (equal? (outline-text st #f #f) (text-of plain))))
      '(#t #t))

;; ---- OR-2 per row ------------------------------------------------------------------------

(define prs (rows-of plain))
(define crs (rows-of cyc-plain))
(define (row-view-fields id)
  (let ((r (row-for prs id))) (and r (list (row-kind r) (row-mode r) (row-path r)))))
(want "OR-2 kind, mode and path are the view's values, #f when absent, the contested path its conflict datum"
      (map row-view-fields (list DOC S1 PS TXT FD L KD1 KD2 CP ALPHA))
      (map (lambda (id) (list (field-of id 'kind) (field-of id 'mode) (field-of id 'path)))
           (list DOC S1 PS TXT FD L KD1 KD2 CP ALPHA)))
(want "OR-2 the contested path is a (conflict ...) datum in the row, not #f"
      (let ((r (row-for prs CP))) (and r (pair? (row-path r)) (car (row-path r))))
      'conflict)
;; THE LABEL TWICE: written out here, and as the text prints it after the
;; id. A change to how the label is computed reds the first, a change to
;; how the renderer prints it the second.
(want "OR-2 labels written out: the title, the path, the library's name, a datum code block's derived name, the unnamed one scheme:2, a pathless section empty"
      (map (lambda (id) (let ((r (row-for prs id))) (and r (row-label r)))) (list DOC TXT L KD2 PS))
      (list "Guide" "notes.txt" "(lib l)" "scheme:2" ""))
(want "OR-2 every row's label is what its text line prints after the id, up to the next column"
      (let ((t (text-of plain)))
        (if (list? prs)
            (filter (lambda (r)
                      (let ((after (after-id (or (line-for t (cadr r)) "") (cadr r))))
                        (not (and after (>= (string-length after) (string-length (row-label r)))
                                  (string=? (substring after 0 (string-length (row-label r))) (row-label r))))))
                    prs)
            prs))
      '())
(want "OR-2 cli1's conflict-and-orphan root is (conflict #t tree), its child (1 #f #f tree)"
      (list (let ((r (row-for crs cB))) (and r (list (row-depth r) (row-mark r) (row-orphan r) (row-group r))))
            (let ((r (row-for crs cC))) (and r (list (row-depth r) (row-mark r) (row-orphan r) (row-group r)))))
      '((0 conflict #t tree) (1 #f #f tree)))
(want "OR-2 an ordinary orphan root is (#f #t orphans), its child (1 #f #f orphans), a placed block (#f #f tree)"
      (map (lambda (id) (let ((r (row-for prs id))) (and r (list (row-depth r) (row-mark r) (row-orphan r) (row-group r)))))
           (list OP OC S1))
      '((0 #f #t orphans) (1 #f #f orphans) (1 #f #f tree)))
(want "OR-2 conflicts: (title) for the contested title, (path) for the contested path, () for a plain block"
      (map (lambda (id) (let ((r (row-for prs id))) (and r (row-conflicts r)))) (list CT CP S1))
      '((title) (path) ()))

;; ---- OR-3 depth, keywords, signatures ---------------------------------------------------

;; The JavaScript file block holding alpha, the one root not made by an insert here.
(define JSF (cadr (assq 'position (cadr (run 'read ALPHA)))))
(define roots (list JSF DOC PS TXT FD CT CP))
(want "OR-3 --depth 1 answers the roots at depth 0 and nothing deeper, then the orphan root"
      (let ((r (rows-of depth1)))
        (and (list? r) (list (map row-depth r) (list-sort string<? (map cadr (filter (lambda (x) (eq? (row-group x) 'tree)) r)))
                             (map cadr (filter (lambda (x) (eq? (row-group x) 'orphans)) r)))))
      (list (map (lambda (x) 0) (iota (+ 1 (length roots))))
            (list-sort string<? roots)
            (list OP)))
(want "OR-3 --depth 0 answers (text \"\") and (rows)"
      (let ((a (run 'outline "--depth" "0"))) (list (clause a 'text) (clause a 'rows)))
      '((text "") (rows)))
(want "OR-3 --with-keywords adds the clause to exactly the rows whose keywords field is a string, equal to it; none without the option"
      (let ((r (rows-of with-kw)))
        (and (list? r)
             (list (map (lambda (x) (list (cadr x) (row-extra x 'keywords))) (filter (lambda (x) (row-extra x 'keywords)) r))
                   (filter (lambda (x) (row-extra x 'keywords)) prs))))
      (list (list (list S1 "start, here") (list S2 "")) '()))
(want "OR-3 --with-signatures adds the clause to exactly the row the lookup answered, equal to what the text prints after ::"
      (let ((r (rows-of with-sig)))
        (and (list? r)
             (list (map (lambda (x) (list (cadr x) (row-extra x 'signature))) (filter (lambda (x) (row-extra x 'signature)) r))
                   (signature-in (or (line-for (text-of with-sig) ALPHA) "")))))
      (list (list (list ALPHA "alpha(): number")) "alpha(): number"))

;; ---- OR-4 PIN the human form --------------------------------------------------------------

;; The human form a CLI prints for `args` on `store`, from the library root
;; `lib`, run locally.
(define (human lib store args)
  (sh-out (string-append "cd '" lib "/theourgia' && CHEZSCHEMELIBDIRS='" lib "' CHEZSCHEMELIBEXTS='" exts
                         "' THEOURGIA_LOCAL=1 THEOURGIA_HOME='" root "/home' scheme --script core.sc outline --store '"
                         store "' " args " 2>/dev/null")))
(define this-lib (string-append tree "/.."))
(define base-lib (getenv "THEOURGIA_BASE_LIBDIR"))
(want "OR-4 PIN cli1's store prints cli1's pinned bytes, and the human form is the text clause"
      (list (human this-lib cyc-store "") (render-human cyc-plain))
      (list cyc-pinned cyc-pinned))
(if (and (string? base-lib) (> (string-length base-lib) 0) (file-exists? (string-append base-lib "/theourgia/core.sc")))
    (want "OR-4 PIN outline, --depth 1, --with-keywords print what the base tree's CLI prints, on both stores"
          (map (lambda (sa) (let ((a (human this-lib (car sa) (cdr sa))) (b (human base-lib (car sa) (cdr sa))))
                              (if (and (equal? a b) (> (string-length a) 0)) 'same (list 'differ a b))))
               (list (cons main-store "") (cons main-store "--depth 1") (cons main-store "--with-keywords")
                     (cons cyc-store "") (cons cyc-store "--depth 1") (cons cyc-store "--with-keywords")))
          '(same same same same same same))
    (printf "NOTE OR-4 the base differential did not run: THEOURGIA_BASE_LIBDIR is unset; the pinned bytes above still ran\n"))

;; ---- OR-5 through the daemon, in wire mode -------------------------------------------------

(define dlog (string-append root "/daemon.out"))
(define dpid
  (guard (e (#t #f))
    (let* ((p (process (string-append "cd '" sock-here "' && CHEZSCHEMELIBDIRS='" libs "' CHEZSCHEMELIBEXTS='" exts
                                      "' THEOURGIA_HOME='" root "/home' scheme --script '" tree "/theourgiad.sc' serve '"
                                      main-store "' --socket s --idle-exit 6 > '" dlog "' 2>&1 < /dev/null & echo $!")))
           (n (read (car p))))
      (close-port (car p)) (close-port (cadr p))
      (and (integer? n) (> n 0) n))))
(define serving
  (let loop ((i 0))
    (cond ((> (string-count (file-text dlog) "(serving ") 0) 'serving)
          ((>= i 300) 'timeout)
          (else (system "sleep 0.05") (loop (+ i 1))))))
(define sock (string-append sock-here "/s"))
;; THE CLI'S WIRE FORM through the daemon. It forwards no writer for
;; outline (its envelope writer is only a --writer, which outline's usage
;; does not take), so the working view is asked through the MCP shell below,
;; which sends THEOURGIA_WRITER's.
(define (wire . args)
  (datum-of (sh-out (string-append "cd '" tree "' && CHEZSCHEMELIBDIRS='" libs "' CHEZSCHEMELIBEXTS='" exts
                                   "' THEOURGIA_HOME='" root "/home' scheme --script core.sc outline --store '" main-store
                                   "' --socket '" sock "' --wire "
                                   (apply string-append (map (lambda (a) (string-append a " ")) args)) " 2>/dev/null"))))
;; THE MCP SHELL'S OUTLINE TOOL, through the same daemon, as writer WW: its
;; content[0].text read as a datum, or a reading of what came back instead.
(define (mcp-outline . argv)
  (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e))))
    (let-values (((to from errs pid)
                  (open-process-ports
                    (string-append "cd '" tree "' && CHEZSCHEMELIBDIRS='" libs "' CHEZSCHEMELIBEXTS='" exts
                                   "' THEOURGIA_HOME='" root "/home' THEOURGIA_RUN='" sock-here "/run' THEOURGIA_WRITER=" WW
                                   " exec scheme --script mcp/server.sc --store '" main-store "' --socket '" sock "' 2>/dev/null")
                    'line (native-transcoder))))
      (for-each (lambda (f) (put-string to f) (put-string to "\n"))
                (list (string-append "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":"
                                     "{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},"
                                     "\"clientInfo\":{\"name\":\"probe\",\"version\":\"1\"}}}")
                      "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"
                      (string-append "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"theourgia_outline\","
                                     "\"arguments\":{\"argv\":" (json->string (list->vector argv)) "}}}")))
      (flush-output-port to)
      (close-port to)
      (let loop ((out '()))
        (let ((line (get-line from)))
          (if (eof-object? line)
              (begin
                (close-port from)
                (let* ((msgs (map (lambda (l) (guard (e (#t #f)) (string->json l))) (reverse out)))
                       (ref (lambda (o k) (and (pair? o) (let ((m (assoc k o))) (and m (cdr m))))))
                       (m3 (find (lambda (m) (equal? (ref m "id") 3)) msgs))
                       (content (ref (ref m3 "result") "content"))
                       (text (and (vector? content) (> (vector-length content) 0) (ref (vector-ref content 0) "text"))))
                  (if (string? text) (datum-of text) (list 'no-text (reverse out)))))
              (loop (cons line out))))))))
(want "SETUP a daemon serves the main store, its socket in place"
      (list serving (and dpid #t) (file-exists? sock))
      '(serving #t #t))
(define via-cli (list (wire) (wire "--with-signatures")))
(define via-mcp (list (mcp-outline) (mcp-outline "--working") (mcp-outline "--with-signatures" "--working")))
(want "OR-5 through the daemon, text, rows, then exactly the clauses the answer carried before, in order: the CLI plain and with signatures, the MCP shell plain, working, and both"
      (map heads (append via-cli via-mcp))
      '((text rows cut) (text rows stale via cut) (text rows cut) (text rows cut working) (text rows stale via cut working)))
(want "OR-5 the daemon's rows are the in-process ones: plain through the CLI and the MCP shell, working through the MCP shell"
      (let ((same? (lambda (a b) (let ((x (rows-of a)) (y (rows-of b))) (and (list? x) (list? y) (equal? x y))))))
        (list (same? (car via-cli) plain) (same? (car via-mcp) plain) (same? (cadr via-mcp) working)))
      '(#t #t #t))
;; THE DAEMON LEAVES BY ITSELF: its idle exit, then its pid gone.
(define daemon-left
  (let loop ((i 0))
    (cond ((> (string-count (file-text dlog) "(idle-exit ") 0) 'idle-exit)
          ((>= i 400) 'still-serving)
          (else (system "sleep 0.05") (loop (+ i 1))))))
(want "SETUP the daemon left by its idle exit and is gone"
      (list daemon-left
            (and dpid (let wait ((k 0)) (cond ((pid-gone? dpid) #t) ((> k 200) #f) (else (system "sleep 0.05") (wait (+ k 1)))))))
      '(idle-exit #t))
;; A WORKING VIEW THAT IS REFUSED IS THE REFUSAL, no rows: a writer id that
;; is not one (upper case), and no writer at all.
(want "OR-5 a refused working view answers the refusal as it did, with no rows clause"
      (list (rpc-dispatch main-store '(outline "--working") "test" #f "Wbad")
            (rpc-dispatch main-store '(outline "--working") "test"))
      '((error bad-request invalid-working-writer) (error writer-required)))
;; AN INCOMPLETE STORE: a mirror's older segment with readable damage; the
;; dispatcher's (incomplete ...) comes after the receipt, the rows before it.
(define inc-store (string-append root "/i"))
(define runi (store-at "i"))
(runi 'init)
(runi 'insert "--title" "Beta" "--text" "a needle")
(let loop ((rs (list (encode-record 1 1757300000001 "peer" '() '(set "t" x "x"))
                     (encode-record 2 1757300000002 "peer" '() '(set "t" y "y"))))
           (k 1))
  (unless (null? rs)
    (log-publish! inc-store peer k (car rs) (segment-sha (car rs)))
    (loop (cdr rs) (+ k 1))))
(let* ((path (string-append (writer-directory inc-store peer) "/" (segment-file-name 1)))
       (bv (call-with-port (open-file-input-port path) get-bytevector-all))
       (i (quotient (bytevector-length bv) 2)))
  (bytevector-u8-set! bv i (fxlogxor (bytevector-u8-ref bv i) 1))
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p bv))))
(want "OR-5 an incomplete store: text, rows, the receipt, then the dispatcher's incomplete clause"
      (heads (runi 'outline))
      '(text rows cut incomplete))
;; THE ROWS OF --working ARE THE WORKING VIEW'S. A new file the writer
;; drafted is in that view and not in the committed store: its holder, at
;; the top of the tree, and its two pieces under it. A handler that answered
;; --working from the committed reduction prints the same text as itself and
;; passes every row above; it lists none of the three here.
(define fresh-draft
  (working-new! main-store #f WW "fresh.sc" '((lang . scheme))
                (string->utf8 "(define (fa) 1)\n\n(define (fb) 2)\n")
                (format "~s" (reduce-applied-cut (open-and-reduce main-store)))))
(define FRESH (provisional-id "fresh.sc"))
(define (fresh-rows answer)
  (let ((rs (rows-of answer)))
    (and (list? rs)
         (map (lambda (id)
                (let ((r (row-for rs id)))
                  (and r (list (row-label r) (row-depth r) (row-mark r) (row-orphan r) (row-group r)))))
              (list FRESH (string-append FRESH "/0") (string-append FRESH "/1"))))))
(want "OR-5 --working's rows are the working view's: a drafted new file's holder at depth 0 in the tree and its two pieces under it, none of them in the committed rows"
      (list (fresh-rows (rpc-dispatch main-store '(outline "--working") "test" #f WW))
            (fresh-rows (run 'outline)))
      '((("fresh.sc" 0 #f #f tree) ("fa" 1 #f #f tree) ("fb" 1 #f #f tree)) (#f #f #f)))

;; ---- OR-6 the printer on hand-built rows ---------------------------------------------------

(define hand-rows
  (list (list 'row "c.2" #f #f #f "B" 0 'conflict #t 'tree '(conflicts ()))
        (list 'row "c.3" #f #f #f "C" 1 #f #f 'tree '(conflicts ()))
        (list 'row "o.1" 'section #f #f "Orphan" 0 #f #t 'orphans '(conflicts ()) '(keywords "k, w") '(signature "f(): x"))
        (list 'row "o.2" 'section #f #f "Under" 1 #f #f 'orphans '(conflicts ()))))
(want "OR-6 render-outline-rows: cli1's pinned bytes, then the orphans section with a keywords and a signature column; its root prints no orphan word"
      (render-outline-rows hand-rows)
      (string-append "- c.2  B  conflict  orphan\n"
                     "  - c.3  C\n"
                     "orphans:\n"
                     "- o.1  Orphan  [k, w]  :: f(): x\n"
                     "  - o.2  Under\n"))

;; ---- OR-7 the migrated assertions (their fixtures run them) ------------------------------
;;
;; q8-report (2), daemon-e1 (3), read-receipt (1) and supply (5) assert the
;; new shape themselves; the gate runs them. The prefix readers named in
;; the brief are not touched by this change.

;; ---- OR-8 one view per block, read in the source ------------------------------------------

;; The `(define (outline-rows ...) ...)` form of ../rpc.sc, read as data.
(define rows-form
  (guard (e (#t #f))
    (let ((lib (call-with-input-file "../rpc.sc"
                 (lambda (p) (let loop ((d (read p))) (if (and (pair? d) (eq? (car d) 'library)) d (loop (read p))))))))
      (let walk ((x lib))
        (cond ((and (pair? x) (eq? (car x) 'define) (pair? (cdr x)) (pair? (cadr x)) (eq? (car (cadr x)) 'outline-rows)) x)
              ((pair? x) (or (walk (car x)) (walk (cdr x))))
              (else #f))))))
(define (symbol-count x s)
  (cond ((eq? x s) 1)
        ((pair? x) (+ (symbol-count (car x) s) (symbol-count (cdr x) s)))
        (else 0)))
;; Every call (label-of a b c) in the form, and the variables bound to
;; (view-of ...) and to (state-outline ...).
(define (calls-of x name)
  (cond ((and (pair? x) (eq? (car x) name)) (cons x (calls-of (cdr x) name)))
        ((pair? x) (append (calls-of (car x) name) (calls-of (cdr x) name)))
        (else '())))
(define (bound-to x name)
  (cond ((and (pair? x) (symbol? (car x)) (pair? (cdr x)) (pair? (cadr x)) (eq? (car (cadr x)) name) (null? (cddr x))) (list (car x)))
        ((pair? x) (append (bound-to (car x) name) (bound-to (cdr x) name)))
        (else '())))
(want "OR-8 in outline-rows: view-of once, state-outline once, state-read and title-of not at all"
      (and rows-form (map (lambda (s) (symbol-count rows-form s)) '(view-of state-outline state-read title-of)))
      '(1 1 0 0))
;; label-of is handed `children-of`, the lookup into the children index
;; (`children-of` reads `children`), and that index is built from the rows
;; state-outline answered: one listing, one set of rows.
(define (defined-as x name)
  (cond ((and (pair? x) (eq? (car x) 'define) (pair? (cdr x))
              (or (eq? (cadr x) name) (and (pair? (cadr x)) (eq? (car (cadr x)) name))))
         x)
        ((pair? x) (or (defined-as (car x) name) (defined-as (cdr x) name)))
        (else #f)))
(want "OR-8 label-of is called once, with the record view-of answered and children-of, whose index is built from the rows state-outline answered"
      (and rows-form
           (let ((calls (calls-of rows-form 'label-of)) (v (bound-to rows-form 'view-of)) (s (bound-to rows-form 'state-outline))
                 (index (defined-as rows-form 'children)))
             (list (length calls)
                   (and (= 1 (length calls)) (= 1 (length v)) (eq? (cadr (car calls)) (car v)))
                   (and (= 1 (length calls)) (= 1 (length s)) (eq? (caddr (car calls)) 'children-of)
                        index (= 1 (symbol-count index (car s)))
                        (let ((lookup (defined-as rows-form 'children-of)))
                          (and lookup (= 1 (symbol-count lookup 'children))))
                        #t))))
      '(1 #t #t))
(want "OR-8b drawn once: as many rows as live blocks, no id twice, on both stores; the unnamed code block's row carries its derived label"
      (let ((cyc-live (filter (lambda (id) (not (cdr (assq 'deleted (cdr (state-read (open-and-reduce cyc-store) id))))))
                              (state-block-ids (open-and-reduce cyc-store)))))
        (and (list? prs) (list? crs)
             (list (= (length prs) (length live-ids))
                   (let loop ((xs (map cadr prs))) (or (null? xs) (and (not (member (car xs) (cdr xs))) (loop (cdr xs)))))
                   (= (length crs) (length cyc-live))
                   (let loop ((xs (map cadr crs))) (or (null? xs) (and (not (member (car xs) (cdr xs))) (loop (cdr xs)))))
                   (row-label (row-for prs KD2)))))
      '(#t #t #t #t "scheme:2"))

(system (string-append "rm -rf '" root "' '" sock-here "'"))
(printf "\n~a failures\nrows: ~a\noutline-rows complete\n" bad rows)
(exit (if (= bad 0) 0 1))
