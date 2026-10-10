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

;; Committing a draft on a text-mode code block (F119).
;;
;; NEVER: A COMMIT THAT ANSWERS OK HAS LANDED. The commit's plan declared the
;; draft as a string (working.sc entry-intent) while its record carried the
;; bytes resolve makes of a text-mode src; the reducer holds a plan's
;; records to the plan verbatim, marked the record plan-mismatch and left
;; it pending, and `read` kept the old text while the commit had answered
;; ok. The rows read the committed state three ways: `read`, the plain
;; export, and the reduction's own notes and pending records.
;;
;; KEY: F119-00 TO F119-05 WRITE THROUGH THE REAL COMMAND LINE, AS PROCESSES,
;; and read the notes in this process afterwards, from a fresh load of the
;; store. F119-06 and F119-07 call with-store-write in this process: they
;; need a planned request carrying an intent no command line can spell.

(import (chezscheme) (theourgia store) (theourgia reduce)
        (only (theourgia wire) storable-encode sexpr->string-extended)
        (only (theourgia log) writer-directory)
        (only (theourgia crc32) crc32-hex))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))
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

(define pid-text (number->string (get-process-id)))
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define here (string-append scratch-base "/committextmode-" pid-text))
(define store (string-append here "/store"))
;; NOTE: THE TWIN HAS A STORE OF ITS OWN, so what the code block's commit
;; left behind is not read as the section's.
(define store2 (string-append here "/store2"))
(define current-store store)
(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
(putenv "THEOURGIA_HOME" (string-append here "/home"))

(define (file-text path)
  (if (not (file-exists? path))
      ""
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))))
(define (put! path text)
  (call-with-output-file path (lambda (p) (put-string p text)) 'truncate))
(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))
(define (quoted a)
  (string-append "'"
                 (apply string-append
                        (map (lambda (c) (if (char=? c #\') "'\\''" (string c)))
                             (string->list a)))
                 "'"))
(define (cli . args)
  (let ((out (string-append here "/out.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
              "scheme --script ../core.sc "
              (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
              "--store " current-store " --wire > " out " 2>&1"))
    (file-text out)))
(define (data-of text)
  (guard (e (#t (list 'UNREADABLE text)))
    (let ((p (open-string-input-port text)))
      (let loop ((acc '()))
        (let ((x (read p)))
          (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))
(define (find-headed x head)
  (cond ((and (pair? x) (eq? (car x) head)) x)
        ((pair? x) (or (find-headed (car x) head) (find-headed (cdr x) head)))
        (else #f)))
(define (block-holding needle)
  (let ((m (find-headed (data-of (cli "grep" needle)) 'match)))
    (and m (pair? (cdr m)) (string? (cadr m)) (cadr m))))
(define (fresh-dir name)
  (let ((d (string-append here "/" name)))
    (system (string-append "rm -rf " (quoted d) "; mkdir -p " (quoted d)))
    d))

;; The block's src as `read` answers it, as text: a text-mode block's is bytes.
(define (read-src id)
  (let* ((d (data-of (cli "read" id)))
         (f (find-headed d 'fields))
         (src (and f (assq 'src (cdr f)))))
    (cond ((not src) (list 'NO-SRC d))
          ((bytevector? (cdr src)) (utf8->string (cdr src)))
          (else (cdr src)))))

;; What the reducer holds back, read from a fresh load of the store.
(define (held-back)
  (let ((r (open-and-reduce current-store)))
    (list (filter (lambda (n) (and (pair? n) (eq? (car n) 'plan-mismatch))) (reduce-noted r))
          (length (reduce-pending r)))))

(system (string-append "rm -rf " here "; mkdir -p " store " " store2))
(cli "init")
(define code-src (fresh-dir "code-src"))
(put! (string-append code-src "/n.sc") "(define (g) 'twocommitted)\n")
(define md-src (fresh-dir "md-src"))
(put! (string-append md-src "/notes.md") "# Beta\n\nbetacommitted here.\n")
(cli "import-code" code-src)
(define code-id (block-holding "twocommitted"))
(set! current-store store2)
(cli "init")
(cli "import-md" md-src)
(define md-id (block-holding "betacommitted"))
(set! current-store store)

(want "F119-00 PREMISE: a text-mode code block and a Markdown section to commit drafts on"
      (list (and (string? code-id) (contains? (cli "read" code-id) "(mode . text)"))
            (string? md-id))
      '(#t #t))

;; ---- the code block ----------------------------------------------------------------
(define code-write (cli "write" "--writer" "w2" code-id "(define (g) 'twopublished)\n"))
(define code-commit (cli "commit" "--writer" "w2" code-id))

(want "F119-01 PREMISE: the draft was written and the commit answered ok"
      (list (contains? code-write "(ok (saved") (contains? code-commit "(ok (items (ok"))
      '(#t #t))

(want "F119-02 after the commit, read gives the committed draft's text"
      (read-src code-id)
      "(define (g) 'twopublished)\n")

(want "F119-03 the reducer holds nothing back: no plan-mismatch noted, nothing pending"
      (held-back)
      '(() 0))

(define code-out (fresh-dir "code-out"))
(want "F119-04 the plain export writes the committed text"
      (begin (cli "export-code" code-out)
             (let ((t (file-text (string-append code-out "/n.sc"))))
               (list (contains? t "twopublished") (contains? t "twocommitted"))))
      '(#t #f))

;; ---- TWIN: a Markdown section, which the plan and the record agreed on before ----
(set! current-store store2)
(cli "write" "--writer" "w3" md-id "betapublished here.\n")
(define md-commit (cli "commit" "--writer" "w3" md-id))
(want "F119-05 TWIN: the same commit on a Markdown section lands, and nothing is held back"
      (list (contains? md-commit "(ok (items (ok")
            (read-src md-id)
            (held-back))
      '(#t "betapublished here.\n" (() 0)))


;; ---- a malformed intent is still refused (F119 r1 review) ------------------
;;
;; NEVER: MAKING INTENTS CANONICAL DOES NOT REPAIR THEM. An `expect` wrapper
;; with an improper tail, `(expect H (set ID title "x") . junk)`, sent as a
;; planned request, was refused as malformed on f5ebd58; r1's canonical
;; step rebuilt the wrapper without its tail and the title changed. The
;; request goes in through with-store-write, as a planned write, in this
;; process.
(define (title-of st id)
  (let* ((b (state-read (open-and-reduce st) id))
         (f (and b (assq 'fields b)))
         (t (and f (assq 'title (cdr f)))))
    (and t (cdr t))))
(define malformed-answer
  (let* ((state (open-and-reduce store2))
         (h (block-hash state md-id))
         (after (car (reduce-applied-cut state)))
         (bad-intent (cons 'expect (cons h (cons (list 'set md-id 'title "retitled") 'junk)))))
    (guard (e (#t (list 'RAISED (if (condition? e) (condition-message e) e))))
      (with-store-write store2 (lambda (state view) (list bad-intent)) "test"
                        (make-write-request "test" 'set (list md-id "title" "retitled") "MALFORMED1" after)
                        #f #t))))
(want "F119-06 a planned expect with an improper tail is refused as malformed, and the title does not change"
      (list (contains? (format "~s" malformed-answer) "malformed-intent")
            (equal? (title-of store2 md-id) "retitled"))
      '(#t #f))

;; NEVER: AN EMPTY INTENT INSIDE A WRAPPER IS REFUSED, NOT RAISED (F119 r2
;; review). `(expect #f (expect "h" ()))` in a planned request is refused
;; as malformed on f5ebd58; r2's canonical step reached the inner `()` and
;; took its car, so the write raised before any answer.
(define empty-answer
  (let* ((state (open-and-reduce store2))
         (after (car (reduce-applied-cut state))))
    (guard (e (#t (list 'RAISED (if (condition? e) (condition-message e) e))))
      (with-store-write store2 (lambda (state view) (list '(expect #f (expect "h" ())))) "test"
                        (make-write-request "test" 'set (list md-id "title" "x") "EMPTY1" after)
                        #f #t))))
(want "F119-07 a planned wrapper around an empty intent is refused as malformed, not raised"
      (list (contains? (format "~s" empty-answer) "malformed-intent")
            (and (pair? empty-answer) (eq? (car empty-answer) 'RAISED)))
      '(#t #f))

;; ---- a draft on a datum block is refused, by write and by commit -------------
;;
;; NEVER: AN EDIT REPORTED SAVED TAKES EFFECT. A datum block's code is its
;; body; a draft is text, and committed it landed as a src beside the body,
;; which export-code --datum, whereis and eval do not read. write refuses a
;; datum block by name, commit refuses a draft that would land on one, and
;; check names a datum block that already carries a src. A store of its own.
(define store3 (string-append here "/store3"))
(system (string-append "mkdir -p " store3))
(set! current-store store3)
(cli "init")
(define datum-src (fresh-dir "datum-src"))
(put! (string-append datum-src "/d.sc") "(define (dval) 'datumcommitted)\n")
(cli "import-code" "--datum" datum-src)
(define text3-src (fresh-dir "text3-src"))
(put! (string-append text3-src "/t.sc") "(define (tval) 'textcommitted)\n")
(cli "import-code" text3-src)
(define datum-id
  (let ((d (find-headed (data-of (cli "whereis" "dval")) 'def)))
    (and d (pair? (cdr d)) (cadr d))))
(define text3-id (block-holding "textcommitted"))
;; THE ANSWER IS THE LAST DATUM PRINTED: the command line's stderr is merged
;; into this text, and a line it writes there, such as the machine home it
;; announces, comes before the answer.
(define (refusal-of text n)
  (let ((d (data-of text)))
    (and (pair? d)
         (let ((answer (car (reverse d))))
           (and (list? answer) (>= (length answer) n) (list-head answer n))))))
(define (draft-on-datum id)
  (list 'error 'bad-request 'draft-on-datum-unsupported (list 'block id) '(use def)))

(want "DD-00 PREMISE: a datum block and a text-mode block, each in its mode"
      (list (and (string? datum-id) (contains? (cli "read" datum-id) "(mode . datum)"))
            (and (string? text3-id) (contains? (cli "read" text3-id) "(mode . text)")))
      '(#t #t))
(want "DD-01 write on a datum block is refused by name, and no draft is saved"
      (let ((a (cli "write" "--writer" "w4" datum-id "(define (dval) 'datumedited)\n")))
        (list (refusal-of a 5)
              (contains? (cli "drafts" "--writer" "w4") datum-id)))
      (list (draft-on-datum datum-id) #f))
;; A DRAFT AN EARLIER BUILD WROTE ON A DATUM BLOCK, at the block's baseline
;; now (forge-draft.ss): the commit's check is the one thing left to refuse
;; it, and the commit would land it as a src beside the body.
;; NEVER: NOT A TEXT-MODE BLOCK. A block's mode, once set, is refused a
;; change (mode-mismatch), so a text-mode block never becomes a datum block;
;; a block `insert` made carries a src and no mode, and takes one.
;;
;; THE DATUM BLOCK IS MADE AS THE PRODUCT MAKES ONE, and its src is an old
;; build's: no write leaves a datum block holding a src now (store.sc,
;; datum-src-refusal), so the src is taken away, the block made datum, and
;; the src put back by a record forged as an earlier build appended it
;; (forge-record.ss), to the same writer, after its own records.
(include "forge-draft.ss")
(include "forge-record.ss")
(cli "insert" "--title" "Legacy" "--text" "legacythreeplaceholder")
(define legacy3-id (block-holding "legacythreeplaceholder"))
(define dd-unsrc
  (with-store-write store3 (lambda (state view) (list (list 'set legacy3-id 'src))) "test"))
(define dd-made
  (with-store-write store3
    (lambda (state view)
      (list (list 'set legacy3-id 'kind 'code)
            (list 'set legacy3-id 'mode 'datum)
            (list 'set legacy3-id 'body '(define (legacy) 'committed))))
    "test"))
(define dd-writer
  (let ((ws (filter (lambda (w) (= 8 (string-length w))) (directory-list (string-append store3 "/writers")))))
    (and (pair? ws) (null? (cdr ws)) (car ws))))
(define dd-forged
  (forge-record-as! store3 dd-writer (string-append "(set \"" legacy3-id "\" src \"legacythreeplaceholder\")")))
(define dd-mode (and (number? dd-forged) (list? dd-unsrc) (list? dd-made) (append dd-unsrc dd-made)))
(define dd-version (forge-fresh-draft! store3 "w5" legacy3-id "(define (legacy) 'edited)\n"))
(define dd-drafts (cli "drafts" "--writer" "w5"))
(define dd-commit (cli "commit" "--writer" "w5" legacy3-id))
(want "DD-02 commit of a fresh draft on a datum block is refused by name, and writes nothing"
      (list (and (list? dd-mode) (for-all (lambda (a) (and (pair? a) (eq? (car a) 'ok))) dd-mode))
            (contains? (cli "read" legacy3-id) "(mode . datum)")
            (and (contains? dd-drafts dd-version) (contains? dd-drafts "(fresh #t)"))
            (refusal-of dd-commit 5)
            (read-src legacy3-id)
            (contains? (cli "drafts" "--writer" "w5") legacy3-id))
      (list #t #t #t (draft-on-datum legacy3-id) "legacythreeplaceholder" #t))
(want "DD-02 TWIN: a text-mode block takes a draft, and its commit lands"
      (let* ((w (cli "write" "--writer" "w7" text3-id "(define (tval) 'textedited)\n"))
             (c (cli "commit" "--writer" "w7" text3-id)))
        (list (contains? w "(ok (saved") (contains? c "(ok (items (ok") (read-src text3-id)))
      '(#t #t "(define (tval) 'textedited)\n"))
(want "DD-03 check names the datum block that already carries a src"
      (find-headed (data-of (cli "check")) 'datum-with-src)
      (list 'datum-with-src (list legacy3-id)))
;; The clause reports and does not judge: nothing that runs or exports the
;; block reads that src, so the store is not damaged by it.
(want "DD-03 and check's verdict beside the clause stays ok"
      (let ((d (data-of (cli "check"))))
        (list (and (find-headed d 'datum-with-src) #t) (find-headed d 'verdict)))
      '(#t (verdict ok)))
(set! current-store store)
(want "DD-03 TWIN: on the store whose text-mode block took its commit, check has no such clause"
      (find-headed (data-of (cli "check")) 'datum-with-src)
      #f)

(system (string-append "rm -rf " here))
(printf "rows: ~a\n~a failures\ncommit-text-mode complete\n" rows bad)
(exit (if (zero? bad) 0 1))
