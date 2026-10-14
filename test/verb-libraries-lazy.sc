#!chezscheme
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


;; THE VERB LIBRARIES ARE ENTERED ON FIRST USE. rpc.sc does not import
;; code-project, code-suggest, datum-project or project; each is entered by
;; eval on the first of its verbs a process runs (rpc.sc, the verb libraries'
;; entries). These rows run each request in a FRESH child that dispatches it
;; itself, as the local route does (THEOURGIA_LOCAL=1), one child per case so
;; an earlier verb's loading cannot hide a later one's, and read which of the
;; four are in (library-list) after the expected answer: the answer's head
;; (ok, or the named refusal) and the libraries, in the order listed.
;;
;; None of the four after an unknown verb, a plain read, read --md of one
;; block, a set of another field than body, a set of body (which asks the
;; block's mode through view.sc's block-code-field) and the requests refused
;; before any work. Exactly the verb's own after each of import-code (and
;; --symbols, --datum), export-code (each arm, --datum each arm), split-suggest,
;; import-md, export-md, read --recursive and def. read --signature enters
;; (theourgia derived), which imports code-project.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch rpc-ok?)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) state-block-ids state-read)
        (only (theourgia digest) sha256 bytevector->hex))

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (set! rows (+ rows 1))
  (if (equal? got expected)
      (printf "ok   ~a -> ~s\n" label got)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a -> ~s   WANT ~s\n" label got expected))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e)) (condition-message e) e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/verb-libraries-lazy-" (number->string (get-process-id))))
(when (file-exists? root) (assertion-violation 'verb-libraries-lazy "scratch directory already exists" root))
(define (sh . xs) (system (apply string-append xs)))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(sh "mkdir -p " (quoted root))
(define (write-file! path text)
  (call-with-port (open-file-output-port path (file-options no-fail)) (lambda (p) (put-bytevector p (string->utf8 text)))))
(define (file-text path)
  (guard (e (#t ""))
    (call-with-port (open-file-input-port path) (lambda (p) (let ((b (get-bytevector-all p))) (if (eof-object? b) "" (utf8->string b)))))))

;; A store at <root>/<name>; all of them share one machine home, set once,
;; which every child inherits.
(sh "mkdir -p " (quoted (string-append root "/home")))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define (fresh-store! name)
  (let ((st (string-append root "/" name "/store")))
    (sh "mkdir -p " (quoted st))
    (rpc-dispatch st '(init) "test")
    st))
(define (ask st . req) (rpc-dispatch st req "test"))
(define (must st . req)
  (let ((a (apply ask st req)))
    (unless (rpc-ok? a) (assertion-violation 'verb-libraries-lazy "setup refused" req a))
    a))

;; The head of an answer: ok when rpc's own rule says so (an import answers
;; (import ...)), else the refusal's kind (and a bad request's reason).
(define child-text
  (string-append
    "(import (chezscheme) (theourgia rpc))\n"
    "(define (head a)\n"
    "  (cond ((not (pair? a)) a)\n"
    "        ((rpc-ok? a) 'ok)\n"
    "        ((and (pair? (cdr a)) (eq? (cadr a) 'bad-request) (pair? (cddr a))) (list (car a) (cadr a) (caddr a)))\n"
    "        ((pair? (cdr a)) (list (car a) (cadr a)))\n"
    "        (else a)))\n"
    "(define libraries '(code-project code-suggest datum-project project))\n"
    "(define store (cadr (command-line)))\n"
    "(define request (read (open-input-string (caddr (command-line)))))\n"
    "(define answer (guard (e (#t (list 'raised))) (rpc-dispatch store request \"test\")))\n"
    "(write (list (head answer) (filter (lambda (n) (member (list 'theourgia n) (library-list))) libraries)))\n"))
(define child (string-append root "/child.sc"))
(write-file! child child-text)
(define runs 0)
;; -> (<answer head> <the four libraries loaded>), from a fresh process.
(define (in-child st . request)
  (set! runs (+ runs 1))
  (let ((out (string-append root "/child-" (number->string runs) ".out")))
    (sh "THEOURGIA_LOCAL=1 scheme --script " (quoted child) " " (quoted st) " "
        (quoted (format "~s" request)) " > " (quoted out) " 2>/dev/null < /dev/null")
    (guard (e (#t (list 'UNREADABLE (file-text out)))) (read (open-string-input-port (file-text out))))))

;; ---- the stores the rows read and write -------------------------------------------
;; one per kind, each tagged t0 after its import: A a text file, C a datum
;; library, M a Markdown note.
(define a (fresh-store! "a"))
(define c (fresh-store! "c"))
(define m (fresh-store! "m"))
(define src (string-append root "/src"))
(define lib (string-append root "/lib"))
(define md (string-append root "/md"))
(sh "mkdir -p " (quoted src) " " (quoted lib) " " (quoted md) " " (quoted (string-append root "/review")))
(write-file! (string-append src "/a.sc") "(define a 1)\n(define b 2)\n")
(write-file! (string-append lib "/pp.sc") "(library (pp) (export pf) (import (rnrs))\n(define (pf) 1))\n")
(write-file! (string-append md "/note.md") "# A note\n\nSome text.\n")
(must a 'import-code src)
(must a 'tag "t0")
(must c 'import-code lib "--datum")
(must c 'tag "t0")
(must m 'import-md md)
(must m 'tag "t0")
(define (field-of st id k)
  (let* ((b (state-read st id)) (p (and b (assq k (cdr (assq 'fields b)))))) (and p (cdr p))))
(define (block-where store pred)
  (let ((st (open-and-reduce store)))
    (or (find (lambda (id) (pred st id)) (state-block-ids st))
        (assertion-violation 'verb-libraries-lazy "no such block"))))
(define note (block-where m (lambda (st id) (equal? (field-of st id 'title) "A note"))))
(define library (block-where c (lambda (st id) (eq? (field-of st id 'kind) 'library))))
;; the library's one definition: a code block in datum mode (its name is
;; derived by the view, not stored)
(define pf (block-where c (lambda (st id) (and (eq? (field-of st id 'kind) 'code) (eq? (field-of st id 'mode) 'datum)))))
(define (out name) (string-append root "/out-" name))

(printf "== none of the four ==\n")
(want "VL-1 an unknown verb loads none of the four" (in-child a 'no-such-verb) '((error unknown-verb) ()))
(want "VL-2 a plain read loads none" (in-child m 'read note) '(ok ()))
(want "VL-3 read --md of one block loads none" (in-child m 'read note "--md") '(ok ()))
(want "VL-4 a set of another field than body loads none" (in-child m 'set note "title" "The note") '(ok ()))
(want "VL-5 a set of body, asking the block's mode, loads none" (in-child c 'set pf "body" "(define (pf) 2)") '(ok ()))
(want "VL-6 import-code with --datum and --symbols is refused before any work, and loads none"
      (in-child a 'import-code src "--datum" "--symbols" (string-append root "/none.sexp"))
      '((error bad-request incompatible-import-options) ()))
(want "VL-7 export-code with --datum and --raw is refused, and loads none"
      (in-child a 'export-code (out "x1") "--datum" "--raw")
      '((error bad-request incompatible-projection-options) ()))
(want "VL-8 export-code with --cut and --working is refused, and loads none"
      (in-child a 'export-code (out "x2") "--cut" "t0" "--working")
      '((error bad-request incompatible-cut-options) ()))
(want "VL-9 export-md with --cut and --working is refused, and loads none"
      (in-child m 'export-md (out "x3") "--cut" "t0" "--working")
      '((error bad-request incompatible-cut-options) ()))

(printf "== exactly the verb's own ==\n")
;; Into stores of their own: a second unmarked import of a file is a second
;; file at its path, and a Markdown import after the note's title was set
;; would ask to delete the section the source no longer names.
(want "VL-10 a text import-code loads code-project" (in-child (fresh-store! "a2") 'import-code src) '(ok (code-project)))
(want "VL-11 a datum import-code loads code-project and datum-project" (in-child c 'import-code lib "--datum")
      '(ok (code-project datum-project)))
(want "VL-12 export-code loads code-project" (in-child a 'export-code (out "c1")) '(ok (code-project)))
(want "VL-13 export-code --working loads code-project" (in-child a 'export-code (out "c2") "--working" "--writer" "w1")
      '(ok (code-project)))
(want "VL-14 export-code --cut loads code-project" (in-child a 'export-code (out "c3") "--cut" "t0") '(ok (code-project)))
(want "VL-15 export-code --datum loads code-project and datum-project" (in-child c 'export-code (out "d1") "--datum")
      '(ok (code-project datum-project)))
(want "VL-16 export-code --datum --working loads code-project and datum-project"
      (in-child c 'export-code (out "d2") "--datum" "--working" "--writer" "w1")
      '(ok (code-project datum-project)))
(want "VL-17 export-code --datum --cut loads code-project and datum-project"
      (in-child c 'export-code (out "d3") "--datum" "--cut" "t0")
      '(ok (code-project datum-project)))
(want "VL-18 split-suggest loads code-project and code-suggest"
      (in-child a 'split-suggest (string-append src "/a.sc") "--output" (string-append root "/review/a.sc"))
      '(ok (code-project code-suggest)))
(want "VL-19 import-md loads code-project and project" (in-child (fresh-store! "m2") 'import-md md) '(ok (code-project project)))
;; export-md writes into a directory that exists
(sh "mkdir -p " (quoted (out "m1")))
(want "VL-20 export-md loads code-project and project" (in-child m 'export-md (out "m1")) '(ok (code-project project)))
(want "VL-21 read --recursive loads code-project and project" (in-child m 'read note "--recursive")
      '(ok (code-project project)))
(want "VL-22 read --md --recursive loads code-project and project" (in-child m 'read note "--md" "--recursive")
      '(ok (code-project project)))
(want "VL-23 def loads code-project and datum-project"
      (in-child c 'def "pg" "--under" library "(define (pg) 2)")
      '(ok (code-project datum-project)))
;; read --signature enters (theourgia derived), which imports code-project;
;; the row reads the libraries, whatever the store's tables answer.
(want "VL-24 read --signature loads code-project, through derived, and none of the other three"
      (let ((r (in-child m 'read note "--signature"))) (if (and (pair? r) (pair? (cdr r))) (cadr r) r))
      '(code-project))

;; import-code --symbols: a first import of a file the symbols file names,
;; in a store of its own.
(define b (fresh-store! "b"))
(define js (string-append root "/js"))
(sh "mkdir -p " (quoted js))
(define two "function alpha() {\n  return 1;\n}\nfunction beta() {\n  return 2;\n}\n")
(write-file! (string-append js "/a.js") two)
(define symbols (string-append root "/symbols.sexp"))
(write-file! symbols
  (string-append
    (format "~s\n" (list 'symbols (list 'path "a.js") (list 'digest (bytevector->hex (sha256 (string->utf8 two))))
                         '(source (vscode "1.138.0" "javascript")) '(top-level #t)))
    (format "~s\n" (list 'symbol 0 32 'function "alpha"))
    (format "~s\n" (list 'symbol 33 64 'function "beta"))))
(want "VL-25 import-code --symbols loads code-project and code-suggest"
      (in-child b 'import-code js "--symbols" symbols)
      '(ok (code-project code-suggest)))

(sh "rm -rf " (quoted root))
(printf "rows: ~a\n~a failures\nverb-libraries-lazy complete\n" rows bad)
(exit (if (zero? bad) 0 1))
