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
;; limitations under the License.

;; A DAEMON THAT HOLDS THE STATE DOES NOT LOAD IT AGAIN FOR A --working
;; REQUEST. The working view is the writer's drafts overlaid on a reduction;
;; when the drafts' cut is the state's own applied cut (no drafts at all, or
;; drafts taken at the present), that reduction is the state the request
;; already holds, and only a draft at another cut opens the log. Counted, not
;; timed: the daemon runs with THEOURGIA_TRACE=1, and every open of the log
;; traces `(trace log-open <store> #f)`, so the rows count those lines in the
;; daemon's log over each request.
;;   WR-1 read --working, twice: no load (the control; it held already)
;;   WR-2 context --working, twice: no load, the same bytes
;;   WR-3 export-md --working, twice: no load, the same files
;;   WR-4 export-md, committed: no load, the files a local export writes
;;   WR-5 a draft at the present: context --working answers it, no load,
;;        the bytes a local run gives
;;   WR-6 a draft at an older cut: the store is loaded, and the draft is read
;;   WR-7 read --working with no writer named: writer-required, no load

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch register-verbs!)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia client) serve-log-path))

(register-verbs! extension-verbs)

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
(include "signal-send.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (with-expected label expect (x) (want-1 label (caught got) (caught x))))))

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/working-reload-" (number->string (get-process-id))))
(when (file-exists? root) (assertion-violation 'working-reload "scratch directory already exists" root))
(define (sh . xs) (system (apply string-append xs)))
(define (quoted a)
  (string-append "'" (apply string-append (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list a))) "'"))
(sh "mkdir -p " (quoted root))
;; THE SOCKETS UNDER THE RUNNER'S SOCKET ROOT, whose path is short enough for one
(define sock-base (let ((v (getenv "THEOURGIA_TEST_SOCK"))) (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define run-root (string-append sock-base "/wr-" (number->string (get-process-id))))
(sh "mkdir -p " (quoted run-root))
(putenv "THEOURGIA_RUN" (string-append run-root "/run"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define (file-text path)
  (if (file-exists? path)
      (call-with-port (open-file-input-port path) (lambda (p) (let ((b (get-bytevector-all p))) (if (eof-object? b) "" (utf8->string b)))))
      ""))
(define (count-in text needle)
  (let loop ((i 0) (n 0))
    (let ((j (let find ((j i)) (and (<= (+ j (string-length needle)) (string-length text))
                                    (if (string=? needle (substring text j (+ j (string-length needle)))) j (find (+ j 1)))))))
      (if j (loop (+ j (string-length needle)) (+ n 1)) n))))
(define (contains? text needle) (> (count-in text needle) 0))
;; -> the first datum of TEXT that is not the machine-home announcement
(define (answer-of text)
  (let loop ((p (open-input-string text)))
    (let ((d (guard (e (#t 'UNPARSABLE)) (read p))))
      (cond ((eof-object? d) 'NO-ANSWER)
            ((and (pair? d) (eq? (car d) 'theourgia)) (loop p))
            (else d)))))
(define (head-of a) (if (and (pair? a) (eq? (car a) 'error) (pair? (cdr a))) (list 'error (cadr a)) (and (pair? a) (car a))))
;; -> the files under DIR, each (<relative path> <text>), sorted
(define (tree-of dir)
  (let walk ((d dir) (prefix ""))
    (apply append
           (map (lambda (n)
                  (let ((full (string-append d "/" n)) (rel (if (string=? prefix "") n (string-append prefix "/" n))))
                    (if (file-directory? full) (walk full rel) (list (list rel (file-text full))))))
                (list-sort string<? (directory-list d))))))

(define store (string-append root "/store"))
(sh "mkdir -p " (quoted store))
(rpc-dispatch store '(init) "test")
(define (local . r) (rpc-dispatch store r "test"))
(define a-id
  (let* ((a (local 'insert "--under" "root" "--title" "Alpha" "--text" "alpha text"))
         (ev (cadr (assq 'events (cdr a)))))
    (string-append (car (car ev)) "." (number->string (cdr (car ev))))))
(local 'set a-id "kind" "doc")
(local 'set a-id "path" "a.md")

(include "serve-for.ss")
(define (stop-daemon!)
  (signal-processes! (serve-processes store) "TERM")
  (let loop ((k 0)) (when (and (< k 50) (pair? (serve-pids store))) (sh "sleep 0.1") (loop (+ k 1))))
  (signal-processes! (serve-processes store) "KILL"))
(let ((previous (base-exception-handler)))
  (base-exception-handler (lambda (e) (stop-daemon!) (previous e))))
(define started (serve-for! root store #f "THEOURGIA_TRACE=1"))
(want "WR-0 the daemon serves the store, traced"
      (string? started)
      #t)
(define log-path (serve-log-path store))

;; ONE REQUEST THROUGH THE DAEMON: -> (<answer> <log-open lines it traced>
;; <answer text>). The client runs with THEOURGIA_LOCAL and THEOURGIA_WRITER
;; unset, so it goes to the daemon and names no writer unless ENV, put
;; before the command, does.
(define n 0)
(define (through env . args)
  (set! n (+ n 1))
  (let* ((before (string-length (file-text log-path)))
         (out (string-append root "/c" (number->string n))))
    (sh "env -u THEOURGIA_LOCAL -u THEOURGIA_WRITER " env " scheme --script ../theourgia.sc "
        (apply string-append (map (lambda (a) (string-append (quoted a) " ")) args))
        "--store " (quoted store) " --wire > " (quoted out) " 2>&1 < /dev/null")
    (sh "sleep 0.3")
    (let* ((t (file-text log-path))
           (tail (substring t (min before (string-length t)) (string-length t)))
           (text (file-text out)))
      (list (answer-of text) (count-in tail "(trace log-open ") text))))
(define W "THEOURGIA_WRITER=wr")

(let ((r1 (through W "read" a-id "--working")) (r2 (through W "read" a-id "--working")))
  (want "WR-1 CONTROL read --working twice through the daemon, no drafts: no log-open either time, the same answer"
        (list (head-of (car r1)) (cadr r1) (cadr r2) (equal? (caddr r1) (caddr r2)))
        '(ok 0 0 #t)))
(let ((k1 (through W "context" "--for" a-id "--budget" "2000" "--working"))
      (k2 (through W "context" "--for" a-id "--budget" "2000" "--working")))
  (want "WR-2 context --working twice through the daemon, no drafts: no log-open either time, the same bytes"
        (list (head-of (car k1)) (cadr k1) (cadr k2) (equal? (caddr k1) (caddr k2)))
        '(ok 0 0 #t)))
(sh "mkdir -p " (quoted (string-append root "/e1")) " " (quoted (string-append root "/e2")))
(let* ((e1 (through W "export-md" (string-append root "/e1") "--working"))
       (e2 (through W "export-md" (string-append root "/e2") "--working")))
  (want "WR-3 export-md --working twice through the daemon, no drafts: no log-open either time, the same files"
        (list (head-of (car e1)) (cadr e1) (cadr e2) (equal? (tree-of (string-append root "/e1")) (tree-of (string-append root "/e2"))))
        '(ok 0 0 #t)))
(sh "mkdir -p " (quoted (string-append root "/e3")) " " (quoted (string-append root "/e4")))
(let* ((e3 (through "" "export-md" (string-append root "/e3")))
       (e4 (local 'export-md (string-append root "/e4"))))
  (want "WR-4 export-md, committed, through the daemon: no log-open, and the files a local export writes"
        (list (head-of (car e3)) (cadr e3) (head-of e4)
              (equal? (tree-of (string-append root "/e3")) (tree-of (string-append root "/e4")))
              (tree-of (string-append root "/e3")))
        (list 'ok 0 'ok #t (list (list "a.md" "alpha text")))))

;; A DRAFT AT THE PRESENT: its cut is the state's, so the view is the drafts
;; over the state in hand; the bytes are the ones a local run, which loads
;; the store for itself, gives.
;; NOTE: A COMMITTED READ GOES FIRST, AND IT IS NOT COUNTED. The draft's write
;; makes the writer's directory, which the store process reads as the disk
;; having moved past its publication, so the next published request
;; refreshes the publication first (a log-open, then `published`). That
;; refresh is the daemon's own and is not the view's load: the committed
;; context takes it, and the --working one is counted after it.
(define w (through W "write" a-id "draft text"))
(define refresh-1 (through "" "context" "--for" a-id "--budget" "2000"))
(let* ((k (through W "context" "--for" a-id "--budget" "2000" "--working"))
       (out (string-append root "/local-context"))
       (local-text (begin (sh W " THEOURGIA_LOCAL=1 scheme --script ../theourgia.sc context --for " (quoted a-id)
                              " --budget 2000 --working --store " (quoted store) " --wire > " (quoted out) " 2>&1 < /dev/null")
                          (file-text out))))
  (want "WR-5 a draft at the present: context --working through the daemon traces no log-open, reads the draft, and answers the bytes a local run gives"
        (list (head-of (car w)) (head-of (car k)) (cadr k) (contains? (caddr k) "draft text") (equal? (answer-of (caddr k)) (answer-of local-text)))
        '(ok ok 0 #t #t)))
;; A DRAFT AT AN OLDER CUT: another block lands after the draft was taken, so
;; the drafts' cut is behind the state's and the view loads the store at it.
(define moved (through "" "insert" "--under" "root" "--title" "Beta" "--text" "beta text"))
(define refresh-2 (through "" "context" "--for" a-id "--budget" "2000"))
(let ((k (through W "context" "--for" a-id "--budget" "2000" "--working")))
  ;; A LOAD AT A CUT OPENS THE LOG TWICE (store.sc, obtain-state*): the whole
  ;; log to judge the cut usable, then the log at the cut. The row asks that
  ;; the view loads, not how many times a load at a cut opens the log.
  (want "WR-6 a draft at an older cut: context --working loads the store for the draft's cut, and reads the draft"
        (list (head-of (car moved)) (head-of (car k)) (> (cadr k) 0) (contains? (caddr k) "draft text"))
        '(ok ok #t #t)))
(let ((r (through "" "read" a-id "--working")))
  (want "WR-7 read --working with no writer named: writer-required, refused before any load"
        (list (car r) (cadr r))
        '((error writer-required) 0)))

(stop-daemon!)
(sh "rm -rf " (quoted root) " " (quoted run-root))
(printf "rows: ~a\n~a failures\nworking-reload complete\n" rows bad)
(exit (if (zero? bad) 0 1))
