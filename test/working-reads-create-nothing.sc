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

;; A read of a writer's working view creates nothing for a writer that has
;; no drafts.
;;
;; Every --working reader but `read --working` went through the writer's
;; draft lock, and the lock first creates the writer's area: a request
;; with a misspelt --writer left writers/<w>/draft.lock and working/ in
;; the store. One row per reader asks it for a writer that has no area and
;; compares the store's writers/ tree, names and contents, before and
;; after. The rows after them pin that the lock is still taken when the
;; writer has an area (its drafts are read under it), so "creates nothing"
;; is not bought by never locking.

(import (chezscheme) (theourgia rpc) (only (theourgia extensions) extension-verbs)
        (only (theourgia reduce) block-id)
        (only (theourgia working) working-state working-snapshot working-baseline))

(register-verbs! extension-verbs)

(define bad 0)
(define rows 0)
(define (want-1 label got expected)
  (let ((ok (equal? got expected)))
    (unless ok (set! bad (+ bad 1)))
    (printf "~a ~a -> ~s~a\n" (if ok "ok  " "FAIL") label got
            (if ok "" (format "   WANT ~s" expected)))))
(define-syntax caught
  (syntax-rules ()
    ((_ e0)
     (guard (e (#t (list 'RAISED (if (and (condition? e) (message-condition? e))
                                     (condition-message e)
                                     e))))
       e0))))
(include "expected.ss")
(define-syntax want
  (syntax-rules ()
    ((_ label got expected)
     (begin (set! rows (+ rows 1))
            (with-expected label expected (x) (want-1 label (caught got) (caught x)))))))

(define root
  (string-append (let ((v (getenv "THEOURGIA_TEST_ROOT")))
                   (if (and (string? v) (> (string-length v) 0)) v "/tmp"))
                 "/working-reads-" (number->string (get-process-id))))
(when (file-exists? root)
  (assertion-violation 'working-reads-create-nothing "scratch directory already exists" root))
(system (string-append "mkdir -p " root "/home " root "/md " root "/out-code " root "/out-md"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))
(define (run . args) (rpc-dispatch store args "test"))
(define (runw w . args) (rpc-dispatch store args "test" #f w))

(define (file-text path)
  (if (file-exists? path)
      (let ((t (call-with-input-file path get-string-all))) (if (string? t) t ""))
      ""))
(define (contains? text needle)
  (let ((n (string-length needle)) (m (string-length text)))
    (let loop ((i 0))
      (cond ((> (+ i n) m) #f)
            ((string=? (substring text i (+ i n)) needle) #t)
            (else (loop (+ i 1)))))))

;; THE STORE'S writers/ TREE, every path and every file's checksum: equal
;; before and after a read means the read created, removed and changed
;; nothing there.
(define (writers-tree)
  (let ((out (string-append root "/tree.txt")))
    (system (string-append
              "cd " store " && find writers | LC_ALL=C sort | while read f; do "
              "if [ -f \"$f\" ]; then echo \"$f $(cksum < \"$f\")\"; else echo \"$f/\"; fi; done > " out))
    (file-text out)))
(define (head a) (and (pair? a) (car a)))
;; -> (<answer head> <writers/ unchanged?>)
(define (read-leaves-nothing thunk)
  (let* ((before (writers-tree))
         (a (thunk))
         (after (writers-tree)))
    (list (head a) (string=? before after))))
(define (exists? path) (file-exists? (string-append store "/" path)))

(run 'init)
(system (string-append "printf '# T\\n\\nbody\\n' > " root "/md/a.md"))
(run 'import-md (string-append root "/md"))
(define (new-id answer) (let* ((ev (assq 'events (cdr answer))) (e (car (cadr ev)))) (block-id (car e) (cdr e))))
(define U (new-id (run 'insert "--title" "u")))

;; ---- one row per reader, for a writer with no area ------------------------

(want "W1 context --working for a writer with no drafts answers and leaves writers/ unchanged"
      (read-leaves-nothing (lambda () (runw "wnone1" 'context "--for" U "--budget" "4000" "--working")))
      '(ok #t))
(want "W2 read --signature --working for a writer with no drafts answers and leaves writers/ unchanged"
      (read-leaves-nothing (lambda () (runw "wnone2" 'read U "--signature" "--working")))
      '(ok #t))
(want "W3 export-code --working for a writer with no drafts answers and leaves writers/ unchanged"
      (read-leaves-nothing (lambda () (runw "wnone3" 'export-code (string-append root "/out-code") "--working")))
      '(ok #t))
(want "W4 export-md --working for a writer with no drafts answers and leaves writers/ unchanged"
      (read-leaves-nothing (lambda () (runw "wnone4" 'export-md (string-append root "/out-md") "--working")))
      '(ok #t))
(want "W5 diagnostics for a writer with no drafts answers and leaves writers/ unchanged"
      (read-leaves-nothing (lambda () (runw "wnone5" 'diagnostics)))
      '(ok #t))

;; eval runs as a child of its caller, so it is asked through the command
;; line, the way a client asks it; its view comes from working-baseline and
;; working-snapshot.
(define libs (getenv "CHEZSCHEMELIBDIRS"))
(define exts (getenv "CHEZSCHEMELIBEXTS"))
(define (cli-eval writer)
  (let ((out (string-append root "/eval.txt")))
    (system (string-append
              "CHEZSCHEMELIBDIRS=" libs " CHEZSCHEMELIBEXTS='" exts "' THEOURGIA_LOCAL=1 "
              "scheme --script ../core.sc eval '(+ 1 2)' --working --writer " writer
              " --store " store " --wire > " out " 2>&1"))
    (file-text out)))
(let* ((before (writers-tree))
       (text (cli-eval "wnone6"))
       (after (writers-tree)))
  (want "W6 eval --working for a writer with no drafts answers 3 and leaves writers/ unchanged"
        (list (contains? text "(ok (values (3))") (string=? before after))
        '(#t #t)))

(want "W7 working-snapshot for a writer with no area is the empty list, and nothing is created"
      (let* ((before (writers-tree)) (a (working-snapshot store #f "wnone7")) (after (writers-tree)))
        (list a (string=? before after)))
      '((ok "wnone7" ()) #t))
(want "W8 working-baseline for a writer with no area is the empty cut, and nothing is created"
      (let* ((before (writers-tree)) (a (working-baseline store #f "wnone8")) (after (writers-tree)))
        (list a (string=? before after)))
      '((ok "wnone8" ()) #t))

;; CONTROLS, green on the base as well: these two readers took no lock.
(want "C1 CONTROL read --working for a writer with no drafts leaves writers/ unchanged"
      (read-leaves-nothing (lambda () (runw "wnone9" 'read U "--working")))
      '(ok #t))
(want "C2 CONTROL drafts for a writer with no drafts leaves writers/ unchanged"
      (read-leaves-nothing (lambda () (runw "wnone10" 'drafts)))
      '(ok #t))

;; ---- a writer with an area still reads under the lock ---------------------

(run 'write U "the draft's text" "--writer" "wd")
(system (string-append "rm -f " store "/writers/wd/draft.lock"))
(want "L1 a writer with a draft gets it in its working view, and the view takes the lock (draft.lock is made again)"
      (let ((w (working-state store #f "wd")))
        (list (car w) (length (cadddr w)) (exists? "writers/wd/draft.lock")))
      '(ok 1 #t))
(system (string-append "mkdir -p " store "/writers/wempty/working"))
(want "L2 a writer whose working/ is there but empty reads no drafts under the lock (draft.lock is made)"
      (let ((w (working-state store #f "wempty")))
        (list (car w) (cadddr w) (exists? "writers/wempty/draft.lock")))
      '(ok () #t))
(system (string-append "mkdir -p " store "/writers/wstruct/structural"))
(want "L3 a writer with only a structural/ directory is read under the lock as well (draft.lock and working/ are made)"
      (let ((w (working-state store #f "wstruct")))
        (list (car w) (exists? "writers/wstruct/draft.lock") (exists? "writers/wstruct/working")))
      '(ok #t #t))

(printf "rows: ~a\n" rows)
(printf "~a failures\n" bad)
(system (string-append "rm -rf " root))
;; THE SENTINEL THE RUNNER READS: without it a fixture is red whatever its
;; rows say (run-fixtures.sh counts "<name> complete").
(printf "working-reads-create-nothing complete\n")
(exit (if (= bad 0) 0 1))
