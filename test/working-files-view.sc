#!r6rs
;; Copyright 2018 - 2026 The Theourgia Authors
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;     http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; A writer's working view shows its new files and hides its deleted ones
;; (working.sc, working-view-with-files): outline, read and context with
;; --working read them as the blocks their commit would make.
;;
;; WN-1 outline --working lists a new file's holder where its commit will put
;; it, after the last committed top-level holder, its pieces in K order, and
;; two new files in the drafts listing's order (provisional id, string<?);
;; outline without --working does not list it.
;; WN-2 read --working of the holder answers what a committed file holder
;; answers (""), of a piece its bytes; the pieces run together are the file;
;; --working-info carries based-on #f and the piece's version; without
;; --working the provisional id is not-committed.
;; WN-3 context --working on a piece answers it as the target, and a
;; committed block that uses a name the new piece defines lists the piece
;; among that name's definitions.
;; WN-4 another writer's view does not hold the new file.
;; WN-5 a deleted file's holder and pieces are absent from the view; asked
;; with --working they are unknown-id, deleted-in-working; without it they
;; read as before.
;; WN-7 the foreign eval --working (a shell runner over the projected tree)
;; reads the same view: a new file is projected once with its draft's bytes,
;; a deleted one is not; without --working the committed files.
;; WN-6 after the commit the file stands under its committed ids at the same
;; place, and the provisional id is not-committed again.
;;
;; THE STORE. Files a.sc, c.sc and l.sc are committed by the importer's own
;; intents (landing-files.sc file-new-intents), as a commit of a new file
;; makes them. l.sc is a library importing (m), which no committed block
;; defines: a name is resolved within a library unit, so the new file m.sc
;; (writer w4) is a library too.
(import (chezscheme) (theourgia rpc)
        (only (theourgia extensions) extension-verbs)
        (only (theourgia store) open-and-reduce)
        (only (theourgia reduce) reduce-applied-cut block-id provisional-id structural-version)
        (only (theourgia digest) sha256 bytevector->hex)
        (only (theourgia working) working-new! working-delete!)
        (only (theourgia landing-files) file-new-intents))

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
(define-syntax want
  (syntax-rules ()
    ((_ name got expected) (with-expected name expected (x) (want-1 name (caught got) x)))))

(register-verbs! extension-verbs)

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp") "/working-files-view-" (number->string (get-process-id))))
(system (string-append "rm -rf '" root "'; mkdir -p '" root "/home'"))
(putenv "THEOURGIA_HOME" (string-append root "/home"))

;; ---- the store ------------------------------------------------------------------------------

(define S (string-append root "/s"))
(rpc-dispatch S '(init) "test")
(define (run . args) (rpc-dispatch S args "test"))
(define (run-as w . args) (rpc-dispatch S args "test" #f w))
(define (clause a name) (and (pair? a) (list? a) (find (lambda (c) (and (pair? c) (eq? (car c) name))) (cdr a))))
(define (made-ids answer)
  (map (lambda (a) (let ((e (car (cadr (assq 'events (cdr a)))))) (block-id (car e) (cdr e))))
       (cadr answer)))
;; A FILE COMMITTED AS A COMMIT OF A NEW FILE MAKES IT: -> (<holder> <piece> ...)
(define (file! path text)
  (made-ids (run 'batch (format "~s" (file-new-intents path (string->utf8 text) 0)))))
(define a-ids (file! "a.sc" "(define (a) 1)\n"))
(define c-ids (file! "c.sc" "(define (c) 3)\n"))
(define l-ids (file! "l.sc" "(library (l)\n  (export l)\n  (import (rnrs) (m))\n\n  (define (l) (m))\n\n)\n"))
(define (cut-text) (format "~s" (reduce-applied-cut (open-and-reduce S))))
(define drafted-cut (reduce-applied-cut (open-and-reduce S)))

(define b-text "(define (b) 2)\n\n(define (b2) 3)\n")
(define d-text "(define (d) 4)\n")
(define (new! w path text)
  (working-new! S #f w path '((lang . scheme)) (string->utf8 text) (cut-text)))
(define b-draft (new! "w1" "b.sc" b-text))
(define b (provisional-id "b.sc"))
(define d (provisional-id "d.sc"))
(define (piece pid k) (string-append pid "/" (number->string k)))
(define not-committed (lambda (id) (list 'error 'provisional-id (list 'id id) '(reason not-committed))))

;; the outline text a listing of ((<id> <title> (<child id> <title>) ...) ...) prints
(define (outline-of files)
  (apply string-append
         (map (lambda (f)
                (apply string-append (format "- ~a  ~a\n" (car f) (cadr f))
                       (map (lambda (c) (format "  - ~a  ~a\n" (car c) (cadr c))) (cddr f))))
              files)))
(define a-file (list (car a-ids) "a.sc" (list (cadr a-ids) "a")))
(define c-file (list (car c-ids) "c.sc" (list (cadr c-ids) "c")))
(define l-file (list (car l-ids) "l.sc" (list (cadr l-ids) "scheme:1") (list (caddr l-ids) "l")))
(define b-file (list b "b.sc" (list (piece b 0) "b") (list (piece b 1) "b2")))
(define d-file (list d "d.sc" (list (piece d 0) "d")))
(define (outline-text-of a) (let ((t (clause a 'text))) (and t (cadr t))))

;; ---- WN-1: where a new file stands ------------------------------------------------------------

(want "WN-0 the new-file draft was written under its provisional id"
      (list (car b-draft) (cadr (assq 'saved (cdr b-draft))))
      (list 'ok b))
(want "WN-1 outline --working lists b.sc after the last committed holder (a.sc, c.sc, l.sc, then b.sc), its pieces in K order, marked (working w1)"
      (let ((o (run-as "w1" 'outline "--working")))
        (list (outline-text-of o) (clause o 'working)))
      (list (outline-of (list a-file c-file l-file b-file)) '(working "w1")))
(want "WN-1 outline without --working does not list it"
      (outline-text-of (run-as "w1" 'outline))
      (outline-of (list a-file c-file l-file)))
(define d-draft (new! "w1" "d.sc" d-text))
;; THE TWO NEW FILES' ORDER is the drafts listing's, provisional id by
;; string<?: d.sc's id sorts first here, so d.sc stands before b.sc although
;; its path sorts after.
(want "WN-1 the two provisional ids sort d.sc first, so the drafts order is not the path order"
      (string<? d b)
      #t)
(want "WN-1 two new files: d.sc then b.sc, in the order the drafts list them (and the landing route commits them)"
      (outline-text-of (run-as "w1" 'outline "--working"))
      (outline-of (list a-file c-file l-file d-file b-file)))
(want "WN-1 the drafts listing names them in that order"
      (map (lambda (i) (cadr (assq 'provisional (cdr i))))
           (filter (lambda (i) (eq? (car i) 'new)) (cdr (clause (run-as "w1" 'drafts) 'items))))
      (list d b))

;; ---- WN-2: reading a new file's blocks --------------------------------------------------------

(want "WN-2 read --working <provisional>: the holder reads as a committed file holder reads, \"\""
      (list (run-as "w1" 'read b "--working") (run-as "w1" 'read (car c-ids) "--working"))
      '((ok (text "")) (ok (text ""))))
(want "WN-2 read --working <provisional>/0 and /1: the pieces, and run together they are the file"
      (let ((p0 (run-as "w1" 'read (piece b 0) "--working")) (p1 (run-as "w1" 'read (piece b 1) "--working")))
        (list p0 (string-append (cadr (clause p0 'text)) (cadr (clause p1 'text)))))
      (list '(ok (text "(define (b) 2)\n\n")) b-text))
(want "WN-2 the piece carries the name the split read (context's target names it b)"
      (let* ((a (run-as "w1" 'context "--for" (piece b 0) "--budget" "6000" "--working"))
             (entry (and (clause a 'for) (cadr (clause a 'for))))
             (block (and entry (assq 'block (filter pair? (cdr entry)))))
             (fields (and block (assq 'fields (cadr block))))
             (name (and fields (assq 'name (cdr fields)))))
        (and name (cdr name)))
      "b")
(want "WN-2 read --working-info <provisional>/0: the projection, based-on #f, the piece's version, the draft's cut"
      (let ((p (cadr (run-as "w1" 'read (piece b 0) "--working-info"))))
        (list (car p) (cadr p) (list-ref p 2) (list-ref p 3) (list-ref p 4) (list-ref p 5) (list-ref p 6) (list-ref p 8)))
      (list 'projection 'working "w1" (piece b 0)
            (structural-version 'new (piece b 0) (bytevector->hex (sha256 (string->utf8 "(define (b) 2)\n\n"))) drafted-cut)
            #f drafted-cut ""))
(want "WN-2 read <provisional> without --working is not-committed"
      (run-as "w1" 'read b)
      (not-committed b))

;; ---- WN-3: context over a new piece --------------------------------------------------------

;; the ids of a context answer's entries that say they define NAME: an
;; entry is a list headed by its id, holding (why defines <name> ...)
(define (definers a name)
  (let walk ((x a) (out '()))
    (cond
      ((not (pair? x)) out)
      ((and (string? (car x)) (list? x)
            (exists (lambda (c) (and (pair? c) (eq? (car c) 'why) (list? c) (> (length c) 2)
                                     (eq? (cadr c) 'defines) (equal? (caddr c) name)))
                    (cdr x)))
       (cons (car x) out))
      (else (walk (cdr x) (walk (car x) out))))))
(want "WN-3 context --for <provisional>/1 --working answers ok with the piece as target"
      (let ((a (run-as "w1" 'context "--for" (piece b 1) "--budget" "6000" "--working")))
        (list (car a) (and (clause a 'for) (car (cadr (clause a 'for)))) (clause a 'working)))
      (list 'ok (piece b 1) '(working "w1")))
(define m (provisional-id "m.sc"))
(define m-draft (new! "w4" "m.sc" "(library (m)\n  (export m)\n  (import (rnrs))\n\n  (define (m) 2)\n\n)\n"))
(want "WN-3 l.sc's piece uses m: in w4's view, whose new library m.sc defines it, its context --working lists m.sc's piece 1 among m's definitions"
      (definers (run-as "w4" 'context "--for" (caddr l-ids) "--budget" "6000" "--working") "m")
      (list (piece m 1)))
(want "WN-3 without --working nothing defines m"
      (definers (run-as "w4" 'context "--for" (caddr l-ids) "--budget" "6000") "m")
      '())

;; ---- WN-4: another writer's view --------------------------------------------------------------

(want "WN-4 another writer's --working view: the provisional id is not-committed, from read and context, and its outline lacks the files"
      (list (run-as "w2" 'read b "--working")
            (run-as "w2" 'context "--for" b "--budget" "6000" "--working")
            (outline-text-of (run-as "w2" 'outline "--working")))
      (list (not-committed b) (not-committed b) (outline-of (list a-file c-file l-file))))

;; ---- WN-5: a deleted file -----------------------------------------------------------------------

(define a-piece-read (run-as "w3" 'read (cadr a-ids)))
(define a-delete (working-delete! S #f "w3" (car a-ids) (cut-text)))
(define (deleted id) (list 'error 'unknown-id id '(reason deleted-in-working) (list 'holder (car a-ids))))
(want "WN-5 the delete draft was written"
      (car a-delete)
      'ok)
(want "WN-5 outline --working omits a.sc and its piece"
      (outline-text-of (run-as "w3" 'outline "--working"))
      (outline-of (list c-file l-file)))
(want "WN-5 read --working of its piece and of its holder: unknown-id, deleted-in-working, naming the holder"
      (list (run-as "w3" 'read (cadr a-ids) "--working") (run-as "w3" 'read (car a-ids) "--working"))
      (list (deleted (cadr a-ids)) (deleted (car a-ids))))
(want "WN-5 context --working of its piece answers the same"
      (run-as "w3" 'context "--for" (cadr a-ids) "--budget" "6000" "--working")
      (deleted (cadr a-ids)))
(want "WN-5 without --working the piece reads as before the draft"
      (list (car a-piece-read) (equal? (run-as "w3" 'read (cadr a-ids)) a-piece-read))
      '(ok #t))

;; ---- WN-7: the foreign eval ------------------------------------------------------------------------

;; One eval through core.sc, as a caller runs it: -> its answer, the last datum
;; on standard output that is (ok ...) or (error ...).
(define (shell-eval w source)
  (let ((o (string-append root "/eval.out")))
    (system (string-append "THEOURGIA_LOCAL=1 THEOURGIA_RUNNERS=on scheme --script ../core.sc eval --lang shell"
                           (if w (string-append " --working --writer " w) "")
                           " '" source "' --store '" S "' > '" o "' 2>/dev/null < /dev/null"))
    (call-with-input-file o
      (lambda (p)
        (let loop ((last #f))
          (let ((x (read p)))
            (cond ((eof-object? x) last)
                  ((and (pair? x) (memq (car x) '(ok error))) (loop x))
                  (else (loop last)))))))))
(define (stdout-of a) (let ((c (clause a 'stdout))) (and c (cadr c))))
(define (projection-of a) (let ((c (clause a 'projection))) (and c (cadr c))))
(want "WN-7 the foreign eval --working of w1: its projection holds its new files, b.sc once with the draft's bytes"
      (let ((a (shell-eval "w1" "ls; cat b.sc\n")))
        (list (and (pair? a) (car a)) (stdout-of a) (projection-of a)))
      (list 'ok (string-append "a.sc\nb.sc\nc.sc\nd.sc\nl.sc\n" b-text) '(files 5)))
(want "WN-7 the foreign eval --working of w3 lacks the deleted a.sc; without --working it lists the committed files"
      (list (stdout-of (shell-eval "w3" "ls\n")) (stdout-of (shell-eval #f "ls\n")))
      (list "c.sc\nl.sc\n" "a.sc\nc.sc\nl.sc\n"))

;; ---- WN-6: after the commit -----------------------------------------------------------------------

(define committed
  (run-as "w1" 'commit "--writer" "w1"
          "--working-new" (string-append d "=" (cadr (assq 'version (cdr d-draft))))
          "--working-new" (string-append b "=" (cadr (assq 'version (cdr b-draft))))))
(define created (let ((c (clause committed 'created))) (if c (cdr c) '())))
(define pieces (let ((c (clause committed 'pieces))) (if c (cdr c) '())))
(define (as-committed f)
  (cons (cdr (assoc (car f) created))
        (cons (cadr f) (map (lambda (c) (list (cdr (assoc (car c) pieces)) (cadr c))) (cddr f)))))
(want "WN-6 the commit maps both provisional ids"
      (map car created)
      (list d b))
(want "WN-6 outline --working lists the files under their committed ids, at the place the view gave them"
      (outline-text-of (run-as "w1" 'outline "--working"))
      (outline-of (list a-file c-file l-file (as-committed d-file) (as-committed b-file))))
(want "WN-6 the provisional id is not-committed again, with and without --working"
      (list (run-as "w1" 'read b "--working") (run-as "w1" 'read b))
      (list (not-committed b) (not-committed b)))

(system (string-append "rm -rf '" root "'"))
(printf "rows: ~a\n~a failures\nworking-files-view complete\n" rows bad)
(exit (if (= bad 0) 0 1))
