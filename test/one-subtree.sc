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

;; "Under this block" has one meaning.
;;
;; A document is a top-level block of kind `doc`. The write path refuses a
;; document anywhere else, by insert, move and set alike, and this product
;; has no nested documents. So nothing branches on one any more: `read
;; --recursive`, `grep --under` and the export all walk the whole subtree,
;; and a document below the root, if some history not made by this write
;; path held one, is a block like any other. It is REPORTED, by `conflicts`
;; as `(nested-document <id>)`, and interpreted by nobody.
;;
;; The store below holds one such block, written through the log the way the
;; write path would refuse to write it, so that the rows can show it is not
;; special.

(import (chezscheme)
        (only (theourgia rpc) rpc-dispatch)
        (only (theourgia reduce) outline-subtree state-outline block-id)
        (only (theourgia store) open-and-reduce)
        (theourgia log))

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
(define-syntax want
  (syntax-rules ()
    ((_ label got expected)
     (begin (set! rows (+ rows 1))
            (want-1 label (caught got) (caught expected))))))

;; SCRATCH PATHS LIVE UNDER THE RUNNER'S SCRATCH ROOT (F71).
(define scratch-base
  (let ((v (getenv "THEOURGIA_TEST_ROOT")))
    (if (and (string? v) (> (string-length v) 0)) v "/tmp")))
(define here (string-append scratch-base "/one-subtree-" (number->string (get-process-id))))
(when (file-exists? here)
  (assertion-violation 'one-subtree "scratch directory already exists" here))
(system (string-append "mkdir -p " here "/store " here "/home " here "/out"))
(putenv "THEOURGIA_HOME" (string-append here "/home"))
(define store (string-append here "/store"))
(define (ask . req) (rpc-dispatch store req "test"))

;; ---- the tree ---------------------------------------------------------------
;;
;;   A  document, top level     "top"
;;     B  section               "needle in b"
;;       C  kind doc, written through the log, with front matter and a body
;;         D  section           "needle in d"
;;       E  section             "needle in e"
;;       F  section             "needle in f"
(ask 'init)
(define (ids-in-outline) (map caddr (state-outline (open-and-reduce store))))
(define (the-new-id before)
  (let loop ((xs (ids-in-outline)))
    (cond ((null? xs) #f)
          ((member (car xs) before) (loop (cdr xs)))
          (else (car xs)))))
(define (insert! . args)
  (let ((before (ids-in-outline)))
    (apply ask 'insert args)
    (the-new-id before)))
(define A (insert! "--title" "A" "--text" "top"))
(ask 'set A "kind" "doc")
(ask 'set A "path" "a.md")
(define B (insert! "--under" A "--title" "B" "--text" "needle in b"))
(define C
  (let* ((sess (log-begin store (lambda args 'applied)))
         (v (session-view sess))
         (id (block-id (view-writer v) (view-expect-seq v))))
    (session-append! sess (make-frame (view-revision v) (view-epoch v) (view-writer v)
                                      (view-expect-seq v) "one-subtree-fixture" '()
                                      (list 'put (list (cons 'kind 'doc)
                                                       (cons 'path "inner.md")
                                                       (cons 'title "C")
                                                       (cons 'front "---\nfm: NESTEDFRONTMARK\n---\n")
                                                       (cons 'src "NESTEDBODYMARK\n")
                                                       (cons 'parent B)
                                                       (cons 'ord 0)))))
    (session-commit! sess)
    (log-end! sess)
    id))
(define D (insert! "--under" C "--title" "D" "--text" "needle in d"))
(define E (insert! "--under" B "--title" "E" "--text" "needle in e"))
(define F (insert! "--under" B "--title" "F" "--text" "needle in f"))

(want "CONTROL F85-0 the six blocks exist, C under B and D under C"
      (let* ((outline (state-outline (open-and-reduce store)))
             (parent-of (lambda (id)
                          (let loop ((rs outline))
                            (cond ((null? rs) 'NOT-PLACED)
                                  ((equal? (caddr (car rs)) id) (car (car rs)))
                                  (else (loop (cdr rs))))))))
        (list (length outline) (equal? (parent-of C) B) (equal? (parent-of D) C)))
      '(6 #t #t))

(define (ids-of answer)
  (if (and (pair? answer) (eq? (car answer) 'ok) (pair? (cadr answer)))
      (map (lambda (it) (let ((e (and (pair? it) (assq 'id it)))) (if e (cdr e) it)))
           (cdr (cadr answer)))
      answer))
(define (match-ids answer)
  (if (and (pair? answer) (eq? (car answer) 'ok) (pair? (cadr answer)))
      (map (lambda (it) (if (and (pair? it) (eq? (car it) 'match)) (cadr it) it))
           (cdr (cadr answer)))
      answer))

(printf "== F85-1: read --recursive walks the whole subtree ==\n")
(want "F85-1 read A --recursive answers A B C D E F, in outline order"
      (ids-of (ask 'read A "--recursive"))
      (list A B C D E F))
;; ONE DEFINITION, NOT TWO THAT AGREE: the answer is the outline's own walk.
(want "F85-1 its set is exactly the outline walk's, root included"
      (let ((got (ids-of (ask 'read A "--recursive")))
            (walk (outline-subtree (state-outline (open-and-reduce store)) A)))
        (list (list-sort string<? got) (list-sort string<? walk)))
      (let ((walk (outline-subtree (state-outline (open-and-reduce store)) A)))
        (list (list-sort string<? walk) (list-sort string<? walk))))

(printf "== F85-2: grep --under searches the same blocks ==\n")
(want "F85-2 grep --under A finds the needles in B, D, E and F"
      (list-sort string<? (match-ids (ask 'grep "needle" "--under" A)))
      (list-sort string<? (list B D E F)))

(printf "== F85-3: the export writes one file per top-level document ==\n")
(let* ((out (string-append here "/out"))
       (answer (caught (ask 'export-md out)))
       (files (list-sort string<? (directory-list out))))
  (want "F85-3 the export answers one file and writes exactly a.md"
        (list (and (pair? answer) (car answer))
              (let ((f (and (pair? answer) (list? answer) (assq 'files (cdr answer))))) (and f (cadr f)))
              files)
        '(ok 1 ("a.md")))
  (want "F85-3 a.md holds C's title and D's text: the block of kind doc is written into its ancestor's file"
        (let ((text (call-with-input-file (string-append out "/a.md")
                      (lambda (i) (let loop ((acc '()))
                                    (let ((c (read-char i)))
                                      (if (eof-object? c) (list->string (reverse acc)) (loop (cons c acc)))))))))
          (list (and (let loop ((i 0)) (and (<= (+ i 3) (string-length text))
                                            (or (string=? "# C" (substring text i (+ i 3)))
                                                (string=? "#C" (substring text i (+ i 2)))
                                                (loop (+ i 1)))))
                     #t)
                (and (let loop ((i 0)) (and (<= (+ i 11) (string-length text))
                                            (or (string=? "needle in d" (substring text i (+ i 11)))
                                                (loop (+ i 1)))))
                     #t)))
        '(#t #t)))

(printf "== F85-7: what the export could not write is said ==\n")
;; C'S FRONT MATTER REACHES NO FILE: a block written into its ancestor's file
;; is rendered as a section, and a section has no front (design R5). On the
;; tree before F85 it was in inner.md. The answer says so in a clause of its
;; own. Not in `skipped`: C's body WAS written, and skipped's 1 + n counts
;; lost blocks (md2 F42).
(let* ((out (string-append here "/out7"))
       (answer (begin (system (string-append "mkdir -p " out)) (caught (ask 'export-md out))))
       (clause (lambda (k) (and (pair? answer) (list? answer)
                                (let ((c (assq k (cdr answer)))) (and c (cdr c))))))
       (text (guard (e (#t ""))
               (call-with-input-file (string-append out "/a.md")
                 (lambda (i) (let loop ((acc '()))
                               (let ((c (read-char i)))
                                 (if (eof-object? c) (list->string (reverse acc)) (loop (cons c acc))))))))))
  (want "F85-7 the export says C's front was not written, in fields-not-written and not in skipped; a.md holds C's body"
        (list (clause 'fields-not-written)
              (clause 'skipped)
              (and (let loop ((i 0)) (and (<= (+ i 14) (string-length text))
                                          (or (string=? "NESTEDBODYMARK" (substring text i (+ i 14)))
                                              (loop (+ i 1)))))
                   #t))
        (list (list (list C 'front)) #f #t)))

(printf "== F85-8: a section's front that reaches no file is named too ==\n")
;; ON A STORE WITH NO BLOCK OF KIND DOC BELOW THE ROOT. A section's front, set
;; through the ordinary write path, reached no file on 409c5c8 either, and
;; nothing said so (measured by the code session, secfront.reading). R5's
;; clause names every written block whose stored front was not written, so it
;; names this one too: a loss the tree already had, now said (design, frame
;; amendment to N).
(let* ((d2 (string-append here "/plain"))
       (st2 (string-append d2 "/store"))
       (out2 (string-append d2 "/out"))
       (ask2 (lambda req (rpc-dispatch st2 req "test"))))
  (system (string-append "mkdir -p " st2 " " out2))
  (ask2 'init)
  (let* ((before (map caddr (state-outline (open-and-reduce st2))))
         (_ (ask2 'insert "--title" "Top" "--text" "top"))
         (top (let loop ((xs (map caddr (state-outline (open-and-reduce st2)))))
                (cond ((null? xs) #f) ((member (car xs) before) (loop (cdr xs))) (else (car xs)))))
         (_k (ask2 'set top "kind" "doc"))
         (_p (ask2 'set top "path" "top.md"))
         (before2 (map caddr (state-outline (open-and-reduce st2))))
         (_s (ask2 'insert "--under" top "--title" "" "--text" "section body"))
         (sec (let loop ((xs (map caddr (state-outline (open-and-reduce st2)))))
                (cond ((null? xs) #f) ((member (car xs) before2) (loop (cdr xs))) (else (car xs)))))
         (setans (ask2 'set sec "front" "---\nsecfront: SECFRONTMARK\n---\n"))
         (answer (caught (ask2 'export-md out2))))
    (want "CONTROL F85-8 the store holds one top-level doc and one section, and the section's front was stored"
          (list (and top sec #t) (and (pair? setans) (car setans)))
          '(#t ok))
    (want "F85-8 the export names the section's front in fields-not-written, and says nothing in skipped"
          (list (and (pair? answer) (list? answer)
                     (let ((c (assq 'fields-not-written (cdr answer)))) (and c (cdr c))))
                (and (pair? answer) (list? answer) (assq 'skipped (cdr answer)) #t))
          (list (list (list sec 'front)) #f))))

(printf "== F85-4: it is still reported ==\n")
(want "GUARD F85-4 conflicts reports C as a nested document"
      (let ((a (ask 'conflicts)))
        (and (pair? a) (list? a)
             (let loop ((xs (if (and (pair? (cdr a)) (pair? (cadr a))) (cdr (cadr a)) '())))
               (cond ((null? xs) #f)
                     ((equal? (car xs) (list 'nested-document C)) #t)
                     (else (loop (cdr xs)))))))
      #t)

(printf "rows: ~a\n" rows)
(printf "~a failures\n" bad)
(system (string-append "rm -rf " here))
(printf "one-subtree complete\n")
