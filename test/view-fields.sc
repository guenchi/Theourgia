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

;; WHICH FIELDS ARE STORED AND WHICH ARE READ OUT.
;;
;; `state-read` answers with what the log says. `view-read` answers with
;; that plus the fields this build derives: a text block's `name` and
;; `doc`, read out of its source through the language table, and a datum
;; block's `names`/`name`, read out of its body.
;;
;; THE HARD ROWS ARE THE REPLAYED ONES. A block written by an older
;; build can carry a `name` or a `doc` IN THE LOG -- the fields were
;; stored back when they were computed inside the reduction. What must
;; happen then is two different things at once:
;;
;;   `state-read`  gives back the stored value, unchanged and without
;;                 comment. It is what the log says; a reader that
;;                 "corrected" it would be rewriting history.
;;   `view-read`   gives the derived value, which for those blocks is a
;;                 DIFFERENT string. It is what this build reads.
;;
;; An implementation that dropped the stored field, or one that let it
;; shadow the derivation, passes half of this and fails the other half;
;; that is why both halves are asserted on the same block.

(import (chezscheme) (theourgia store) (theourgia reduce) (theourgia view)
        (theourgia rpc) (theourgia ffi)
        (only (theourgia languages) register-language!))

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
     (guard (e (#t (list 'RAISED
                         (if (and (condition? e) (message-condition? e))
                             (condition-message e) e))))
       e0))))
(define-syntax want
  (syntax-rules ()
    ((_ label got expect) (want-1 label (caught got) (caught expect)))))

(define root (string-append (or (getenv "THEOURGIA_TEST_ROOT") "/tmp")
                            "/view-fields-" (number->string (get-process-id))))
(mkdir-p! root)
(putenv "THEOURGIA_HOME" (string-append root "/home"))
(define store (string-append root "/store"))
(rpc-dispatch store '(init) "test")

(define (state) (open-and-reduce store))
(define (fields read id)
  (let ((b (read (state) id))) (and b (cdr (assq 'fields b)))))
(define (field read id key)
  (let ((p (and (fields read id) (assq key (fields read id))))) (and p (cdr p))))
(define (has? read id key) (and (fields read id) (assq key (fields read id)) #t))

;; NOTE: A STORED `doc` HAS TO BE IN THE LANGUAGE'S COMMENT FORM. A plain
;; string was refused with `(error malformed-intent (invalid-doc))` and
;; the fixture died on the next line reading an empty store -- the write
;; path validates a doc even when nothing derives one.
;;
;; A TEXT BLOCK WITH A NAME AND A DOC ALREADY IN THE LOG. `set` refuses
;; to write a derived field, which is the point of the rule -- so these
;; are put in through the store's own write path, the way a record from
;; an older build arrives at the reducer during replay.
(define text-src "(define (derived x) x)\n")
(with-store-write store
  (lambda (s v)
    (list (list 'insert 'root #f
                (list '(kind . code) '(mode . text) '(lang . scheme)
                      (cons 'src text-src)
                      '(name . "legacy-name")
                      '(doc . ";; legacy doc\n")))))
  "test")
(define text-id (cadar (state-datum (state))))

(want "VF-00 the replayed text block is there" (field state-read text-id 'kind) 'code)

(want "VF-01 state-read gives back the name the log holds"
      (field state-read text-id 'name) "legacy-name")
(want "VF-01 and the doc the log holds"
      (field state-read text-id 'doc) ";; legacy doc\n")

(want "VF-02 view-read reads the name out of the source instead"
      (field view-read text-id 'name) "derived")
(want "VF-02 TWIN: and the two really are different strings"
      (equal? (field state-read text-id 'name) (field view-read text-id 'name)) #f)

;; A DATUM BLOCK, SAME SHAPE. The body has to bind MORE THAN ONE name,
;; so that `names` being the whole list and `name` being the first of
;; them are two statements and not one.
;;
;; NOTE: IT IS NOT `define-values`, WHICH THIS READER DOES NOT NAME.
;; Measured: `datum-names` handles `define`, `define-syntax` and
;; `define-record-type`, and answers `()` for `(define-values (a b)
;; (values 1 2))` -- a legal R6RS definition form. That is a gap in the
;; reader, reported rather than worked around here; a cell written on it
;; would be asserting a feature instead of the split this file is about.
;; `define-record-type` binds four names and makes the same point.
(with-store-write store
  (lambda (s v)
    (list (list 'insert 'root #f
                (list '(kind . code) '(mode . datum)
                      '(body define-record-type point (fields x))
                      '(name . "legacy")
                      '(names "legacy")))))
  "test")
(define datum-id
  (let ((ids (map cadr (state-datum (state)))))
    (car (filter (lambda (id) (not (equal? id text-id))) ids))))

(want "VF-03 state-read keeps the datum block's stored name"
      (field state-read datum-id 'name) "legacy")
(want "VF-03 and its stored names list"
      (field state-read datum-id 'names) '("legacy"))

(want "VF-04 view-read reads every name the body binds"
      (field view-read datum-id 'names) '(point make-point point? point-x))
(want "VF-04 and takes the first of them as the name"
      (field view-read datum-id 'name) 'point)

;; THE CONTROL PAIR. Blocks written the ordinary way carry neither
;; field, so the rows above are about REPLAY and not about writes in
;; general -- and `view-read` must fill them in all the same.
(rpc-dispatch store (list 'insert "--title" "Plain" "--text" text-src) "test")
(define plain-id
  (let ((ids (map cadr (state-datum (state)))))
    (car (filter (lambda (id) (and (not (equal? id text-id)) (not (equal? id datum-id)))) ids))))

(want "VF-05 an ordinary write stores no name"
      (has? state-read plain-id 'name) #f)
(want "VF-05 and no doc"
      (has? state-read plain-id 'doc) #f)

;; A DERIVATION THAT RAISES NAMES THE BLOCK. The language below is one
;; register-language! accepts and whose comment prefixes are not a list,
;; so reading a text block's name and doc through it raises. `read`,
;; `read --recursive` and `outline` refuse as any raise is refused, and
;; the refusal ends with the block's id.
(register-language! '((lang "brittle") (extensions ("brittle")) (def-heads ()) (comment-prefixes 5)))
(with-store-write store
  (lambda (s v)
    (list (list 'insert 'root #f
                (list '(kind . code) '(mode . text) '(lang . brittle)
                      (cons 'src "brittle line\n")))))
  "test")
(define brittle-id
  (let ((ids (map cadr (state-datum (state)))))
    (car (filter (lambda (id) (not (member id (list text-id datum-id plain-id)))) ids))))
(define (refusal-head a)
  (and (pair? a) (pair? (cdr a))
       (list (car a) (cadr a) (and (list? a) (assq 'id (cddr a))))))
(want "VF-06 a read whose field derivation raises is refused internal and names the block"
      (refusal-head (rpc-dispatch store (list 'read brittle-id) "test"))
      (list 'error 'internal (list 'id brittle-id)))
(want "VF-06 and read --recursive names it too"
      (refusal-head (rpc-dispatch store (list 'read brittle-id "--recursive") "test"))
      (list 'error 'internal (list 'id brittle-id)))
(want "VF-06 and outline, which shows its title, names it too"
      (refusal-head (rpc-dispatch store (list 'outline) "test"))
      (list 'error 'internal (list 'id brittle-id)))
(want "VF-06 the refusal keeps the condition clause it had, before the id"
      (let ((a (rpc-dispatch store (list 'read brittle-id) "test")))
        (and (list? a) (>= (length a) 4)
             (list (car (caddr a)) (string? (cadr (caddr a))) (list-tail a 3))))
      (list 'condition #t (list (list 'id brittle-id))))

(printf "rows: ~a\n~a failures\nview-fields complete\n" rows bad)
