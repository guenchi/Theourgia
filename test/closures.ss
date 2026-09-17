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

;; WHAT THE REDUCER IS ALLOWED TO REACH.
;;
;; The block hash is a fact about what was stored. The derived view --
;; a title read out of a document, the language a block is written in,
;; the name a definition carries -- is a fact about how this version of
;; this core reads it. While the reducer could reach the language table,
;; registering a language could change a hash, and a store written by
;; one build would stop agreeing with the same store read by the next.
;;
;; THE RULE IS ABOUT REACHING, NOT ABOUT CALLING, so nothing local can
;; witness it. `reduce.ss` never named `languages`; it named
;; `text-code`, which names `languages`. A reviewer reading `reduce.ss`
;; sees a clean file. The property is a property of the CLOSURE, and the
;; closure is what this reads.
;;
;; ⛔ NOT A GREP OVER THE FILE. The walk is `import-walk.scm`, shared
;; with `facade-gate.ss`; why it reads the forms as data rather than the
;; text is written there.
;;
;; THE FOURTH ROW IS THE TWIN, AND IT IS THE POINT OF THE OTHER THREE.
;; Three closures that do not contain a name are also what a walker that
;; returns nothing produces, and a walker that returns nothing is the
;; likelier of the two. `rpc` reaches all five of the forbidden names --
;; it is the layer that is SUPPOSED to project a view -- so the same
;; question, asked about it, has to come back non-empty. Without that
;; row these cells would be green on a tree with no import graph at all.

(import (chezscheme))

(define failures 0)
(define rows 0)
(define (want-1 name actual expected)
  (set! rows (+ rows 1))
  (if (equal? actual expected)
      (printf "ok ~a\n" name)
      (begin (set! failures (+ failures 1))
             (printf "FAIL ~a: ~s WANT ~s\n" name actual expected))))
;; BOTH SIDES OF A ROW ARE GUARDED. A `want` written as a procedure
;; evaluates its arguments before the call, so an argument that raises
;; kills the fixture: the row never prints, and what the suite sees is a
;; missing sentinel rather than a red row naming the question that could
;; not be answered. This directory's `run-fixtures.sh` counts fixtures
;; whose `want` is a bare procedure for exactly that reason.
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

(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define root
  (let ((up (string-append script-dir "/..")))
    (if (file-exists? (string-append up "/cli.ss")) up script-dir)))

(load (string-append script-dir "/import-walk.scm"))

;; THE GRAPH IS BUILT FROM WHAT EACH FILE DECLARES ITSELF TO BE, not
;; from its filename. They agree in this tree, and a graph keyed on
;; filenames would go on agreeing with itself after they stopped.
(define graph
  (let loop ((names (source-files root)) (out '()))
    (if (null? names)
        out
        (let* ((path (string-append root "/" (car names)))
               (declared (declared-library-name path)))
          (loop (cdr names)
                (if (and (pair? declared) (eq? 'theourgia (car declared)) (pair? (cdr declared)))
                    (cons (cons (cadr declared)
                                (map cadr (filter (lambda (r) (pair? (cdr r)))
                                                  (imports-of-file 'theourgia path))))
                          out)
                    out))))))

;; THE CLOSURE, WITH A SEEN SET. Chez refuses an import cycle outright
;; -- measured, on two libraries importing each other: `Exception:
;; cyclic dependency involving import of library (cyclic a)` -- so this
;; tree cannot hold one. The seen set is here anyway, because a walker
;; that assumes its input is acyclic answers a question about the
;; walker: it would spin rather than say so.
(define (closure start)
  (let loop ((todo (list start)) (seen '()))
    (cond
      ((null? todo) (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) seen))
      ((memq (car todo) seen) (loop (cdr todo) seen))
      (else
       (let ((edges (cond ((assq (car todo) graph) => cdr) (else '()))))
         (loop (append edges (cdr todo)) (cons (car todo) seen)))))))

;; THE NAMES THAT PROJECT A VIEW. A block's stored bytes do not depend
;; on any of them; what a reader shows for that block does.
(define derived '(languages text-code datum-code code-project datum-project))

(define (reaches start)
  (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
             (filter (lambda (n) (memq n derived)) (closure start))))

(define (report start)
  (printf "closure ~a (~a): ~s\n" start (length (closure start)) (closure start)))

;; THE INSTRUMENT'S OWN FIRST READING, BEFORE ANY OF THE ROWS BELOW.
;; An empty graph makes every "does not reach" row green.
(want "CL-00 the graph holds this core's libraries"
      (> (length graph) 20) #t)
(want "CL-00 and every name the rows below ask about is in it"
      (filter (lambda (n) (not (assq n graph)))
              '(reduce markers datum-metadata rpc view))
      '())

(for-each report '(reduce markers datum-metadata view rpc))

(want "CL-01 the reducer cannot reach a derived view" (reaches 'reduce) '())
(want "CL-02 the marker grammar cannot reach a derived view" (reaches 'markers) '())
(want "CL-03 datum metadata cannot reach a derived view" (reaches 'datum-metadata) '())

;; THE TWIN. Same question, a layer that is supposed to answer yes.
(want "CL-04 rpc does reach all five, which is how we know the walk walks"
      (reaches 'rpc)
      (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b))) derived))

;; AND THE PROJECTION LIVES SOMEWHERE. `view-read` was moved out of the
;; reducer, not deleted; if it had been deleted the three rows above
;; would read exactly the same.
;;
;; THE THREE NAMES ARE THE THREE IMPORTS `reduce.ss` LOST, taken from
;; the change rather than from a reading of the answer: an expectation
;; copied from what the instrument prints cannot disagree with it. The
;; other two derived names are NOT here and must not be -- `code-project`
;; and `datum-project` are built ON the view, so a closure of `view`
;; that contained them would be a cycle, not a success.
(want "CL-05 the view layer reaches the three projections the reducer gave up"
      (reaches 'view)
      '(datum-code languages text-code))

(printf "rows: ~a\n~a failures\nclosures complete\n" rows failures)
(exit (if (zero? failures) 0 1))
