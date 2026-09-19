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

;; READING THIS TREE'S IMPORTS AS DATA. ONE COPY, TWO GATES.
;;
;; `facade-gate.sc` asks which root files reach igropyr; `closures.sc`
;; asks what a library's transitive import closure contains. Those are
;; the same walk over the same shape, and the walk is not obvious enough
;; to be written twice: it has to go into `only`/`except`/`rename`/
;; `prefix`, into every branch of a `meta-cond` including the one this
;; host would not take, and over IMPROPER lists. A second copy drifts,
;; and the copy that drifts is the one nobody is currently reading.
;;
;; NEVER: IT DOES NOT GREP, and that is the whole reason it exists. Measured
;; on the untouched tree before this batch: three root files carry
;; `(igropyr` in their TEXT -- `ffi.sc`, `wire.sc` and `log.sc` -- and
;; every one of those occurrences is a comment. A grep gate would have
;; named three files, one of them not even under discussion, on a tree
;; where nothing imported igropyr at all. Read as data, a comment is not
;; a datum and `"(igropyr sexpr)"` is a string.
;;
;; NEVER: IT IS LOADED, NOT INCLUDED. Chez's `include` resolves a relative
;; path against the CURRENT DIRECTORY, not against the including file --
;; measured both ways: from `test/` it found this file, and from the
;; repository root the same script died with `Exception in include:
;; failed for import-walk.scm: no such file or directory`. The runner
;; starts fixtures from `test/`; a person reading a red row does not.
;; So each caller computes its own directory from `(car (command-line))`
;; and hands `load` an absolute path.

;; EVERY FORM IN THE FILE, not just the first. A library is one form; a
;; top-level program (`cli.sc`, `rpc-worker.ss`, `eval-worker.sc`) is a
;; bare `(import ...)` followed by many, and its import is as much a use
;; of a dependency as a library's.
(define (forms-of path)
  (call-with-input-file path
    (lambda (p)
      (let loop ((out '()))
        (let ((x (read p)))
          (if (eof-object? x) (reverse out) (loop (cons x out))))))))

;; EVERY LIBRARY REFERENCE UNDER `prefix` WHEREVER IT SITS IN THE FORM.
;;
;; THE WALK IS PAIRWISE, NOT LIST-WISE. `(map ... (filter pair? form))`
;; raised on the first improper list it met -- a lambda's formals,
;; `(store supplied ids actor req . selected-version)`, is a pair whose
;; cdr chain ends in a symbol. Source is a tree of pairs, not a tree of
;; lists, and a walker written for lists dies on the first one.
(define (libs-in prefix x)
  (cond
    ((and (pair? x) (eq? prefix (car x))) (list x))
    ((pair? x) (append (libs-in prefix (car x)) (libs-in prefix (cdr x))))
    (else '())))

;; THE LIBRARY REFERENCES INSIDE THE `import` FORMS OF ONE FORM.
(define (imports-of prefix form)
  (cond
    ((not (pair? form)) '())
    ((eq? 'import (car form)) (libs-in prefix (cdr form)))
    (else (append (imports-of prefix (car form))
                  (imports-of prefix (cdr form))))))

(define (imports-of-file prefix path)
  (apply append (map (lambda (f) (imports-of prefix f)) (forms-of path))))

;; THE SOURCES IN A DIRECTORY, by the suffixes Chez is told to search.
;; NOTE: ONE LIST, READ BY BOTH SCANS. `facade-gate.sc` walks
;; subdirectories with its own reader; when it carried its own copy of
;; this list -- `.ss` only -- a nested `helper.sls` importing igropyr
;; went unseen by the very check added to see it.
(define source-suffixes '(".ss" ".sls" ".sc" ".scm"))

(define (source-files dir)
  (let ((exts source-suffixes))
    (list-sort string<?
      (filter (lambda (n)
                (exists (lambda (e)
                          (let ((ln (string-length n)) (le (string-length e)))
                            (and (> ln le) (string=? e (substring n (- ln le) ln)))))
                        exts))
              (directory-list dir)))))

(define (stem name)
  (let loop ((i (- (string-length name) 1)))
    (cond ((< i 0) name)
          ((char=? (string-ref name i) #\.) (string->symbol (substring name 0 i)))
          (else (loop (- i 1))))))

;; THE NAME A FILE DECLARES, not the name its filename suggests. A
;; program declares none and answers #f.
(define (declared-library-name path)
  (let loop ((fs (forms-of path)))
    (cond ((null? fs) #f)
          ((and (pair? (car fs)) (eq? 'library (caar fs)) (pair? (cdar fs)))
           (cadr (car fs)))
          (else (loop (cdr fs))))))
