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

(import (chezscheme) (theourgia text-code))
(define bad 0)
(define (want label got expected)
  (if (equal? got expected) (printf "ok ~a\n" label)
      (begin (set! bad (+ bad 1)) (printf "FAIL ~a: ~s WANT ~s\n" label got expected))))

;; THE SOURCES ARE FOUND FROM THIS SCRIPT, AND IDENTIFIED BY WHAT THEY
;; CONTAIN. The runner starts every fixture from `test/`, where none of
;; these five names exists as a library, so the audit died at its first
;; read with rc=255 -- a red about the caller's directory rather than
;; about the sources.
;;
;; ⚠️ AND LOOKING BESIDE ITSELF FIRST WAS WRONG, because two of these
;; names exist twice. `test/code-suggest.ss` is a FIXTURE and
;; `code-suggest.ss` is the library it tests; a locator that takes the
;; first path that exists read the fixture, whose first datum is an
;; `(import ...)` form, and both audits then walked it and found
;; nothing. All four rows printed `ok` while the library they name went
;; unread. (`code-markers.ss` is the same pair, and is not in either
;; list today -- which is luck, not a design.)
;;
;; So a candidate is accepted only if its first datum is
;; `(library (theourgia <name>) ...)` for the <name> being asked for.
;; That is a property of the file rather than of its position, and it is
;; the same property the rows below then assert they saw.
(define script-dir
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1)))))))
    (if cut (substring self 0 cut) ".")))
(define (library-name form)
  (and (pair? form) (eq? 'library (car form)) (pair? (cdr form))
       (let ((spec (cadr form)))
         (and (list? spec) (= 2 (length spec)) (eq? 'theourgia (car spec))
              (cadr spec)))))
(define (source-form file)
  (let* ((wanted (string->symbol (substring file 0 (- (string-length file) 3))))
         (candidates (list (string-append script-dir "/../" file)
                           (string-append script-dir "/" file)))
         (found (filter (lambda (path)
                          (and (file-exists? path)
                               (eq? wanted
                                    (library-name
                                      (guard (e (#t #f))
                                        (call-with-input-file path read))))))
                        candidates)))
    (if (null? found)
        (assertion-violation 'code-text-audit
          "no candidate holds the library being audited"
          (list wanted candidates))
        (call-with-input-file (car found) read))))

;; THE AUDIT SAYS WHICH LIBRARIES IT READ, BY NAME. Both original rows
;; are satisfied by an empty list, so a locator that found nothing --
;; or the wrong file -- would read as a clean audit. Counting the names
;; ASKED FOR would not have caught it either: the first version of these
;; two rows was `(length (map source-file '(...)))` against a
;; three-element literal, which is three whatever the locator does.
;; These name what came back.
(define (names-read files) (map (lambda (f) (library-name (source-form f))) files))
(define numeric-files '("text-code.ss" "regex.ss" "languages.ss"))
(define branch-files '("text-code.ss" "code-project.ss" "code-suggest.ss"))

(define numeric-sites '())
(define language-sites '())
(define language-names '("scheme" "javascript" "typescript" "python" "go" "rust" "c" "java" "shell" "markdown"))
(define (walk-numeric x file)
  (when (pair? x)
    (unless (eq? 'quote (car x))
      (when (memq (car x) '(read get-datum string->number eval load)) (set! numeric-sites (cons (list file x) numeric-sites)))
      (walk-numeric (car x) file)
      (walk-numeric (cdr x) file))))
(define (contains-language? x)
  (cond ((string? x) (and (member x language-names) #t))
        ((pair? x) (or (contains-language? (car x)) (contains-language? (cdr x)))) (else #f)))
(define (walk-branches x file)
  (when (pair? x)
    (when (and (memq (car x) '(string=? equal? eq? eqv? case)) (contains-language? x))
      (set! language-sites (cons (list file x) language-sites)))
    (unless (eq? 'quote (car x))
      (walk-branches (car x) file)
      (walk-branches (cdr x) file))))
(for-each (lambda (file) (walk-numeric (source-form file) file)) numeric-files)
(for-each (lambda (file) (walk-branches (source-form file) file)) branch-files)
(want "CT-14 the audit read the three name-extraction libraries"
      (names-read numeric-files) '(text-code regex languages))
(want "CT-02 the audit read the three shared-engine libraries"
      (names-read branch-files) '(text-code code-project code-suggest))
(want "CT-14 source inventory excludes numeric readers from name extraction" numeric-sites '())
(want "CT-02 shared engines have no language-name branch" language-sites '())
(printf "~a failures\ncode-text-audit complete\n" bad)
(exit (if (zero? bad) 0 1))
