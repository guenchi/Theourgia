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

(import (chezscheme))

;; THE SOURCE IS FOUND FROM THIS SCRIPT, NOT FROM THE CURRENT DIRECTORY.
;; The runner starts every fixture from `test/`, so `"project.ss"` named
;; nothing and this file died at its first form with rc=255 -- a red that
;; is about the caller's directory rather than about the audit. The two
;; layouts the suite is run in are the repository (fixtures under test/,
;; libraries one level up) and a flat delivery directory, so both are
;; tried, and neither existing is said once rather than discovered by
;; each reader.
(define (source-file name)
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1))))))
         (dir (if cut (substring self 0 cut) "."))
         (beside (string-append dir "/" name))
         (above (string-append dir "/../" name)))
    (cond
      ((file-exists? beside) beside)
      ((file-exists? above) above)
      (else (assertion-violation 'crlf-consumers
              "the library source is neither beside this fixture nor one level up"
              (list beside above))))))

;; The Scheme reader excludes comments and treats strings as leaves.
;; Audit the entire binding body, rejecting any new use or shadowing of line.
(define form (call-with-input-file (source-file "project.ss") read))
(define definition
  (find (lambda (x) (and (pair? x) (eq? 'define (car x))
                         (equal? '(strip-recovery text) (cadr x)))) (cddddr form)))
(define binding-body #f)
(define (find-binding x)
  (when (pair? x)
    (when (and (eq? 'let* (car x)) (assq 'line (cadr x))) (set! binding-body x))
    (for-each find-binding (filter pair? x))))
(find-binding definition)
(unless binding-body (error 'crlf-consumers "Missing lexical binding"))
(define uses '())
(define (walk x parent)
  (cond ((eq? x 'line) (set! uses (cons parent uses)))
        ((pair? x) (for-each (lambda (a) (walk a x)) x))))
;; Later let* initializers and the body are the scope of this binding.
(for-each (lambda (b) (walk (cadr b) b)) (cdr (memq (assq 'line (cadr binding-body)) (cadr binding-body))))
(for-each (lambda (body) (walk body #f)) (cddr binding-body))
(printf "QR-04 lexical consumers: ~s\n" (reverse uses))
(unless (equal? (reverse uses) '((recovery-id line) (heading-line? line)))
  (error 'crlf-consumers "The dead-copy rationale must be reviewed" uses))
(printf "crlf-consumers complete\n")
