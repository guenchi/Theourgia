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

;; Every durability point declares the stage it belongs to.
;;
;; A staged fault never matches a call site that declares no stage, so an
;; unlabelled flush is a step of the system that no case can arm -- and a
;; step nothing can arm reads, in every log, exactly like a step that
;; passed. Two of them sat in the publish path until a row was written
;; that tried to arm them.
;;
;; So the rule is checked rather than remembered. This walks the sources
;; with the READER, not with a regular expression: a flush inside a
;; string, a commented-out call, a call split across lines and a call
;; written on one are all the same datum to a reader and all different
;; text to a grep, and the tree has already paid for six hand-written
;; scanners.
;;
;; ONE ACCEPTED SHAPE, stated here so that "it looked fine" is not a
;; verdict anyone can reach: every call to a durability primitive or to
;; one of the helpers that wrap it names its stage in the stage
;; position, and that argument is either a quoted stage from the list
;; ffi.sc keeps, or the identifier `stage` -- which is only legal inside
;; a definition that takes `stage` as a formal, and every call to such a
;; definition is itself checked by this same rule.
;;
;; The shape is LOCAL on purpose. A stage that arrives dynamically, from
;; a `parameterize` somewhere up the call chain, is correct at run time
;; and unreadable at the call site: answering "which stage is this flush
;; in" then means tracing every caller, and a checker that tries to do
;; it is making a whole-program claim that quietly stops being true. A
;; required argument is answerable by looking at the line.
;;
;; THE LIST OF STAGES IS READ OUT OF ffi.sc, not restated here. A second
;; copy of it would be a second answer to "is this a real stage", and
;; this file exists because a second answer is what goes wrong.

(import (chezscheme))

(define (read-forms path)
  (call-with-port (open-input-file path)
    (lambda (p)
      (let loop ((out '()))
        (let ((d (read p)))
          (if (eof-object? d) (reverse out) (loop (cons d out))))))))

;; (define known-stages '(...)) wherever ffi.sc keeps it.
(define (known-stages-from path)
  (let walk ((forms (read-forms path)))
    (cond
      ((not (pair? forms)) #f)
      ((and (pair? (car forms)) (eq? (caar forms) 'define)
            (eq? (cadr (car forms)) 'known-stages))
       (cadr (caddr (car forms))))
      ((pair? (car forms))
       (or (walk (car forms)) (walk (cdr forms))))
      (else (walk (cdr forms))))))

;; The primitives that make something durable, and the helpers that wrap
;; them. A helper is listed with the position its stage argument sits in,
;; counting the operator as 0.
;; A NAME THAT IS NOT DEFINED TODAY STAYS ON THE LIST. `fsync-existing!`
;; was removed when it turned out to be a second flush of bytes that were
;; already durable; leaving its entry here costs nothing and means that
;; if it -- or anything else on this list -- comes back, it arrives
;; already checked rather than needing someone to remember this file.
(define durability-calls
  '((fsync! . 3)
    (fsync-dir! . 2)
    (atomic-write! . 3)
    (directory-entry-durable! . 2)
    (fsync-existing! . 2)
    (stage-candidate! . 4)))

(define (source-path name)
  (let* ((self (car (command-line)))
         (cut (let loop ((i (- (string-length self) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref self i) #\/) i)
                      (else (loop (- i 1))))))
         (dir (if cut (substring self 0 cut) "."))
         (beside (string-append dir "/" name))
         (above (string-append dir "/../" name)))
    (cond ((file-exists? beside) beside)
          ((file-exists? above) above)
          (else (assertion-violation 'stage-gate
                  "source is neither beside this checker nor one level up"
                  (list beside above))))))

(define ffi-source (source-path "ffi.sc"))

(define sources
  (map source-path
       (let ((extra (cdr (command-line))))
         (if (null? extra) '("log.sc" "store.sc") extra))))

(define known-stages
  (or (known-stages-from ffi-source)
      (assertion-violation 'stage-gate
        "ffi.sc does not define known-stages where this expects it" ffi-source)))

(define (literal-stage? x)
  (and (pair? x) (eq? (car x) 'quote) (pair? (cdr x))
       (memq (cadr x) known-stages) #t))

(define findings '())
(define checked 0)
(define (note! what where) (set! findings (cons (list what where) findings)))

;; A definition that takes `stage` as a formal may pass the identifier
;; on; anything else must name a stage outright.
(define (takes-stage? form)
  (and (pair? form) (eq? (car form) 'define) (pair? (cdr form)) (pair? (cadr form))
       (let loop ((xs (cdr (cadr form))))
         (cond ((not (pair? xs)) (and (eq? xs 'stage) #t))
               ((eq? (car xs) 'stage) #t)
               (else (loop (cdr xs)))))))

(define (arg-at form pos)
  (let loop ((xs form) (i 0))
    (cond ((not (pair? xs)) 'missing)
          ((= i pos) (car xs))
          (else (loop (cdr xs) (+ i 1))))))

(define (check-call form where may-pass-stage?)
  (let* ((entry (assq (car form) durability-calls))
         (pos (cdr entry))
         (arg (arg-at form pos)))
    (set! checked (+ checked 1))
    (cond
      ((literal-stage? arg) (void))
      ((and (eq? arg 'stage) may-pass-stage?) (void))
      ((eq? arg 'stage) (note! (list (car form) 'stage-passed-but-not-a-formal) where))
      (else (note! (list (car form) arg) where)))))

;; A DEFINITION'S HEADER IS NOT A CALL. `(define (fsync-existing! path
;; stage) ...)` has the shape of a call to the thing it defines, and
;; counting it as one both inflates the tally this checker reports and
;; would accept a header in place of the call site it is meant to
;; inspect. The header is skipped and only the body is walked.
(define (walk form where may-pass-stage?)
  (when (pair? form)
    (if (and (eq? (car form) 'define) (pair? (cdr form)) (pair? (cadr form)))
        (let ((inner-where (car (cadr form)))
              (inner-stage (takes-stage? form)))
          (for-each (lambda (f) (walk f inner-where inner-stage)) (cddr form)))
        (begin
          (when (and (symbol? (car form)) (assq (car form) durability-calls))
            (check-call form where may-pass-stage?))
          (let loop ((xs form))
            (when (pair? xs)
              (walk (car xs) where may-pass-stage?)
              (loop (cdr xs))))))))

(define (check-file path)
  (for-each (lambda (form) (walk form 'top #f)) (read-forms path)))

(for-each (lambda (p) (printf "stage-gate reading ~a\n" p)) sources)
(for-each check-file sources)
(printf "durability calls examined: ~a\n" checked)
(if (null? findings)
    (begin (printf "every one declares a known stage\n")
           (printf "stage-gate complete\n")
           (exit 0))
    (begin
      (for-each (lambda (f) (printf "UNSTAGED ~s in ~s\n" (car f) (cadr f)))
                (reverse findings))
      (printf "~a unstaged durability call(s)\n" (length findings))
      (printf "stage-gate complete\n")
      (exit 1)))
