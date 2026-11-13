#!r6rs
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

;;; (theourgia degeneracy) -- the test-only switch under which a maintained
;;; structure is checked against the computation it replaces.
;;;
;;; THEOURGIA_DEGENERACY IN THE PROCESS THAT EXPANDS A LIBRARY (as
;;; THEOURGIA_INJECT is read, ffi.sc) says which checks exist: the
;;; expressions a library wraps in `(degeneracy-check <name> ...)` run only
;;; when the check <name> is on, and `(degeneracy-value <name> <on> <off>)`
;;; is <on> when it is on and <off> when it is not, the other expression not
;;; compiled at all. The forms a library wraps in `degeneracy-definitions` are
;;; definitions when any check is on; unset, all of it expands to nothing, and
;;; nothing it holds is compiled or run. The point (SDF 3.9, degeneracy): the
;;; old computation runs beside the new one, and a difference is red where it
;;; appears.
;;;
;;; THE VARIABLE TAKES THREE FORMS:
;;;   1                     every check;
;;;   only:<name>,<name>    those checks and no other;
;;;   all-but:<name>,<name> every check but those.
;;; Unset, empty or 0, no check is on: those are how an environment says off.
;;; A whole-suite gate names what it leaves out (all-but), so a check added
;;; later is on in the gate unless someone writes its name there. A name this
;;; library does not know, in the variable or at a use, an empty one (a
;;; trailing comma's too), and any value of the variable but those above,
;;; refuse at expansion, naming it: a misspelt name must not turn
;;; a check off in silence. A name given twice is that name once.
;;;
;;; THE CHECKS, in the order a reading lists them:
;;;   structure-per-record      reduce.sc: the structure recomputed and compared
;;;                             after every applied record
;;;   structure-restored        reduce.sc: the same, once per restore
;;;   siblings-list             reduce.sc: a parent's sibling list recomputed
;;;   sibling-order             reduce.sc: a sibling's neighbour recomputed
;;;   replay-scan               log.sc: every replay of a remembered segment
;;;                             against scan-segment of the same bytes, and
;;;                             every delivery watched
;;;   kept-prefix               log.sc: after each committed append that keeps
;;;                             a prefix, the writer's log validated in full
;;;                             and compared field by field
;;;   admission-judgements      admission.sc: the indexed judgement against the
;;;                             whole-bucket one, request.sc's identity-gates/full
;;;                             being that reference (one check, two places)
;;;   changed-files-reference   landing-files.sc: changed-files against its
;;;                             materialised reference
;;;   slot-classified-once      request.sc: no slot classified twice in a run
;;; TWO ARE LEFT OUT OF A WHOLE-SUITE GATE FOR THEIR COST and judged in their
;;; own fixtures, whose children set the variable to 1 whatever the pass sets:
;;; structure-per-record costs records times blocks in a load (structure-graph
;;; SG-1 judges it), kept-prefix a pass over the writer's log per write
;;; (append-prefix AP-D judges it). Each witness's child reports that check
;;; on by name and its row reads it, and the degeneracy-switch fixture's
;;; census pins that both are so.
;;;
;;; `(degeneracy-active)` is #t when any check is on, `(degeneracy-active
;;; <name>)` when that one is, and `(degeneracy-checks-on)` is the list of the
;;; checks on, so a reading can print the instrument it was taken with.

(library (theourgia degeneracy)
  (export degeneracy-definitions degeneracy-check degeneracy-active degeneracy-value
          degeneracy-checks-on degeneracy-known-checks)
  (import (chezscheme))

  (meta define degeneracy-known
    '(structure-per-record structure-restored siblings-list sibling-order replay-scan kept-prefix
      admission-judgements changed-files-reference slot-classified-once))

  ;; -> the names in TEXT, separated by commas, each a symbol; an empty name
  ;; or an unknown one refuses
  (meta define (degeneracy-names text form)
    (let loop ((i 0) (start 0) (out '()))
      (if (or (= i (string-length text)) (char=? (string-ref text i) #\,))
          (let* ((piece (substring text start i))
                 (name (string->symbol piece))
                 (out (cond ((string=? piece "")
                             (assertion-violation 'theourgia-degeneracy
                               "THEOURGIA_DEGENERACY names an empty check" form))
                            ((memq name degeneracy-known) (cons name out))
                            (else
                             (assertion-violation 'theourgia-degeneracy
                               "THEOURGIA_DEGENERACY names a check this library does not know"
                               piece degeneracy-known)))))
            (if (= i (string-length text))
                (reverse out)
                (loop (+ i 1) (+ i 1) out)))
          (loop (+ i 1) start out))))

  (meta define (degeneracy-prefix? prefix v)
    (and (>= (string-length v) (string-length prefix))
         (string=? (substring v 0 (string-length prefix)) prefix)))

  ;; THE CHECKS ON, in the known order, read once at this library's expansion
  (meta define degeneracy-on
    (let ((v (getenv "THEOURGIA_DEGENERACY")))
      (cond
        ((or (not v) (string=? v "") (string=? v "0")) '())
        ((string=? v "1") degeneracy-known)
        ((degeneracy-prefix? "only:" v)
         (let ((names (degeneracy-names (substring v 5 (string-length v)) v)))
           (filter (lambda (n) (memq n names)) degeneracy-known)))
        ((degeneracy-prefix? "all-but:" v)
         (let ((names (degeneracy-names (substring v 8 (string-length v)) v)))
           (filter (lambda (n) (not (memq n names))) degeneracy-known)))
        (else
         (assertion-violation 'theourgia-degeneracy
           "THEOURGIA_DEGENERACY is 1, only:<names> or all-but:<names>" v)))))

  ;; -> whether the check NAME (an identifier at a use) is on; an unknown name
  ;; refuses the use
  (meta define (degeneracy-named-on? form name)
    (let ((n (syntax->datum name)))
      (unless (memq n degeneracy-known)
        (syntax-violation #f "a degeneracy check this library does not know" form name))
      (and (memq n degeneracy-on) #t)))

  ;; (degeneracy-definitions <definition> ...): the definitions, when any
  ;; check is on
  (define-syntax degeneracy-definitions
    (lambda (x)
      (syntax-case x ()
        ((_ form ...) (if (pair? degeneracy-on) #'(begin form ...) #'(begin))))))

  ;; (degeneracy-check <name> <expression> ...): evaluated for effect when the
  ;; check <name> is on
  (define-syntax degeneracy-check
    (lambda (x)
      (syntax-case x ()
        ((_ name e ...)
         (identifier? #'name)
         (if (degeneracy-named-on? x #'name) #'(begin e ... (if #f #f)) #'(if #f #f))))))

  ;; (degeneracy-value <name> <on> <off>): <on> when the check is on; the other
  ;; is not compiled
  (define-syntax degeneracy-value
    (lambda (x)
      (syntax-case x ()
        ((_ name on off)
         (identifier? #'name)
         (if (degeneracy-named-on? x #'name) #'on #'off)))))

  ;; (degeneracy-active): any check on; (degeneracy-active <name>): that one
  (define-syntax degeneracy-active
    (lambda (x)
      (syntax-case x ()
        ((_) (if (pair? degeneracy-on) #'#t #'#f))
        ((_ name)
         (identifier? #'name)
         (if (degeneracy-named-on? x #'name) #'#t #'#f)))))

  ;; (degeneracy-checks-on): the names of the checks on, a quoted list
  (define-syntax degeneracy-checks-on
    (lambda (x)
      (syntax-case x ()
        ((k) (with-syntax ((names (datum->syntax #'k degeneracy-on))) #''names)))))

  ;; (degeneracy-known-checks): every name this library knows, a quoted list
  (define-syntax degeneracy-known-checks
    (lambda (x)
      (syntax-case x ()
        ((k) (with-syntax ((names (datum->syntax #'k degeneracy-known))) #''names)))))
)
