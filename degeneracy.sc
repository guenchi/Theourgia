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
;;; THEOURGIA_DEGENERACY=1 IN THE PROCESS THAT EXPANDS A LIBRARY (as
;;; THEOURGIA_INJECT is read, ffi.sc): the forms a library wraps in
;;; `degeneracy-definitions` are definitions only then, and the expressions it
;;; wraps in `degeneracy-check` run only then; unset, both expand to nothing,
;;; and nothing they hold is compiled or run. The point (SDF 3.9, degeneracy):
;;; the old computation runs beside the new one after every record, and a
;;; difference is red at the record where it appears.
;;;
;;; THE MACROS ARE DEFINED IN THE BRANCH ITSELF, so the one expansion of this
;;; library fixes which pair exists: nothing reads the variable again where
;;; they are used.

(library (theourgia degeneracy)
  (export degeneracy-definitions degeneracy-check degeneracy-active)
  (import (chezscheme))

  (meta define degeneracy-mode
    (let ((v (getenv "THEOURGIA_DEGENERACY")))
      (and v (string=? v "1") #t)))

  (meta-cond
    (degeneracy-mode
     ;; (degeneracy-definitions <definition> ...): the definitions
     (define-syntax degeneracy-definitions
       (syntax-rules () ((_ form ...) (begin form ...))))
     ;; (degeneracy-check <expression> ...): evaluated, for effect
     (define-syntax degeneracy-check
       (syntax-rules () ((_ e ...) (begin e ... (if #f #f)))))
     ;; (degeneracy-active): #t, so a row can show it ran under the switch
     (define-syntax degeneracy-active
       (syntax-rules () ((_) #t))))
    (else
     (define-syntax degeneracy-definitions
       (syntax-rules () ((_ form ...) (begin))))
     (define-syntax degeneracy-check
       (syntax-rules () ((_ e ...) (if #f #f))))
     (define-syntax degeneracy-active
       (syntax-rules () ((_) #f)))))
)
