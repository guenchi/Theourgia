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

(import (chezscheme) (theourgia rpc))
(define args (cdr (command-line)))
;; NO ARGUMENTS IS A USAGE LINE, NOT A CRASH. Every script in this
;; directory is run by the runner, and a script that needs arguments is
;; told apart from a fixture that failed by the usage line it prints:
;; without one this died in `car` at rc=255 and was counted red.
(when (< (length args) 3)
  (printf "usage: working-fault-child <store> <block-id> <writer>\n")
  (exit 0))
;; ⭐ THE WRITER COMES FROM THE PARENT, NOT FROM A DEFAULT HERE. A draft
;; verb with no writer named is refused outright, and the refusal the
;; exit status below looks for is `working-unavailable` -- so a child
;; that guessed wrong would exit non-zero for a reason that has nothing
;; to do with the fault it was sent to trigger.
(define answer
  (rpc-dispatch (car args)
                (list 'write (cadr args) "not-durable" "--writer" (caddr args))
                "test"))
(write answer)
(newline)
;; THE EXIT STATUS NAMES THE REFUSAL, NOT MERELY ITS KIND. `(eq? (car
;; answer) 'error)` was true for any refusal at all, so a child that was
;; turned away for an unrelated reason -- a bad request, an invalid
;; writer -- exited 0 and the parent read it as "the write was stopped
;; where the environment said it would be". An unrelated refusal also
;; leaves the old envelope on disk, so the rows after it agree.
(exit (if (and (pair? answer) (eq? (car answer) 'error)
               (pair? (cdr answer)) (eq? (cadr answer) 'working-unavailable))
          0 1))
