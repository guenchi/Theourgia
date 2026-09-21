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

;; HOW A FIXTURE RENDERS SOMETHING THAT WAS RAISED.
;;
;; INCLUDED, NOT IMPORTED. `include` resolves against the working directory,
;; and fixtures are run from this directory -- that is how `run-fixtures.sh`
;; invokes them and what `RUN.md` tells a person to do. A fixture run from
;; somewhere else fails to expand, loudly, which is the failure mode to
;; prefer: the alternative was a copy of this renderer in every fixture, and
;; a copy is a second renderer that drifts in silence.
;;
;; THE DEFECT IT REPAIRS. Fixtures used to read a raised condition with
;; `condition-message` alone:
;;
;;     (RAISED "variable ~:s is not bound")
;;
;; The message is a TEMPLATE. Its directives are filled from the IRRITANTS,
;; which that reading threw away -- so the one fact the failure carried,
;; WHICH variable, never reached the person reading the row. The same shape
;; produced `(RAISED "improper list ~s")` with no list in it.
;;
;; `display-condition` is the one that does not drop them. Measured on four
;; real conditions: where it can substitute it does (`~s is not a pair` ->
;; `() is not a pair`), and where it cannot it appends them instead:
;;
;;     variable ~:s is not bound with irritant zork
;;
;; -- so an unsubstituted directive can no longer cost a reading its facts.
;; It also answers for a raise that is not a condition at all, which the
;; message-only reading rendered as the bare object.

(define (condition->text e)
  (let ((p (open-output-string)))
    (display-condition e p)
    (get-output-string p)))

;; WHICH PART OF THE ROW BROKE.
;;
;; A row whose assertion has three parts used to answer with ONE `RAISED`,
;; and two different failures in two different parts read the same. `part`
;; names a piece of an assertion, so the reading says where it stopped
;; rather than only that it stopped.
;;
;; It catches per part ON PURPOSE: the other parts still run and still
;; report, so one broken part does not hide the answers of the two beside
;; it. A row that wants the whole thing to stop at the first failure should
;; use `caught` around the whole expression instead.
(define-syntax part
  (syntax-rules ()
    ((_ name e)
     (guard (c (#t (list 'RAISED-IN name (condition->text c)))) e))))
