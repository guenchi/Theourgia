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

;;; (theourgia json) -- reading and writing JSON, for the one place this
;;; core speaks it.
;;;
;;; THIS IS A FACADE, and `test/facades.sexp` names it with the others.
;;; Every use this core makes of igropyr goes through one, so replacing
;;; the dependency changes these files and no caller.
;;;
;;; NEVER: FOUR NAMES, NOT THE SEVENTEEN `(igropyr json)` OFFERS. The core
;;; speaks JSON in exactly one place -- the MCP shell, where JSON is the
;;; JSON-RPC envelope and the payload inside it is S-expression text
;;; (§7.6.4, §7.6.37). Reading a frame and writing a reply needs to parse,
;;; to print, and to look things up; it never needs to BUILD one
;;; structurally, so `json-set`, `json-drop`, `json-push`, `json-insert`
;;; and `json-update` with their starred forms are deliberately absent.
;;;
;;; NOTE: THE LIST IS THE MEASURABLE ANSWER to "what does this core use
;;; igropyr's JSON for", and re-exporting wholesale would make it say
;;; nothing while quietly widening what a replacement has to provide.
;;; `test/facade-exports.sexp` pins these four literally, so widening
;;; this list is a change somebody has to make on purpose.

;;; NOTE: THE TWO ACCESSORS ARE NOT A PAIR OF THE SAME THING, and their
;;; names suggest otherwise. `json-ref` is a MACRO over a written-out
;;; path -- `(json-ref x "a" 1 "b")` -- and `json-ref*` is a PROCEDURE
;;; taking ONE step, `(json-ref* x k [absent])`. Measured by getting it
;;; wrong: `(json-ref* d "a" 1 "b")` is an arity error, not a deep
;;; lookup. Both are here because the shell needs both shapes.

;;; KEY: THE REPRESENTATION, MEASURED RATHER THAN GUESSED, because the two
;;; empties are exactly where a guess goes wrong:
;;;
;;;   object   an alist of (key . value); the EMPTY object is `()`
;;;   array    a VECTOR; the empty array is `#()`
;;;   null     the symbol `null`
;;;   true     `#t`
;;;
;;; So `()` and `#()` are distinguishable, and a check that accepted `()`
;;; for an array would accept an object where a list of arguments
;;; belongs. Measured by getting it wrong: the shell read `argv` as a
;;; list and refused every legal call with -32602.
;;;
;;; NOTE: `json->string` WRITES RAW UTF-8, not `\uXXXX`. Both are valid JSON
;;; and decode to the same string; a consumer comparing bytes against
;;; another writer's output has to know which one it is looking at.

(library (theourgia json)
  (export string->json json->string json-ref json-ref*)
  (import (only (igropyr json) string->json json->string json-ref json-ref*)))
