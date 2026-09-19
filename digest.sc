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

;;; (theourgia digest) -- SHA-256 (FIPS 180-4), and bytes as lower-case hex.
;;;
;;; THIS IS A FACADE, AND ONE OF EXACTLY THREE. Every use this core makes
;;; of igropyr goes through a library of its own that re-exports what the
;;; core actually uses, so the dependency has a single seam: replacing
;;; igropyr, or vendoring it again, changes these three files and nothing
;;; else. `test/facades.sexp` names them and two gates read that list.
;;;
;;; THIS FILE IS WHY THE RULE IS WORTH HAVING. Thirty-four libraries and
;;; fixtures import `(theourgia digest)`; when the two definitions below
;;; moved from a vendored copy to igropyr and back again, none of those
;;; thirty-four changed by a character.
;;;
;;; THE EXPORT LIST IS THE MEASURABLE ANSWER to "what does this core use
;;; igropyr's crypto for". It is two names, not the ten `(igropyr crypto)`
;;; offers: re-exporting the library wholesale would make the list say
;;; nothing, and would quietly widen what a replacement has to provide.
;;; `sha1`, the HMACs, PBKDF2 and the base64 family are deliberately not
;;; here -- base64 is in `wire.sc`, where the codec that needs it lives.

(library (theourgia digest)
  (export sha256 bytevector->hex)
  (import (only (igropyr crypto) sha256 bytevector->hex)))
